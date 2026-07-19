###############################################################################
# 01b_prep_estimates.R — Prepare the "Impact Estimates" long-form table
#
# One row per (project, site_subgroup, outcome_type, time_point). Each row's
# time_point is the midpoint (in years since random assignment) of the window
# the source reports — e.g. 0.5 = middle of follow-up year 1.
#
# Steps:
#   1. Read the Impact Estimates sheet from the working extraction xlsx.
#   2. Decode HTML entities (same helper as 01_prep.R) so project / site
#      strings join cleanly.
#   3. Left-join experiment-level covariates from processed_data.rds.
#   4. Impute missing earnings SEs from significance stars (same convention
#      as 01_prep.R §3).
#   5. Annualize earnings impact, SE, and control_mean using the `cadence`
#      column (the window length in years each estimate covers): divide by
#      cadence so every earnings figure is an annual rate.
#   6. Inflate annualized earnings impact, SE, and control_mean to 2025 dollars
#      using the same CPI table as 01_prep.R §5. Dollar-year priority:
#        a. "XXXX dollars" or "XXXX$" mentioned in the notes;
#        b. otherwise "calendar year XXXX" in the notes;
#        c. otherwise randomization_midpoint + time_point (nominal-dollar
#           default, matching 01_prep.R's extract_dollar_year).
#
# Employment impacts are percentage-point stocks ("% employed during window"),
# not flows, so they are neither annualized nor inflation-adjusted.
###############################################################################

options(error = function() {
  bar <- strrep("!", 78)
  cat("\n", bar, "\n", sep = "")
  cat("!!! SCRIPT FAILED — ", geterrmessage(), sep = "")
  cat(bar, "\n\n", sep = "")
  if (!interactive()) quit(status = 1, save = "no")
})

library(openxlsx)

XLSX <- "data/extraction_full_v62.xlsx"
ie <- read.xlsx(XLSX, sheet = "Impact Estimates")

# ── 1. Decode HTML entities (mirrors 01_prep.R) ───────────────────────────────
decode_html_entities <- function(x) {
  if (!is.character(x)) return(x)
  matches <- regmatches(x, gregexpr("&#\\d+;", x))
  for (i in seq_along(x)) {
    if (length(matches[[i]]) == 0) next
    for (ent in unique(matches[[i]])) {
      num <- as.integer(sub("&#(\\d+);", "\\1", ent))
      x[i] <- gsub(ent, intToUtf8(num), x[i], fixed = TRUE)
    }
  }
  matches <- regmatches(x, gregexpr("&#x[0-9A-Fa-f]+;", x))
  for (i in seq_along(x)) {
    if (length(matches[[i]]) == 0) next
    for (ent in unique(matches[[i]])) {
      num <- strtoi(sub("&#x([0-9A-Fa-f]+);", "\\1", ent), base = 16)
      x[i] <- gsub(ent, intToUtf8(num), x[i], fixed = TRUE)
    }
  }
  x <- gsub("&lt;",   "<",  x, fixed = TRUE)
  x <- gsub("&gt;",   ">",  x, fixed = TRUE)
  x <- gsub("&quot;", "\"", x, fixed = TRUE)
  x <- gsub("&apos;", "'",  x, fixed = TRUE)
  x <- gsub("&amp;",  "&",  x, fixed = TRUE)
  x
}
# Strip leaked `xml:space="preserve">` prefixes (mirrors 01_prep.R) — 40 cells
# in the `notes` column are affected, which §5 parses for the dollar year.
strip_xml_leakage <- function(x) {
  if (!is.character(x)) return(x)
  trimws(sub('xml:space="preserve">', "", x, fixed = TRUE))
}

ie <- as.data.frame(lapply(ie, function(x) strip_xml_leakage(decode_html_entities(x))),
                    stringsAsFactors = FALSE)

cat("Raw Impact Estimates:", nrow(ie), "rows x", ncol(ie), "cols\n")

# ── 2. Left-join experiment-level covariates ─────────────────────────────────
dat <- readRDS("data/processed_data.rds")

# Sanity: (project, site_subgroup) is unique within dat
key_dup <- duplicated(dat[, c("project", "site_subgroup")])
stopifnot("dat has duplicate (project, site_subgroup) keys" = !any(key_dup))

join_cols <- c(
  "project", "site_subgroup", "short_name",
  "randomization_midpoint", "target_pop",
  "training_role", "mandatory_voluntary", "scaled",
  "census_region", "geo_type",
  "cost_per_treated", "cost_per_treated_k"
)
join_cols <- intersect(join_cols, names(dat))
ie <- merge(ie, dat[, join_cols],
            by = c("project", "site_subgroup"),
            all.x = TRUE, all.y = FALSE, sort = FALSE)

unmatched <- unique(ie[is.na(ie$randomization_midpoint),
                       c("project", "site_subgroup")])
if (nrow(unmatched) > 0) {
  cat("WARNING: Impact Estimates rows with no Data Table match:\n")
  print(unmatched)
} else {
  cat("Join: all Impact Estimates rows matched a Data Table row.\n")
}

# ── 3. Impute earnings SEs from significance stars ────────────────────────────
stars_to_p <- function(stars) {
  ifelse(is.na(stars) | stars == "",  0.55,
  ifelse(stars == "*",                0.075,
  ifelse(stars == "**",               0.03,
  ifelse(stars == "***",              0.01, NA_real_))))
}

impute_se_from_stars <- function(impact, stars) {
  p  <- stars_to_p(stars)
  z  <- qnorm(1 - p / 2)
  abs(impact) / z
}

ie$se_source <- ifelse(is.na(ie$impact), NA_character_,
                ifelse(!is.na(ie$se),    "reported",
                                         "missing"))

is_earn <- !is.na(ie$outcome_type) & ie$outcome_type == "earnings"
need_se <- is_earn & is.na(ie$se) & !is.na(ie$impact)
ie$se[need_se] <- impute_se_from_stars(ie$impact[need_se], ie$stars[need_se])
ie$se_source[need_se] <- "imputed_from_stars"

near_zero <- need_se & abs(ie$impact) < 1
ie$se_source[near_zero] <- "imputed_from_stars_CAUTION_near_zero"

cat(sprintf("Earnings SE: %d reported, %d imputed (%d near-zero), %d earnings rows\n",
            sum(is_earn & ie$se_source == "reported", na.rm = TRUE),
            sum(need_se, na.rm = TRUE),
            sum(near_zero, na.rm = TRUE),
            sum(is_earn)))

# ── 4. Annualize earnings (earnings only) ────────────────────────────────────
#
# `cadence` is the length, in years, of the window each estimate covers
# (e.g. 0.25 = quarterly, 1 = annual, 2 = two-year cumulative). Dividing the
# impact, SE, and control mean by cadence puts every earnings figure on a
# common annual-rate basis.

stopifnot("Impact Estimates sheet lacks a `cadence` column" =
            "cadence" %in% names(ie))
ie$cadence <- as.numeric(ie$cadence)

ann_mask <- is_earn & !is.na(ie$cadence) & ie$cadence > 0
ie$impact[ann_mask] <- ie$impact[ann_mask] / ie$cadence[ann_mask]
have_se <- ann_mask & !is.na(ie$se)
ie$se[have_se] <- ie$se[have_se] / ie$cadence[have_se]
have_cm <- ann_mask & !is.na(ie$control_mean)
ie$control_mean[have_cm] <- ie$control_mean[have_cm] / ie$cadence[have_cm]

n_earn_imp     <- sum(is_earn & !is.na(ie$impact))
n_cadence_miss <- sum(is_earn & !is.na(ie$impact) &
                      (is.na(ie$cadence) | ie$cadence <= 0))
cat(sprintf("Earnings annualized: %d of %d earnings rows (%d missing/invalid cadence)\n",
            sum(ann_mask & !is.na(ie$impact)), n_earn_imp, n_cadence_miss))

# ── 5. Inflation adjustment to 2025 dollars (earnings only) ──────────────────
# CPI-U annual averages (BLS, 1982-84 = 100). Source: 01_prep.R §5.
cpi <- c(
  "1965" = 31.5,  "1966" = 32.4,  "1967" = 33.4,  "1968" = 34.8,
  "1969" = 36.7,  "1970" = 38.8,  "1971" = 40.5,  "1972" = 41.8,
  "1973" = 44.4,  "1974" = 49.3,
  "1975" = 53.8,  "1976" = 56.9,  "1977" = 60.6,  "1978" = 65.2,
  "1979" = 72.6,  "1980" = 82.4,  "1981" = 90.9,  "1982" = 96.5,
  "1983" = 99.6,  "1984" = 103.9, "1985" = 107.6, "1986" = 109.6,
  "1987" = 113.6, "1988" = 118.3, "1989" = 124.0, "1990" = 130.7,
  "1991" = 136.2, "1992" = 140.3, "1993" = 144.5, "1994" = 148.2,
  "1995" = 152.4, "1996" = 156.9, "1997" = 160.5, "1998" = 163.0,
  "1999" = 166.6, "2000" = 172.2, "2001" = 177.1, "2002" = 179.9,
  "2003" = 184.0, "2004" = 188.9, "2005" = 195.3, "2006" = 201.6,
  "2007" = 207.3, "2008" = 215.3, "2009" = 214.5, "2010" = 218.1,
  "2011" = 224.9, "2012" = 229.6, "2013" = 233.0, "2014" = 236.7,
  "2015" = 237.0, "2016" = 240.0, "2017" = 245.1, "2018" = 251.1,
  "2019" = 255.7, "2020" = 258.8, "2021" = 271.0, "2022" = 292.7,
  "2023" = 304.7, "2024" = 313.7, "2025" = 321.9
)
cpi_2025 <- cpi[["2025"]]

get_cpi <- function(year) {
  if (is.na(year)) return(NA_real_)
  yr_floor <- floor(year); yr_ceil <- ceiling(year)
  frac     <- year - yr_floor
  cpi_lo   <- cpi[[as.character(yr_floor)]]
  cpi_hi   <- cpi[[as.character(yr_ceil)]]
  if (is.null(cpi_lo) || is.null(cpi_hi)) return(NA_real_)
  cpi_lo + frac * (cpi_hi - cpi_lo)
}

# Pull dollar-year from notes when stated; else use randomization midpoint +
# time_point (nominal-dollar default, mirrors 01_prep.R extract_dollar_year).
extract_dollar_year_note <- function(note, midpoint, time_pt) {
  if (!is.na(note)) {
    s <- tolower(note)
    m <- regmatches(s, regexpr("\\d{4}\\s*dollars", s))
    if (length(m) == 1 && nchar(m) > 0) {
      return(as.numeric(sub("\\D.*$", "", m)))
    }
    m <- regmatches(s, regexpr("\\d{4}\\$", s))
    if (length(m) == 1 && nchar(m) > 0) {
      return(as.numeric(sub("\\$", "", m)))
    }
    m <- regmatches(s, regexpr("calendar year \\d{4}", s))
    if (length(m) == 1 && nchar(m) > 0) {
      return(as.numeric(sub("calendar year ", "", m)))
    }
  }
  if (!is.na(midpoint) && !is.na(time_pt)) return(midpoint + time_pt)
  NA_real_
}

ie$dollar_year <- mapply(extract_dollar_year_note,
                         ie$notes, ie$randomization_midpoint, ie$time_point)
ie$cpi_factor  <- sapply(ie$dollar_year, function(dy) {
  if (is.na(dy)) return(NA_real_)
  cpi_dy <- get_cpi(dy)
  if (is.na(cpi_dy)) return(NA_real_)
  cpi_2025 / cpi_dy
})

infl_mask <- is_earn & !is.na(ie$cpi_factor) & !is.na(ie$impact)
ie$impact[infl_mask] <- ie$impact[infl_mask] * ie$cpi_factor[infl_mask]
have_se <- infl_mask & !is.na(ie$se)
ie$se[have_se] <- ie$se[have_se] * ie$cpi_factor[have_se]
have_cm <- infl_mask & !is.na(ie$control_mean)
ie$control_mean[have_cm] <- ie$control_mean[have_cm] * ie$cpi_factor[have_cm]

cat(sprintf("Earnings inflation-adjusted: %d of %d earnings rows (%d missing dollar-year)\n",
            sum(infl_mask),
            sum(is_earn & !is.na(ie$impact)),
            sum(is_earn & !is.na(ie$impact) & is.na(ie$cpi_factor))))

# ── 6. Diagnostics + save ────────────────────────────────────────────────────
cat("\n=== Outcome × time_point coverage ===\n")
print(table(ie$outcome_type, useNA = "always"))
cat("\nTime_point range:\n")
print(summary(ie$time_point))

cat("\n=== SE source summary (earnings rows) ===\n")
print(table(ie$se_source[is_earn], useNA = "always"))

saveRDS(ie, "data/impact_estimates.rds")
cat("\nSaved data/impact_estimates.rds (", nrow(ie), " rows)\n", sep = "")
