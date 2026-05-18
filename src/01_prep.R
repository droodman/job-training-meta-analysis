###############################################################################
# 01_prep.R — Read extraction_full.xlsx and prepare for meta-analysis
#
# Two SE strategies:
#   1. Employment (binary): compute SE from proportions and sample sizes
#   2. Earnings (continuous): impute SE from significance stars following
#      Smedslund et al. 2006 (Campbell), p. 25
###############################################################################

# Make errors impossible to miss: print a loud banner and (non-interactively)
# exit with non-zero status so the failure propagates to the shell.
options(error = function() {
  bar <- strrep("!", 78)
  cat("\n", bar, "\n", sep = "")
  cat("!!! SCRIPT FAILED — ", geterrmessage(), sep = "")
  cat(bar, "\n\n", sep = "")
  if (!interactive()) quit(status = 1, save = "no")
})

library(openxlsx)

# ── 1. Read data ─────────────────────────────────────────────────────────────
#
# Using openxlsx rather than readxl: readxl returns NA when a formula cell
# has no cached value, which has bitten us before (e.g. JTPA Year-5
# employment impacts entered as formulas with no cache).

dat <- read.xlsx("data/extraction_full_v18.xlsx", sheet = "Data Table")
reasoning <- read.xlsx("data/extraction_full_v18.xlsx", sheet = "Reasoning Table")

# openxlsx does not auto-decode XML/HTML entities — strings like "PACE &#8211;
# Year Up" or "GAIN Education &amp; Training" arrive with the entity literals
# intact, which then fail to match project lists below. Decode here.
decode_html_entities <- function(x) {
  if (!is.character(x)) return(x)
  # Numeric decimal references: &#NNNN;
  matches <- regmatches(x, gregexpr("&#\\d+;", x))
  for (i in seq_along(x)) {
    if (length(matches[[i]]) == 0) next
    for (ent in unique(matches[[i]])) {
      num <- as.integer(sub("&#(\\d+);", "\\1", ent))
      x[i] <- gsub(ent, intToUtf8(num), x[i], fixed = TRUE)
    }
  }
  # Hex references: &#xHHHH;
  matches <- regmatches(x, gregexpr("&#x[0-9A-Fa-f]+;", x))
  for (i in seq_along(x)) {
    if (length(matches[[i]]) == 0) next
    for (ent in unique(matches[[i]])) {
      num <- strtoi(sub("&#x([0-9A-Fa-f]+);", "\\1", ent), base = 16)
      x[i] <- gsub(ent, intToUtf8(num), x[i], fixed = TRUE)
    }
  }
  # Named entities — decode &amp; LAST so "&amp;lt;" stays as "&lt;"
  x <- gsub("&lt;",   "<",  x, fixed = TRUE)
  x <- gsub("&gt;",   ">",  x, fixed = TRUE)
  x <- gsub("&quot;", "\"", x, fixed = TRUE)
  x <- gsub("&apos;", "'",  x, fixed = TRUE)
  x <- gsub("&amp;",  "&",  x, fixed = TRUE)
  x
}
dat       <- as.data.frame(lapply(dat,       decode_html_entities), stringsAsFactors = FALSE)
reasoning <- as.data.frame(lapply(reasoning, decode_html_entities), stringsAsFactors = FALSE)

cat("Raw data:", nrow(dat), "rows x", ncol(dat), "cols\n")

# ── 2. Compute employment SEs ────────────────────────────────────────────────
#
# Employment impact is a percentage-point difference: impact = p_T - p_C
# (both expressed as percentages, 0–100).
#
# SE(p_T - p_C) = sqrt[ p_T(1-p_T)/n_T + p_C(1-p_C)/n_C ]
#   where p_T and p_C are proportions (0–1).
# Multiply by 100 to get SE in percentage-point units matching the impact.

compute_emp_se <- function(impact_ppt, control_mean_pct, n_t, n_c) {
  # Convert from percentages to proportions
  p_c <- control_mean_pct / 100
  p_t <- p_c + impact_ppt / 100

  # Agresti-Caffo small-sample correction: add 1 pseudo-success and
  # 1 pseudo-failure to each group before computing variance
  x_t <- p_t * n_t   # implied successes
  x_c <- p_c * n_c
  p_t_ac <- (x_t + 1) / (n_t + 2)
  p_c_ac <- (x_c + 1) / (n_c + 2)

  se_prop <- sqrt(p_t_ac * (1 - p_t_ac) / (n_t + 2) +
                  p_c_ac * (1 - p_c_ac) / (n_c + 2))
  se_ppt  <- se_prop * 100
  return(se_ppt)
}

for (prefix in c("st", "mt", "lt")) {
  imp_col  <- paste0(prefix, "_emp_impact")
  se_col   <- paste0(prefix, "_emp_se")
  cm_col   <- paste0(prefix, "_emp_control_mean")
  src_col  <- paste0(prefix, "_emp_se_source")  # track provenance

  # Initialize source tracker
  dat[[src_col]] <- ifelse(is.na(dat[[imp_col]]), NA_character_, "missing")

  # Where SE is already reported, mark it
  has_se <- !is.na(dat[[se_col]])
  dat[[src_col]][has_se] <- "reported"

  # Where SE is missing but we can compute it
  can_compute <- is.na(dat[[se_col]]) &
    !is.na(dat[[imp_col]]) &
    !is.na(dat[[cm_col]]) &
    !is.na(dat$n_treatment) &
    !is.na(dat$n_control)

  dat[[se_col]][can_compute] <- compute_emp_se(
    dat[[imp_col]][can_compute],
    dat[[cm_col]][can_compute],
    dat$n_treatment[can_compute],
    dat$n_control[can_compute]
  )
  dat[[src_col]][can_compute] <- "computed_from_proportions"

  n_reported <- sum(has_se, na.rm = TRUE)
  n_computed <- sum(can_compute, na.rm = TRUE)
  n_impact   <- sum(!is.na(dat[[imp_col]]))
  n_still_missing <- n_impact - n_reported - n_computed
  cat(sprintf("%s employment SE: %d reported, %d computed, %d still missing (of %d impacts)\n",
              toupper(prefix), n_reported, n_computed, n_still_missing, n_impact))
}

# ── 2b. Compute uptake SEs ───────────────────────────────────────────────────
#
# Uptake of evaluated program and uptake of any training are binary outcomes
# like employment, but without time-horizon prefixes. Compute SEs from
# proportions using the same Agresti-Caffo approach.

for (outcome in c("program_uptake", "any_training")) {
  imp_col  <- paste0(outcome, "_impact")
  se_col   <- paste0(outcome, "_se")
  cm_col   <- paste0(outcome, "_control_mean")
  src_col  <- paste0(outcome, "_se_source")

  # Initialize SE and source columns
  dat[[se_col]]  <- NA_real_
  dat[[src_col]] <- ifelse(is.na(dat[[imp_col]]), NA_character_, "missing")

  # Compute SE from proportions where possible
  can_compute <- !is.na(dat[[imp_col]]) &
    !is.na(dat[[cm_col]]) &
    !is.na(dat$n_treatment) &
    !is.na(dat$n_control)

  dat[[se_col]][can_compute] <- compute_emp_se(
    dat[[imp_col]][can_compute],
    dat[[cm_col]][can_compute],
    dat$n_treatment[can_compute],
    dat$n_control[can_compute]
  )
  dat[[src_col]][can_compute] <- "computed_from_proportions"

  n_computed <- sum(can_compute, na.rm = TRUE)
  n_impact   <- sum(!is.na(dat[[imp_col]]))
  n_still_missing <- n_impact - n_computed
  cat(sprintf("%s SE: %d computed, %d still missing (of %d impacts)\n",
              outcome, n_computed, n_still_missing, n_impact))
}

# ── 3. Impute earnings SEs from significance stars ───────────────────────────
#
# Greenberg, Michalopoulos, and Robins (2006), note 9:
#   Not significant → p = 0.55
#   * (p < 0.10)   → p = 0.075
#   ** (p < 0.05)  → p = 0.03
#   *** (p < 0.01) → p = 0.01
#
# SE = |impact| / qnorm(1 - p/2)
#
# NOTE: When impact ≈ 0 and result is not significant, this gives SE ≈ 0,
#       which is nonsensical. We flag these cases.

stars_to_p <- function(stars) {
  ifelse(is.na(stars) | stars == "",  0.55,
  ifelse(stars == "*",                0.075,
  ifelse(stars == "**",              0.03,
  ifelse(stars == "***",             0.01, NA_real_))))
}

impute_se_from_stars <- function(impact, stars) {
  p  <- stars_to_p(stars)
  z  <- qnorm(1 - p / 2)
  se <- abs(impact) / z
  return(se)
}

for (prefix in c("st", "mt", "lt")) {
  imp_col   <- paste0(prefix, "_earn_impact")
  se_col    <- paste0(prefix, "_earn_se")
  star_col  <- paste0(prefix, "_earn_stars")
  src_col   <- paste0(prefix, "_earn_se_source")

  dat[[src_col]] <- ifelse(is.na(dat[[imp_col]]), NA_character_, "missing")

  # Where SE is already reported
  has_se <- !is.na(dat[[se_col]])
  dat[[src_col]][has_se] <- "reported"

  # Where SE is missing but we have an impact to impute from
  can_impute <- is.na(dat[[se_col]]) & !is.na(dat[[imp_col]])

  dat[[se_col]][can_impute] <- impute_se_from_stars(
    dat[[imp_col]][can_impute],
    dat[[star_col]][can_impute]
  )
  dat[[src_col]][can_impute] <- "imputed_from_stars"

  # Flag zero/near-zero impacts where SE imputation is unreliable
  near_zero <- can_impute & abs(dat[[imp_col]]) < 1
  dat[[src_col]][near_zero] <- "imputed_from_stars_CAUTION_near_zero"

  n_reported <- sum(has_se, na.rm = TRUE)
  n_imputed  <- sum(can_impute, na.rm = TRUE)
  n_caution  <- sum(near_zero, na.rm = TRUE)
  n_impact   <- sum(!is.na(dat[[imp_col]]))
  cat(sprintf("%s earnings SE: %d reported, %d imputed (%d near-zero caution), %d total impacts\n",
              toupper(prefix), n_reported, n_imputed, n_caution, n_impact))
}

# ── 4. Annualize earnings ────────────────────────────────────────────────────
#
# Earnings impacts are reported in various cadences (weekly, monthly,
# quarterly, annual, cumulative over N months/quarters). Convert all to
# annual rate by multiplying impact and SE by an annualization factor.
#
# Strategy: parse cadence strings into a multiplier.
#   weekly  → ×52    monthly → ×12    quarterly → ×4    annual → ×1
#   cumulative over N months → ×(12/N)
#   cumulative over N quarters → ×(4/N)
#   biennial / cumulative over 2 years → ×0.5

get_annual_multiplier <- function(cadence) {
  if (is.na(cadence)) return(NA_real_)
  cad <- tolower(cadence)

  # Already annual (various flavors)
  if (grepl("^annual", cad)) return(1)

  # Weekly
  if (grepl("^weekly", cad)) return(52)

  # Monthly
  if (grepl("^monthly", cad)) return(12)

  # Quarterly
  if (grepl("^quarterly", cad)) return(4)

  # Biennial
  if (grepl("^biennial", cad)) return(0.5)

  # "cumulative over N months" or "cumulative over months X-Y" (compute span)
  m <- regmatches(cad, regexpr("cumulative over (\\d+) months", cad))
  if (length(m) == 1 && nchar(m) > 0) {
    n_months <- as.numeric(sub("cumulative over (\\d+) months.*", "\\1", m))
    return(12 / n_months)
  }

  # "cumulative over months X-Y (...)" — compute span from range
  m <- regmatches(cad, regexpr("cumulative over months (\\d+)-(\\d+)", cad))
  if (length(m) == 1 && nchar(m) > 0) {
    nums <- as.numeric(unlist(regmatches(m, gregexpr("\\d+", m))))
    n_months <- nums[2] - nums[1] + 1
    return(12 / n_months)
  }

  # "(Z months)" as fallback for other cumulative patterns
  m <- regmatches(cad, regexpr("\\((\\d+) months\\)", cad))
  if (length(m) == 1 && nchar(m) > 0) {
    n_months <- as.numeric(gsub("[^0-9]", "", m))
    return(12 / n_months)
  }

  # "cumulative over quarters X-Y" — count quarters from range
  m <- regmatches(cad, regexpr("quarters (\\d+)-(\\d+)", cad))
  if (length(m) == 1 && nchar(m) > 0) {
    nums <- as.numeric(unlist(regmatches(m, gregexpr("\\d+", m))))
    n_quarters <- nums[2] - nums[1] + 1
    return(4 / n_quarters)
  }

  # "cumulative over Q5-Q6" style
  m <- regmatches(cad, regexpr("q(\\d+)-q(\\d+)", cad))
  if (length(m) == 1 && nchar(m) > 0) {
    nums <- as.numeric(unlist(regmatches(m, gregexpr("\\d+", m))))
    n_quarters <- nums[2] - nums[1] + 1
    return(4 / n_quarters)
  }

  # "cumulative over 5 years"
  m <- regmatches(cad, regexpr("cumulative over (\\d+) year", cad))
  if (length(m) == 1 && nchar(m) > 0) {
    n_years <- as.numeric(sub("cumulative over (\\d+) year.*", "\\1", m))
    return(1 / n_years)
  }

  # "cumulative over Years 2-3 (8 quarters)"
  m <- regmatches(cad, regexpr("(\\d+) quarters\\)", cad))
  if (length(m) == 1 && nchar(m) > 0) {
    n_quarters <- as.numeric(sub("(\\d+) quarters.*", "\\1", m))
    return(4 / n_quarters)
  }

  warning("Unrecognized cadence: ", cadence)
  return(NA_real_)
}

for (prefix in c("st", "mt", "lt")) {
  cad_col <- paste0(prefix, "_earn_cadence")
  imp_col <- paste0(prefix, "_earn_impact")
  se_col  <- paste0(prefix, "_earn_se")
  cm_col  <- paste0(prefix, "_earn_control_mean")
  mult_col <- paste0(prefix, "_earn_annual_multiplier")

  dat[[mult_col]] <- sapply(dat[[cad_col]], get_annual_multiplier)

  # Annualize impact, SE, and control mean
  has_mult <- !is.na(dat[[mult_col]]) & !is.na(dat[[imp_col]])
  dat[[imp_col]][has_mult] <- dat[[imp_col]][has_mult] * dat[[mult_col]][has_mult]
  dat[[se_col]][has_mult]  <- dat[[se_col]][has_mult]  * dat[[mult_col]][has_mult]
  if (cm_col %in% names(dat)) {
    has_cm <- has_mult & !is.na(dat[[cm_col]])
    dat[[cm_col]][has_cm] <- dat[[cm_col]][has_cm] * dat[[mult_col]][has_cm]
  }

  n_converted <- sum(has_mult & dat[[mult_col]] != 1, na.rm = TRUE)
  n_already   <- sum(has_mult & dat[[mult_col]] == 1, na.rm = TRUE)
  n_failed    <- sum(!is.na(dat[[imp_col]]) & is.na(dat[[mult_col]]))
  cat(sprintf("%s earnings: %d already annual, %d converted, %d unrecognized cadence\n",
              toupper(prefix), n_already, n_converted, n_failed))
}

# ── 5. Inflation adjustment to 2025 dollars ──────────────────────────────────
#
# CPI-U annual averages (BLS, 1982-84 = 100).
# Source: Minneapolis Fed / BLS. https://www.minneapolisfed.org/about-us/monetary-policy/inflation-calculator/consumer-price-index-1913-, viewed April 2, 2026

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

# Interpolate CPI for fractional years (linear between annual averages)
get_cpi <- function(year) {
  if (is.na(year)) return(NA_real_)
  yr_floor <- floor(year)
  yr_ceil  <- ceiling(year)
  frac     <- year - yr_floor
  cpi_lo   <- cpi[[as.character(yr_floor)]]
  cpi_hi   <- cpi[[as.character(yr_ceil)]]
  if (is.null(cpi_lo) || is.null(cpi_hi)) return(NA_real_)
  return(cpi_lo + frac * (cpi_hi - cpi_lo))
}

# Determine the dollar-year for each observation.
# Priority:
#   1. If cadence text states an explicit dollar year (e.g. "1986 dollars",
#      "constant 2019 dollars", "2012 dollars"), use that.
#   2. If cadence says "calendar year XXXX", use XXXX.
#   3. Otherwise (nominal or unspecified), estimate as
#      randomization_midpoint + followup_years.

extract_dollar_year <- function(cadence, midpoint, followup_yrs) {
  if (!is.na(cadence)) {
    cad <- tolower(cadence)

    # "constant 2019 dollars", "1986 dollars", "December 1984 dollars",
    # "2012 dollars", "1985 dollars"
    m <- regmatches(cad, regexpr("(\\d{4}) dollars", cad))
    if (length(m) == 1 && nchar(m) > 0) {
      return(as.numeric(sub(" dollars", "", m)))
    }

    # "calendar year 1997", "calendar year 2003"
    m <- regmatches(cad, regexpr("calendar year (\\d{4})", cad))
    if (length(m) == 1 && nchar(m) > 0) {
      return(as.numeric(sub("calendar year ", "", m)))
    }
  }

  # Default: nominal dollars at time of measurement
  if (!is.na(midpoint) && !is.na(followup_yrs)) {
    return(midpoint + followup_yrs)
  }

  return(NA_real_)
}

for (prefix in c("st", "mt", "lt")) {
  cad_col   <- paste0(prefix, "_earn_cadence")
  imp_col   <- paste0(prefix, "_earn_impact")
  se_col    <- paste0(prefix, "_earn_se")
  cm_col    <- paste0(prefix, "_earn_control_mean")
  fu_col    <- paste0(prefix, "_followup_years")
  dy_col    <- paste0(prefix, "_dollar_year")
  infl_col  <- paste0(prefix, "_earn_cpi_factor")

  # Compute dollar year
  dat[[dy_col]] <- mapply(
    extract_dollar_year,
    dat[[cad_col]], dat$randomization_midpoint, dat[[fu_col]]
  )

  # Compute CPI adjustment factor
  dat[[infl_col]] <- sapply(dat[[dy_col]], function(dy) {
    if (is.na(dy)) return(NA_real_)
    cpi_dy <- get_cpi(dy)
    if (is.na(cpi_dy)) return(NA_real_)
    return(cpi_2025 / cpi_dy)
  })

  # Apply inflation adjustment to impact, SE, and control mean
  has_factor <- !is.na(dat[[infl_col]]) & !is.na(dat[[imp_col]])
  dat[[imp_col]][has_factor] <- dat[[imp_col]][has_factor] * dat[[infl_col]][has_factor]
  dat[[se_col]][has_factor]  <- dat[[se_col]][has_factor]  * dat[[infl_col]][has_factor]
  if (cm_col %in% names(dat)) {
    has_cm <- has_factor & !is.na(dat[[cm_col]])
    dat[[cm_col]][has_cm] <- dat[[cm_col]][has_cm] * dat[[infl_col]][has_cm]
  }

  n_adjusted <- sum(has_factor, na.rm = TRUE)
  n_missing  <- sum(!is.na(dat[[imp_col]]) & is.na(dat[[infl_col]]))
  cat(sprintf("%s earnings: %d inflation-adjusted to 2025$, %d missing dollar-year\n",
              toupper(prefix), n_adjusted, n_missing))
}

# Cost: assume incurred at the randomization midpoint
dat$cost_cpi_factor <- sapply(dat$randomization_midpoint, function(yr) {
  if (is.na(yr)) return(NA_real_)
  cpi_yr <- get_cpi(yr)
  if (is.na(cpi_yr)) return(NA_real_)
  cpi_2025 / cpi_yr
})
cost_has_factor <- !is.na(dat$cost_cpi_factor) & !is.na(dat$cost_per_treated)
cost_n_missing  <- sum(!is.na(dat$cost_per_treated) & is.na(dat$cost_cpi_factor))
dat$cost_per_treated[cost_has_factor] <-
  dat$cost_per_treated[cost_has_factor] * dat$cost_cpi_factor[cost_has_factor]
cat(sprintf("Cost: %d inflation-adjusted to 2025$, %d missing dollar-year\n",
            sum(cost_has_factor), cost_n_missing))

# Benefit: same adjustment, same dollar-year assumption
if ("benefit_per_treated" %in% names(dat)) {
  ben_has_factor <- !is.na(dat$cost_cpi_factor) & !is.na(dat$benefit_per_treated)
  ben_n_missing  <- sum(!is.na(dat$benefit_per_treated) & is.na(dat$cost_cpi_factor))
  dat$benefit_per_treated[ben_has_factor] <-
    dat$benefit_per_treated[ben_has_factor] * dat$cost_cpi_factor[ben_has_factor]
  cat(sprintf("Benefit: %d inflation-adjusted to 2025$, %d missing dollar-year\n",
              sum(ben_has_factor), ben_n_missing))
}

# ── 6. Recode outcome data source ────────────────────────────────────────────
#
# Binary: "admin" (UI wage records, NDNH, SSA, state admin) vs "survey"
# Separate variables for employment and earnings, since some studies use
# different sources for each (e.g., "UI wage records (earnings); survey (employment)").

classify_data_source <- function(src) {
  if (is.na(src)) return(c(emp = NA_character_, earn = NA_character_))
  s <- tolower(src)

  # Explicitly split source
  if (grepl("earnings.*survey.*employment|employment.*survey.*earnings", s) ||
      grepl("ui.*earnings.*survey.*employment", s)) {
    return(c(emp = "survey", earn = "admin"))
  }

  # Admin sources
  if (grepl("ui wage|ui quarterly|ndnh|administrative wage|ssa |state ui|dept of employment|workforce commission|indiana ui|minnesota ui|w-2/irs", s)) {
    return(c(emp = "admin", earn = "admin"))
  }

  # Combined survey + admin — UI records are primary for earnings
  if (grepl("survey.*ui wage|ui wage.*combined", s)) {
    return(c(emp = "admin", earn = "admin"))
  }

  # CA/WI UI + AFDC admin records
  if (grepl("ca ui|wi ui", s)) {
    return(c(emp = "admin", earn = "admin"))
  }

  # Survey sources
  if (grepl("self-report|survey|interview|staff records", s)) {
    return(c(emp = "survey", earn = "survey"))
  }

  warning("Unclassified data source: ", src)
  return(c(emp = NA_character_, earn = NA_character_))
}

source_coded <- t(sapply(dat$outcome_data_source, classify_data_source))
dat$emp_data_source  <- source_coded[, "emp"]
dat$earn_data_source <- source_coded[, "earn"]

cat("\n=== Employment data source ===\n")
print(table(dat$emp_data_source, useNA = "always"))
cat("\n=== Earnings data source ===\n")
print(table(dat$earn_data_source, useNA = "always"))

stopifnot("Unclassified emp_data_source rows" = sum(is.na(dat$emp_data_source)) == 0)
stopifnot("Unclassified earn_data_source rows" = sum(is.na(dat$earn_data_source)) == 0)

# ── 6a2. Code response rate (≥80% threshold; admin-source = high) ────────────
#
# For each (horizon, outcome) combination, classify a study as high or low
# response rate. Threshold = 80%. If the relevant outcome data source is
# administrative records, treat as high (admin records cover the universe).
# Uptake outcomes use short-term overall response rate, no admin override.

classify_resprate <- function(resprate, admin_override) {
  factor(ifelse(admin_override, "\u226580%",
         ifelse(is.na(resprate), NA_character_,
         ifelse(resprate >= 0.80, "\u226580%",
                "<80%"))),
         levels = c("\u226580%", "<80%"))
}

for (prefix in c("st", "mt", "lt")) {
  rr_col <- paste0(prefix, "_resprate_overall")
  for (oc_name in c("emp", "earn")) {
    ds_col      <- paste0(oc_name, "_data_source")
    label_col   <- paste0("resprate_label_", oc_name, "_", prefix)
    low_col     <- paste0("resprate_low_",   oc_name, "_", prefix)
    cont_col    <- paste0("resprate_",       oc_name, "_", prefix)
    admin_override <- !is.na(dat[[ds_col]]) & dat[[ds_col]] == "admin"
    dat[[label_col]] <- classify_resprate(dat[[rr_col]], admin_override)
    dat[[low_col]]   <- as.integer(dat[[label_col]] == "Low response rate (<80%)")
    # Continuous response rate in percentage points, with admin treated as
    # 100 (admin records cover the universe). Used as a continuous moderator
    # in the meta-regressions.
    dat[[cont_col]]  <- ifelse(admin_override, 100, dat[[rr_col]] * 100)
  }
}

dat$resprate_label_uptake <- classify_resprate(
  dat$st_resprate_overall, rep(FALSE, nrow(dat)))

cat("\n=== Response rate (employment, short-term) ===\n")
print(table(dat$resprate_label_emp_st, useNA = "always"))

# ── 6b. Code at-scale programs ───────────────────────────────────────────────
#
# "scaled" = 1 for programs that operated at scale (large federal programs
# evaluated as they normally ran, not small demonstration projects).

scaled_projects <- c(
  "National JTPA Study",
  "Food Stamp Employment and Training (FSET) Program",
  "National Job Corps Study",
  "American Conservation and Youth Service Corps (ACYSC)",
  "YouthBuild",
  "WIA Gold Standard"
)
dat$scaled <- as.integer(dat$project %in% scaled_projects)

cat("\n=== Scaled program ===\n")
print(table(dat$scaled, useNA = "always"))

# ── 7. Code target population ────────────────────────────────────────────────
#
# 8 categories reflecting the primary population served:
#   welfare            — Current AFDC/TANF recipients in welfare-to-work
#   youth              — Youth/young adults (≤24), incl. teen parents, dropouts
#   low_income_adult   — Low-income adults in voluntary training programs
#   justice_involved   — Ex-offenders, parolees
#   dislocated         — Displaced/dislocated workers
#   noncustodial_parent — Noncustodial parents with child support obligations
#   substance_abuse    — Ex-addicts
#   disability         — People with intellectual/developmental disabilities
#
# Coding rules are project-level defaults with site-level overrides for
# projects that span multiple populations (NSW, JTPA, ETJD, ERA, STED, WIA).

assign_target_pop <- function(project, site) {

  # ── Projects with uniform coding ──────────────────────────────────────────

  # welfare (all sites)
  welfare_projects <- c(
    "GAIN", "NEWWS",
    "Saturation Work Initiative Model (SWIM)",
    "AFDC Homemaker-Home Health Aide Demonstrations",
    "Florida Project Independence",
    "Virginia Employment Services Program",
    "Baltimore Options Program",
    "New Jersey Grant Diversion",
    "Maine TOPS",
    "Wisconsin Welfare Employment Experiment (WEJT)",
    "Vermont Welfare Restructuring Project (WRP)",
    "Indiana Welfare Reform (IMPACT)",
    "Enhanced Services for the Hard-to-Employ",
    "New Visions (GAIN Education & Training Program)",
    "Food Stamp Employment and Training (FSET) Program",
    "Maryland Employment Initiatives (Baltimore Options)",
    "Pennsylvania Saturation Work Program",
    "Ohio Transitions to Independence Demonstration (JOBS)",
    "Ohio Transitions to Independence Demonstration (Work Choice)"
  )
  if (project %in% welfare_projects) return("welfare")

  # youth (all sites)
  youth_projects <- c(
    "JOBSTART",
    "National Job Corps Study",
    "Career Academies",
    "PACE \u2013 Year Up",
    "Year Up Pathways to Components (PTC)",
    "Year Up Professional Training Corps (PTC)",
    "American Conservation and Youth Service Corps (ACYSC)",
    "CET Replication",
    "YouthBuild",
    "New Chance",
    "Teenage Parent Demonstration (TPD)",
    "Alternative Youth Employment Strategies (AYES)"
  )
  if (project %in% youth_projects) return("youth")

  # low_income_adult (all sites)
  lia_projects <- c(
    "PACE",
    "WorkAdvance",
    "Sectoral Employment Impact Study",
    "Health Profession Opportunity Grants 1.0 (HPOG 1.0)",
    "Health Profession Opportunity Grants 2.0 (HPOG 2.0)",
    "Green Jobs and Health Care",
    "Project QUEST",
    "Accelerating Connections to Employment (ACE)",
    "Accelerated Training for Illinois Manufacturing (ATIM)",
    "New Hope for Families and Children",
    "Work Advancement and Support Center (WASC)",
    "Minority Female Single Parent Demonstration"
  )
  if (project %in% lia_projects) return("low_income_adult")

  # justice_involved (all sites)
  justice_projects <- c(
    "TJRD",
    "CEO"
  )
  if (project %in% justice_projects) return("justice_involved")

  # dislocated (all sites)
  if (project == "Texas Worker Adjustment Demonstration") return("dislocated")
  if (project == "New Jersey UI Reemployment Demonstration") return("dislocated")

  # noncustodial_parent (all sites)
  if (project == "Child Support Noncustodial Parent Employment Demonstration (CSPED)")
    return("noncustodial_parent")

  # disability (all sites)
  if (project == "Structured Training and Employment Transitional Services (STETS)")
    return("disability")
  if (project == "Transitional Employment Training Demonstration (TETD)")
    return("disability")

  # ── Projects requiring site-level splits ──────────────────────────────────

  # National Supported Work Demonstration
  if (project == "National Supported Work Demonstration") {
    if (grepl("AFDC", site))        return("welfare")
    if (grepl("Ex-addict", site))   return("substance_abuse")
    if (grepl("Ex-offender", site)) return("justice_involved")
    if (grepl("Young|Youth", site)) return("youth")
  }

  # Wildcat (Supported Work) — Manhattan ex-addicts site
  if (project == "Wildcat (Supported Work)") {
    if (grepl("ex-addict", site, ignore.case = TRUE)) return("substance_abuse")
  }

  # National JTPA Study
  if (project == "National JTPA Study") {
    if (grepl("Adult", site))       return("low_income_adult")
    if (grepl("youth", site, ignore.case = TRUE)) return("youth")
  }

  # ETJD — noncustodial parents vs. ex-offenders by site
  if (project == "ETJD") {
    if (grepl("Fort Worth|Indianapolis|New York", site)) return("justice_involved")
    return("noncustodial_parent")  # Atlanta, Milwaukee, San Francisco, Syracuse
  }

  # Employment Retention and Advancement (ERA)
  if (project == "Employment Retention and Advancement (ERA)") {
    # Welfare: current TANF recipients
    if (grepl("Chicago|Los Angeles|Riverside Training|Riverside Work|NYC PRIDE|Minnesota",
              site)) return("welfare")
    # Low-income adult: employed former recipients, low-wage workers
    if (grepl("Cleveland|Eugene|Medford|PASS", site)) return("low_income_adult")
  }

  # STED
  if (project == "STED") {
    if (grepl("LA County|Minnesota", site)) return("welfare")
    if (grepl("NYC|Chicago", site))         return("youth")
    if (grepl("San Francisco", site))       return("low_income_adult")
  }

  # WIA Gold Standard
  if (project == "WIA Gold Standard") {
    if (grepl("Dislocated", site)) return("dislocated")
    return("low_income_adult")  # Adults
  }

  warning("Unclassified: ", project, " — ", site)
  return(NA_character_)
}

dat$target_pop <- mapply(assign_target_pop, dat$project, dat$site_subgroup)

cat("\n=== target_pop distribution ===\n")
print(table(dat$target_pop, useNA = "always"))

# Verify no missing
n_na <- sum(is.na(dat$target_pop))
if (n_na > 0) {
  cat(sprintf("WARNING: %d rows unclassified:\n", n_na))
  unc <- dat[is.na(dat$target_pop), c("project", "site_subgroup")]
  print(as.data.frame(unc))
}
stopifnot("Unclassified target_pop rows" = sum(is.na(dat$target_pop)) == 0)

# ── 8. Code Census regions ───────────────────────────────────────────────────
#
# 4-region and 9-division classification per Census Bureau definitions. https://www.census.gov/programs-surveys/economic-census/guidance-geographies/levels.html#par_textimage_34
# "multiple" or empty state → "Multiple" for both region and division.

region4 <- c(
  # Northeast
  CT = "Northeast", ME = "Northeast", MA = "Northeast", NH = "Northeast",
  NJ = "Northeast", NY = "Northeast", PA = "Northeast", RI = "Northeast",
  VT = "Northeast",
  # Midwest
  IL = "Midwest", IN = "Midwest", IA = "Midwest", KS = "Midwest",
  MI = "Midwest", MN = "Midwest", MO = "Midwest", NE = "Midwest",
  ND = "Midwest", OH = "Midwest", SD = "Midwest", WI = "Midwest",
  # South
  AL = "South", AR = "South", DE = "South", DC = "South", FL = "South",
  GA = "South", KY = "South", LA = "South", MD = "South", MS = "South",
  NC = "South", OK = "South", SC = "South", TN = "South", TX = "South",
  VA = "South", WV = "South",
  # West
  AK = "West", AZ = "West", CA = "West", CO = "West", HI = "West",
  ID = "West", MT = "West", NV = "West", NM = "West", OR = "West",
  UT = "West", WA = "West", WY = "West"
)

division9 <- c(
  # New England
  CT = "New England", ME = "New England", MA = "New England",
  NH = "New England", RI = "New England", VT = "New England",
  # Middle Atlantic
  NJ = "Middle Atlantic", NY = "Middle Atlantic", PA = "Middle Atlantic",
  # East North Central
  IL = "East North Central", IN = "East North Central",
  MI = "East North Central", OH = "East North Central",
  WI = "East North Central",
  # West North Central
  IA = "West North Central", KS = "West North Central",
  MN = "West North Central", MO = "West North Central",
  NE = "West North Central", ND = "West North Central",
  SD = "West North Central",
  # South Atlantic
  DE = "South Atlantic", DC = "South Atlantic", FL = "South Atlantic",
  GA = "South Atlantic", MD = "South Atlantic", NC = "South Atlantic",
  SC = "South Atlantic", VA = "South Atlantic", WV = "South Atlantic",
  # East South Central
  AL = "East South Central", KY = "East South Central",
  MS = "East South Central", TN = "East South Central",
  # West South Central
  AR = "West South Central", LA = "West South Central",
  OK = "West South Central", TX = "West South Central",
  # Mountain
  AZ = "Mountain", CO = "Mountain", ID = "Mountain", MT = "Mountain",
  NV = "Mountain", NM = "Mountain", UT = "Mountain", WY = "Mountain",
  # Pacific
  AK = "Pacific", CA = "Pacific", HI = "Pacific", OR = "Pacific",
  WA = "Pacific"
)

dat$census_region   <- ifelse(is.na(dat$state) | dat$state == "multiple",
                              "Multiple", region4[dat$state])
dat$census_division <- ifelse(is.na(dat$state) | dat$state == "multiple",
                              "Multiple", division9[dat$state])

cat("\n=== Census region (4) ===\n")
print(table(dat$census_region, useNA = "always"))
cat("\n=== Census division (9) ===\n")
print(table(dat$census_division, useNA = "always"))
cat("\n=== geo_type ===\n")
print(table(dat$geo_type, useNA = "always"))

# ── 8b. Average unemployment over each follow-up window ──────────────────────
#
# For each program × horizon (st/mt/lt), compute mean monthly unemployment
# rate over [randomization_midpoint, midpoint + fu_years]. Use the program's
# Census-region series; fall back to the national rate (UNRATE) when the
# region is "Multiple" or NA. Downloaded via FRED's no-key fredgraph CSV.

fred_csv <- function(series_id) {
  url <- sprintf("https://fred.stlouisfed.org/graph/fredgraph.csv?id=%s",
                 series_id)
  d <- read.csv(url, stringsAsFactors = FALSE,
                na.strings = c("", ".", "NA"))
  names(d) <- c("date", "value")  # cols are "observation_date" + series id
  d$date <- as.Date(d$date)
  d
}

region_series_id <- c(Northeast = "CNERUR", Midwest   = "CMWRUR",
                      South     = "CSOUUR", West      = "CWSTUR",
                      national  = "UNRATE")
unrate_series <- lapply(region_series_id, fred_csv)
cat("FRED downloads:\n")
for (k in names(unrate_series)) {
  s <- unrate_series[[k]]
  cat(sprintf("  %-10s (%s): %s..%s\n", k, region_series_id[k],
              min(s$date), max(s$date)))
}

# Decimal year (e.g. 1995.5) → calendar Date
year_to_date <- function(yr) {
  if (is.na(yr)) return(NA)
  y <- floor(yr); frac <- yr - y
  as.Date(sprintf("%d-01-01", y)) + round(frac * 365.25)
}

avg_in_window <- function(series, midpoint_yr, fu_years) {
  if (is.na(midpoint_yr) || is.na(fu_years)) return(NA_real_)
  start <- year_to_date(midpoint_yr)
  end   <- start + round(fu_years * 365.25)
  in_w  <- series$date >= start & series$date <= end
  if (!any(in_w, na.rm = TRUE)) return(NA_real_)
  mean(series$value[in_w], na.rm = TRUE)
}

value_at <- function(series, yr) {
  if (is.na(yr)) return(NA_real_)
  d <- year_to_date(yr)
  ok <- !is.na(series$value)
  if (!any(ok)) return(NA_real_)
  j <- which.min(abs(series$date[ok] - d))
  series$value[ok][j]
}

# Pick the region series for each row; "Multiple" or NA → national.
region_key <- ifelse(is.na(dat$census_region) |
                     !(dat$census_region %in% c("Northeast", "Midwest",
                                                "South", "West")),
                     "national", as.character(dat$census_region))

for (hz in c("st", "mt", "lt")) {
  fu_col  <- paste0(hz, "_followup_years")
  out_col <- paste0("unrate_", hz)
  dat[[out_col]] <- mapply(function(rk, mp, fu) {
    if (is.na(mp) || is.na(fu)) return(NA_real_)
    s <- unrate_series[[rk]]
    # Regional series start in 1976; fall back to UNRATE if window precedes.
    if (rk != "national" && year_to_date(mp) < min(s$date)) {
      s <- unrate_series[["national"]]
    }
    avg_in_window(s, mp, fu)
  }, region_key, dat$randomization_midpoint, dat[[fu_col]],
  USE.NAMES = FALSE)
}

# Regional unemployment at the randomization midpoint itself (no follow-up
# dependence). Same regional/national fallback as the windowed averages.
dat$unrate_at_rand <- mapply(function(rk, mp) {
  if (is.na(mp)) return(NA_real_)
  s <- unrate_series[[rk]]
  if (rk != "national" && year_to_date(mp) < min(s$date)) {
    s <- unrate_series[["national"]]
  }
  value_at(s, mp)
}, region_key, dat$randomization_midpoint, USE.NAMES = FALSE)

cat("\n=== Region series used (count of rows) ===\n")
print(table(region_key, useNA = "always"))
cat("\n=== Average regional unemployment over follow-up windows ===\n")
print(summary(dat[, c("unrate_st", "unrate_mt", "unrate_lt",
                      "unrate_at_rand")]))

# ── 9. Rescale continuous moderators ─────────────────────────────────────────

dat$cost_per_treated_k <- dat$cost_per_treated / 1000  # 2025$ thousands

# ── 10. Set reference levels for categorical moderators ──────────────────────

dat$target_pop          <- relevel(factor(dat$target_pop), ref = "welfare")
dat$training_role[dat$training_role == "incidental"] <- "secondary"
dat$training_role       <- relevel(factor(dat$training_role), ref = "primary")
dat$mandatory_voluntary <- relevel(factor(dat$mandatory_voluntary), ref = "mandatory")
dat$funding_public_private <- relevel(factor(dat$funding_public_private), ref = "public")
dat$admin_public_private   <- relevel(factor(dat$admin_public_private), ref = "public")
dat$census_region       <- factor(dat$census_region,
  levels = c("South", "Midwest", "Northeast", "West", "Multiple"))
dat$census_division     <- factor(dat$census_division,
  levels = c("South Atlantic", "East South Central", "West South Central",
             "East North Central", "West North Central", "Middle Atlantic",
             "New England", "Mountain", "Pacific", "Multiple"))
dat$geo_type            <- relevel(factor(dat$geo_type), ref = "urban")
dat$emp_data_source     <- relevel(factor(dat$emp_data_source), ref = "admin")
dat$earn_data_source    <- relevel(factor(dat$earn_data_source), ref = "admin")

# ── 10. Diagnostics ──────────────────────────────────────────────────────────

cat("\n=== Employment SE source summary ===\n")
for (prefix in c("st", "mt", "lt")) {
  src_col <- paste0(prefix, "_emp_se_source")
  tbl <- table(dat[[src_col]], useNA = "always")
  cat(prefix, ":\n")
  print(tbl)
}

cat("\n=== Earnings SE source summary ===\n")
for (prefix in c("st", "mt", "lt")) {
  src_col <- paste0(prefix, "_earn_se_source")
  tbl <- table(dat[[src_col]], useNA = "always")
  cat(prefix, ":\n")
  print(tbl)
}

cat("\n=== Rows with employment impact but still no SE ===\n")
for (prefix in c("st", "mt", "lt")) {
  imp_col <- paste0(prefix, "_emp_impact")
  se_col  <- paste0(prefix, "_emp_se")
  problem <- which(!is.na(dat[[imp_col]]) & is.na(dat[[se_col]]))
  if (length(problem) > 0) {
    cat(sprintf("%s: %d rows — projects: %s\n", toupper(prefix), length(problem),
                paste(unique(dat$project[problem]), collapse = "; ")))
  } else {
    cat(sprintf("%s: none\n", toupper(prefix)))
  }
}

cat("\n=== Dollar-year and CPI factor examples (first 10 non-NA) ===\n")
for (prefix in c("st", "mt", "lt")) {
  dy_col   <- paste0(prefix, "_dollar_year")
  infl_col <- paste0(prefix, "_earn_cpi_factor")
  imp_col  <- paste0(prefix, "_earn_impact")
  cad_col  <- paste0(prefix, "_earn_cadence")
  idx <- which(!is.na(dat[[dy_col]]))[1:min(5, sum(!is.na(dat[[dy_col]])))]
  cat(sprintf("\n%s:\n", toupper(prefix)))
  for (i in idx) {
    cat(sprintf("  %s | %s | dollar_year=%.1f | cpi_factor=%.3f | impact(2025$)=%.0f\n",
                dat$project[i], dat[[cad_col]][i],
                dat[[dy_col]][i], dat[[infl_col]][i], dat[[imp_col]][i]))
  }
}

cat("\n=== Uptake SE source summary ===\n")
for (outcome in c("program_uptake", "any_training")) {
  src_col <- paste0(outcome, "_se_source")
  tbl <- table(dat[[src_col]], useNA = "always")
  cat(outcome, ":\n")
  print(tbl)
}

cat("\n=== Rows with uptake impact but still no SE ===\n")
for (outcome in c("program_uptake", "any_training")) {
  imp_col <- paste0(outcome, "_impact")
  se_col  <- paste0(outcome, "_se")
  problem <- which(!is.na(dat[[imp_col]]) & is.na(dat[[se_col]]))
  if (length(problem) > 0) {
    cat(sprintf("%s: %d rows — projects: %s\n", outcome, length(problem),
                paste(unique(dat$project[problem]), collapse = "; ")))
  } else {
    cat(sprintf("%s: none\n", outcome))
  }
}

cat("\n=== Rows with earnings impact but missing dollar-year ===\n")
for (prefix in c("st", "mt", "lt")) {
  imp_col <- paste0(prefix, "_earn_impact")
  dy_col  <- paste0(prefix, "_dollar_year")
  problem <- which(!is.na(dat[[imp_col]]) & is.na(dat[[dy_col]]))
  if (length(problem) > 0) {
    cat(sprintf("%s: %d rows — projects: %s\n", toupper(prefix), length(problem),
                paste(unique(dat$project[problem]), collapse = "; ")))
  } else {
    cat(sprintf("%s: none\n", toupper(prefix)))
  }
}

# ── 11. Save processed data ──────────────────────────────────────────────────

saveRDS(dat, "data/processed_data.rds")
saveRDS(reasoning, "data/reasoning.rds")
cat("\nSaved data/processed_data.rds and data/reasoning.rds\n")
