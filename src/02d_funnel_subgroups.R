###############################################################################
# 02d_funnel_subgroups.R — Subgroup funnel plots (employment & earnings)
#
# One funnel plot per outcome × horizon × subgroup, with points colored by
# subgroup level. Parallels 02b_forest_subgroups.R.
###############################################################################

library(metafor)
library(sysfonts)
library(showtext)
font_add_google("Source Serif 4", "Source Serif 4")
showtext_auto()

dat <- readRDS("data/processed_data.rds")
dir.create("output", showWarnings = FALSE)

# ── 1. Define subgroup variables ─────────────────────────────────────────────
# (identical to 02b_forest_subgroups.R)

dat$decade <- cut(dat$randomization_midpoint,
  breaks = c(-Inf, 1980, 1990, 2000, 2010, 2020, Inf),
  labels = c("1970s", "1980s", "1990s", "2000s", "2010s", "2020s"),
  right = FALSE)

dat$training_modality <- factor(ifelse(
  dat$has_classroom == 1 & dat$has_ojt == 1, "Both classroom & on-the-job training",
  ifelse(dat$has_classroom == 1 & dat$has_ojt == 0, "Classroom only",
  ifelse(dat$has_classroom == 0 & dat$has_ojt == 1, "On-the-job training only",
         "Neither"))),
  levels = c("Classroom only", "On-the-job training only",
             "Both classroom & on-the-job training", "Neither"))

dat$treatment_duration_label <- factor(
  ifelse(is.na(dat$treatment_duration_months), NA,
  ifelse(dat$treatment_duration_months < 6,
         "Treatment <6 months", "Treatment ≥6 months")),
  levels = c("Treatment <6 months", "Treatment ≥6 months"))

dat$sector_label <- factor(
  ifelse(dat$sector_program == 1, "Sector program", "Not sector program"),
  levels = c("Sector program", "Not sector program"))

dat$funding_label <- factor(dat$funding_public_private,
  levels = c("public", "private", "mixed"),
  labels = c("Public funding", "Private funding", "Mixed funding"))

dat$academic_label <- factor(
  ifelse(dat$academic == 1, "Academic journal source", "Non-academic source"),
  levels = c("Academic journal source", "Non-academic source"))

dat$age_decade <- cut(dat$mean_age,
  breaks = c(-Inf, 20, 30, 40, Inf),
  labels = c("Under 20", "20\u201329", "30\u201339", "40+"),
  right = FALSE)
dat$age_decade[is.na(dat$age_decade) & dat$target_pop == "youth"] <- "Under 20"

dat$gender_majority <- factor(
  ifelse(is.na(dat$pct_male), NA,
  ifelse(dat$pct_male >= 50, "Majority male (\u226550%)", "Majority female (<50%)")),
  levels = c("Majority male (\u226550%)", "Majority female (<50%)"))

dat$scaled_label <- factor(
  ifelse(dat$scaled == 1, "At-scale program", "Demonstration/pilot"),
  levels = c("At-scale program", "Demonstration/pilot"))

dat$data_source_label <- factor(dat$emp_data_source,
  levels = c("admin", "survey"),
  labels = c("Administrative records", "Survey"))

dat$employengage_curricula_label <- factor(
  ifelse(is.na(dat$employengage_curricula), NA,
  ifelse(dat$employengage_curricula == 1,
         "Employers engaged in curriculum design",
         "Employers not engaged in curriculum design")),
  levels = c("Employers engaged in curriculum design",
             "Employers not engaged in curriculum design"))

# ── 2. Definitions ───────────────────────────────────────────────────────────

subgroups <- list(
  list(var = "decade", label = "by decade",
       file_suffix = "decade"),
  list(var = "mandatory_voluntary", label = "by mandatory vs. voluntary",
       file_suffix = "mandatory"),
  list(var = "training_modality", label = "by training modality",
       file_suffix = "modality"),
  list(var = "treatment_duration_label",
       label = "by treatment duration",
       file_suffix = "duration"),
  list(var = "geo_type", label = "by urban vs. rural",
       file_suffix = "geo"),
  list(var = "sector_label", label = "by sector program",
       file_suffix = "sector"),
  list(var = "funding_label", label = "by funding source",
       file_suffix = "funding"),
  list(var = "academic_label", label = "by academic vs. non-academic source",
       file_suffix = "academic"),
  list(var = "census_region", label = "by Census region",
       file_suffix = "region"),
  list(var = "target_pop", label = "by target population",
       file_suffix = "targetpop"),
  list(var = "age_decade", label = "by mean age",
       file_suffix = "age"),
  list(var = "scaled_label", label = "by at-scale vs. demonstration",
       file_suffix = "scaled"),
  list(var = "data_source_label", label = "by outcome data source",
       file_suffix = "datasource"),
  list(var = "gender_majority", label = "by gender majority",
       file_suffix = "gender"),
  list(var = "employengage_curricula_label",
       label = "by employer engagement in curriculum design",
       file_suffix = "employengage"),
  list(var = ".resprate", label = "by outcome response rate",
       file_suffix = "resprate")
)

resolve_resprate_var <- function(oc, prefix) {
  paste0("resprate_label_", oc$name, "_", prefix)
}

outcome_types <- list(
  list(name = "emp",  imp_suffix = "_emp_impact",  se_suffix = "_emp_se",
       xlab = "Employment impact (percentage points)",
       file_prefix = "funnel_emp"),
  list(name = "earn", imp_suffix = "_earn_impact", se_suffix = "_earn_se",
       xlab = "Earnings impact (2025$, annualized)",
       file_prefix = "funnel_earn")
)

horizons <- c(st = "Short-term (~1-year)",
              mt = "Medium-term (~2-year)",
              lt = "Long-term (>~3-year)")

# Palette for up to 8 subgroup levels
sg_palette <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
                "#FF7F00", "#A65628", "#F781BF", "#999999")

# ── 3. Build subgroup funnel plot ────────────────────────────────────────────

make_subgroup_funnel <- function(dat, prefix, horizon_label, sg_var,
                                  sg_label, file_suffix, oc, sample_suffix) {
  if (sg_var == ".resprate") sg_var <- resolve_resprate_var(oc, prefix)
  imp_col <- paste0(prefix, oc$imp_suffix)
  se_col  <- paste0(prefix, oc$se_suffix)

  idx <- which(!is.na(dat[[imp_col]]) & !is.na(dat[[se_col]]) &
               !is.na(dat[[sg_var]]))
  if (length(idx) < 3) {
    cat(sprintf("  Skipping %s %s %s — too few observations\n",
                oc$name, horizon_label, sg_label))
    return(invisible(NULL))
  }

  d   <- dat[idx, ]
  yi  <- d[[imp_col]]
  sei <- d[[se_col]]
  grp <- factor(d[[sg_var]])

  res_re <- suppressWarnings(rma(yi = yi, sei = sei, method = "REML"))

  lvls <- levels(grp)
  cols <- sg_palette[seq_along(lvls)]
  pt_col <- cols[as.integer(grp)]

  filename <- sprintf("output/%s_%s_%s%s.png",
                      oc$file_prefix, prefix, file_suffix, sample_suffix)
  png(filename, width = 8, height = 6, units = "in", res = 150)
  on.exit(dev.off())
  showtext_opts(dpi = 150)
  par(family = "Source Serif 4")

  funnel(res_re, xlab = oc$xlab, pch = 19, col = pt_col,
         main = sprintf("%s %s impacts (%s)",
                        horizon_label, oc$name, sg_label))

  legend("topright", legend = lvls, col = cols[seq_along(lvls)],
         pch = 19, cex = 0.8, bg = "white")

  cat(sprintf("  Saved %s\n", filename))
}

# ── 4. Generate all plots ────────────────────────────────────────────────────

samples <- list(
  list(name = "Full sample", suffix = "",
       filter = function(d) d),
  list(name = "Training-primary", suffix = "_primary",
       filter = function(d) d[!is.na(d$training_role) &
                              d$training_role == "primary", ])
)

for (sm in samples) {
  d_sm <- sm$filter(dat)
  for (oc in outcome_types) {
    cat(sprintf("=== %s [%s] ===\n", oc$name, sm$name))
    for (sg in subgroups) {
      cat(sprintf("Subgroup: %s\n", sg$label))
      for (hz in names(horizons)) {
        make_subgroup_funnel(d_sm, hz, horizons[hz], sg$var, sg$label,
                             sg$file_suffix, oc, sm$suffix)
      }
    }
  }
}

cat("\nAll subgroup funnel plots written.\n")
