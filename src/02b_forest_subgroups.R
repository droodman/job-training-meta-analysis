###############################################################################
# 02b_forest_subgroups.R — Subgroup forest plots
#
# Stacked forest plots broken out by subgroup, with RE diamonds.
# Loops over outcome types (employment, earnings) and all subgroups.
###############################################################################

library(metafor)
library(sysfonts)
library(showtext)
font_add_google("Source Serif 4", "Source Serif 4")
showtext_auto()

# Override fmtx to use Unicode minus sign (U+2212) instead of hyphen (once only)
if (!exists(".fmtx_patched", envir = .GlobalEnv)) {
  original_fmtx <- metafor::fmtx
  assignInNamespace("fmtx", function(x, digits, flag = "", ...) {
    gsub("-", "\u2212", original_fmtx(x, digits, flag, ...))
  }, ns = "metafor")
  assign(".fmtx_patched", TRUE, envir = .GlobalEnv)
}

dat <- readRDS("data/processed_data.rds")
dir.create("output", showWarnings = FALSE)

# ── 1. Define subgroup variables ─────────────────────────────────────────────

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

# Mean age by decade; impute youth target_pop with missing age into youngest group
dat$age_decade <- cut(dat$mean_age,
  breaks = c(-Inf, 20, 30, 40, Inf),
  labels = c("Under 20", "20\u201329", "30\u201339", "40+"),
  right = FALSE)
dat$age_decade[is.na(dat$age_decade) & dat$target_pop == "youth"] <- "Under 20"

# Pct male majority
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

dat$cost_info_label <- factor(
  ifelse(!is.na(dat$cost_per_treated),
         "Cost info available", "Cost info missing"),
  levels = c("Cost info available", "Cost info missing"))

# ── 2. Definitions ───────────────────────────────────────────────────────────

subgroups <- list(
  list(var = "decade", label = "by Decade",
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
  list(var = "cost_info_label",
       label = "by cost info availability",
       file_suffix = "costinfo"),
  list(var = ".resprate", label = "by outcome response rate",
       file_suffix = "resprate")
)

# Sentinel ".resprate" resolves to a horizon- and outcome-specific column
# precomputed in 01_prep.R. Uptake outcomes (no horizon) use the uptake column.
resolve_resprate_var <- function(oc, prefix) {
  if (oc$name %in% c("program_uptake", "any_training")) {
    "resprate_label_uptake"
  } else {
    paste0("resprate_label_", oc$name, "_", prefix)
  }
}

horizons <- c(st = "Short-term (~1-year)", mt = "Medium-term (~2-year)", lt = "Long-term (~3-5-year)")

outcome_types <- list(
  list(name = "emp",  imp_suffix = "_emp_impact",  se_suffix = "_emp_se",
       title_name = "employment",
       xlab = "Employment impact (percentage points)", file_prefix = "forest_emp", dig = 1L,
       alim = c(-25, 45), xlim = c(-90, 60)),
  list(name = "earn", imp_suffix = "_earn_impact", se_suffix = "_earn_se",
       title_name = "earnings",
       xlab = "Earnings impact (2025$, annualized)",  file_prefix = "forest_earn", dig = 0L,
       alim = NULL, xlim = NULL),
  list(name = "program_uptake",
       imp_suffix = "program_uptake_impact", se_suffix = "program_uptake_se",
       title_name = "Program uptake",
       xlab = "Program uptake impact (percentage points)",
       file_prefix = "forest_program_uptake", dig = 1L,
       alim = NULL, xlim = NULL,
       no_horizon = TRUE),
  list(name = "any_training",
       imp_suffix = "any_training_impact", se_suffix = "any_training_se",
       title_name = "Any training uptake",
       xlab = "Any training uptake impact (percentage points)",
       file_prefix = "forest_any_training", dig = 1L,
       alim = NULL, xlim = NULL,
       no_horizon = TRUE)
)

# ── 3. Build subgroup forest plot ────────────────────────────────────────────

make_subgroup_forest <- function(dat, prefix, horizon_label, sg_var, sg_label,
                                  file_suffix, oc, so, sample_suffix) {
  if (sg_var == ".resprate") sg_var <- resolve_resprate_var(oc, prefix)
  imp_col <- paste0(prefix, oc$imp_suffix)
  se_col  <- paste0(prefix, oc$se_suffix)

  idx <- which(!is.na(dat[[imp_col]]) & !is.na(dat[[se_col]]) &
               !is.na(dat[[sg_var]]))
  if (length(idx) < 3) {
    cat(sprintf("  Skipping %s %s %s — too few observations\n",
                oc$title_name, horizon_label, sg_label))
    return(invisible(NULL))
  }

  d <- dat[idx, ]
  yi <- d[[imp_col]]
  sei <- d[[se_col]]
  grp_raw <- d[[sg_var]]
  if (is.factor(grp_raw)) {
    grp <- as.character(grp_raw)
    grp_levels <- rev(levels(grp_raw)[levels(grp_raw) %in% unique(grp)])
  } else {
    grp <- as.character(grp_raw)
    grp_levels <- rev(sort(unique(grp)))
  }

  d$label <- paste0(d$project, " \u2014 ", d$site_subgroup,
                    " (", round(d$randomization_midpoint), ")")
  d$label <- ifelse(nchar(d$label) > 60,
                    paste0(substr(d$label, 1, 57), "\u2026"),
                    d$label)

  ord <- if (so$name == "alpha") {
    order(factor(grp, levels = grp_levels), -rank(d$label))
  } else {
    order(factor(grp, levels = grp_levels), -yi)
  }
  d <- d[ord, ]
  yi <- yi[ord]
  sei <- sei[ord]
  grp <- grp[ord]

  n <- nrow(d)
  n_grps <- length(grp_levels)

  rows_list <- list()
  re_diamond_rows <- c()
  header_rows <- c()
  current_row <- 1
  first_group <- TRUE

  for (g in grp_levels) {
    g_idx <- which(grp == g)
    n_g <- length(g_idx)
    if (!first_group) current_row <- current_row + 2
    re_diamond_rows <- c(re_diamond_rows, current_row)
    current_row <- current_row + 1
    study_rows <- seq(current_row, current_row + n_g - 1)
    rows_list[[g]] <- study_rows
    current_row <- current_row + n_g
    header_rows <- c(header_rows, current_row - 0.2)
    first_group <- FALSE
  }

  total_rows <- current_row
  ylim <- c(0, total_rows + 0.5)

  title_label <- if (horizon_label == "") {
    oc$title_name
  } else {
    sprintf("%s %s", horizon_label, oc$title_name)
  }

  fig_height <- max(6, 0.18 * n + 1.0 * n_grps + 2)
  filename <- if (prefix == "") {
    sprintf("output/%s%s_%s%s.png",
            oc$file_prefix, so$suffix, file_suffix, sample_suffix)
  } else {
    sprintf("output/%s_%s%s_%s%s.png",
            oc$file_prefix, prefix, so$suffix, file_suffix, sample_suffix)
  }
  png(filename, width = 14, height = fig_height, units = "in", res = 150)
  on.exit(dev.off())
  showtext_opts(dpi = 150)
  par(mar = c(4, 4, 2, 1), family = "Source Serif 4")

  forest_args <- list(yi, sei = sei,
         slab = d$label,
         xlab = oc$xlab, digits = oc$dig,
         header = FALSE, top = 0,
         cex = 1.05, cex.lab = 1.2, efac = 0, pch = 18, psize = 1.5,
         main = sprintf("%s impacts (%s)", title_label, sg_label),
         rows = unlist(rows_list),
         ylim = ylim,
         fonts = c("Source Serif 4", "Source Serif 4"))
  if (!is.null(oc$alim)) forest_args$alim <- oc$alim
  if (!is.null(oc$xlim)) forest_args$xlim <- oc$xlim
  do.call(forest, forest_args)

  study_rows <- unlist(rows_list)
  # Light dotted leader rules through each study row and each subgroup-RE row
  # (not the group-header rows) so the eye can scan label → CI → estimate.
  # Drawn before the grey overdraw so the whisker/marker sit on top.
  abline(h = c(study_rows, re_diamond_rows),
         lty = "dotted", col = "grey85", lwd = 0.6)

  # Overdraw study markers and CI lines in grey for visual contrast with
  # the steelblue1 subgroup-RE rows below — text labels stay default color.
  qval <- qnorm(0.975)
  segments(yi - qval * sei, study_rows, yi + qval * sei, study_rows,
           col = "grey50", lwd = 1.4)
  points(yi, study_rows, pch = 18, col = "grey50", cex = 1.05 * 1.5)

  for (i in seq_along(grp_levels)) {
    text(par("usr")[1], header_rows[i], grp_levels[i],
         pos = 4, cex = 1.15, font = 2)
  }

  for (i in seq_along(grp_levels)) {
    g <- grp_levels[i]
    g_idx <- which(grp == g)

    fit_re <- tryCatch(
      suppressWarnings(rma(yi = yi[g_idx], sei = sei[g_idx], method = "REML")),
      error = function(e) {
        tryCatch(suppressWarnings(rma(yi = yi[g_idx], sei = sei[g_idx], method = "DL")),
                 error = function(e2) NULL)
      })

    if (!is.null(fit_re)) {
      # Dot+whisker rendering to match summary forest plots in 02g:
      # suppress addpoly's diamond and overlay segments/points.
      addpoly(fit_re, row = re_diamond_rows[i], cex = 1.05, digits = oc$dig,
              efac = 0, col = "steelblue1", border = NA,
              fonts = c("Source Serif 4", "Source Serif 4"),
              mlab = sprintf("RE: %s (k=%d, I\u00b2=%.0f%%)",
                             g, length(g_idx), fit_re$I2))
      segments(fit_re$ci.lb, re_diamond_rows[i], fit_re$ci.ub, re_diamond_rows[i],
               col = "steelblue1", lwd = 1.4)
      points(coef(fit_re), re_diamond_rows[i], pch = 19,
             col = "steelblue1", cex = 1.5)
    }
  }

  cat(sprintf("  Saved %s\n", filename))
}

# ── 4. Generate all plots ────────────────────────────────────────────────────

sort_orders <- list(
  list(name = "effect", suffix = ""),
  list(name = "alpha",  suffix = "_alpha")
)

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
    cat(sprintf("=== %s [%s] ===\n", oc$title_name, sm$name))
    for (sg in subgroups) {
      cat(sprintf("Subgroup: %s\n", sg$label))
      for (so in sort_orders) {
        if (isTRUE(oc$no_horizon)) {
          make_subgroup_forest(d_sm, "", "", sg$var, sg$label,
                               sg$file_suffix, oc, so, sm$suffix)
        } else {
          for (hz in names(horizons)) {
            make_subgroup_forest(d_sm, hz, horizons[hz], sg$var, sg$label,
                                 sg$file_suffix, oc, so, sm$suffix)
          }
        }
      }
    }
  }
}

cat("\nAll subgroup forest plots written.\n")
