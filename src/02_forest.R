###############################################################################
# 02_forest.R — Overall forest plots
#
# Sorted by point estimate, with RE summary diamond.
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

horizons <- c(st = "Short-term (~1-year)", mt = "Medium-term (~2-year)", lt = "Long-term (~3-5-year)")

outcome_types <- list(
  list(name = "emp",  imp_suffix = "_emp_impact",  se_suffix = "_emp_se",
       title_name = "employment",
       xlab = "Employment impact (percentage points)", dig = 1L,
       alim = c(-25, 45), xlim = c(-90, 60)),
  list(name = "earn", imp_suffix = "_earn_impact", se_suffix = "_earn_se",
       title_name = "earnings",
       xlab = "Earnings impact (2025$, annualized)", dig = 0L,
       alim = NULL, xlim = NULL),
  list(name = "program_uptake",
       imp_suffix = "program_uptake_impact", se_suffix = "program_uptake_se",
       title_name = "Program uptake",
       xlab = "Program uptake impact (percentage points)", dig = 1L,
       alim = NULL, xlim = NULL,
       no_horizon = TRUE),
  list(name = "any_training",
       imp_suffix = "any_training_impact", se_suffix = "any_training_se",
       title_name = "Any training uptake",
       xlab = "Any training uptake impact (percentage points)", dig = 1L,
       alim = NULL, xlim = NULL,
       no_horizon = TRUE)
)

make_forest <- function(dat, prefix, horizon_label, oc, so, sample_suffix) {
  imp_col <- paste0(prefix, oc$imp_suffix)
  se_col  <- paste0(prefix, oc$se_suffix)

  idx <- which(!is.na(dat[[imp_col]]) & !is.na(dat[[se_col]]))
  if (length(idx) < 3) {
    cat(sprintf("  Skipping %s %s — too few observations\n",
                oc$title_name, horizon_label))
    return(invisible(NULL))
  }

  d <- dat[idx, ]

  d$label <- paste0(d$project, " \u2014 ", d$site_subgroup,
                    " (", round(d$randomization_midpoint), ")")
  d$label <- ifelse(nchar(d$label) > 60,
                    paste0(substr(d$label, 1, 57), "\u2026"),
                    d$label)

  d <- if (so$name == "alpha") d[order(d$label), ] else d[order(d[[imp_col]]), ]

  yi  <- d[[imp_col]]
  sei <- d[[se_col]]
  n   <- nrow(d)

  res_re <- suppressWarnings(rma(yi = yi, sei = sei, method = "REML"))

  title_label <- if (horizon_label == "") {
    oc$title_name
  } else {
    sprintf("%s %s", horizon_label, oc$title_name)
  }

  cat(sprintf("\n=== %s (%d estimates) ===\n", title_label, n))
  cat(sprintf("RE: %.2f [%.2f, %.2f], tau²=%.2f, I²=%.1f%%\n",
              coef(res_re), res_re$ci.lb, res_re$ci.ub, res_re$tau2, res_re$I2))

  fig_height <- max(5, 0.22 * n + 2.5)
  filename <- if (prefix == "") {
    sprintf("output/forest_%s%s%s.png", oc$name, so$suffix, sample_suffix)
  } else {
    sprintf("output/forest_%s_%s%s%s.png", oc$name, prefix, so$suffix, sample_suffix)
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
         main = sprintf("%s impacts", title_label),
         order = seq_len(n),
         rows = seq(n + 1, 2),
         ylim = c(-1, n + 2),
         fonts = c("Source Serif 4", "Source Serif 4"))
  if (!is.null(oc$alim)) forest_args$alim <- oc$alim
  if (!is.null(oc$xlim)) forest_args$xlim <- oc$xlim
  do.call(forest, forest_args)

  # Overdraw study markers and CI lines in grey so they contrast with the
  # steelblue4 RE row at the bottom — without recoloring the text labels
  # (forest()'s `col`/`colout` can tint annotation text in some metafor
  # builds, so leave them default and just paint over the marks).
  study_rows <- seq(n + 1, 2)
  qval <- qnorm(0.975)
  segments(yi - qval * sei, study_rows, yi + qval * sei, study_rows,
           col = "grey50", lwd = 1.4)
  points(yi, study_rows, pch = 18, col = "grey50", cex = 1.05 * 1.5)

  # Render the RE estimate as a dot+whisker (matching the summary forest
  # plots in 02g): keep addpoly for label/number formatting but suppress
  # the diamond (efac = 0, border = NA), then overlay segments and a dot.
  addpoly(res_re, row = 0, cex = 1.05, digits = oc$dig, efac = 0,
          col = "steelblue4", border = NA,
          fonts = c("Source Serif 4", "Source Serif 4"),
          mlab = sprintf("RE Model (I\u00b2 = %.1f%%, \u03c4\u00b2 = %.2f)",
                         res_re$I2, res_re$tau2))
  segments(res_re$ci.lb, 0, res_re$ci.ub, 0, col = "steelblue4", lwd = 1.4)
  points(coef(res_re), 0, pch = 19, col = "steelblue4", cex = 1.5)

  cat(sprintf("Saved %s\n", filename))
}

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
    for (so in sort_orders) {
      if (isTRUE(oc$no_horizon)) {
        make_forest(d_sm, "", "", oc, so, sm$suffix)
      } else {
        for (hz in names(horizons)) {
          make_forest(d_sm, hz, horizons[hz], oc, so, sm$suffix)
        }
      }
    }
  }
}
