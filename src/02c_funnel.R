###############################################################################
# 02c_funnel.R — Overall funnel plots (employment & earnings)
#
# One funnel plot per outcome × time horizon, paralleling 02_forest.R.
###############################################################################

library(metafor)
library(sysfonts)
library(showtext)
font_add_google("Source Serif 4", "Source Serif 4")
showtext_auto()

dat <- readRDS("data/processed_data.rds")
dir.create("output", showWarnings = FALSE)

outcome_types <- list(
  list(name = "emp",  imp_suffix = "_emp_impact",  se_suffix = "_emp_se",
       xlab = "Employment impact (percentage points)"),
  list(name = "earn", imp_suffix = "_earn_impact", se_suffix = "_earn_se",
       xlab = "Earnings impact (2025$, annualized)")
)

horizons <- c(st = "Short-term (~1-year)",
              mt = "Medium-term (~2-year)",
              lt = "Long-term (~3-5-year)")

make_funnel <- function(dat, prefix, horizon_label, oc, sample_suffix) {
  imp_col <- paste0(prefix, oc$imp_suffix)
  se_col  <- paste0(prefix, oc$se_suffix)

  idx <- which(!is.na(dat[[imp_col]]) & !is.na(dat[[se_col]]))
  if (length(idx) < 3) {
    cat(sprintf("  Skipping %s %s — too few observations\n",
                oc$name, horizon_label))
    return(invisible(NULL))
  }

  d   <- dat[idx, ]
  yi  <- d[[imp_col]]
  sei <- d[[se_col]]

  res_re <- suppressWarnings(rma(yi = yi, sei = sei, method = "REML"))

  filename <- sprintf("output/funnel_%s_%s%s.png", oc$name, prefix, sample_suffix)
  png(filename, width = 8, height = 6, units = "in", res = 150)
  on.exit(dev.off())
  showtext_opts(dpi = 150)
  par(family = "Source Serif 4")

  funnel(res_re, xlab = oc$xlab, pch = 19, col = "black",
         main = sprintf("%s %s impacts", horizon_label, oc$name))

  cat(sprintf("Saved %s\n", filename))
}

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
    for (hz in names(horizons)) {
      make_funnel(d_sm, hz, horizons[hz], oc, sm$suffix)
    }
  }
}
