###############################################################################
# 02e_cost_scatter.R — Scatter plots of impact vs cost per treated
#
# Medium- and long-term employment and earnings impacts vs cost per treated
# (inflation-adjusted to 2025$ in 01_prep.R), for the full sample and the
# training-role-primary subsample.
###############################################################################

library(sysfonts)
library(showtext)
font_add_google("Source Serif 4", "Source Serif 4")

# Latin Modern Roman 10 (system-installed). Per-user font dir on Windows is
# not in sysfonts' default search path, so add it before font_add.
font_paths(c(font_paths(), file.path(Sys.getenv("LOCALAPPDATA"),
                                     "Microsoft/Windows/Fonts")))
font_add("LM Roman 10",
         regular    = "lmroman10-regular.otf",
         bold       = "lmroman10-bold.otf",
         italic     = "lmroman10-italic.otf",
         bolditalic = "lmroman10-bolditalic.otf")
showtext_auto()

dat <- readRDS("data/processed_data.rds")
dir.create("output", showWarnings = FALSE)

samples <- list(
  list(name = "Full sample", suffix = "",
       filter = function(d) d),
  list(name = "Training-primary subsample", suffix = "_primary",
       filter = function(d) d[!is.na(d$training_role) &
                              d$training_role == "primary", ])
)

scatters <- list(
  list(imp = "mt_emp_impact",  oc = "emp",  hz = "mt",
       outcome = "employment", horizon = "Medium-term (~2-year)",
       ylab = "Employment impact (percentage points)"),
  list(imp = "lt_emp_impact",  oc = "emp",  hz = "lt",
       outcome = "employment", horizon = "Long-term (~3-5-year)",
       ylab = "Employment impact (percentage points)"),
  list(imp = "mt_earn_impact", oc = "earn", hz = "mt",
       outcome = "earnings",   horizon = "Medium-term (~2-year)",
       ylab = "Earnings impact (2025$, annualized)"),
  list(imp = "lt_earn_impact", oc = "earn", hz = "lt",
       outcome = "earnings",   horizon = "Long-term (~3-5-year)",
       ylab = "Earnings impact (2025$, annualized)")
)

make_scatter <- function(dat, sp, sm) {
  d <- sm$filter(dat)
  idx <- which(!is.na(d[[sp$imp]]) & !is.na(d$cost_per_treated_k))
  if (length(idx) < 3) {
    cat(sprintf("  Skipping %s/%s %s — too few observations\n",
                sp$oc, sp$hz, sm$name))
    return(invisible(NULL))
  }
  d <- d[idx, ]
  x <- d$cost_per_treated_k
  y <- d[[sp$imp]]

  filename <- sprintf("output/scatter_%s_%s_cost%s.png",
                      sp$oc, sp$hz, sm$suffix)
  png(filename, width = 8, height = 6, units = "in", res = 150)
  on.exit(dev.off())
  showtext_opts(dpi = 150)
  par(mar = c(4.2, 4.5, 2.5, 1), family = "Source Serif 4")

  plot(x, y,
       pch = 19, col = "steelblue4",
       xlab = "Cost per treated (2025$, thousands)",
       ylab = sp$ylab,
       main = sprintf("%s %s impact vs cost — %s (k=%d)",
                      sp$horizon, sp$outcome, sm$name, length(idx)),
       cex = 1.1, cex.lab = 1.1, cex.main = 1.1)
  abline(h = 0, lty = 3, col = "grey50")
  fit <- lm(y ~ x)
  abline(fit, col = "firebrick", lwd = 1.5)
  slope <- coef(fit)[["x"]]
  slope_se <- sqrt(diag(vcov(fit)))[["x"]]
  legend("topright",
         legend = sprintf("Slope = %.3g (SE %.2g)", slope, slope_se),
         lty = 1, lwd = 1.5, col = "firebrick", bty = "n", cex = 1.05)

  cat(sprintf("Saved %s (k=%d)\n", filename, length(idx)))
}

for (sm in samples) {
  for (sp in scatters) make_scatter(dat, sp, sm)
}

# ── Benefit vs cost scatter ──────────────────────────────────────────────────
# Both cost_per_treated and benefit_per_treated are inflation-adjusted to
# 2025$ in 01_prep.R (using the randomization-midpoint dollar year).
#
# Edit `labeled_short_names` to control which points are labelled.
# Initially populated with every short_name that has both cost and benefit
# information; remove entries to declutter the plot.

labeled_short_names <- c(
  "NSWD-Welfare mothers",
  "NSWD-Ex-addicts",
  "NSWD-School dropouts",
  "JTPA-Adult women",
  "JTPA-Adult men",
  "JTPA-Female youths",
  "JTPA-Male youths",
  "Year Up",
  "Per Scholas",
  "St. Nicks",
  "Madison Strategies Group",
  "Towards Employment",
  "QUEST",
  "WIA-Adult",
  "WIA-Dislocated",
  "Job Corps"
)

make_benefit_cost_scatter <- function(dat, labeled) {
  idx <- which(!is.na(dat$cost_per_treated) & !is.na(dat$benefit_per_treated))
  d <- dat[idx, ]
  x <- d$cost_per_treated / 1000      # 2025$ thousands
  y <- d$benefit_per_treated / 1000   # 2025$ thousands
  primary <- !is.na(d$training_role) & d$training_role == "primary"

  filename <- "output/scatter_benefit_cost.png"
  png(filename, width = 10, height = 7, units = "in", res = 150)
  on.exit(dev.off())
  showtext_opts(dpi = 150)
  par(mar = c(4.2, 4.5, 2.5, 1), family = "LM Roman 10", las = 1)

  plot(x, y,
       pch = ifelse(primary, 19, 1),
       col = "steelblue4",
       xlab = "Cost per treated (2025$, thousands)",
       ylab = "Benefit per treated (2025$, thousands)",
       xlim = c(0, 55),
       cex = 1.3, cex.lab = 1.15, cex.axis = 1.15, lwd = 2, bty = "l")
  to_label <- !is.na(d$short_name) & d$short_name %in% labeled
  if (any(to_label)) {
    text(x[to_label], y[to_label], labels = d$short_name[to_label],
         pos = 4, cex = 1.15, offset = 0.7, col = "grey25")
  }

  fit <- lm(y ~ x)
  abline(fit, col = "firebrick", lwd = 1.5)
  slope <- coef(fit)[["x"]]
  slope_se <- sqrt(diag(vcov(fit)))[["x"]]

  legend("topleft",
         legend = c("Training is primary", "Training is secondary",
                    sprintf("Slope = %.2f (SE %.2f)", slope, slope_se)),
         pch    = c(19, 1, NA),
         lty    = c(NA, NA, 1),
         lwd    = c(2, 2, 1.5),
         col    = c("steelblue4", "steelblue4", "firebrick"),
         bty = "n", cex = 1.15)

  cat(sprintf("Saved %s (k=%d, %d labelled)\n",
              filename, length(idx), sum(to_label)))
}

make_benefit_cost_scatter(dat, labeled_short_names)

cat("\nAll cost-impact scatter plots written.\n")
