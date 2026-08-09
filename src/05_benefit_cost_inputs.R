###############################################################################
# 05_key_results_csv.R — Export a handful of headline results from
# table_descriptives / table_outcomes to CSV, split by training-primary vs.
# sector-program subsample.
###############################################################################

library(metafor)

dat <- readRDS("data/processed_data.rds")

primary_idx <- which(!is.na(dat$training_role) & dat$training_role == "primary")
sector_idx  <- which(!is.na(dat$sector_program) & dat$sector_program == 1)

groups <- list(training_primary = primary_idx, sector_programs = sector_idx)

# REML random-effects mean, cluster-robust at the project level — same
# weighting as the "Impacts" / "Control-group means" rows in table_outcomes.
# See src/03_tables.R's ms_reml() for the source of this logic.
reml_mean <- function(yi, sei, cluster) {
  ok <- !is.na(yi) & !is.na(sei)
  yi <- yi[ok]; sei <- sei[ok]; cluster <- cluster[ok]
  k <- length(yi)
  if (k == 0) return(NA_real_)
  if (k == 1) return(yi)
  fit <- tryCatch(suppressWarnings(rma(yi = yi, sei = sei, method = "REML")),
                  error = function(e) NULL)
  if (is.null(fit)) return(NA_real_)
  if (length(unique(cluster)) > 1) {
    fit <- tryCatch(suppressWarnings(metafor::robust(fit, cluster = cluster)),
                    error = function(e) fit)
  }
  unname(coef(fit))
}

reml_row <- function(y_col, se_col) {
  vapply(groups, function(idx)
    reml_mean(dat[[y_col]][idx], dat[[se_col]][idx], dat$project[idx]),
    numeric(1))
}

# Unweighted mean over non-missing values — same as descriptive_value() in
# src/03_tables.R for the "Cost per treated" row.
plain_mean <- function(x_col) {
  vapply(groups, function(idx) mean(dat[[x_col]][idx], na.rm = TRUE),
         numeric(1))
}

# net_cost_broad also nets out the difference in all training-related
# services received (including any reduction in other, business-as-usual
# training the control group would otherwise get); net_cost_narrow counts
# only the evaluated program's own experiment spending. Their ratio, less 1,
# is the "reduced other training" adjustment, expressed as a fraction of
# net_cost_narrow. Mean of ratios (not ratio of means) because studies with
# small narrow costs represent proportionally large adjustments. Rows where
# narrow == broad are excluded from the estimation sample rather than
# treated as a true zero adjustment — that equality usually just reflects
# missing cost-breakdown information, not a genuine finding of no adjustment.
#
# Those excluded rows (plus any row missing net_cost_broad entirely) get an
# imputed net_cost_broad = narrow * (1 + rate), then the ratio stat is
# recomputed over the full narrow-available set, real values and imputed
# values together. (This necessarily reproduces `rate` itself — imputing
# with the estimation sample's own mean ratio cannot move that mean — but
# the recomputation is kept explicit because it's the actual mechanism, and
# it leaves `broad_imputed` available if a row-level broad figure is needed
# elsewhere.)
cost_adjustment <- function(idx) {
  narrow <- dat$net_cost_narrow[idx]
  broad  <- dat$net_cost_broad[idx]
  has_narrow <- !is.na(narrow)
  used <- has_narrow & !is.na(broad) & narrow != broad

  rate <- mean(broad[used] / narrow[used]) - 1

  broad_imputed <- broad
  impute_me <- has_narrow & !used
  broad_imputed[impute_me] <- narrow[impute_me] * (1 + rate)

  mean(broad_imputed[has_narrow] / narrow[has_narrow]) - 1
}

results <- rbind(
  "Impact on employment, years 3-5"                = reml_row("lt_emp_impact", "lt_emp_se")/100,
  "Earnings impact, year 1"                         = reml_row("st_earn_impact", "st_earn_se"),
  "Earnings impact, year 2"                         = reml_row("mt_earn_impact", "mt_earn_se"),
  "Earnings impact, years 3-5"                      = reml_row("lt_earn_impact", "lt_earn_se"),
  "Control group earnings, years 3-5"               = reml_row("lt_earn_control_mean", "lt_earn_se"),
  "Average cost"                                    = plain_mean("net_cost_narrow"),
  "Cost adjustment for reduced other training"      = vapply(groups, cost_adjustment, numeric(1))
)

out <- data.frame(result = rownames(results), results, row.names = NULL,
                  check.names = FALSE)
names(out) <- c("Result", "Training-primary programs", "Sector programs")

dir.create("output", showWarnings = FALSE)
write.csv(out, "output/benefit_cost_inputs.csv", row.names = FALSE)
cat("Wrote output/benefit_cost_inputs.csv\n")
print(out)
