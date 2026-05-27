###############################################################################
# 00_run_all.R — Master script: runs all analysis steps in order
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

cat("====== 01_prep.R ======\n")
source("src/01_prep.R")

cat("\n====== 01b_prep_estimates.R ======\n")
source("src/01b_prep_estimates.R")

cat("\n====== 02_forest.R ======\n")
source("src/02_forest.R")

cat("\n====== 02b_forest_subgroups.R ======\n")
source("src/02b_forest_subgroups.R")

cat("\n====== 02c_funnel.R ======\n")
source("src/02c_funnel.R")

cat("\n====== 02d_funnel_subgroups.R ======\n")
source("src/02d_funnel_subgroups.R")

cat("\n====== 02e_cost_scatter.R ======\n")
source("src/02e_cost_scatter.R")

cat("\n====== 02f_estimates_scatter.R ======\n")
source("src/02f_estimates_scatter.R")

cat("\n====== 03_tables.R ======\n")
source("src/03_tables.R")

cat("\n====== All steps complete. ======\n")
