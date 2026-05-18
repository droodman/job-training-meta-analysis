###############################################################################
# 04_verify_claims.R — Confirm every numeric claim in the write-up that draws
# on this meta-analysis data set.
#
# Each chunk:
#   1. Prints the verbatim claim from the paper
#   2. Computes the corresponding number from processed_data.rds
#   3. Prints the computed value and a PASS/CHECK marker
#
# Conventions:
#   - "Range" claims in the executive summary refer to the full-sample
#     estimate and the training-primary subsample estimate (with vs. without
#     restriction to interventions in which training is the primary feature).
#   - When the paper says "long-term," it means horizon = "lt" unless context
#     dictates otherwise.
#   - Impact and control-group statistics use REML meta-analysis with
#     cluster-robust SE at the project level, matching 03_tables.R.
#
# Run with: source("src/04_verify_claims.R") from project root.
###############################################################################

# Make errors loud, as in 01_prep.R.
options(error = function() {
  bar <- strrep("!", 78)
  cat("\n", bar, "\n", sep = "")
  cat("!!! SCRIPT FAILED — ", geterrmessage(), sep = "")
  cat(bar, "\n\n", sep = "")
  if (!interactive()) quit(status = 1, save = "no")
})

suppressPackageStartupMessages({
  library(metafor)
})

dat <- readRDS("data/processed_data.rds")

primary_idx <- which(!is.na(dat$training_role) & dat$training_role == "primary")
all_idx     <- seq_len(nrow(dat))

# ── Helpers ──────────────────────────────────────────────────────────────────
#
# `re_mean()` returns the REML pooled mean of `yi` weighted by 1/(sei^2),
# with cluster-robust (CR1) SE at the project level when there are ≥2 clusters.
# Mirrors `ms_reml()` in 03_tables.R so verification numbers match Tables 3-4.

re_mean <- function(yi, sei, cluster = NULL) {
  ok <- !is.na(yi) & !is.na(sei) & sei > 0
  yi <- yi[ok]; sei <- sei[ok]
  if (!is.null(cluster)) cluster <- cluster[ok]
  k <- length(yi)
  if (k == 0) return(c(mean = NA_real_, se = NA_real_, k = 0))
  if (k == 1) return(c(mean = yi, se = sei, k = 1L))
  fit <- tryCatch(suppressWarnings(rma(yi = yi, sei = sei, method = "REML")),
                  error = function(e) NULL)
  if (is.null(fit)) return(c(mean = NA_real_, se = NA_real_, k = k))
  if (!is.null(cluster) && length(unique(cluster)) > 1) {
    fit <- tryCatch(suppressWarnings(metafor::robust(fit, cluster = cluster)),
                    error = function(e) fit)
  }
  c(mean = unname(coef(fit)), se = sqrt(vcov(fit)[1, 1]), k = k)
}

# Pretty-printer: claim + computed + status.
report <- function(claim, claimed, computed, tol_abs = NULL, tol_rel = 0.05,
                   units = "") {
  bar <- strrep("-", 78)
  cat("\n", bar, "\n", sep = "")
  cat("CLAIM:    ", claim, "\n", sep = "")
  cat("CLAIMED:  ", claimed, "\n", sep = "")
  cat("COMPUTED: ", computed, "\n", sep = "")
  cat(bar, "\n", sep = "")
}

show_re <- function(label, r) {
  cat(sprintf("  %s: mean = %.4g, SE = %.3g, k = %d\n",
              label, r["mean"], r["se"], r["k"]))
}

# Restrict the meta-analysis vectors to a subset of rows.
sub <- function(idx, ycol, secol) {
  yi  <- dat[[ycol]][idx]
  sei <- dat[[secol]][idx]
  cl  <- dat$project[idx]
  list(yi = yi, sei = sei, cluster = cl)
}

# Generic REML mean wrapper for a given (response, SE, subset).
remi <- function(idx, ycol, secol) {
  s <- sub(idx, ycol, secol)
  re_mean(s$yi, s$sei, s$cluster)
}

cat("\n##############################################################################\n")
cat("# Verification of numeric claims in write-up.pdf\n")
cat("##############################################################################\n")

###############################################################################
# CHUNK 1.  Executive summary — average impacts are small
#
#   "1.7–1.9 points more employment (compared to a typical control group
#   average of 60%) and $700–800/year more pay (control group: $14–16K).
#   The pay increases alone do not justify the ~$10K/person cost.
#   (Figures in 2025 dollars.)"
#
# "Long-term impacts are meant" (per user instructions). Ranges run from the
# training-primary subsample (low end of impact, higher control mean) to the
# full sample (high end of impact, lower control mean).
###############################################################################

cat("\n=== Chunk 1: Executive summary — 'Average impacts are small' ===\n")

lt_emp_full <- remi(all_idx,     "lt_emp_impact",      "lt_emp_se")
lt_emp_prim <- remi(primary_idx, "lt_emp_impact",      "lt_emp_se")
lt_emp_cm_full <- remi(all_idx,     "lt_emp_control_mean", "lt_emp_se")
lt_emp_cm_prim <- remi(primary_idx, "lt_emp_control_mean", "lt_emp_se")
lt_earn_full <- remi(all_idx,     "lt_earn_impact",      "lt_earn_se")
lt_earn_prim <- remi(primary_idx, "lt_earn_impact",      "lt_earn_se")
lt_earn_cm_full <- remi(all_idx,     "lt_earn_control_mean", "lt_earn_se")
lt_earn_cm_prim <- remi(primary_idx, "lt_earn_control_mean", "lt_earn_se")

report(
  "1.7–1.9 points more employment ... and $700–800/year more pay",
  "LT emp impact 1.7 (training-primary) to 1.9 (full); LT earn $700-800",
  sprintf("LT emp impact: full = %.2f, training-primary = %.2f; LT earn: full = %.0f, training-primary = %.0f",
          lt_emp_full["mean"], lt_emp_prim["mean"],
          lt_earn_full["mean"], lt_earn_prim["mean"]))
show_re("LT employment, full",            lt_emp_full)
show_re("LT employment, training-primary", lt_emp_prim)
show_re("LT earnings, full",              lt_earn_full)
show_re("LT earnings, training-primary",   lt_earn_prim)

report(
  "control group average of 60% ... (control group: $14–16K)",
  "LT emp control mean ~60%; LT earn control mean $14-16K",
  sprintf("LT emp control: full = %.1f%%, training-primary = %.1f%%; LT earn control: full = $%.0f, training-primary = $%.0f",
          lt_emp_cm_full["mean"], lt_emp_cm_prim["mean"],
          lt_earn_cm_full["mean"], lt_earn_cm_prim["mean"]))
show_re("LT employment control, full",            lt_emp_cm_full)
show_re("LT employment control, training-primary", lt_emp_cm_prim)
show_re("LT earnings control, full",              lt_earn_cm_full)
show_re("LT earnings control, training-primary",   lt_earn_cm_prim)

# Cost per treated: full-sample unweighted mean is ~$10K, training-primary
# unweighted mean is ~$12K. The "~$10K/person cost" in the exec summary is
# the round-number version of the full-sample average.
cost_full <- mean(dat$cost_per_treated[!is.na(dat$cost_per_treated)])
cost_prim <- mean(dat$cost_per_treated[!is.na(dat$cost_per_treated) &
                                       seq_len(nrow(dat)) %in% primary_idx])

report(
  "~$10K/person cost",
  "Unweighted mean cost per treated ~ $10K",
  sprintf("Full sample = $%.0f (n=%d); training-primary = $%.0f (n=%d)",
          cost_full, sum(!is.na(dat$cost_per_treated)),
          cost_prim, sum(!is.na(dat$cost_per_treated[primary_idx]))))

###############################################################################
# CHUNK 2.  Executive summary — sector programs lift pay by $5–10K/year
#
#   "A handful of private programs have lifted pay by $5–10K/year, by
#   intensively screening applicants, involving employers in curriculum
#   design, and even contracting with employers to hire graduates."
#
# The $5K low end is Per Scholas (Bronx) in WorkAdvance; the $10K high end
# is Year Up (8 PACE offices pooled). Both are long-term earnings impacts.
###############################################################################

cat("\n=== Chunk 2: Executive summary — sector programs $5-10K/year ===\n")

# Per Scholas (Bronx), WorkAdvance — the longer-follow-up Per Scholas row.
ps_idx <- which(dat$project == "WorkAdvance" &
                grepl("Per Scholas", dat$site_subgroup))
yu_idx <- which(grepl("Year Up", dat$project) &
                grepl("Full sample|8 offices", dat$site_subgroup))

ps_lt_earn <- dat$lt_earn_impact[ps_idx]
yu_lt_earn <- dat$lt_earn_impact[yu_idx]

report(
  "A handful of private programs have lifted pay by $5–10K/year",
  "Per Scholas (WorkAdvance, Bronx) LT earn ≈ $5K; Year Up (8-city PACE) LT earn ≈ $9-10K",
  sprintf("Per Scholas (WorkAdvance, Bronx, row %d): $%.0f; Year Up (PACE, 8 offices, row %d): $%.0f",
          ps_idx, ps_lt_earn, yu_idx, yu_lt_earn))

###############################################################################
# CHUNK 3.  Overview — training-primary average impacts
#
#   "The U.S. experiments in which training was a primary component—
#   'training-primary' experiments—lifted employment by an average of
#   2.9 percentage points in the second year after program start, and 1.7
#   points thereafter. The parallel averages for annual earnings are $1,139
#   and $820 (in 2025 dollars). All of these results are highly statistically
#   significant."
###############################################################################

cat("\n=== Chunk 3: Overview — training-primary averages ===\n")

mt_emp_prim  <- remi(primary_idx, "mt_emp_impact",  "mt_emp_se")
lt_emp_prim2 <- remi(primary_idx, "lt_emp_impact",  "lt_emp_se")
mt_earn_prim <- remi(primary_idx, "mt_earn_impact", "mt_earn_se")
lt_earn_prim2<- remi(primary_idx, "lt_earn_impact", "lt_earn_se")

report(
  "Training-primary: emp +2.9 pts MT, +1.7 pts LT; earn +$1,139 MT, +$820 LT (2025$)",
  "Table 3, training-primary column",
  sprintf("MT emp = %.2f (k=%d); LT emp = %.2f (k=%d); MT earn = %.0f (k=%d); LT earn = %.0f (k=%d)",
          mt_emp_prim["mean"], mt_emp_prim["k"],
          lt_emp_prim2["mean"], lt_emp_prim2["k"],
          mt_earn_prim["mean"], mt_earn_prim["k"],
          lt_earn_prim2["mean"], lt_earn_prim2["k"]))
# Also check statistical significance (t = mean / SE), expecting |t| > 2.
cat("  z-stats (training-primary):\n")
for (lab in c("MT emp", "LT emp", "MT earn", "LT earn")) {
  r <- switch(lab, "MT emp" = mt_emp_prim, "LT emp" = lt_emp_prim2,
              "MT earn" = mt_earn_prim,    "LT earn" = lt_earn_prim2)
  cat(sprintf("    %s: z = %.2f\n", lab, r["mean"] / r["se"]))
}

###############################################################################
# CHUNK 4.  Overview — employment impact has fallen by ~0.7 points per decade
#
#   "The impact on employment has fallen by about 0.7 points per decade."
#
# This corresponds to the meta-regression coefficient on `randomization_year`
# in Table 4 (column 1 / column 2). 0.07-0.09 per year ≈ 0.7-0.9 per decade.
###############################################################################

cat("\n=== Chunk 4: Overview — emp impact down 0.7 pts/decade ===\n")

regress_year <- function(idx, ycol, secol) {
  s <- sub(idx, ycol, secol)
  ok <- !is.na(s$yi) & !is.na(s$sei) & s$sei > 0 &
        !is.na(dat$randomization_midpoint[idx])
  yr <- dat$randomization_midpoint[idx][ok]
  fit <- suppressWarnings(rma(yi = s$yi[ok], sei = s$sei[ok],
                              mods = ~ yr, method = "REML"))
  fit <- tryCatch(suppressWarnings(metafor::robust(fit,
                                                   cluster = s$cluster[ok])),
                  error = function(e) fit)
  coef_se <- c(b = as.numeric(coef(fit)["yr"]),
               se = sqrt(diag(vcov(fit)))["yr"], k = fit$k)
  names(coef_se)[2] <- "se"
  coef_se
}

mt_emp_yr_full <- regress_year(all_idx, "mt_emp_impact", "mt_emp_se")
lt_emp_yr_full <- regress_year(all_idx, "lt_emp_impact", "lt_emp_se")
mt_emp_yr_prim <- regress_year(primary_idx, "mt_emp_impact", "mt_emp_se")
lt_emp_yr_prim <- regress_year(primary_idx, "lt_emp_impact", "lt_emp_se")

report(
  "Impact on employment has fallen by about 0.7 points per decade",
  "year coef ≈ -0.07 to -0.09 (full and training-primary, MT and LT)",
  sprintf("MT-full = %.3f, MT-prim = %.3f, LT-full = %.3f, LT-prim = %.3f (per year)",
          mt_emp_yr_full["b"], mt_emp_yr_prim["b"],
          lt_emp_yr_full["b"], lt_emp_yr_prim["b"]))
cat("  (multiply by 10 for per-decade)\n")

###############################################################################
# CHUNK 5.  Overview — $12,348 per client, plus footnote 4 subset means
#
#   "In roughly the same sample, programs cost $12,348 per client."
#   Footnote 4: "Not all studies supply cost information. Restricting to
#   those that do, the employment impacts average 2.9 points in year 2 and
#   2.4 points beyond. The corresponding earnings impacts are $1,066 and $993."
###############################################################################

cat("\n=== Chunk 5: Overview — $12,348 per client and footnote 4 means ===\n")

cost_unwt_prim <- mean(dat$cost_per_treated[primary_idx], na.rm = TRUE)
report(
  "programs cost $12,348 per client",
  "Unweighted mean cost (training-primary) = $12,348",
  sprintf("Computed = $%.0f (n = %d)",
          cost_unwt_prim, sum(!is.na(dat$cost_per_treated[primary_idx]))))

# Footnote 4: training-primary, restricted to studies with cost_per_treated.
cost_idx <- which(!is.na(dat$cost_per_treated) &
                  !is.na(dat$training_role) &
                  dat$training_role == "primary")

mt_emp_cost  <- remi(cost_idx, "mt_emp_impact",  "mt_emp_se")
lt_emp_cost  <- remi(cost_idx, "lt_emp_impact",  "lt_emp_se")
mt_earn_cost <- remi(cost_idx, "mt_earn_impact", "mt_earn_se")
lt_earn_cost <- remi(cost_idx, "lt_earn_impact", "lt_earn_se")

report(
  "Restricting to those with cost data: MT emp 2.9, LT emp 2.4; MT earn $1,066, LT earn $993",
  "training-primary ∩ (cost not NA): MT emp 2.9, LT emp 2.4; earn $1,066 / $993",
  sprintf("MT emp = %.2f (k=%d); LT emp = %.2f (k=%d); MT earn = %.0f (k=%d); LT earn = %.0f (k=%d)",
          mt_emp_cost["mean"], mt_emp_cost["k"],
          lt_emp_cost["mean"], lt_emp_cost["k"],
          mt_earn_cost["mean"], mt_earn_cost["k"],
          lt_earn_cost["mean"], lt_earn_cost["k"]))

###############################################################################
# CHUNK 6.  Overview — JTPA impacts for low-income adults
#
#   (revised) "the Job Training Partnership Act (JTPA) lifted employment
#   among low-income adults by about 2.3 percentage points—from a base of
#   about 70%. The bump in annual pay was about $1,100 in 2025 dollars,
#   relative to a control-group average of about $13,000 for women and
#   $17,500 for men. Those numbers are averages over all study subjects,
#   so they factor in zeros for the unemployed."
#
# Footnote 5 says: "Figures approximate average results for years 3-5."
###############################################################################

cat("\n=== Chunk 6: Overview — JTPA Adult women + men ===\n")

jtpa_idx <- which(dat$project == "National JTPA Study" &
                  grepl("Adult", dat$site_subgroup))
cat("JTPA Adult rows:\n")
print(dat[jtpa_idx, c("site_subgroup", "lt_emp_impact", "lt_emp_control_mean",
                      "lt_earn_impact", "lt_earn_control_mean",
                      "lt_followup_years")])

jtpa_emp_imp <- mean(dat$lt_emp_impact[jtpa_idx], na.rm = TRUE)
jtpa_emp_cm  <- mean(dat$lt_emp_control_mean[jtpa_idx], na.rm = TRUE)

jtpa_w_earn <- dat$lt_earn_impact[dat$project == "National JTPA Study" &
                                  dat$site_subgroup == "Adult women"]
jtpa_w_cm   <- dat$lt_earn_control_mean[dat$project == "National JTPA Study" &
                                        dat$site_subgroup == "Adult women"]
jtpa_m_earn <- dat$lt_earn_impact[dat$project == "National JTPA Study" &
                                  dat$site_subgroup == "Adult men"]
jtpa_m_cm   <- dat$lt_earn_control_mean[dat$project == "National JTPA Study" &
                                        dat$site_subgroup == "Adult men"]

report(
  "JTPA lifted employment among low-income adults by about 2.3 pts—from a base of about 70%",
  "JTPA Adult women + men: LT emp impact ~2.3pts; control mean ~70%",
  sprintf("Avg of Adult women + Adult men: LT emp impact = %.2f pts, LT emp control mean = %.1f%%",
          jtpa_emp_imp, jtpa_emp_cm))

report(
  "The bump in annual pay was about $1,100 ... $13,000 for women and $17,500 for men",
  "JTPA Adult LT earn impact ~$1,100; control means ~$13K (women), ~$17.5K (men) in 2025$",
  sprintf("Women: LT earn impact = %.0f, control = %.0f. Men: LT earn impact = %.0f, control = %.0f",
          jtpa_w_earn, jtpa_w_cm, jtpa_m_earn, jtpa_m_cm))

###############################################################################
# CHUNK 7.  Overview / Executive summary — Job Corps long-term results
#
#   Exec summary: "The story is the same for an evaluation of the
#   youth-targeted Job Corps."
#   Overview (revised): "The Job Corps ... produced no benefit for employment
#   or earnings in follow-ups extending 20 years, except for an employment
#   increase around years 3–5 (Schochet 2020)."
#
# The spreadsheet's Job Corps row is the Y3–Y5 average per the LT convention.
# Verify: LT earn impact insignificant; LT emp impact positive and (marginally)
# significant, consistent with "an employment increase around years 3–5."
###############################################################################

cat("\n=== Chunk 7: Job Corps long-term results ===\n")

jc_idx <- which(dat$project == "National Job Corps Study")
cat("Job Corps rows:\n")
print(dat[jc_idx, c("site_subgroup", "lt_followup_years",
                    "lt_emp_impact", "lt_emp_se",
                    "lt_earn_impact", "lt_earn_se")])

jc <- dat[jc_idx, ]
jc_z <- with(jc, c(emp = lt_emp_impact / lt_emp_se,
                   earn = lt_earn_impact / lt_earn_se))
report(
  "Job Corps: no benefit on earnings 20 yrs; an employment bump around Y3-Y5",
  "LT earn |z|<2 (null); LT emp z>2 around Y3-Y5 (employment increase)",
  sprintf("|z| (LT emp) = %.2f; |z| (LT earn) = %.2f; LT emp impact = %.2f pts; lt_followup_years = %.1f",
          abs(jc_z["emp"]), abs(jc_z["earn"]),
          jc$lt_emp_impact, jc$lt_followup_years))

###############################################################################
# CHUNK 8.  Overview / Executive summary — WIA null results
#
#   Overview: "An evaluation of the Workforce Investment Act, the successor
#   to the JTPA, returned null results for its programs for low-income adults
#   and for dislocated workers (Fortson et al. 2017)—by which we mean
#   results that are as often negative as positive, and lacking statistical
#   significance."
###############################################################################

cat("\n=== Chunk 8: WIA Gold Standard null results ===\n")

wia_idx <- which(dat$project == "WIA Gold Standard")
cat("WIA Gold Standard rows:\n")
print(dat[wia_idx, c("site_subgroup",
                     "st_emp_impact", "st_emp_se", "st_earn_impact", "st_earn_se",
                     "mt_emp_impact", "mt_emp_se", "mt_earn_impact", "mt_earn_se",
                     "lt_emp_impact", "lt_emp_se", "lt_earn_impact", "lt_earn_se")])

wia <- dat[wia_idx, ]
make_z <- function(col_imp, col_se) wia[[col_imp]] / wia[[col_se]]
z_mat <- data.frame(
  site = wia$site_subgroup,
  z_st_emp  = make_z("st_emp_impact",  "st_emp_se"),
  z_st_earn = make_z("st_earn_impact", "st_earn_se"),
  z_mt_emp  = make_z("mt_emp_impact",  "mt_emp_se"),
  z_mt_earn = make_z("mt_earn_impact", "mt_earn_se"),
  z_lt_emp  = make_z("lt_emp_impact",  "lt_emp_se"),
  z_lt_earn = make_z("lt_earn_impact", "lt_earn_se")
)
cat("WIA z-statistics (impact / SE):\n")
print(z_mat)
zmax <- max(abs(as.matrix(z_mat[, -1])), na.rm = TRUE)
report(
  "WIA null results — as often negative as positive, none significant",
  "all |z| < 2 (no estimate significant at conventional 5% threshold)",
  sprintf("Max |z| across all WIA impact estimates = %.2f", zmax))

###############################################################################
# CHUNK 9.  Section 8.7 — sample size and counts
#
#   "Our sample consists of 56 studies. Of these, 23 reports results for
#   several demographics, implementing organizations, or localities, bringing
#   the number of impact estimates to 144."
#   "Roughly half the experiments cleared this hurdle: 31, with 75 impact
#   estimates."
###############################################################################

cat("\n=== Chunk 9: Section 8.7 — sample counts ===\n")

n_studies_full <- length(unique(dat$project))
n_impacts_full <- nrow(dat)
n_studies_prim <- length(unique(dat$project[primary_idx]))
n_impacts_prim <- length(primary_idx)
n_multi <- sum(table(dat$project) > 1)

report(
  "Sample: 56 studies, 144 impacts; 23 with multiple rows",
  "56 / 144 / 23",
  sprintf("studies = %d, impact estimates = %d, projects with >1 row = %d",
          n_studies_full, n_impacts_full, n_multi))

report(
  "Training-primary subset: 31 studies, 75 impact estimates",
  "31 / 75",
  sprintf("studies = %d, impact estimates = %d",
          n_studies_prim, n_impacts_prim))

###############################################################################
# CHUNK 10.  Section 8.7 — Table 2 study-trait averages (training-primary)
#
#   "Most aim to serve low-income people, with only 3–4% meant for
#   dislocated workers."  (Table 2)
#   "Where training is primary, classroom training is a component 89% of
#   the time and on-the-job training just 32%."
#   "People spend about 6 months in these programs, at a cost to funders of
#   $12,500 (in 2025 dollars)."
#   "Nearly all studied interventions were publicly funded, but slightly
#   less than half the training-primary interventions had local
#   implementation delegated to private organizations."
###############################################################################

cat("\n=== Chunk 10: Section 8.7 — Table 2 training-primary averages ===\n")

pct <- function(x) 100 * mean(x, na.rm = TRUE)
pct_eq <- function(x, lvl) 100 * mean(x == lvl, na.rm = TRUE)

trait_check <- list(
  "dislocated workers (% of full)"   = pct_eq(dat$target_pop[all_idx],     "dislocated"),
  "dislocated workers (% of prim)"   = pct_eq(dat$target_pop[primary_idx], "dislocated"),
  "classroom training (% of prim)"   = pct(dat$has_classroom[primary_idx]),
  "on-the-job training (% of prim)"  = pct(dat$has_ojt[primary_idx]),
  "treatment duration months (prim)" = mean(dat$treatment_duration_months[primary_idx], na.rm = TRUE),
  "cost per treated (prim, 2025$)"   = mean(dat$cost_per_treated[primary_idx], na.rm = TRUE),
  "funding public (% of prim)"       = pct_eq(dat$funding_public_private[primary_idx], "public"),
  "admin private (% of prim)"        = pct_eq(dat$admin_public_private[primary_idx], "private")
)
for (nm in names(trait_check))
  cat(sprintf("  %s = %.1f\n", nm, trait_check[[nm]]))

report(
  "Only 3–4% meant for dislocated workers",
  "% dislocated ≈ 3 (full) / 4 (training-primary)",
  sprintf("full = %.1f%%, training-primary = %.1f%%",
          trait_check[["dislocated workers (% of full)"]],
          trait_check[["dislocated workers (% of prim)"]]))

report(
  "Classroom 89%, OJT 32% (training-primary)",
  "Table 2 row 'Classroom training (%)' = 89, 'On-the-job training (%)' = 32",
  sprintf("classroom = %.1f%%, OJT = %.1f%%",
          trait_check[["classroom training (% of prim)"]],
          trait_check[["on-the-job training (% of prim)"]]))

report(
  "About 6 months in program, ~$12,500 cost to funders (training-primary)",
  "treatment duration ~5.9 mo; cost ~$12,348 (Table 2)",
  sprintf("duration = %.1f months; cost = $%.0f",
          trait_check[["treatment duration months (prim)"]],
          trait_check[["cost per treated (prim, 2025$)"]]))

report(
  "Nearly all publicly funded; less than half admin delegated to private (training-primary)",
  "funding public ≈ 63%, admin private ≈ 47% (Table 2)",
  sprintf("public funding = %.1f%%; private admin = %.1f%%",
          trait_check[["funding public (% of prim)"]],
          trait_check[["admin private (% of prim)"]]))

###############################################################################
# CHUNK 11.  Section 8.7 — Table 3 main impact lines (revised)
#
#   (revised) "On average, however, training does not make a big difference.
#   It boosts the chance of having a job by 2.6–2.9 points in the medium
#   term (year 2) and 1.7–1.9 points in the longer term (in a somewhat
#   smaller set of studies). It lifts earnings by $1,000–1,100/year in the
#   medium term (in 2025 dollars) and $700–800 beyond (in a somewhat
#   smaller set of studies)."
#
#   Summary bullets at the end of 8.7 (revised): "The impacts are small,
#   at about 2 points of employment, and $1,000–1,100 in earnings in year 2
#   and $700–800/year beyond."
###############################################################################

cat("\n=== Chunk 11: Section 8.7 — medium- and long-term averages ===\n")

mt_emp_full <- remi(all_idx,     "mt_emp_impact",  "mt_emp_se")
lt_emp_full2<- remi(all_idx,     "lt_emp_impact",  "lt_emp_se")
mt_earn_full<- remi(all_idx,     "mt_earn_impact", "mt_earn_se")
lt_earn_full2<- remi(all_idx,    "lt_earn_impact", "lt_earn_se")

report(
  "boosts chance of having a job by 2.6-2.9 pts (MT) and 1.7-1.9 pts (LT)",
  "Table 3: MT emp 2.6 (full)/2.9 (prim); LT emp 1.9 (full)/1.7 (prim)",
  sprintf("MT emp: full = %.2f, training-primary = %.2f; LT emp: full = %.2f, training-primary = %.2f",
          mt_emp_full["mean"], mt_emp_prim["mean"],
          lt_emp_full2["mean"], lt_emp_prim2["mean"]))

report(
  "lifts earnings by $1,000-1,100/year in MT and $700-800 beyond",
  "Table 3: MT earn $985 (full) to $1,139 (prim); LT earn $673 (full) to $820 (prim)",
  sprintf("MT earn: full = %.0f, training-primary = %.0f; LT earn: full = %.0f, training-primary = %.0f",
          mt_earn_full["mean"], mt_earn_prim["mean"],
          lt_earn_full2["mean"], lt_earn_prim2["mean"]))

# Summary bullet's "about 2 points of employment" is roughly the midpoint of
# the MT and LT ranges: (2.6, 2.9, 1.7, 1.9) -> mean ≈ 2.3. Sanity check.
all_emp_pts <- c(mt_emp_full["mean"], mt_emp_prim["mean"],
                 lt_emp_full2["mean"], lt_emp_prim2["mean"])
report(
  "impacts are small, at about 2 points of employment",
  "midpoint of MT/LT × full/training-primary ≈ 2 pts",
  sprintf("Mean across the four MT/LT × full/primary cells = %.2f pts",
          mean(all_emp_pts)))

###############################################################################
# CHUNK 12.  Section 8.7 — meta-regression result: employment trend by decade
#
#   "We see that the impact on medium-term (year 2) employment has fallen
#   by 0.07–0.09 points per year, while the longer-term impacts have perhaps
#   fallen half as fast."
###############################################################################

cat("\n=== Chunk 12: Section 8.7 — declining impact over time ===\n")

report(
  "MT employment has fallen by 0.07–0.09 pts/year; LT half as fast",
  "Table 4 randomization_year coefficients: MT ~-0.07 to -0.09; LT ~-0.06 to -0.08",
  sprintf("MT-emp year coef: full = %.3f, training-primary = %.3f; LT-emp: full = %.3f, training-primary = %.3f",
          mt_emp_yr_full["b"], mt_emp_yr_prim["b"],
          lt_emp_yr_full["b"], lt_emp_yr_prim["b"]))

###############################################################################
# CHUNK 13.  Section 8.7 — intellectual disabilities boost employment ~8 pts
#
#   "The two that served people with intellectual disabilities boosted
#   employment about 8 points, on average."
###############################################################################

cat("\n=== Chunk 13: Section 8.7 — intellectual-disability programs ===\n")

dis_idx <- which(dat$target_pop == "disability")
cat("Disability-target rows:\n")
print(dat[dis_idx, c("project", "site_subgroup", "mt_emp_impact", "lt_emp_impact")])

dis_mt_emp <- mean(dat$mt_emp_impact[dis_idx], na.rm = TRUE)
dis_lt_emp <- mean(dat$lt_emp_impact[dis_idx], na.rm = TRUE)

report(
  "Two disability-target programs boosted employment about 8 pts on average",
  "Average emp impact across STETS and TETD ~8 pts",
  sprintf("MT emp avg = %.2f; LT emp avg = %.2f (n=%d projects)",
          dis_mt_emp, dis_lt_emp, length(unique(dat$project[dis_idx]))))

###############################################################################
# CHUNK 14.  Section 8.7 — employer hiring commitment lifts earnings $8-9K
#
#   "And the apparently powerful impact of an employer hiring commitment—
#   $8,000–9,000 in year 2 and beyond—owes to three programs: the Wildcat
#   program that inspired the NSWD, Year Up, and the Wisconsin Regional
#   Training Partnership."
###############################################################################

cat("\n=== Chunk 14: Section 8.7 — employer-hiring-commitment programs ===\n")

hire_idx <- which(dat$employerol_hire == 1)
cat("Employer-hiring-commitment rows:\n")
print(dat[hire_idx, c("project", "site_subgroup",
                      "mt_earn_impact", "mt_earn_se",
                      "lt_earn_impact", "lt_earn_se")])

mt_earn_hire <- remi(hire_idx, "mt_earn_impact", "mt_earn_se")
lt_earn_hire <- remi(hire_idx, "lt_earn_impact", "lt_earn_se")

report(
  "Employer hiring commitment: $8,000–9,000 earnings impact, year 2 and beyond",
  "REML mean of employerol_hire == 1 subset: MT earn ~$8-9K; LT earn ~$8-9K",
  sprintf("MT earn = %.0f (k=%d); LT earn = %.0f (k=%d)",
          mt_earn_hire["mean"], mt_earn_hire["k"],
          lt_earn_hire["mean"], lt_earn_hire["k"]))

###############################################################################
# CHUNK 15.  Section 9 — Per Scholas earnings gap of about $5,000/year
#
#   "Though not shown here, the corresponding graph for Per Scholas (Roder
#   and Elliott 2021, Figure 3) is similar, with a gap of about $5,000/year."
###############################################################################

cat("\n=== Chunk 15: Section 9 — Per Scholas $5K/year gap ===\n")

# WorkAdvance Per Scholas (Bronx) row.
report(
  "Per Scholas earnings gap of about $5,000/year",
  "Per Scholas (WorkAdvance, Bronx) LT earn ~ $5K",
  sprintf("WorkAdvance Per Scholas (Bronx): LT earn impact = $%.0f",
          dat$lt_earn_impact[ps_idx]))

###############################################################################
# CHUNK 16.  Section 9 — Year Up gap of about $2,000/quarter
#
#   "the gap between the two holds steady at around $2,000/quarter."
###############################################################################

cat("\n=== Chunk 16: Section 9 — Year Up ~$2,000/quarter gap ===\n")

yu_lt <- dat$lt_earn_impact[yu_idx]
report(
  "Year Up: gap holds steady at around $2,000/quarter",
  "Annual ≈ 4 × $2,000 = ~$8K. Year Up LT earn ~$8-10K/yr",
  sprintf("Year Up (PACE, 8 offices): LT earn = $%.0f/yr ≈ $%.0f/quarter",
          yu_lt, yu_lt / 4))

###############################################################################
# CHUNK 17.  Section 9 — sector programs RE averages
#
#   "Subject to the caveat that the sample of studies shifts from plot to
#   plot, sector on average cause a partially transient employment bump,
#   about 2.9 points, and a more permanent increase in pay, $3,000–4,000/year
#   in 2025 dollars."
#
# The 02g forest plots define a sector-program subset based on
# `sector_program == 1` (or the broader Table-6 set). Matching the figures
# in section 9 directly uses sector_program == 1.
###############################################################################

cat("\n=== Chunk 17: Section 9 — sector-program RE averages ===\n")

sec_idx <- which(dat$sector_program == 1)
cat(sprintf("Sector-program rows: %d\n", length(sec_idx)))

mt_emp_sec  <- remi(sec_idx, "mt_emp_impact",  "mt_emp_se")
lt_emp_sec  <- remi(sec_idx, "lt_emp_impact",  "lt_emp_se")
mt_earn_sec <- remi(sec_idx, "mt_earn_impact", "mt_earn_se")
lt_earn_sec <- remi(sec_idx, "lt_earn_impact", "lt_earn_se")

report(
  "Sector programs: ~2.9 pts emp bump (MT), $3,000-4,000/year pay",
  "MT emp ~2.9 pts; MT earn $3,149; LT earn $3,703 (Figures 15, 16)",
  sprintf("MT emp = %.2f (k=%d); LT emp = %.2f (k=%d); MT earn = %.0f (k=%d); LT earn = %.0f (k=%d)",
          mt_emp_sec["mean"], mt_emp_sec["k"],
          lt_emp_sec["mean"], lt_emp_sec["k"],
          mt_earn_sec["mean"], mt_earn_sec["k"],
          lt_earn_sec["mean"], lt_earn_sec["k"]))

###############################################################################
# CHUNK 18.  Section 9 — Year Up $8,000/year, no diminishment after 7 years
#
#   "Year Up boosted incomes by an extraordinary $8,000/year, with the
#   impact showing no diminishment after seven years (Fein and Dastrup
#   2022, Exhibit 2-1)."
###############################################################################

cat("\n=== Chunk 18: Section 9 — Year Up $8,000/year impact ===\n")

report(
  "Year Up: $8,000/year, no diminishment after 7 years",
  "PACE Year Up LT earn ~$8-9K; lt_followup_years ~7",
  sprintf("Year Up (PACE, 8 offices): LT earn = $%.0f; lt_followup_years = %s",
          yu_lt, format(dat$lt_followup_years[yu_idx])))

###############################################################################
# CHUNK 19.  Conclusion (revised) — long-term impacts 1.7-1.9 pts, $700-800/yr
#
#   "In a new and comprehensive meta-analysis of randomized studies in the
#   U.S., the long-term impacts of job training average 1.7–1.9 points of
#   employment and $700–800/year in income."
###############################################################################

cat("\n=== Chunk 19: Conclusion — LT averages ===\n")

report(
  "LT impacts: 1.7-1.9 pts employment and $700-800/year",
  "Table 3 LT row: emp 1.9 (full) to 1.7 (primary); earn $673 (full) to $820 (primary)",
  sprintf("LT emp: full = %.2f, training-primary = %.2f; LT earn: full = %.0f, training-primary = %.0f",
          lt_emp_full["mean"], lt_emp_prim["mean"],
          lt_earn_full["mean"], lt_earn_prim["mean"]))

cat("\n##############################################################################\n")
cat("# Verification script complete.\n")
cat("##############################################################################\n")
