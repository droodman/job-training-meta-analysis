###############################################################################
# 04_verify_claims.R — Confirm every numeric claim in the write-up that draws
# on this meta-analysis data set.
#
# Source document (as of this re-sync):
#   doc/Subsidized Job Training evidence review 2.txt
#
# Each chunk:
#   1. Prints the verbatim claim from the write-up
#   2. Computes the corresponding number from processed_data.rds
#   3. Prints the computed value
#
# Conventions:
#   - Where a claim gives a pair of numbers "for all programs and training-
#     primary ones," the first is the full sample and the second the
#     training-primary subsample (training_role == "primary").
#   - Section numbers below refer to the "2" revision: the old "Executive
#     summary" was folded into an unnumbered lead-in and findings 1.1-1.7; the
#     old meta-analysis section §6.6 is now §6.5, with subsections renumbered
#     to match (§6.5.1 sample, §6.5.2 average impacts, §6.5.3 benefit-cost,
#     §6.5.4 correlates, §6.5.5 summary). Table numbers each shifted down by
#     1 (old Table 2 study-traits -> new Table 1, ... old Table 7 sector
#     impacts -> new Table 6); figure numbers for the sector-program plots
#     shifted down by 3 (old Figure 15/16/17 -> new Figure 12/13/14).
#   - When the write-up says "long-term" / "years 3-5," it means horizon
#     "lt"; "medium term" / "year 2" is "mt"; "short term" / "year 1" is "st".
#   - Impact and control-group statistics use REML meta-analysis with
#     cluster-robust SE at the project level, matching 03_tables.R.
#   - Benefit-cost summary statistics (B-C ratio, MVPF, NPV, IRR) are OUT OF
#     SCOPE here: they come from the appendix-A benefit-cost model, not from
#     processed_data.rds, so this script does not attempt to reproduce them.
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
# Mirrors the pooling in 03_tables.R so verification numbers match Tables 3-4.

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
cat("# Verification of numeric claims in Subsidized Job Training evidence review\n")
cat("##############################################################################\n")

###############################################################################
# CHUNK 1.  Table 2 (§6.5.2) — LT control-group base rates
#
# The old write-up had an executive-summary sentence combining headline
# impact and control-base numbers ("They increase employment by 1.7
# percentage points, over a control group average of 63%, and $800/year more
# pay, against a control group average of $16K..."). That sentence was
# removed in the revision; the headline LT impact numbers now surface only
# via §1.1 and §8 (see Chunks 3 and 19). This chunk is kept to verify the LT
# control-group base rates, which still appear as data in Table 2 even
# though no single sentence quotes them together anymore.
###############################################################################

cat("\n=== Chunk 1: Table 2 (§6.5.2) — LT control-group base rates ===\n")

lt_emp_full <- remi(all_idx,     "lt_emp_impact",      "lt_emp_se")
lt_emp_prim <- remi(primary_idx, "lt_emp_impact",      "lt_emp_se")
lt_emp_cm_full <- remi(all_idx,     "lt_emp_control_mean", "lt_emp_se")
lt_emp_cm_prim <- remi(primary_idx, "lt_emp_control_mean", "lt_emp_se")
lt_earn_full <- remi(all_idx,     "lt_earn_impact",      "lt_earn_se")
lt_earn_prim <- remi(primary_idx, "lt_earn_impact",      "lt_earn_se")
lt_earn_cm_full <- remi(all_idx,     "lt_earn_control_mean", "lt_earn_se")
lt_earn_cm_prim <- remi(primary_idx, "lt_earn_control_mean", "lt_earn_se")

report(
  "[context, no longer quoted directly] Table 2 LT control-group base rates",
  "LT emp control ~59-63%; LT earn control ~$14-16K (full / training-primary)",
  sprintf("LT emp control: full = %.1f%%, training-primary = %.1f%%; LT earn control: full = $%.0f, training-primary = $%.0f",
          lt_emp_cm_full["mean"], lt_emp_cm_prim["mean"],
          lt_earn_cm_full["mean"], lt_earn_cm_prim["mean"]))
show_re("LT employment control, full",            lt_emp_cm_full)
show_re("LT employment control, training-primary", lt_emp_cm_prim)
show_re("LT earnings control, full",              lt_earn_cm_full)
show_re("LT earnings control, training-primary",   lt_earn_cm_prim)

###############################################################################
# CHUNK 2.  Lead-in bullets — sector programs lift pay by $5–10K/year
#
#   "A few programs have lifted pay $5–10K/year by intensively screening
#   applicants, involving employers in deciding what to teach, and tracking
#   local trends in the demand for skills."
#
# The $5K low end is Per Scholas (Bronx) in WorkAdvance; the $10K high end
# is Year Up (8 PACE offices pooled). Both are long-term earnings impacts.
###############################################################################

cat("\n=== Chunk 2: Lead-in bullets — sector programs $5-10K/year ===\n")

# Per Scholas (Bronx), WorkAdvance — the longer-follow-up Per Scholas row.
ps_idx <- which(dat$project == "WorkAdvance" &
                grepl("Per Scholas", dat$site_subgroup))
yu_idx <- which(grepl("Year Up", dat$project) &
                grepl("Full sample|8 offices", dat$site_subgroup))

ps_lt_earn <- dat$lt_earn_impact[ps_idx]
yu_lt_earn <- dat$lt_earn_impact[yu_idx]

report(
  "A few programs have lifted pay $5–10K/year",
  "Per Scholas (WorkAdvance, Bronx) LT earn ≈ $5-6K; Year Up (8-city PACE) LT earn ≈ $9-10K",
  sprintf("Per Scholas (WorkAdvance, Bronx, row %d): $%.0f; Year Up (PACE, 8 offices, row %d): $%.0f",
          ps_idx, ps_lt_earn, yu_idx, yu_lt_earn))

###############################################################################
# CHUNK 3.  Overview — training-primary average impacts
#
#   "The programs in which training was a primary component lifted employment
#   by an average 2.8 percentage points in the second year after randomized
#   assignment to treatment, and 1.7 points in years 3–5. The parallel impacts
#   on pre-tax earnings are $1,139 and $791 per year (in 2025 dollars). All
#   these averages are highly significant, statistically."  (See Table 2 in
#   §6.5.2; this is now §1.1's lead finding.)
###############################################################################

cat("\n=== Chunk 3: Overview — training-primary averages ===\n")

mt_emp_prim  <- remi(primary_idx, "mt_emp_impact",  "mt_emp_se")
lt_emp_prim2 <- remi(primary_idx, "lt_emp_impact",  "lt_emp_se")
mt_earn_prim <- remi(primary_idx, "mt_earn_impact", "mt_earn_se")
lt_earn_prim2<- remi(primary_idx, "lt_earn_impact", "lt_earn_se")

report(
  "Training-primary: emp +2.8 pts MT, +1.7 pts LT; earn +$1,139 MT, +$791 LT (2025$)",
  "Table 3, training-primary column: MT emp 2.8, LT emp 1.7, MT earn 1139, LT earn 791",
  sprintf("MT emp = %.2f (k=%d); LT emp = %.2f (k=%d); MT earn = %.0f (k=%d); LT earn = %.0f (k=%d)",
          mt_emp_prim["mean"], mt_emp_prim["k"],
          lt_emp_prim2["mean"], lt_emp_prim2["k"],
          mt_earn_prim["mean"], mt_earn_prim["k"],
          lt_earn_prim2["mean"], lt_earn_prim2["k"]))
# Also check statistical significance (z = mean / SE), expecting |z| > 2.
cat("  z-stats (training-primary):\n")
for (lab in c("MT emp", "LT emp", "MT earn", "LT earn")) {
  r <- switch(lab, "MT emp" = mt_emp_prim, "LT emp" = lt_emp_prim2,
              "MT earn" = mt_earn_prim,    "LT earn" = lt_earn_prim2)
  cat(sprintf("    %s: z = %.2f\n", lab, r["mean"] / r["se"]))
}

###############################################################################
# CHUNK 4.  §1.2 — cost per treatment group member
#
#   "Among the 67% of training-primary interventions whose write-ups include
#   cost information, costs average $13,598 per treatment group member."
#
# "Interventions" here = studies (projects).
#
# NOTE: §1.2's cost figure was updated to $13,598 in this revision (from
# $13,046), consistent with the recent cost-concept refactor in 01/02_prep
# (see commits "Refine cost concepts" and "Fix new factor of 1000 bug in cost
# figures"). But Table 1's note in §6.5.1 and the sector-program comparison
# in §7 ("$11,677 versus $13,046") were NOT updated and still show the old
# $13,046 figure — see Chunk 10 below, which still targets $13,046 because
# that's what Table 1's text currently says. This is a real inconsistency in
# the write-up, worth flagging to the author.
###############################################################################

cat("\n=== Chunk 4: §1.2 — $13,598 per treatment group member ===\n")

cost_unwt_prim <- mean(dat$net_cost_narrow[primary_idx], na.rm = TRUE)
prim_projects      <- unique(dat$project[primary_idx])
prim_cost_projects <- unique(dat$project[primary_idx][
                        !is.na(dat$net_cost_narrow[primary_idx])])
cov_pct <- 100 * length(prim_cost_projects) / length(prim_projects)

report(
  "67% of training-primary interventions report cost; cost averages $13,598",
  "cost coverage ≈ 67% of training-primary studies; unweighted mean cost = $13,598",
  sprintf("cost coverage = %d/%d projects = %.1f%%; mean cost = $%.0f (n = %d rows)",
          length(prim_cost_projects), length(prim_projects), cov_pct,
          cost_unwt_prim, sum(!is.na(dat$net_cost_narrow[primary_idx]))))

###############################################################################
# CHUNK 5.  §1.3 — JTPA impacts for low-income adults
#
#   "the Job Training Partnership Act (JTPA) lifted employment among low-income
#   adults by about 2.3 points, from a base of about 70%. It lifted annual pay
#   by $1,100 in 2025 dollars, from about $13,000 for women and $17,500 for
#   men."
#
# Footnote 5: "Figures approximate average results for years 3-5."
###############################################################################

cat("\n=== Chunk 5: §1.3 — JTPA Adult women + men ===\n")

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
  "JTPA lifted employment among low-income adults by about 2.3 pts, from a base of about 70%",
  "JTPA Adult women + men: LT emp impact ~2.3pts; control mean ~70%",
  sprintf("Avg of Adult women + Adult men: LT emp impact = %.2f pts, LT emp control mean = %.1f%%",
          jtpa_emp_imp, jtpa_emp_cm))

report(
  "It lifted annual pay by $1,100 ... from about $13,000 for women and $17,500 for men",
  "JTPA Adult LT earn impact ~$1,100; control means ~$13K (women), ~$17.5K (men) in 2025$",
  sprintf("Women: LT earn impact = %.0f, control = %.0f. Men: LT earn impact = %.0f, control = %.0f",
          jtpa_w_earn, jtpa_w_cm, jtpa_m_earn, jtpa_m_cm))

###############################################################################
# CHUNK 6.  §1.3 — Job Corps long-term results
#
#   "It did not affect employment or earnings in follow-ups extending 20 years,
#   except for a transitory 1–2% employment bump in years 3–5 (Schochet 2020)."
#
# The spreadsheet's Job Corps row is the Y3–Y5 average per the LT convention.
# Verify: LT earn impact insignificant; LT emp impact a positive 1-2 points.
###############################################################################

cat("\n=== Chunk 6: Job Corps long-term results ===\n")

jc_idx <- which(dat$project == "National Job Corps Study")
cat("Job Corps rows:\n")
print(dat[jc_idx, c("site_subgroup", "lt_followup_years",
                    "lt_emp_impact", "lt_emp_se",
                    "lt_earn_impact", "lt_earn_se")])

jc <- dat[jc_idx, ]
jc_z <- with(jc, c(emp = lt_emp_impact / lt_emp_se,
                   earn = lt_earn_impact / lt_earn_se))
report(
  "Job Corps: no benefit on earnings over 20 yrs; a transitory 1-2% employment bump in Y3-Y5",
  "LT earn |z|<2 (null); LT emp impact ~1-2 pts around Y3-Y5",
  sprintf("|z| (LT emp) = %.2f; |z| (LT earn) = %.2f; LT emp impact = %.2f pts; lt_followup_years = %.1f",
          abs(jc_z["emp"]), abs(jc_z["earn"]),
          jc$lt_emp_impact, jc$lt_followup_years))

###############################################################################
# CHUNK 7.  §1.3 — WIA null results
#
#   "An evaluation of the Workforce Investment Act, the successor to the JTPA,
#   returned essentially zeros for the programs for low-income adults and for
#   dislocated workers (Fortson et al. 2017). Estimates were as often negative
#   as positive and lack statistical significance."
###############################################################################

cat("\n=== Chunk 7: WIA Gold Standard null results ===\n")

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
  "WIA essentially zeros — as often negative as positive, none significant",
  "all |z| < 2 (no estimate significant at conventional 5% threshold)",
  sprintf("Max |z| across all WIA impact estimates = %.2f", zmax))

###############################################################################
# CHUNK 8.  §6.5.2 — take-up rates and the ITT/LATE gap
#
#   "In training-primary interventions, the experimental offer of entry is
#   accepted 75% of the time, while only 9% of the control group manages to
#   access the treatment. Dividing that difference, 66% ... the impact on
#   participation in any training program is only 28%, because many control
#   group members find alternative trainings."  (§6.5.2, Table 2.)
#
# NB: the write-up now calls this ratio "LATE-program" / "LATE-training"
# (previously "TOT-program" / "TOT-training"); the underlying take-up
# differentials this chunk checks are unaffected by the renaming.
#
# program_takeup_impact is the treatment-control differential in take-up of the
# tested program; program_takeup_control_mean is the control group's take-up.
# Treatment take-up ≈ differential + control mean.
###############################################################################

cat("\n=== Chunk 8: Take-up rates (training-primary) ===\n")

prog_diff <- remi(primary_idx, "program_takeup_impact",      "program_takeup_se")
prog_ctrl <- remi(primary_idx, "program_takeup_control_mean", "program_takeup_se")
any_diff  <- remi(primary_idx, "any_training_impact",        "any_training_se")
any_ctrl  <- remi(primary_idx, "any_training_control_mean",  "any_training_se")

report(
  "Program offer accepted 75%; control take-up 9%; differential 66%; any-training differential 28%",
  "program take-up: treatment ~75%, control ~9%, differential ~66%; any-training differential ~28% (control ~49%)",
  sprintf("program: differential = %.1f + control = %.1f => treatment = %.1f; any-training: differential = %.1f (control = %.1f)",
          prog_diff["mean"], prog_ctrl["mean"],
          prog_diff["mean"] + prog_ctrl["mean"],
          any_diff["mean"], any_ctrl["mean"]))

###############################################################################
# CHUNK 9.  §6.5.1 — sample size and counts
#
#   "Our sample consists of 56 studies. Of these, 24 report results for several
#   demographics, implementing organizations, or localities. This brings the
#   number of impact estimates to 146."
#   "Roughly half the experiments cleared this hurdle: 33, with 78 impact
#   estimates."
###############################################################################

cat("\n=== Chunk 9: §6.5.1 — sample counts ===\n")

n_studies_full <- length(unique(dat$project))
n_impacts_full <- nrow(dat)
n_studies_prim <- length(unique(dat$project[primary_idx]))
n_impacts_prim <- length(primary_idx)
n_multi <- sum(table(dat$project) > 1)

report(
  "Sample: 56 studies, 146 impacts; 24 with multiple rows",
  "56 / 146 / 24",
  sprintf("studies = %d, impact estimates = %d, projects with >1 row = %d",
          n_studies_full, n_impacts_full, n_multi))

report(
  "Training-primary subset: 33 studies, 78 impact estimates",
  "33 / 78",
  sprintf("studies = %d, impact estimates = %d",
          n_studies_prim, n_impacts_prim))

###############################################################################
# CHUNK 10.  §6.5.1 — Table 1 study-trait averages (training-primary)
#
#   "Most aim to serve low-income people, with only 3–4% meant for dislocated
#   workers. Where training is primary, classroom training is a component 90%
#   of the time and on-the-job training just 29%. People spend about 6 months
#   in these programs, at a cost of $13,046 per person offered treatment ...
#   Nearly all studied interventions were publicly funded, but slightly less
#   than half the training-primary interventions delegated local implementation
#   to private organizations."
#
# NOTE: this $13,046 cost figure in Table 1's text is stale relative to
# §1.2's updated $13,598 (see Chunk 4's note) — flagged here again since it
# is verified directly against the data below.
###############################################################################

cat("\n=== Chunk 10: §6.5.1 — Table 1 training-primary averages ===\n")

pct <- function(x) 100 * mean(x, na.rm = TRUE)
pct_eq <- function(x, lvl) 100 * mean(x == lvl, na.rm = TRUE)

trait_check <- list(
  "dislocated workers (% of full)"   = pct_eq(dat$target_pop[all_idx],     "dislocated"),
  "dislocated workers (% of prim)"   = pct_eq(dat$target_pop[primary_idx], "dislocated"),
  "classroom training (% of prim)"   = pct(dat$has_classroom[primary_idx]),
  "on-the-job training (% of prim)"  = pct(dat$has_ojt[primary_idx]),
  "treatment duration months (prim)" = mean(dat$treatment_duration_months[primary_idx], na.rm = TRUE),
  "cost per treated (prim, 2025$)"   = mean(dat$net_cost_narrow[primary_idx], na.rm = TRUE),
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
  "Classroom 90%, OJT 29% (training-primary)",
  "Table 1 row 'Classroom training (%)' = 90, 'On-the-job training (%)' = 29",
  sprintf("classroom = %.1f%%, OJT = %.1f%%",
          trait_check[["classroom training (% of prim)"]],
          trait_check[["on-the-job training (% of prim)"]]))

report(
  "About 6 months in program, $13,046 per person offered treatment (training-primary)",
  "treatment duration ~6 mo; cost ~$13,046 (Table 1; stale — see note above)",
  sprintf("duration = %.1f months; cost = $%.0f",
          trait_check[["treatment duration months (prim)"]],
          trait_check[["cost per treated (prim, 2025$)"]]))

report(
  "Nearly all publicly funded; slightly less than half admin delegated to private (training-primary)",
  "funding public (majority); admin private < 50% (Table 1)",
  sprintf("public funding = %.1f%%; private admin = %.1f%%",
          trait_check[["funding public (% of prim)"]],
          trait_check[["admin private (% of prim)"]]))

###############################################################################
# CHUNK 11.  §6.5.2 — Table 2 main impact lines
#
#   "From a base employment rate of about 60%, it boosts the chance of having a
#   job by 2.5–2.9 points in the medium term (year 2) and 1.7–1.8 points in the
#   longer term (the pairs of numbers being for all programs and training-
#   primary ones). The average program lifts pre-tax earnings by 7.8% in the
#   medium term, meaning by $1,000–1,100/year from $12,500 in the full sample
#   and $14,700 in the training-primary sample. The long-term earnings impacts
#   are 4.9%, i.e., $700–800 from bases of $14,400 and $16,100 (2025 dollars)."
#
#   Summary bullet at the end of §6.5.5: "The ITT impacts are small, at about 2
#   points of employment, and $1,000–1,100 in pre-tax earnings in year 2 and
#   $700–800/year beyond."
###############################################################################

cat("\n=== Chunk 11: §6.5.2 — medium- and long-term averages ===\n")

mt_emp_full  <- remi(all_idx, "mt_emp_impact",  "mt_emp_se")
lt_emp_full2 <- remi(all_idx, "lt_emp_impact",  "lt_emp_se")
mt_earn_full <- remi(all_idx, "mt_earn_impact", "mt_earn_se")
lt_earn_full2<- remi(all_idx, "lt_earn_impact", "lt_earn_se")
mt_earn_cm_full <- remi(all_idx,     "mt_earn_control_mean", "mt_earn_se")
mt_earn_cm_prim <- remi(primary_idx, "mt_earn_control_mean", "mt_earn_se")

report(
  "boosts chance of having a job by 2.5-2.9 pts (MT) and 1.7-1.8 pts (LT)",
  "Table 2: MT emp 2.5 (full)/2.9 (prim); LT emp is an ascending range 1.7–1.8 (full 1.84, prim 1.66)",
  sprintf("MT emp: full = %.2f, training-primary = %.2f; LT emp: full = %.2f, training-primary = %.2f",
          mt_emp_full["mean"], mt_emp_prim["mean"],
          lt_emp_full2["mean"], lt_emp_prim2["mean"]))

report(
  "lifts earnings by $1,000-1,100/year in MT and $700-800 beyond",
  "Table 2: MT earn $985 (full) to $1,139 (prim); LT earn $657 (full) to $791 (prim)",
  sprintf("MT earn: full = %.0f, training-primary = %.0f; LT earn: full = %.0f, training-primary = %.0f",
          mt_earn_full["mean"], mt_earn_prim["mean"],
          lt_earn_full2["mean"], lt_earn_prim2["mean"]))

report(
  "MT earnings base $12,500 (full) / $14,700 (prim); LT earnings base $14,400 / $16,100",
  "MT earn control ~$12.5K/$14.7K; LT earn control ~$14.4K/$16.1K",
  sprintf("MT earn control: full = $%.0f, prim = $%.0f; LT earn control: full = $%.0f, prim = $%.0f",
          mt_earn_cm_full["mean"], mt_earn_cm_prim["mean"],
          lt_earn_cm_full["mean"], lt_earn_cm_prim["mean"]))

# Summary bullet's "about 2 points of employment" is roughly the midpoint of
# the MT and LT ranges: (2.5, 2.9, 1.7, 1.8) -> mean ≈ 2.2. Sanity check.
all_emp_pts <- c(mt_emp_full["mean"], mt_emp_prim["mean"],
                 lt_emp_full2["mean"], lt_emp_prim2["mean"])
report(
  "impacts are small, at about 2 points of employment",
  "midpoint of MT/LT × full/training-primary ≈ 2 pts",
  sprintf("Mean across the four MT/LT × full/primary cells = %.2f pts",
          mean(all_emp_pts)))

###############################################################################
# CHUNK 12.  §6.5.4 — meta-regression: employment impact declining over time
#
#   "We see that the medium- and long-term impacts on employment have fallen by
#   about 0.07 points per year since the 1970s."  (Table 3.)
###############################################################################

cat("\n=== Chunk 12: §6.5.4 — declining employment impact over time ===\n")

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
  "MT and LT employment impact has fallen by about 0.07 pts/year since the 1970s",
  "Table 3 randomization_year coefficients ≈ -0.07 (MT and LT, full and training-primary)",
  sprintf("MT-emp year coef: full = %.3f, prim = %.3f; LT-emp: full = %.3f, prim = %.3f (per year)",
          mt_emp_yr_full["b"], mt_emp_yr_prim["b"],
          lt_emp_yr_full["b"], lt_emp_yr_prim["b"]))
cat("  (The full survivor meta-regressions of Table 3 — region, classroom,\n")
cat("   dislocated-worker terms — are produced and reported by 03_tables.R.)\n")

###############################################################################
# CHUNK 13.  §6.5.4 (Table 3 / survivors) — intellectual disabilities ~8 pts
#
#   "The two programs that served people with intellectual disabilities boosted
#   employment about 8 points in the long term."
#
# This "about 8 points" is the survivor meta-regression COEFFICIENT on the
# `target_popdisability` term in the long-term employment regression — NOT a
# raw subgroup mean. It is produced by 03_tables.R and reported in the
# Disability row of table_survivors[_primary].* (currently ~8.2 pts LT emp,
# training-primary; ~7.9 full sample), consistent with "about 8 points."
# The two underlying rows are shown here for context only; their raw mean runs
# higher (~10) because the coefficient is adjusted for the other survivors.
###############################################################################

cat("\n=== Chunk 13: §6.5.4 — intellectual-disability programs ===\n")

dis_idx <- which(dat$target_pop == "disability")
cat("Disability-target rows (raw, for context — not the verification basis):\n")
print(dat[dis_idx, c("project", "site_subgroup", "mt_emp_impact", "lt_emp_impact")])

report(
  "Two disability-target programs boosted employment about 8 pts in the long term",
  "LT-emp survivor coefficient on target_popdisability ≈ 8 pts (see table_survivors_primary.*)",
  paste("survivor coefficient computed by 03_tables.R (Table 3);",
        "current value ~8.2*** (LT emp, training-primary). Raw two-row mean",
        sprintf("(%.1f LT) is NOT the right comparison.",
                mean(dat$lt_emp_impact[dis_idx], na.rm = TRUE))))

###############################################################################
# CHUNK 14.  §6.5.4 — employer hiring commitment lifts earnings $8-9K
#
#   "And the apparently powerful impact of an employer committing to hire
#   graduates—$8,000–9,000 in year 2 and beyond—owes to three programs: the
#   Wildcat program that inspired the NSWD, Year Up, and the Wisconsin Regional
#   Training Partnership."
###############################################################################

cat("\n=== Chunk 14: §6.5.4 — employer-hiring-commitment programs ===\n")

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
# CHUNK 15.  §7 — sector-program RE averages, cost, and take-up
#
#   "sector programs on average cause a partially transient employment bump,
#   about 2.9 points, and a more permanent increase in pre-tax pay,
#   $3,000–4,000/year in 2025 dollars." (Figures 13, 14.)
#   "their costs do not: $11,677 per treatment group member ..." (Table 6.)
#   "The average sector experiment generated a 56-point differential in
#   participation in the program being evaluated ... the impact on take-up of
#   any training is about 25%."
#
# The sector-program subset is `sector_program == 1`.
###############################################################################

cat("\n=== Chunk 15: §7 — sector-program RE averages ===\n")

sec_idx <- which(dat$sector_program == 1)
cat(sprintf("Sector-program rows: %d\n", length(sec_idx)))

mt_emp_sec  <- remi(sec_idx, "mt_emp_impact",  "mt_emp_se")
lt_emp_sec  <- remi(sec_idx, "lt_emp_impact",  "lt_emp_se")
mt_earn_sec <- remi(sec_idx, "mt_earn_impact", "mt_earn_se")
lt_earn_sec <- remi(sec_idx, "lt_earn_impact", "lt_earn_se")
sec_cost    <- mean(dat$net_cost_narrow[sec_idx], na.rm = TRUE)
sec_prog_diff <- remi(sec_idx, "program_takeup_impact", "program_takeup_se")
sec_any_diff  <- remi(sec_idx, "any_training_impact",   "any_training_se")

report(
  "Sector programs: ~2.9 pts emp bump (MT), $3,000-4,000/year pay",
  "MT emp ~2.9 pts; MT earn ~$3,100; LT earn ~$3,700 (Figures 13, 14)",
  sprintf("MT emp = %.2f (k=%d); LT emp = %.2f (k=%d); MT earn = %.0f (k=%d); LT earn = %.0f (k=%d)",
          mt_emp_sec["mean"], mt_emp_sec["k"],
          lt_emp_sec["mean"], lt_emp_sec["k"],
          mt_earn_sec["mean"], mt_earn_sec["k"],
          lt_earn_sec["mean"], lt_earn_sec["k"]))

report(
  "Sector cost $11,677 per treatment group member; take-up differential 56 pts; any-training ~25%",
  "sector cost ~$11,677; program take-up differential ~56; any-training differential ~25",
  sprintf("cost = $%.0f (n=%d); program take-up differential = %.1f; any-training differential = %.1f",
          sec_cost, sum(!is.na(dat$net_cost_narrow[sec_idx])),
          sec_prog_diff["mean"], sec_any_diff["mean"]))

###############################################################################
# CHUNK 16.  §7 — Year Up gap of about $2,000/quarter
#
#   "Thereafter the gap between the two holds steady at around $2,000 per
#   quarter per treatment group member." (Figure 12.)
###############################################################################

cat("\n=== Chunk 16: §7 — Year Up ~$2,000/quarter gap ===\n")

yu_lt <- dat$lt_earn_impact[yu_idx]
report(
  "Year Up: gap holds steady at around $2,000/quarter",
  "Annual ≈ 4 × $2,000 = ~$8K. Year Up LT earn ~$8-10K/yr",
  sprintf("Year Up (PACE, 8 offices): LT earn = $%.0f/yr ≈ $%.0f/quarter",
          yu_lt, yu_lt / 4))

###############################################################################
# CHUNK 17.  §7 — Year Up $8,000/year, no diminishment after 7 years
#
#   "Year Up boosted incomes by an extraordinary $8,000/year, with the impact
#   showing no diminishment after seven years (Fein and Dastrup 2022, Exhibit
#   2–1), although this also had standout costs of around $30,000 per
#   participant."
#
# NB: the write-up's "seven years" is the study's own follow-up horizon; the
# spreadsheet LT window is years 3-5 (lt_followup_years), so the LT earnings
# figure here is the Y3-Y5 value, not the 7-year one.
###############################################################################

cat("\n=== Chunk 17: §7 — Year Up $8,000/year impact, ~$30K cost ===\n")

report(
  "Year Up: $8,000/year, no diminishment after 7 years, ~$30,000 per participant",
  "PACE Year Up LT earn ~$8-10K; cost ~$30K",
  sprintf("Year Up (PACE, 8 offices): LT earn = $%.0f; lt_followup_years = %s; cost = $%.0f",
          yu_lt, format(dat$lt_followup_years[yu_idx]),
          dat$net_cost_narrow[yu_idx]))

###############################################################################
# CHUNK 18.  §7 — sector earnings impacts 3–4× the average training program
#
#   "Even when averaging in programs that did not manage to work closely with
#   employers, the earnings impacts are 3–5 times higher than for the average
#   job training program."
###############################################################################

cat("\n=== Chunk 18: §7 — sector earnings 3–4× the average program ===\n")

report(
  "Sector earnings impacts are 3–5 times higher than for the average job training program",
  "sector MT/LT earn ÷ full-sample MT/LT earn ≈ 3–5×",
  sprintf("MT: %.0f / %.0f = %.1f×; LT: %.0f / %.0f = %.1f×",
          mt_earn_sec["mean"], mt_earn_full["mean"],
          mt_earn_sec["mean"] / mt_earn_full["mean"],
          lt_earn_sec["mean"], lt_earn_full2["mean"],
          lt_earn_sec["mean"] / lt_earn_full2["mean"]))

###############################################################################
# CHUNK 19.  §8 (major findings) — long-term impacts 1.7-1.8 pts, $700-800/yr
#
#   "In a new meta-analysis of randomized studies in the US, the long-term ITT
#   impacts of job training average 1.7–1.8 points of employment and
#   $700–800/year in income."
###############################################################################

cat("\n=== Chunk 19: §8 — LT averages ===\n")

report(
  "LT impacts: 1.7-1.8 pts employment and $700-800/year",
  "Table 2 LT row: emp ascending range 1.7–1.8 (full 1.84, prim 1.66); earn $657 (full)/$791 (prim)",
  sprintf("LT emp: full = %.2f, training-primary = %.2f; LT earn: full = %.0f, training-primary = %.0f",
          lt_emp_full["mean"], lt_emp_prim["mean"],
          lt_earn_full["mean"], lt_earn_prim["mean"]))

cat("\n##############################################################################\n")
cat("# Verification script complete.\n")
cat("##############################################################################\n")

# hand-coded: average deviation of net_cost_broad from net_cost_narrow, for rows where both are non-missing and different
dat <- read.xlsx("data/extraction_full_v69.xlsx", sheet = "Data Table")
comparators = !is.na(dat$net_cost_narrow) & !is.na(dat$net_cost_broad) & dat$net_cost_narrow!= dat$net_cost_broad
mean(dat$net_cost_broad[comparators] / dat$net_cost_narrow[comparators]) - 1
# unique(dat$short_name[comparators & dat$net_cost_narrow < dat$net_cost_broad])