###############################################################################
# 05_tables_rtf.R — CKW Table 7-style meta-regression tables in RTF
#
# 6 tables: 3 horizons × 2 approaches (complete-case, MI)
# Each table: Panel A (Employment) + Panel B (Earnings)
# Columns: one regression per cluster + final combined specification
# RE (REML) only, following CKW convention.
###############################################################################

library(metafor)
library(mice)
library(flextable)
library(officer)

set.seed(20260402)

dat <- readRDS("data/processed_data.rds")
dir.create("output", showWarnings = FALSE)

# Combine "welfare" and "low_income_adult" into a single target_pop level so
# the regression's variable set matches the descriptive table's groupings
# (the descriptive table shows them as one row "Welfare or low-income adults").
dat$target_pop <- factor(
  ifelse(dat$target_pop %in% c("welfare", "low_income_adult"),
         "welfare_or_lia", as.character(dat$target_pop)),
  levels = c("welfare_or_lia", "noncustodial_parent", "substance_abuse",
             "justice_involved", "dislocated", "youth", "disability"))

# ── 1. Column specifications ─────────────────────────────────────────────────
#
# Each column is a separate regression. The last column combines all.
# Data source moderator switches depending on outcome (emp vs earn).

make_specs <- function(outcome_type, hz, approach = "cc") {
  ds_var <- if (outcome_type == "emp") "emp_data_source" else "earn_data_source"
  rr_var <- paste0("resprate_", outcome_type, "_", hz)
  un_var <- paste0("unrate_", hz)
  list(
    "(1)" = list(
      formula = as.formula(sprintf(
        "~ randomization_midpoint + census_region + geo_type + %s", un_var)),
      label = "Setting"
    ),
    "(2)" = list(
      formula = as.formula("~ target_pop"),
      label = "Target pop."
    ),
    "(3)" = list(
      formula = as.formula(
        "~ pct_male + mean_age + pct_black + pct_hispanic + pct_no_hs_diploma"),
      label = "Demographics"
    ),
    "(4)" = list(
      formula = as.formula(paste0(
        "~ has_classroom + has_ojt + has_jsa + mandatory_voluntary",
        " + treatment_duration_months + cost_per_treated_k + scaled")),
      label = "Treatment"
    ),
    "(5)" = list(
      formula = as.formula(paste0(
        "~ employengage_curricula + jobdev_yes",
        " + employerol_curriculum + employerol_hire")),
      label = "Employer engagement"
    ),
    "(6)" = list(
      formula = as.formula(
        "~ funding_public_private + admin_public_private"),
      label = "Administration"
    ),
    "(7)" = list(
      formula = as.formula(sprintf("~ %s + %s + academic", ds_var, rr_var)),
      label = "Study characteristics"
    )
  )
}

# ── 2. Row labels — ordered to match the column specs ────────────────────────
# Each row maps a coefficient name (from rma output) to a display label
# and a section header.

row_defs <- list(
  list(section = "Setting (omitted = South, urban)",
       vars = list(
         "randomization_midpoint"   = "Randomization year",
         "census_regionNortheast"   = "Northeast",
         "census_regionMidwest"     = "Midwest",
         "census_regionWest"        = "West",
         "geo_typerural"            = "Rural",
         "UNRATE_PLACEHOLDER"       = "Unemployment over follow-up (%)"
       )),
  list(section = "Target population (omitted = welfare or low-income adult)",
       vars = list(
         "target_popnoncustodial_parent"= "Noncustodial parents",
         "target_popsubstance_abuse"    = "Substance abuse",
         "target_popjustice_involved"   = "Justice-involved",
         "target_popdislocated"         = "Dislocated workers",
         "target_popyouth"              = "Youth",
         "target_popdisability"         = "Disability"
       )),
  list(section = "Demographics",
       vars = list(
         "pct_male"          = "% male",
         "mean_age"          = "Mean age",
         "pct_black"         = "% Black",
         "pct_hispanic"      = "% Hispanic",
         "pct_no_hs_diploma" = "% no high school diploma"
       )),
  list(section = "Treatment (omitted = mandatory)",
       vars = list(
         "has_classroom"             = "Classroom training",
         "has_ojt"                   = "On-the-job training",
         "has_jsa"                   = "Job search assistance",
         "mandatory_voluntaryvoluntary"  = "Voluntary",
         "treatment_duration_months" = "Treatment duration (months)",
         "cost_per_treated_k"        = "Cost per treated ($1,000s)",
         "scaled"                    = "Scaled"
       )),
  list(section = "Employer engagement",
       vars = list(
         "employengage_curricula" = "Curricula adapted for employers",
         "jobdev_yes"             = "Job development/placement",
         "employerol_curriculum"  = "Employer input on curriculum",
         "employerol_hire"        = "Employer commits to hiring"
       )),
  list(section = "Administration (omitted = public funding, public admin)",
       vars = list(
         "funding_public_privatemixed"   = "Funding: public & private",
         "funding_public_privateprivate" = "Funding: private",
         "admin_public_privatemixed"     = "Admin: public & private",
         "admin_public_privateprivate"   = "Admin: private"
       )),
  list(section = "Study characteristics (omitted = outcome data source: official records)",
       vars = list(
         "DATA_SOURCE_PLACEHOLDER"  = "Outcome data source: survey",
         "RESPRATE_PLACEHOLDER"     = "Response rate (%)",
         "academic"                 = "Academic publication"
       ))
)

# ── 3. MI setup ──────────────────────────────────────────────────────────────

imp_vars <- c(
  "mean_age", "pct_male", "pct_white", "pct_black", "pct_hispanic",
  "pct_no_hs_diploma", "cost_per_treated_k",
  "has_classroom", "has_ojt", "has_jsa",
  "training_role", "mandatory_voluntary",
  "funding_public_private", "admin_public_private",
  "target_pop", "randomization_midpoint", "geo_type",
  "emp_data_source", "earn_data_source",
  "resprate_emp_st", "resprate_emp_mt", "resprate_emp_lt",
  "resprate_earn_st", "resprate_earn_mt", "resprate_earn_lt",
  "unrate_st", "unrate_mt", "unrate_lt",
  "sector_program", "academic",
  "employengage_curricula", "jobdev_yes",
  "employerol_curriculum", "employerol_hire", "scaled",
  "treatment_duration_months",
  "st_emp_impact", "mt_emp_impact", "lt_emp_impact",
  "st_earn_impact", "mt_earn_impact", "lt_earn_impact",
  "n_treatment", "n_control"
)

m_imp <- 10
cat("Running mice imputation...\n")
imp_data <- dat[, imp_vars]
ini <- mice(imp_data, maxit = 0, printFlag = FALSE)
meth <- ini$method
for (v in names(meth)) {
  if (meth[v] == "") next
  if (is.factor(imp_data[[v]])) {
    meth[v] <- if (nlevels(imp_data[[v]]) == 2) "logreg" else "polyreg"
  } else meth[v] <- "pmm"
}
imp <- mice(imp_data, m = m_imp, method = meth, maxit = 20, printFlag = FALSE)
cat("Imputation complete.\n")

# ── 4. Rubin's rules pooler ─────────────────────────────────────────────────

pool_rma <- function(fits) {
  # Filter to fits that succeeded and have consistent coefficient names
  fits <- Filter(Negate(is.null), fits)
  if (length(fits) == 0) return(NULL)
  m <- length(fits)
  p <- length(coef(fits[[1]]))
  ref_names <- names(coef(fits[[1]]))
  # Ensure all fits have same coefficients
  fits <- Filter(function(f) identical(names(coef(f)), ref_names), fits)
  if (length(fits) < 2) return(NULL)
  m <- length(fits)
  betas <- sapply(fits, coef)
  if (is.vector(betas)) betas <- matrix(betas, nrow = 1)
  vcovs <- lapply(fits, vcov)
  Q_bar <- rowMeans(betas)
  U_bar <- diag(Reduce("+", vcovs) / m)
  B <- if (p == 1) var(betas[1, ]) else apply(betas, 1, var)
  T_var <- U_bar + (1 + 1/m) * B
  se <- sqrt(T_var)
  r <- (1 + 1/m) * B / U_bar
  df_old <- (m - 1) * (1 + 1/r)^2
  df_old[is.infinite(df_old) | is.nan(df_old)] <- Inf
  t_stat <- Q_bar / se
  p_val <- 2 * pt(-abs(t_stat), df_old)
  tau2_avg <- mean(sapply(fits, function(f) f$tau2))
  I2_avg <- mean(sapply(fits, function(f) f$I2))
  R2_vals <- sapply(fits, function(f) if (!is.null(f$R2)) f$R2 else NA)
  R2_avg <- mean(R2_vals, na.rm = TRUE)
  list(coef_names = names(coef(fits[[1]])),
       beta = setNames(Q_bar, names(coef(fits[[1]]))),
       se = setNames(se, names(coef(fits[[1]]))),
       p = setNames(p_val, names(coef(fits[[1]]))),
       tau2 = tau2_avg, I2 = I2_avg, R2 = R2_avg, k = fits[[1]]$k)
}

# ── 5. Fit one specification ─────────────────────────────────────────────────

# Returns the row indices into `dat` for studies with non-missing impact and
# SE for the given (horizon, outcome), restricted to the active sample.
panel_idx <- function(hz, outcome_type, sample_idx) {
  imp_col <- paste0(hz, "_", outcome_type, "_impact")
  se_col  <- paste0(hz, "_", outcome_type, "_se")
  idx <- which(!is.na(dat[[imp_col]]) & !is.na(dat[[se_col]]))
  intersect(idx, sample_idx)
}

# Cluster-robust standard errors at the project level — wraps a fitted rma
# object so $se / $pval / $vb reflect CR1 SEs.
robustify <- function(fit, cluster) {
  if (is.null(fit)) return(NULL)
  tryCatch(metafor::robust(fit, cluster = cluster),
           error = function(e) fit)
}

fit_spec <- function(hz, outcome_type, frm, approach, sample_idx) {
  imp_col <- paste0(hz, "_", outcome_type, "_impact")
  se_col  <- paste0(hz, "_", outcome_type, "_se")
  idx <- panel_idx(hz, outcome_type, sample_idx)
  if (length(idx) < 3) return(NULL)
  yi  <- dat[[imp_col]][idx]
  sei <- dat[[se_col]][idx]
  cluster <- dat$project[idx]

  if (approach == "cc") {
    fit <- tryCatch(rma(yi = yi, sei = sei, mods = frm, data = dat[idx, ],
                        method = "REML"),
                    error = function(e) NULL)
    robustify(fit, cluster)
  } else {
    fits <- list()
    for (i in 1:m_imp) {
      d_i <- complete(imp, i)[idx, , drop = FALSE]
      d_i$census_region <- dat$census_region[idx]
      d_i$census_division <- dat$census_division[idx]
      f <- tryCatch(rma(yi = yi, sei = sei, mods = frm, data = d_i,
                        method = "REML"),
                    error = function(e) NULL)
      fits[[i]] <- robustify(f, cluster)
    }
    pool_rma(fits)
  }
}

# ── 5b. Survivor selection ──────────────────────────────────────────────────
#
# Returns the model.matrix column names whose coefficient cleared
# |standardized beta| > beta_thresh AND two-sided p < p_thresh in the given
# group fit. Selection is coefficient-level: only specific factor levels that
# clear the hurdle pass through, not the whole factor term. Standardization
# is beta * sd(x) / sd(y) using the studies that contribute to the fit.

get_surviving_coefs <- function(fit, formula, hz, oc_type, approach, sample_idx,
                                beta_thresh = 0.1, p_thresh = 0.1) {
  if (is.null(fit)) return(character(0))
  if (approach == "cc") {
    nms   <- rownames(fit$beta)
    betas <- as.numeric(fit$beta)
    pvals <- as.numeric(fit$pval)
  } else {
    nms   <- names(fit$beta)
    betas <- as.numeric(fit$beta)
    pvals <- as.numeric(fit$p)
  }

  imp_col <- paste0(hz, "_", oc_type, "_impact")
  idx <- panel_idx(hz, oc_type, sample_idx)
  sd_y <- sd(dat[[imp_col]][idx], na.rm = TRUE)

  sd_x <- if (approach == "cc") {
    mm <- model.matrix(formula, data = dat[idx, , drop = FALSE],
                       na.action = na.pass)
    setNames(apply(mm, 2, sd, na.rm = TRUE), colnames(mm))
  } else {
    sds <- sapply(seq_len(m_imp), function(i) {
      d_i <- complete(imp, i)[idx, , drop = FALSE]
      d_i$census_region   <- dat$census_region[idx]
      d_i$census_division <- dat$census_division[idx]
      mm <- model.matrix(formula, data = d_i)
      apply(mm, 2, sd)
    })
    setNames(rowMeans(sds), rownames(sds))
  }

  # metafor names the intercept "intrcpt"; model.matrix uses "(Intercept)".
  std_betas <- betas * sd_x[match(nms, names(sd_x))] / sd_y

  passes <- !is.na(std_betas) & !is.na(pvals) &
            nms != "intrcpt" &
            abs(std_betas) > beta_thresh & pvals < p_thresh
  nms[passes]
}

# Augment data with one numeric column per surviving coefficient: continuous
# columns pass through; factor levels become 0/1 indicators (NA-preserving).
# Coefficient names are model.matrix-style (e.g. "target_popdisability").
augment_with_coefs <- function(d, coefs) {
  d <- as.data.frame(d)
  factor_cols <- names(d)[vapply(d, is.factor, logical(1))]
  for (cf in coefs) {
    if (cf %in% names(d)) next
    matched <- FALSE
    for (fc in factor_cols) {
      lvls <- levels(d[[fc]])
      lv <- lvls[paste0(fc, lvls) == cf]
      if (length(lv) == 1L) {
        d[[cf]] <- as.numeric(d[[fc]] == lv)
        matched <- TRUE
        break
      }
    }
    if (!matched) d[[cf]] <- NA_real_
  }
  d
}

# Fit the survivor regression — column (7) — using one binary-indicator column
# per surviving coefficient. Factor levels that did not clear the hurdle do
# not enter, so every coef in this fit corresponds to one that survived its
# group regression.
fit_survivor_spec <- function(hz, oc_type, approach, coefs, sample_idx) {
  if (length(coefs) == 0) return(NULL)
  imp_col <- paste0(hz, "_", oc_type, "_impact")
  se_col  <- paste0(hz, "_", oc_type, "_se")
  idx <- panel_idx(hz, oc_type, sample_idx)
  if (length(idx) < 3) return(NULL)
  yi  <- dat[[imp_col]][idx]
  sei <- dat[[se_col]][idx]
  cluster <- dat$project[idx]
  # Backtick-quote each coef name so non-syntactic names (e.g. with spaces) work
  surv_formula <- as.formula(paste("~", paste0("`", coefs, "`", collapse = " + ")))

  if (approach == "cc") {
    d_aug <- augment_with_coefs(dat[idx, , drop = FALSE], coefs)
    fit <- tryCatch(rma(yi = yi, sei = sei, mods = surv_formula,
                        data = d_aug, method = "REML"),
                    error = function(e) NULL)
    robustify(fit, cluster)
  } else {
    fits <- list()
    for (i in seq_len(m_imp)) {
      d_i <- complete(imp, i)[idx, , drop = FALSE]
      d_i$census_region   <- dat$census_region[idx]
      d_i$census_division <- dat$census_division[idx]
      d_aug <- augment_with_coefs(d_i, coefs)
      f <- tryCatch(rma(yi = yi, sei = sei, mods = surv_formula,
                        data = d_aug, method = "REML"),
                    error = function(e) NULL)
      fits[[i]] <- robustify(f, cluster)
    }
    pool_rma(fits)
  }
}

# Run the seven group regressions and the survivor regression for one
# (horizon, outcome, approach, sample) panel. Returns the survivor coefficient
# names and the fitted survivor model — used both by build_panel_rows and by
# the cross-panel combined survivors table built later.
compute_survivor_panel <- function(hz, oc_type, approach, sm) {
  specs <- make_specs(oc_type, hz, approach)
  group_ids <- paste0("(", 1:7, ")")
  group_fits <- lapply(group_ids, function(cn)
    fit_spec(hz, oc_type, specs[[cn]]$formula, approach, sm$idx))
  names(group_fits) <- group_ids
  survivors <- unique(unlist(lapply(group_ids, function(cn)
    get_surviving_coefs(group_fits[[cn]], specs[[cn]]$formula, hz, oc_type,
                        approach, sm$idx))))
  fit <- if (length(survivors) > 0)
    fit_survivor_spec(hz, oc_type, approach, survivors, sm$idx) else NULL
  list(fit = fit, survivors = survivors)
}

# ── 5c. Survivor correlation matrices ────────────────────────────────────────
#
# For each panel, write an HTML correlation matrix on the model.matrix
# expansion of the survivor formula. Factor terms expand to dummies, so
# colinearity among levels is visible. CC: complete cases on survivors.
# MI: average of pairwise correlations across imputations.

# When the generated page is loaded inside an iframe, post its rendered
# height to the parent so the parent can size the iframe to fit (works
# cross-origin, unlike DOM access).
iframe_resize_script <- paste0(
  "<script>",
  "(function(){function send(){",
  "if(window.parent===window)return;",
  "var h=Math.max(document.documentElement.scrollHeight,",
  "document.body?document.body.scrollHeight:0);",
  "window.parent.postMessage({type:'iframeHeight',height:h},'*');}",
  "window.addEventListener('load',send);",
  "var ro=window.ResizeObserver?new ResizeObserver(send):null;",
  "if(ro)ro.observe(document.documentElement);",
  "})();",
  "</script>")

write_corr_html <- function(cor_mat, filename, caption) {
  vars <- colnames(cor_mat)
  cell_style <- function(r) {
    if (is.na(r)) return("")
    a <- min(abs(r), 1)
    if (r >= 0) sprintf("background:rgba(70,130,180,%.2f);", a * 0.6)
    else        sprintf("background:rgba(178,34,34,%.2f);", a * 0.6)
  }
  fmt_cell <- function(r) {
    if (is.na(r)) return("")
    if (abs(r - 1) < 1e-9) return("1.00")
    gsub("-", "−", sprintf("%+.2f", r))
  }
  rows <- vapply(seq_along(vars), function(i) {
    cells <- vapply(seq_along(vars), function(j) {
      r <- cor_mat[i, j]
      sprintf("<td style='%s'>%s</td>", cell_style(r), fmt_cell(r))
    }, character(1))
    sprintf("<tr><th class='rowlabel'>%s</th>%s</tr>",
            vars[i], paste(cells, collapse = ""))
  }, character(1))
  header <- paste0("<th></th>",
                   paste0("<th class='collabel'><div>", vars, "</div></th>",
                          collapse = ""))
  html <- paste0(
    "<!DOCTYPE html>\n<html>\n<head><meta charset='UTF-8'>\n",
    "<title>", caption, "</title>\n",
    "<style>",
    "@import url('https://fonts.googleapis.com/css2?family=Source+Serif+4&display=swap');",
    "body{font-family:'Source Serif 4',serif;margin:20px;}",
    "h2{font-size:1.05em;font-weight:600;}",
    "table{border-collapse:collapse;font-size:9pt;}",
    "th,td{padding:3px 6px;text-align:center;border:1px solid #ddd;}",
    "td{min-width:52px;}",
    ".collabel{background:#f5f5f5;font-weight:600;height:140px;vertical-align:bottom;}",
    ".collabel div{writing-mode:vertical-rl;transform:rotate(180deg);white-space:nowrap;}",
    ".rowlabel{text-align:right;background:#f5f5f5;font-weight:600;white-space:nowrap;}",
    "</style></head>\n<body>\n",
    "<h2>", caption, "</h2>\n",
    "<table><tr>", header, "</tr>\n",
    paste(rows, collapse = "\n"),
    "\n</table>\n", iframe_resize_script, "\n</body>\n</html>\n")
  writeBin(charToRaw(enc2utf8(html)), filename)
}

write_corr_matrix <- function(coefs, hz, oc_type, approach, sample_suffix,
                              sample_idx) {
  if (length(coefs) == 0) return(invisible(NULL))
  idx <- panel_idx(hz, oc_type, sample_idx)
  if (length(idx) < 3) return(invisible(NULL))

  if (approach == "cc") {
    d_aug <- augment_with_coefs(dat[idx, , drop = FALSE], coefs)
    X <- as.matrix(d_aug[, coefs, drop = FALSE])
    cc <- complete.cases(X)
    if (sum(cc) < 3) return(invisible(NULL))
    cor_mat <- cor(X[cc, , drop = FALSE])
    k_used <- sum(cc)
  } else {
    cors <- lapply(seq_len(m_imp), function(i) {
      d_i <- complete(imp, i)[idx, , drop = FALSE]
      d_i$census_region   <- dat$census_region[idx]
      d_i$census_division <- dat$census_division[idx]
      d_aug <- augment_with_coefs(d_i, coefs)
      cor(as.matrix(d_aug[, coefs, drop = FALSE]))
    })
    cor_mat <- Reduce("+", cors) / length(cors)
    k_used <- length(idx)
  }
  if (any(is.nan(cor_mat))) cor_mat[is.nan(cor_mat)] <- NA

  oc_label <- c(emp = "Employment", earn = "Earnings")[oc_type]
  hz_label <- c(st = "Short-term", mt = "Medium-term", lt = "Long-term")[hz]
  ap_label <- if (approach == "cc") "complete-case" else "MI averaged"
  caption <- sprintf("Survivor correlations: %s, %s (%s, k = %d)",
                     oc_label, hz_label, ap_label, k_used)
  filename <- sprintf("output/corr_%s_%s_%s%s.html",
                      oc_type, hz, approach, sample_suffix)
  write_corr_html(cor_mat, filename, caption)
  cat(sprintf("  Corr: %s\n", filename))
}

# ── 6. Formatting helpers ────────────────────────────────────────────────────

fmt_stars <- function(p) {
  ifelse(is.na(p), "", ifelse(p < 0.01, "***", ifelse(p < 0.05, "**",
    ifelse(p < 0.10, "*", ""))))
}

get_coef_cell <- function(fit, varname, dig, approach) {
  if (is.null(fit)) return(list(coef = "", se = "\u00a0"))
  if (approach == "cc") {
    nms <- rownames(fit$beta)
    j <- match(varname, nms)
    if (is.na(j)) return(list(coef = "", se = "\u00a0"))
    b <- fit$beta[j]; s <- fit$se[j]; p <- fit$pval[j]
  } else {
    j <- match(varname, names(fit$beta))
    if (is.na(j)) return(list(coef = "", se = "\u00a0"))
    b <- fit$beta[j]; s <- fit$se[j]; p <- fit$p[j]
  }
  # Use Unicode minus sign (U+2212) instead of hyphen for negative values
  coef_str <- gsub("-", "\u2212", sprintf("%s%s",
    formatC(b, format = "f", digits = dig), fmt_stars(p)))
  se_str <- sprintf("(%s)", formatC(s, format = "f", digits = dig))
  list(coef = coef_str, se = se_str)
}

get_k <- function(fit, approach) {
  if (is.null(fit)) return(NA)
  if (approach == "cc") fit$k else fit$k
}

get_R2 <- function(fit, approach) {
  if (is.null(fit)) return("")
  r2 <- if (approach == "cc") fit$R2 else fit$R2
  if (is.null(r2) || is.na(r2)) return("")
  sprintf("%.2f", r2 / 100)  # metafor returns R2 as percentage
}

# ── 7. Build tables ──────────────────────────────────────────────────────────

# Build rows for one panel (one outcome × one horizon × one approach)
build_panel_rows <- function(hz, oc_type, oc_label, dig, approach, col_ids, sm) {
  # Resolve placeholders for this outcome × horizon
  ds_coef <- if (oc_type == "emp") "emp_data_sourcesurvey" else "earn_data_sourcesurvey"
  rr_coef <- paste0("resprate_", oc_type, "_", hz)
  un_coef <- paste0("unrate_", hz)
  panel_row_defs <- lapply(row_defs, function(rd) {
    idx <- which(names(rd$vars) == "DATA_SOURCE_PLACEHOLDER")
    if (length(idx) > 0) names(rd$vars)[idx] <- ds_coef
    idx <- which(names(rd$vars) == "RESPRATE_PLACEHOLDER")
    if (length(idx) > 0) names(rd$vars)[idx] <- rr_coef
    idx <- which(names(rd$vars) == "UNRATE_PLACEHOLDER")
    if (length(idx) > 0) names(rd$vars)[idx] <- un_coef
    rd
  })
  specs <- make_specs(oc_type, hz, approach)
  group_ids <- paste0("(", 1:7, ")")
  fits <- list()
  for (cn in group_ids) {
    fits[[cn]] <- fit_spec(hz, oc_type, specs[[cn]]$formula, approach, sm$idx)
  }
  # Survivor regression: each surviving coefficient (specific factor level or
  # continuous regressor) cleared |std beta|>0.1 & p<0.1 in its group fit.
  survivors <- unique(unlist(lapply(group_ids, function(cn) {
    get_surviving_coefs(fits[[cn]], specs[[cn]]$formula, hz, oc_type, approach,
                       sm$idx)
  })))
  cat(sprintf("    Survivors (%s/%s/%s): %s\n", oc_type, hz, sm$name,
              if (length(survivors)) paste(survivors, collapse = ", ") else "(none)"))
  if (length(survivors) > 0) {
    fits[["(8)"]] <- fit_survivor_spec(hz, oc_type, approach, survivors, sm$idx)
    write_corr_matrix(survivors, hz, oc_type, approach, sm$suffix, sm$idx)
  } else {
    fits[["(8)"]] <- NULL
  }
  # Stash the survivor results so the cross-panel combined table built later
  # can reuse them without re-running the seven group regressions.
  survivor_panel_cache[[paste(sm$suffix, approach, hz, oc_type, sep = "/")]] <<-
    list(fit = fits[["(8)"]], survivors = survivors)

  all_rows <- list()

  # Moderator rows by section
  for (rd in panel_row_defs) {
    sec_row <- c(rd$section, rep("", length(col_ids)))
    names(sec_row) <- c("variable", col_ids)
    all_rows[[length(all_rows) + 1]] <- list(values = sec_row, type = "section")

    for (varname in names(rd$vars)) {
      display_label <- rd$vars[[varname]]
      for (rtype in c("coef", "se")) {
        vals <- c(if (rtype == "coef") sprintf("  %s", display_label) else "")
        for (cn in col_ids) {
          cell <- get_coef_cell(fits[[cn]], varname, dig, approach)
          vals <- c(vals, cell[[rtype]])
        }
        names(vals) <- c("variable", col_ids)
        all_rows[[length(all_rows) + 1]] <- list(values = vals, type = rtype)
      }
    }
  }

  # Blank separator row before the intercept block
  blank_vals <- c("", rep("", length(col_ids)))
  names(blank_vals) <- c("variable", col_ids)
  all_rows[[length(all_rows) + 1]] <- list(values = blank_vals, type = "blank")

  # Intercept (placed at the bottom, just above R\u00b2)
  for (rtype in c("coef", "se")) {
    vals <- c(if (rtype == "coef") "  Intercept" else "")
    for (cn in col_ids) {
      cell <- get_coef_cell(fits[[cn]], "intrcpt", dig, approach)
      vals <- c(vals, cell[[rtype]])
    }
    names(vals) <- c("variable", col_ids)
    all_rows[[length(all_rows) + 1]] <- list(values = vals, type = rtype)
  }

  # Blank separator row between intercept and R\u00b2 / k stats
  all_rows[[length(all_rows) + 1]] <- list(values = blank_vals, type = "blank")

  r2_vals <- c("R\u00b2")
  for (cn in col_ids) {
    r2_vals <- c(r2_vals, get_R2(fits[[cn]], approach))
  }
  names(r2_vals) <- c("variable", col_ids)
  all_rows[[length(all_rows) + 1]] <- list(values = r2_vals, type = "stat")

  # Number of experiments row
  k_vals <- c("Number of experiments")
  for (cn in col_ids) {
    k <- get_k(fits[[cn]], approach)
    k_vals <- c(k_vals, if (is.na(k)) "" else as.character(k))
  }
  names(k_vals) <- c("variable", col_ids)
  all_rows[[length(all_rows) + 1]] <- list(values = k_vals, type = "stat")

  all_rows
}

# Convert row list to data.frame + row_types vector
rows_to_df <- function(all_rows) {
  df <- do.call(rbind, lapply(all_rows, function(r) {
    as.data.frame(t(r$values), stringsAsFactors = FALSE)
  }))
  row_types <- sapply(all_rows, function(r) r$type)
  list(df = df, row_types = row_types)
}

# Style a flextable from df + row_types. `setup_header`, if non-NULL, is a
# function(ft) -> ft that sets header labels/spans/bolding for the table; the
# default labels columns 1..(ncols-1) blank and the last column "Survivors",
# as expected by the per-horizon panel tables.
style_table <- function(df, row_types, caption, footer_note, fontname = NULL,
                        setup_header = NULL) {
  ncols  <- ncol(df)
  n_data <- ncols - 1L
  label_width <- 2.6
  # Add the width of one asterisk per data column so "***" doesn't wrap.
  asterisk_width <- 0.06
  data_width  <- (8.33 - label_width) / n_data + asterisk_width
  ft <- flextable(df)

  if (is.null(setup_header)) {
    header_labels <- as.list(setNames(c(rep("", ncols - 1L),
                                        "Survivors"), names(df)))
    ft <- set_header_labels(ft, values = header_labels)
    ft <- bold(ft, j = ncols, part = "header")
  } else {
    ft <- setup_header(ft)
  }

  ft <- fontsize(ft, size = 10, part = "all")
  if (!is.null(fontname)) ft <- font(ft, fontname = fontname, part = "all")
  ft <- align(ft, j = 1, align = "left", part = "body")
  ft <- align(ft, j = 2:ncols, align = "center", part = "body")
  ft <- align(ft, j = 2:ncols, align = "center", part = "header")
  ft <- width(ft, j = 1, width = label_width)
  ft <- width(ft, j = 2:ncols, width = data_width)
  ft <- padding(ft, padding.top = 0, padding.bottom = 0, part = "body")
  ft <- line_spacing(ft, space = 0.9, part = "body")

  section_rows <- which(row_types == "section")
  se_rows_idx <- which(row_types == "se")
  stat_rows <- which(row_types == "stat")
  blank_rows <- which(row_types == "blank")

  if (length(section_rows) > 0) {
    ft <- bold(ft, i = section_rows, j = 1, part = "body")
    # Span the section header across all columns so it doesn't wrap in col 1
    for (i in section_rows) {
      ft <- merge_h_range(ft, i = i, j1 = 1, j2 = ncols, part = "body")
    }
  }
  if (length(blank_rows) > 0) {
    # Give the spacer rows an explicit height; with no content they'd collapse.
    ft <- height(ft, i = blank_rows, height = 0.18, part = "body")
    ft <- hrule(ft, i = blank_rows, rule = "exact", part = "body")
  }
  if (length(se_rows_idx) > 0) {
    # Pin SE-row height so rows whose SE is just a non-breaking space (no
    # coefficient identified in any column) match populated SE rows in height.
    ft <- height(ft, i = se_rows_idx, height = 0.16, part = "body")
    ft <- hrule(ft, i = se_rows_idx, rule = "atleast", part = "body")
  }
  # No borders except thin horizontal rules at top and bottom
  ft <- border_remove(ft)
  thin_rule <- fp_border(color = "black", width = 0.5)
  ft <- hline_top(ft, part = "body", border = thin_rule)
  ft <- hline_bottom(ft, part = "body", border = thin_rule)

  ft <- set_caption(ft, caption = caption)
  ft <- add_footer_lines(ft, values = footer_note)
  ft <- fontsize(ft, size = 8, part = "footer")
  if (!is.null(fontname)) ft <- font(ft, fontname = fontname, part = "footer")
  ft
}

# ── 8. Generate tables ───────────────────────────────────────────────────────

panels <- list(
  list(oc = "emp",  label = "Employment impacts (percentage points)", dig = 2),
  list(oc = "earn", label = "Earnings impacts (2025$, annualized)", dig = 0)
)

col_ids <- c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)", "(8)")

footer_note <- function(approach) {
  paste("Notes: Standard errors in parentheses.",
        "* p < 0.10, ** p < 0.05, *** p < 0.01.",
        "REML estimates.",
        sprintf("%s.",
                if (approach == "mi")
                  sprintf("Pooled across %d imputations via Rubin's rules", m_imp)
                else "Complete-case (listwise deletion)"))
}

samples <- list(
  list(name = "Full sample", suffix = "",
       idx  = seq_len(nrow(dat))),
  list(name = "Training-primary", suffix = "_primary",
       idx  = which(!is.na(dat$training_role) & dat$training_role == "primary"))
)

# Filled by build_panel_rows; consumed by the combined-survivors table below.
# Key: paste(sm$suffix, approach, hz, oc, sep="/")  Value: list(fit, survivors)
survivor_panel_cache <- list()

for (sm in samples) {
  for (hz in c("st", "mt", "lt")) {
    hz_label <- c(st = "Short-term (~1-year)", mt = "Medium-term (~2-year)", lt = "Long-term (~3-5-year)")[hz]

    for (approach in c("cc", "mi")) {
      app_label <- if (approach == "cc") "Complete Case" else sprintf("Multiple Imputation (m = %d)", m_imp)
      cat(sprintf("Building: %s %s [%s]...\n", hz_label, approach, sm$name))

      # Build both panels
      all_rows <- list()
      for (panel in panels) {
        panel_rows <- build_panel_rows(hz, panel$oc, panel$label, panel$dig,
                                       approach, col_ids, sm)
        all_rows <- c(all_rows, panel_rows)
      }

      # RTF: combined two-panel table
      combined <- rows_to_df(all_rows)
      caption <- sprintf("Meta-Regression of %s Program Impacts (%s%s)",
                         hz_label, app_label,
                         if (sm$suffix == "") "" else sprintf(", %s", sm$name))
      ft <- style_table(combined$df, combined$row_types, caption, footer_note(approach))
      filename <- sprintf("output/table_%s_%s%s.rtf", hz, approach, sm$suffix)
      save_as_rtf(ft, path = filename)
      cat(sprintf("  RTF: %s\n", filename))

      # HTML: separate per-panel tables
      for (panel in panels) {
        panel_rows <- build_panel_rows(hz, panel$oc, panel$label, panel$dig,
                                       approach, col_ids, sm)
        p <- rows_to_df(panel_rows)
        caption_p <- sprintf("%s: %s (%s%s)", hz_label, panel$label, app_label,
                             if (sm$suffix == "") "" else sprintf(", %s", sm$name))
        ft_p <- style_table(p$df, p$row_types, caption_p, footer_note(approach),
                            fontname = "Source Serif 4")
        filename_html <- sprintf("output/table_%s_%s_%s%s.html",
                                 panel$oc, hz, approach, sm$suffix)
        # Generate HTML directly from flextable's internal method
        html_body <- flextable:::gen_raw_html(ft_p)
        full_html <- enc2utf8(paste0(
          "<!DOCTYPE html>\n<html>\n<head><meta charset='UTF-8'>\n",
          "<style>@import url('https://fonts.googleapis.com/css2?family=Source+Serif+4:ital,opsz,wght@0,8..60,200..900;1,8..60,200..900&display=swap');body{font-family:'Source Serif 4',serif;margin:20px;}table{border-collapse:collapse;}</style>\n",
          "</head>\n<body>\n", html_body, "\n",
          iframe_resize_script, "\n</body>\n</html>\n"))
        writeBin(charToRaw(full_html), filename_html)
        cat(sprintf("  HTML: %s\n", filename_html))
      }
    }
  }
}

# ── 8b. Combined survivors table (employment / earnings × mt / lt × cc / mi) ──
#
# One eight-column table per sample showing the survivor regression for each
# (outcome, horizon, approach) panel side by side. Hierarchy is outcome →
# horizon → approach (CC vs MI). A row is kept only if its coefficient
# survived in at least one of the eight panels. The intercept is omitted.
# Instead of a third header level, a body row above R² uses checkmarks to
# mark the MI columns.

# Map placeholder names in row_defs to the actual model.matrix coefficient
# names for a given (horizon, outcome).
resolve_placeholder_coef <- function(varname, hz, oc_type) {
  if (varname == "DATA_SOURCE_PLACEHOLDER")
    return(if (oc_type == "emp") "emp_data_sourcesurvey"
           else "earn_data_sourcesurvey")
  if (varname == "RESPRATE_PLACEHOLDER") return(paste0("resprate_", oc_type, "_", hz))
  if (varname == "UNRATE_PLACEHOLDER")   return(paste0("unrate_", hz))
  varname
}

# Order: outcome (emp → earn) × horizon (mt → lt) × approach (cc → mi)
combined_panels <- local({
  out <- list(); k <- 0L
  for (oc in c("emp", "earn")) for (hz in c("mt", "lt")) for (ap in c("cc", "mi")) {
    k <- k + 1L
    out[[k]] <- list(id = sprintf("(%d)", k), hz = hz, oc = oc, approach = ap,
                     dig = if (oc == "emp") 2 else 0)
  }
  out
})

build_combined_survivors_rows <- function(sm) {
  col_ids <- vapply(combined_panels, `[[`, character(1), "id")
  panel_data <- lapply(combined_panels, function(p) {
    key <- paste(sm$suffix, p$approach, p$hz, p$oc, sep = "/")
    cached <- survivor_panel_cache[[key]]
    if (is.null(cached)) cached <- compute_survivor_panel(p$hz, p$oc, p$approach, sm)
    cached
  })
  fits      <- lapply(panel_data, `[[`, "fit")
  surv_sets <- lapply(panel_data, `[[`, "survivors")

  is_row_kept <- function(varname) {
    any(vapply(seq_along(combined_panels), function(i) {
      p <- combined_panels[[i]]
      resolve_placeholder_coef(varname, p$hz, p$oc) %in% surv_sets[[i]]
    }, logical(1)))
  }

  blank_vals <- setNames(c("", rep("", length(col_ids))), c("variable", col_ids))

  all_rows <- list()
  for (rd in row_defs) {
    kept_vars <- Filter(is_row_kept, names(rd$vars))
    if (length(kept_vars) == 0) next
    sec_row <- setNames(c(rd$section, rep("", length(col_ids))),
                        c("variable", col_ids))
    all_rows[[length(all_rows) + 1]] <- list(values = sec_row, type = "section")
    for (varname in kept_vars) {
      display_label <- rd$vars[[varname]]
      for (rtype in c("coef", "se")) {
        vals <- c(if (rtype == "coef") sprintf("  %s", display_label) else "")
        for (i in seq_along(combined_panels)) {
          p <- combined_panels[[i]]
          resolved <- resolve_placeholder_coef(varname, p$hz, p$oc)
          cell <- get_coef_cell(fits[[i]], resolved, p$dig, p$approach)
          vals <- c(vals, cell[[rtype]])
        }
        names(vals) <- c("variable", col_ids)
        all_rows[[length(all_rows) + 1]] <- list(values = vals, type = rtype)
      }
    }
  }

  all_rows[[length(all_rows) + 1]] <- list(values = blank_vals, type = "blank")

  # MI indicator row (replaces a third header level)
  mi_marks <- vapply(combined_panels, function(p)
    if (p$approach == "mi") "✓" else "", character(1))
  mi_vals <- c("Multiple imputation", mi_marks)
  names(mi_vals) <- c("variable", col_ids)
  all_rows[[length(all_rows) + 1]] <- list(values = mi_vals, type = "stat")

  r2_vals <- c("R²", vapply(seq_along(combined_panels), function(i)
    get_R2(fits[[i]], combined_panels[[i]]$approach), character(1)))
  names(r2_vals) <- c("variable", col_ids)
  all_rows[[length(all_rows) + 1]] <- list(values = r2_vals, type = "stat")

  k_vals <- c("Number of experiments", vapply(seq_along(combined_panels), function(i) {
    k <- get_k(fits[[i]], combined_panels[[i]]$approach)
    if (is.na(k)) "" else as.character(k)
  }, character(1)))
  names(k_vals) <- c("variable", col_ids)
  all_rows[[length(all_rows) + 1]] <- list(values = k_vals, type = "stat")

  all_rows
}

# Two-row header: top spans "Employment" / "Earnings" across the four columns
# of each outcome; bottom labels "Medium-term" / "Long-term" within each outcome.
# CC vs MI is shown by the body indicator row, not a third header level.
combined_header_setup <- function(df) {
  function(ft) {
    ncols <- ncol(df)
    # Bottom header row: time-frame labels for each pair of (cc, mi) columns.
    # Pair indices in the data area: 2-3, 4-5, 6-7, 8-9.
    hz_labels <- c("Medium-term", "Long-term", "Medium-term", "Long-term")
    label_list <- as.list(setNames(c("", rep(hz_labels, each = 2)), names(df)))
    ft <- set_header_labels(ft, values = label_list)
    ft <- merge_h_range(ft, i = 1, j1 = 2, j2 = 3, part = "header")
    ft <- merge_h_range(ft, i = 1, j1 = 4, j2 = 5, part = "header")
    ft <- merge_h_range(ft, i = 1, j1 = 6, j2 = 7, part = "header")
    ft <- merge_h_range(ft, i = 1, j1 = 8, j2 = 9, part = "header")
    # Top header row: outcome group spanning the four columns of each outcome.
    ft <- add_header_row(ft, values = c("", "Employment", "Earnings"),
                         colwidths = c(1, 4, 4), top = TRUE)
    ft <- bold(ft, j = 2:ncols, part = "header")
    ft <- align(ft, j = 2:ncols, align = "center", part = "header")
    ft
  }
}

combined_footer_note <- paste(
  "Notes: Standard errors in parentheses.",
  "* p < 0.10, ** p < 0.05, *** p < 0.01.",
  "REML estimates.",
  sprintf("MI columns (✓) pooled across %d imputations via Rubin's rules; remaining columns use complete cases.", m_imp))

cat("\nBuilding combined survivors tables...\n")
for (sm in samples) {
  cat(sprintf("Combined survivors: [%s]\n", sm$name))
  rows <- build_combined_survivors_rows(sm)
  out  <- rows_to_df(rows)
  caption <- sprintf("Survivor Meta-Regressions, Medium- and Long-Term Outcomes%s",
                     if (sm$suffix == "") "" else sprintf(" (%s)", sm$name))
  setup <- combined_header_setup(out$df)

  ft <- style_table(out$df, out$row_types, caption,
                    combined_footer_note, setup_header = setup)
  filename <- sprintf("output/table_survivors%s.rtf", sm$suffix)
  save_as_rtf(ft, path = filename)
  cat(sprintf("  RTF: %s\n", filename))

  ft_h <- style_table(out$df, out$row_types, caption,
                      combined_footer_note, fontname = "Source Serif 4",
                      setup_header = setup)
  filename_html <- sprintf("output/table_survivors%s.html", sm$suffix)
  html_body <- flextable:::gen_raw_html(ft_h)
  full_html <- enc2utf8(paste0(
    "<!DOCTYPE html>\n<html>\n<head><meta charset='UTF-8'>\n",
    "<style>@import url('https://fonts.googleapis.com/css2?family=Source+Serif+4:ital,opsz,wght@0,8..60,200..900;1,8..60,200..900&display=swap');body{font-family:'Source Serif 4',serif;margin:20px;}table{border-collapse:collapse;}</style>\n",
    "</head>\n<body>\n", html_body, "\n",
    iframe_resize_script, "\n</body>\n</html>\n"))
  writeBin(charToRaw(full_html), filename_html)
  cat(sprintf("  HTML: %s\n", filename_html))
}

# ── 9. Descriptive statistics table ──────────────────────────────────────────
#
# Sample averages for every trait that appears in the meta-regression tables,
# plus the six outcome means. Three columns: training-role-primary subsample,
# the rest, and all rows. Continuous variables → mean. Binary indicators and
# factor-level dummies → proportion. Placeholder labels resolve to a single
# representative column (emp_data_source / resprate_emp_st / unrate_at_rand).

descriptive_value <- function(varname, subset_idx) {
  na_pair <- c(mean = NA_real_, sd = NA_real_, n = 0)
  # Variables whose underlying values are bounded in [0,1] (0/1 indicators
  # and factor-level dummies) are rescaled by 100 so they display on the
  # same 0-100 scale as percentage variables (pct_male, response rate, etc.).
  # The "sd" slot holds the unweighted standard deviation across studies
  # (rescaled the same way); n is the count of non-missing values.
  ms <- function(v) {
    vv <- v[!is.na(v)]
    if (length(vv) == 0) return(na_pair)
    factor_ <- if (all(vv >= 0 & vv <= 1)) 100 else 1
    sdv <- if (length(vv) > 1) sd(vv) else NA_real_
    c(mean = mean(vv) * factor_, sd = sdv * factor_, n = length(vv))
  }
  if (varname == "DATA_SOURCE_PLACEHOLDER")
    return(ms(as.numeric(dat$emp_data_source[subset_idx] == "survey")))
  if (varname == "DATA_SOURCE_ADMIN_PLACEHOLDER")
    return(ms(as.numeric(dat$emp_data_source[subset_idx] == "admin")))
  if (varname == "RESPRATE_PLACEHOLDER")
    return(ms(dat$resprate_emp_st[subset_idx]))
  if (varname == "UNRATE_PLACEHOLDER")
    return(ms(dat$unrate_at_rand[subset_idx]))
  if (varname == "cost_per_treated_k")
    return(ms(dat$cost_per_treated_k[subset_idx] * 1000))
  if (varname %in% names(dat)) {
    x <- dat[[varname]][subset_idx]
    if (is.factor(x) || is.character(x)) return(na_pair)
    return(ms(as.numeric(x)))
  }
  # Factor-level dummy: "<factor><level>"
  for (col in names(dat)) {
    if (!is.factor(dat[[col]])) next
    lvls <- levels(dat[[col]])
    matched <- lvls[paste0(col, lvls) == varname]
    if (length(matched) == 1) {
      return(ms(as.numeric(dat[[col]][subset_idx] == matched)))
    }
  }
  na_pair
}

# Row definitions for the descriptives table only — section headers without
# the "(omitted = …)" qualifier (which only matters in regression tables),
# and reference categories included so every factor's levels are shown.
desc_row_defs <- list(
  list(section = "Setting",
       vars = list(
         "randomization_midpoint"   = "Randomization year",
         "census_regionSouth"       = "South (%)",
         "census_regionNortheast"   = "Northeast (%)",
         "census_regionMidwest"     = "Midwest (%)",
         "census_regionWest"        = "West (%)",
         "geo_typeurban"            = "Urban (%)",
         "geo_typerural"            = "Rural (%)",
         "UNRATE_PLACEHOLDER"       = "Unemployment at randomization (%)"
       )),
  list(section = "Target population",
       vars = list(
         "target_popwelfare_or_lia"     = "Welfare or low-income adults (%)",
         "target_popnoncustodial_parent"= "Noncustodial parents (%)",
         "target_popsubstance_abuse"    = "Substance abuse (%)",
         "target_popjustice_involved"   = "Justice-involved (%)",
         "target_popdislocated"         = "Dislocated workers (%)",
         "target_popyouth"              = "Youth (%)",
         "target_popdisability"         = "Disability (%)"
       )),
  list(section = "Demographics",
       vars = list(
         "pct_male"          = "Male (%)",
         "mean_age"          = "Mean age",
         "pct_black"         = "Black (%)",
         "pct_hispanic"      = "Hispanic (%)",
         "pct_no_hs_diploma" = "No high school diploma (%)"
       )),
  list(section = "Treatment",
       vars = list(
         "has_classroom"                = "Classroom training (%)",
         "has_ojt"                      = "On-the-job training (%)",
         "has_jsa"                      = "Job search assistance (%)",
         "mandatory_voluntarymandatory" = "Mandatory (%)",
         "treatment_duration_months"    = "Treatment duration (months)",
         "cost_per_treated_k"           = "Cost per treated (2025 $)",
         "scaled"                       = "Scaled (%)"
       )),
  list(section = "Employer engagement",
       vars = list(
         "sector_program"         = "Sector program (%)",
         "employengage_curricula" = "Curricula adapted for employers (%)",
         "jobdev_yes"             = "Job development/placement (%)",
         "employerol_curriculum"  = "Employer input on curriculum (%)",
         "employerol_hire"        = "Employer commits to hiring (%)"
       )),
  list(section = "Administration",
       vars = list(
         "funding_public_privatepublic"  = "Funding: public (%)",
         "funding_public_privatemixed"   = "Funding: public & private (%)",
         "funding_public_privateprivate" = "Funding: private (%)",
         "admin_public_privatepublic"    = "Admin: public (%)",
         "admin_public_privatemixed"     = "Admin: public & private (%)",
         "admin_public_privateprivate"   = "Admin: private (%)"
       )),
  list(section = "Study characteristics",
       vars = list(
         "n_total"                       = "Sample size",
         "DATA_SOURCE_ADMIN_PLACEHOLDER" = "Outcome data from official records (%)",
         "RESPRATE_PLACEHOLDER"          = "Response rate (%)",
         "academic"                      = "Academic publication (%)"
       ))
)

# Override style_table's default header (which expects 8 columns and labels
# the last "Survivors") with the three subsample labels.
apply_desc_headers <- function(ft, groups) {
  ids   <- col_ids_for(groups)
  jdata <- 2:(length(ids) + 1L)             # data columns (3 per group)
  # Sub-header row: Mean / SD / N for each sample group.
  sub <- setNames(as.list(rep(c("Mean", "SD", "N"), length(groups))), ids)
  ft <- set_header_labels(ft, values = c(list(variable = ""), sub))
  # Super-header row: one spanning label per group, each over 3 columns.
  ft <- add_header_row(ft,
    values    = c("", vapply(groups, `[[`, "", "header")),
    colwidths = c(1, rep(3, length(groups))),
    top       = TRUE)
  ft <- bold(ft, j = jdata, part = "header")
  ft <- align(ft, j = jdata, align = "center", part = "header")
  # ~2 chars wider than style_table's default 2.6" label column.
  ft <- width(ft, j = 1, width = 2.8)
  ft <- width(ft, j = jdata, width = 0.68)
  # Thin rule under the super-header to separate it from sub-headers.
  thin_rule <- fp_border(color = "black", width = 0.5)
  ft <- hline(ft, i = 1, j = jdata, part = "header", border = thin_rule)
  # Tighten the gap between the bottom rule and the notes.
  ft <- padding(ft, padding.top = 1, part = "footer")
  ft
}

# ── 9b. Mixed-weighting version of the descriptive statistics table ──────────
#
# Same row layout as Section 9 above, but with three different estimators:
#   - Impacts            : REML meta-analytic pooling using reported per-study
#                          sampling SEs (yi = impact, sei = se). Pooled mean
#                          and SE come from rma(method = "REML").
#   - Most trait rows    : sample-size weighted; each study contributes
#                          proportional to N_i = n_treatment_i + n_control_i.
#                            mu_w = sum(N_i * y_i) / sum(N_i)
#                            SE   = sqrt(sigma2_w * sum(N_i^2)) / sum(N_i)
#                            sigma2_w = sum(N_i * (y_i - mu_w)^2) / sum(N_i)
#   - Study-characteristics section: unweighted (those rows are study-level
#                          meta-properties — publication status, data source,
#                          response rate — not population characteristics).
# Studies missing n_treatment + n_control are dropped from the weighted rows
# (those studies are typically also missing many right-hand-side moderators).

dat$n_total <- dat$n_treatment + dat$n_control

# REML meta-analytic pooling of (yi, sei) for the Impacts rows.
ms_reml <- function(yi, sei, cluster = NULL) {
  ok <- !is.na(yi) & !is.na(sei)
  yi <- yi[ok]; sei <- sei[ok]
  if (!is.null(cluster)) cluster <- cluster[ok]
  k <- length(yi)
  if (k == 0) return(c(mean = NA_real_, sd = NA_real_, n = 0))
  if (k == 1) return(c(mean = yi, sd = sei, n = 1L))
  fit <- tryCatch(suppressWarnings(rma(yi = yi, sei = sei, method = "REML")),
                  error = function(e) NULL)
  if (is.null(fit)) return(c(mean = NA_real_, sd = NA_real_, n = k))
  # CR1 cluster-robust SE at the project level, matching 03_tables.R's
  # regression specs and 02g's summary forest plots. Needs >=2 clusters.
  if (!is.null(cluster) && length(unique(cluster)) > 1) {
    fit <- tryCatch(suppressWarnings(metafor::robust(fit, cluster = cluster)),
                    error = function(e) fit)
  }
  c(mean = unname(coef(fit)), sd = sqrt(vcov(fit)[1, 1]), n = k)
}

# Sample-size-weighted mean and SD for trait rows.
ms_sw <- function(yi, n) {
  ok <- !is.na(yi) & !is.na(n) & n > 0
  yi <- yi[ok]; n <- n[ok]
  k <- length(yi)
  if (k == 0) return(c(mean = NA_real_, sd = NA_real_, n = 0))
  if (k == 1) return(c(mean = yi, sd = NA_real_, n = 1L))
  W <- sum(n)
  mu_w <- sum(n * yi) / W
  var_w <- sum(n * (yi - mu_w)^2) / W
  c(mean = mu_w, sd = sqrt(var_w), n = k)
}

descriptive_sw_value <- function(varname, subset_idx) {
  na_pair <- c(mean = NA_real_, sd = NA_real_, n = 0)
  n_sub <- dat$n_total[subset_idx]
  ms <- function(v) {
    ok <- !is.na(v) & !is.na(n_sub) & n_sub > 0
    vv <- v[ok]; nn <- n_sub[ok]
    if (length(vv) == 0) return(na_pair)
    factor_ <- if (all(vv >= 0 & vv <= 1)) 100 else 1
    ms_sw(vv * factor_, nn)
  }
  if (varname == "DATA_SOURCE_PLACEHOLDER")
    return(ms(as.numeric(dat$emp_data_source[subset_idx] == "survey")))
  if (varname == "DATA_SOURCE_ADMIN_PLACEHOLDER")
    return(ms(as.numeric(dat$emp_data_source[subset_idx] == "admin")))
  if (varname == "RESPRATE_PLACEHOLDER")
    return(ms(dat$resprate_emp_st[subset_idx]))
  if (varname == "UNRATE_PLACEHOLDER")
    return(ms(dat$unrate_at_rand[subset_idx]))
  if (varname == "TARGETPOP_WELFARE_OR_LIA")
    return(ms(as.numeric(dat$target_pop[subset_idx] %in%
                         c("welfare", "low_income_adult"))))
  if (varname == "cost_per_treated_k")
    return(ms(dat$cost_per_treated_k[subset_idx] * 1000))
  if (varname %in% names(dat)) {
    x <- dat[[varname]][subset_idx]
    if (is.factor(x) || is.character(x)) return(na_pair)
    return(ms(as.numeric(x)))
  }
  for (col in names(dat)) {
    if (!is.factor(dat[[col]])) next
    lvls <- levels(dat[[col]])
    matched <- lvls[paste0(col, lvls) == varname]
    if (length(matched) == 1) {
      return(ms(as.numeric(dat[[col]][subset_idx] == matched)))
    }
  }
  na_pair
}

primary_idx <- which(!is.na(dat$training_role) & dat$training_role == "primary")
sector_idx  <- which(!is.na(dat$sector_program) & dat$sector_program == 1)
all_idx     <- seq_len(nrow(dat))

# Column groups shared by both descriptive tables. Each group is one
# Mean/SD/N triplet: `prefix` keys the column ids, `header` is the spanning
# super-header, `idx` selects rows. Sector programs are the full sector
# subset (all `sector_program == 1` rows, regardless of training_role).
desc_groups <- list(
  list(prefix = "all",     header = "Experiments incorporating training",
       idx = all_idx),
  list(prefix = "primary", header = "Those with training primary",
       idx = primary_idx),
  list(prefix = "sector",  header = "Sector programs",
       idx = sector_idx)
)
col_ids_for <- function(groups)
  unlist(lapply(groups, function(g)
    paste0(g$prefix, c("_mean", "_se", "_n"))))

# fmt_row / blank_row / section_row read this global.
desc_col_ids <- col_ids_for(desc_groups)

fmt_v_cells <- function(stats, digits = 2, big_mark = "") {
  if (is.na(stats[["mean"]])) return(c("–", "", ""))
  m <- formatC(stats[["mean"]], format = "f", digits = digits,
               big.mark = big_mark)
  se_str <- if (is.na(stats[["sd"]])) "" else
    formatC(stats[["sd"]], format = "f", digits = digits,
            big.mark = big_mark)
  n_str <- if (is.na(stats[["n"]]) || stats[["n"]] == 0) ""
           else as.character(stats[["n"]])
  c(m, se_str, n_str)
}
fmt_row <- function(label, vals, digits = 2) {
  big_mark <- if (grepl("\\$|sample size", label, ignore.case = TRUE)) ","
              else ""
  cells <- unlist(lapply(vals, fmt_v_cells, digits = digits,
                         big_mark = big_mark))
  out <- c(sprintf("  %s", label), cells)
  names(out) <- c("variable", desc_col_ids)
  out
}
blank_row <- function() {
  bv <- rep("", length(desc_col_ids) + 1L)
  names(bv) <- c("variable", desc_col_ids)
  list(values = bv, type = "blank")
}
section_row <- function(title) {
  sr <- c(title, rep("", length(desc_col_ids)))
  names(sr) <- c("variable", desc_col_ids)
  list(values = sr, type = "section")
}

build_traits_rows <- function() {
  rows <- list()
  for (i in seq_along(desc_row_defs)) {
    rd <- desc_row_defs[[i]]
    if (i > 1) rows[[length(rows) + 1]] <- blank_row()
    rows[[length(rows) + 1]] <- section_row(rd$section)
    for (varname in names(rd$vars)) {
      vals <- lapply(desc_groups,
                     function(g) descriptive_value(varname, g$idx))
      digits_t <- if (varname %in% c("treatment_duration_months",
                                     "UNRATE_PLACEHOLDER")) 1L else 0L
      rows[[length(rows) + 1]] <- list(
        values = fmt_row(rd$vars[[varname]], vals, digits = digits_t),
        type = "coef")
    }
  }
  rows
}

# ── Stacked-cell variant (used by the outcomes table only) ───────────────────
# Each group renders as two columns instead of three: an estimate column that
# stacks the SD/SE under the point estimate in parentheses, plus an N column.
# The descriptives table keeps the side-by-side Mean/SD/N layout above.
desc_col_ids_stacked <- unlist(lapply(desc_groups, function(g)
  paste0(g$prefix, c("_mean", "_n"))))

fmt_v_cells_stacked <- function(stats, digits = 2, big_mark = "") {
  if (is.na(stats[["mean"]])) return(c("–", ""))
  m <- formatC(stats[["mean"]], format = "f", digits = digits,
               big.mark = big_mark)
  est <- if (is.na(stats[["sd"]])) m else
    paste0(m, "\n(",
           formatC(stats[["sd"]], format = "f", digits = digits,
                   big.mark = big_mark), ")")
  n_str <- if (is.na(stats[["n"]]) || stats[["n"]] == 0) ""
           else as.character(stats[["n"]])
  c(est, n_str)
}
fmt_row_stacked <- function(label, vals, digits = 2) {
  big_mark <- if (grepl("\\$|sample size", label, ignore.case = TRUE)) ","
              else ""
  cells <- unlist(lapply(vals, fmt_v_cells_stacked, digits = digits,
                         big_mark = big_mark))
  out <- c(sprintf("  %s", label), cells)
  names(out) <- c("variable", desc_col_ids_stacked)
  out
}
blank_row_stacked <- function() {
  bv <- rep("", length(desc_col_ids_stacked) + 1L)
  names(bv) <- c("variable", desc_col_ids_stacked)
  list(values = bv, type = "blank")
}
section_row_stacked <- function(title) {
  sr <- c(title, rep("", length(desc_col_ids_stacked)))
  names(sr) <- c("variable", desc_col_ids_stacked)
  list(values = sr, type = "section")
}

# Header for the stacked layout: super-header per group spanning 2 columns,
# sub-headers "Mean" / "N" (the SD/SE now sits under the mean in parentheses).
apply_desc_headers_stacked <- function(ft, groups) {
  ids   <- unlist(lapply(groups, function(g)
    paste0(g$prefix, c("_mean", "_n"))))
  jdata <- 2:(length(ids) + 1L)             # data columns (2 per group)
  sub <- setNames(as.list(rep(c("Mean", "N"), length(groups))), ids)
  ft <- set_header_labels(ft, values = c(list(variable = ""), sub))
  ft <- add_header_row(ft,
    values    = c("", vapply(groups, `[[`, "", "header")),
    colwidths = c(1, rep(2, length(groups))),
    top       = TRUE)
  ft <- bold(ft, j = jdata, part = "header")
  ft <- align(ft, j = jdata, align = "center", part = "header")
  ft <- width(ft, j = 1, width = 2.8)
  # Estimate columns (odd positions within jdata) wider than the N columns.
  mean_j <- jdata[seq(1, length(jdata), by = 2)]
  n_j    <- jdata[seq(2, length(jdata), by = 2)]
  ft <- width(ft, j = mean_j, width = 1.05)
  ft <- width(ft, j = n_j,    width = 0.55)
  thin_rule <- fp_border(color = "black", width = 0.5)
  ft <- hline(ft, i = 1, j = jdata, part = "header", border = thin_rule)
  ft <- padding(ft, padding.top = 1, part = "footer")
  ft
}

build_outcomes_rows <- function() {
  # First-stage (take-up) outcomes have no time-horizon prefix; they are
  # listed first so they precede the employment and earnings impacts.
  outcomes <- list(
    list("program_takeup", "", "Program take-up (%)",        1L),
    list("any_training",   "", "Any training take-up (%)",    1L),
    list("emp",  "st", "Employment, short-term (%)",      1L),
    list("emp",  "mt", "Employment, medium-term (%)",     1L),
    list("emp",  "lt", "Employment, long-term (%)",       1L),
    list("earn", "st", "Earnings, short-term (2025 $)",   0L),
    list("earn", "mt", "Earnings, medium-term (2025 $)",  0L),
    list("earn", "lt", "Earnings, long-term (2025 $)",    0L)
  )
  # Control-group means and impacts both use REML on the reported per-study
  # impact SE: same weights study-by-study, just a different `yi`.
  reml_section <- function(yi_suffix) {
    function(oc) {
      oc_type <- oc[[1]]; hz <- oc[[2]]; lab <- oc[[3]]; digits_oc <- oc[[4]]
      # First-stage outcomes (hz == "") carry no time-horizon prefix.
      colbase <- if (hz == "") oc_type else paste0(hz, "_", oc_type)
      y_col  <- paste0(colbase, "_", yi_suffix)
      se_col <- paste0(colbase, "_se")
      vals <- lapply(desc_groups, function(g)
        ms_reml(dat[[y_col]][g$idx], dat[[se_col]][g$idx],
                dat$project[g$idx]))
      list(values = fmt_row_stacked(lab, vals, digits = digits_oc),
           type = "coef")
    }
  }
  rows <- list()
  rows[[length(rows) + 1]] <- section_row_stacked("Control-group means")
  for (oc in outcomes) rows[[length(rows) + 1]] <- reml_section("control_mean")(oc)
  rows[[length(rows) + 1]] <- blank_row_stacked()
  rows[[length(rows) + 1]] <- section_row_stacked("Impacts")
  for (oc in outcomes) rows[[length(rows) + 1]] <- reml_section("impact")(oc)
  rows
}

write_desc_table <- function(rows, groups, basename, caption, footer,
                             header_fn = apply_desc_headers) {
  obj <- rows_to_df(rows)
  ft <- style_table(obj$df, obj$row_types, caption, footer)
  ft <- header_fn(ft, groups)
  rtf_path <- sprintf("output/%s.rtf", basename)
  save_as_rtf(ft, path = rtf_path)
  cat("  RTF: ", rtf_path, "\n", sep = "")

  ft_h <- style_table(obj$df, obj$row_types, caption, footer,
                      fontname = "Source Serif 4")
  ft_h <- header_fn(ft_h, groups)
  html_path <- sprintf("output/%s.html", basename)
  html_body <- flextable:::gen_raw_html(ft_h)
  # flextable's gen_raw_html drops the caption set by set_caption(); inject
  # it as a heading above the table so the HTML version shows a title too.
  esc <- function(s) gsub("<", "&lt;", gsub("&", "&amp;", s, fixed = TRUE), fixed = TRUE)
  title_html <- sprintf(
    "<h2 style='font-family:\"Source Serif 4\",serif;font-weight:600;font-size:1.15em;margin:0 0 10px 0;'>%s</h2>\n",
    esc(caption))
  full_html <- enc2utf8(paste0(
    "<!DOCTYPE html>\n<html>\n<head><meta charset='UTF-8'>\n",
    "<style>@import url('https://fonts.googleapis.com/css2?family=Source+Serif+4:ital,opsz,wght@0,8..60,200..900;1,8..60,200..900&display=swap');body{font-family:'Source Serif 4',serif;margin:20px;}table{border-collapse:collapse;}</style>\n",
    "</head>\n<body>\n", title_html, html_body, "\n",
    iframe_resize_script, "\n</body>\n</html>\n"))
  writeBin(charToRaw(full_html), html_path)
  cat("  HTML: ", html_path, "\n", sep = "")
}

write_desc_table(
  build_traits_rows(), desc_groups,
  basename = "table_descriptives",
  caption  = "Study traits in U.S.-based job training experiments",
  footer   = paste(
    'Notes: Full sample is 144 estimates from 56 studies. Averages are unweighted. Sample sizes vary because of missing data. For multi-region experiments, “regional” unemployment is national.'))

write_desc_table(
  build_outcomes_rows(), desc_groups,
  basename = "table_outcomes",
  caption  = "Control groups means and impact estimates in U.S.-based job training experiments",
  footer   = paste(
    'Notes: Impacts are estimated with random-effects maximum likelihood (REML) meta-analysis. Control-group statistics are com-puted using the corresponding REML weights. Figures in parentheses below each estimate are standard deviations of control-group values and standard errors of impact estimates. Sample sizes vary because of missing data. Short-term outcomes have a ~1-year fol-low-up, medium-term ~2 years, and long-term ~3-5 years.'),
  header_fn = apply_desc_headers_stacked)

cat("\nAll tables written.\n")
