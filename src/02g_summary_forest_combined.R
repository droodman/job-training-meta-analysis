###############################################################################
# 02g_summary_forest_combined.R — Combined 4-column summary forest plot
#
# One figure with the same row layout as 02f, but four forest panels:
# Employment / Earnings × Medium-term / Long-term. The two emp panels share
# an x-axis scale; the two earn panels share theirs (different scale).
#
# All gap/layout tricks are mirrored from 02f_summary_forest.R:
#   yaxs = "i" to kill R's 4% ylim padding
#   subtitles drawn inside the plot region (no top margin space)
#   tick labels and outer x-axis label placed via mgp / mtext lines
#   broken x = 0 reference line per group
###############################################################################

library(metafor)
library(sysfonts)
library(showtext)
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

# ── 1. Subgroup definitions (mirror 02f_summary_forest.R) ────────────────────

dat$decade <- cut(dat$randomization_midpoint,
  breaks = c(-Inf, 1980, 1990, 2000, 2010, Inf),
  labels = c("1970s", "1980s", "1990s", "2000s", "2010s"),
  right = FALSE)

dat$training_modality <- factor(ifelse(
  dat$has_classroom == 1 & dat$has_ojt == 1, "Both",
  ifelse(dat$has_classroom == 1 & dat$has_ojt == 0, "Classroom",
  ifelse(dat$has_classroom == 0 & dat$has_ojt == 1, "On the job", "Neither"))),
  levels = c("Classroom", "On the job", "Both", "Neither"))

dat$treatment_duration_label <- factor(
  ifelse(is.na(dat$treatment_duration_months), NA,
  ifelse(dat$treatment_duration_months < 6, "<6 months", ">6 months")),
  levels = c("<6 months", ">6 months"))

dat$mandatory_voluntary <- factor(dat$mandatory_voluntary,
  levels = c("voluntary", "mandatory"),
  labels = c("Yes",       "No"))

dat$sector_label <- factor(
  ifelse(dat$sector_program == 1, "Yes", "No"),
  levels = c("Yes", "No"))

dat$funding_label <- factor(dat$funding_public_private,
  levels = c("public", "private", "mixed"),
  labels = c("Public", "Private", "Mixed"))

dat$academic_label <- factor(
  ifelse(dat$academic == 1, "Yes", "No"),
  levels = c("Yes", "No"))

dat$age_decade <- cut(dat$mean_age,
  breaks = c(-Inf, 20, 30, 40, Inf),
  labels = c("<20", "20–29", "30–39", "40+"),
  right = FALSE)
dat$age_decade[is.na(dat$age_decade) & dat$target_pop == "youth"] <- "<20"

dat$gender_majority <- factor(
  ifelse(is.na(dat$pct_male), NA,
  ifelse(dat$pct_male >= 50, "Male", "Female")),
  levels = c("Male", "Female"))

dat$scaled_label <- factor(
  ifelse(dat$scaled == 1, "At-scale", "Demo/pilot"),
  levels = c("Demo/pilot", "At-scale"))

dat$data_source_label <- factor(dat$emp_data_source,
  levels = c("admin", "survey"),
  labels = c("Administrative", "Survey"))

for (col in grep("^resprate_label_(emp|earn)_(st|mt|lt)$",
                 names(dat), value = TRUE)) {
  v <- as.character(dat[[col]])
  v <- ifelse(grepl("≥80|>=80|high", v, ignore.case = TRUE), ">80%",
       ifelse(grepl("<80|low", v, ignore.case = TRUE),         "<80%", NA))
  dat[[col]] <- factor(v, levels = c("<80%", ">80%"))
}

dat$cost_info_label <- factor(
  ifelse(!is.na(dat$net_cost_narrow), "Available", "Missing"),
  levels = c("Available", "Missing"))

# ── 2. Subgroup specs (same order as 02f) ────────────────────────────────────

subgroups <- list(
  list(var = "decade",                       label = "Decade"),
  list(var = "census_region",                label = "Census region"),
  list(var = "geo_type",                     label = "Urban/rural"),
  list(var = "target_pop",                   label = "Target population"),
  list(var = "gender_majority",              label = "Gender majority"),
  list(var = "age_decade",                   label = "Mean age"),
  list(var = "training_modality",            label = "Training modality"),
  list(var = "mandatory_voluntary",          label = "Voluntary"),
  list(var = "treatment_duration_label",     label = "Treatment duration"),
  list(var = "scaled_label",                 label = "Demo vs. at-scale"),
  list(var = "sector_label",                 label = "Sector program"),
  list(var = "funding_label",                label = "Funding source"),
  list(var = "data_source_label",            label = "Outcome data source"),
  list(var = ".resprate",                    label = "Response rate"),
  list(var = "academic_label",               label = "Academically published"),
  list(var = "cost_info_label",              label = "Cost info")
)

# ── 3. REML pooler ───────────────────────────────────────────────────────────

pool_rma <- function(yi, sei, cluster = NULL) {
  ok  <- !is.na(yi) & !is.na(sei)
  yi  <- yi[ok]; sei <- sei[ok]
  if (!is.null(cluster)) cluster <- cluster[ok]
  k   <- length(yi)
  if (k == 0) return(list(b = NA_real_, ci.lb = NA_real_,
                          ci.ub = NA_real_, k = 0L))
  if (k == 1) return(list(b = yi, ci.lb = yi - 1.96 * sei,
                          ci.ub = yi + 1.96 * sei, k = 1L))
  fit <- tryCatch(suppressWarnings(rma(yi = yi, sei = sei, method = "REML")),
    error = function(e) tryCatch(
      suppressWarnings(rma(yi = yi, sei = sei, method = "DL")),
      error = function(e2) NULL))
  if (is.null(fit)) return(list(b = NA_real_, ci.lb = NA_real_,
                                ci.ub = NA_real_, k = k))
  # Cluster-robust (CR1) SEs at the project level, matching 03_tables.R.
  # Need at least 2 clusters; with 1, robust() is undefined — fall back.
  if (!is.null(cluster) && length(unique(cluster)) > 1) {
    fit <- tryCatch(suppressWarnings(metafor::robust(fit, cluster = cluster)),
                    error = function(e) fit)
  }
  list(b = unname(coef(fit)), ci.lb = fit$ci.lb, ci.ub = fit$ci.ub, k = k)
}

# ── 4. Layout (rows are common across all four panels) ───────────────────────

build_layout <- function() {
  rows <- list()
  rows[[length(rows) + 1]] <- list(type = "level",
                                    section = "Overall",
                                    level = "Overall")
  for (sg in subgroups) {
    rows[[length(rows) + 1]] <- list(type = "header",
                                      section = sg$label,
                                      level = NA_character_)
    var_actual <- if (sg$var == ".resprate") "resprate_label_emp_mt"
                  else sg$var
    grp <- dat[[var_actual]]
    lvls <- if (is.factor(grp)) levels(grp) else sort(unique(na.omit(grp)))
    for (lv in lvls) {
      if (sg$var == "target_pop" && lv == "substance_abuse") next
      if (sg$var == "geo_type"   && lv == "major city")      next
      rows[[length(rows) + 1]] <- list(type = "level",
                                        section = sg$label,
                                        level = as.character(lv))
    }
  }
  rows
}

# ── 5. Compute estimates for all four (outcome × horizon) combos ─────────────

compute_estimates <- function(layout_rows, sample_idx = seq_len(nrow(dat))) {
  for (i in seq_along(layout_rows)) {
    lr <- layout_rows[[i]]
    if (lr$type == "header") next
    for (oc_name in c("emp", "earn")) {
      for (hz in c("mt", "lt")) {
        imp_col <- paste0(hz, "_", oc_name, "_impact")
        se_col  <- paste0(hz, "_", oc_name, "_se")
        if (lr$section == "Overall") {
          idx <- sample_idx
        } else {
          sg <- subgroups[[which(vapply(subgroups,
                         function(s) s$label == lr$section, logical(1)))]]
          var_actual <- if (sg$var == ".resprate") {
            paste0("resprate_label_", oc_name, "_", hz)
          } else sg$var
          idx <- intersect(which(as.character(dat[[var_actual]]) == lr$level),
                           sample_idx)
        }
        yi  <- dat[[imp_col]][idx]
        sei <- dat[[se_col]][idx]
        cluster <- dat$project[idx]
        layout_rows[[i]][[paste0(oc_name, "_", hz)]] <- pool_rma(yi, sei,
                                                                  cluster)
      }
    }
  }
  layout_rows
}

draw_point <- function(x, ci.lb, ci.ub, y,
                       color = "steelblue1", cex = 0.9) {
  segments(ci.lb, y, ci.ub, y, col = color, lwd = 1.4)
  points(x, y, pch = 19, col = color, cex = cex)
}

prettify <- function(s) {
  s <- gsub("_", " ", s)
  paste0(toupper(substr(s, 1, 1)), substr(s, 2, nchar(s)))
}

# ── 6. Plot ──────────────────────────────────────────────────────────────────

make_combined_forest <- function(sample_idx = seq_len(nrow(dat)),
                                 filename_suffix = "") {
  layout_rows <- build_layout()
  layout_rows <- compute_estimates(layout_rows, sample_idx)
  n_rows <- length(layout_rows)
  for (i in seq_along(layout_rows)) layout_rows[[i]]$y <- n_rows - i + 1

  # Per-outcome shared x-limits.
  emp_xs <- numeric(0); earn_xs <- numeric(0)
  for (lr in layout_rows) {
    if (lr$type != "level") next
    for (hz in c("mt", "lt")) {
      e_emp  <- lr[[paste0("emp_",  hz)]]
      e_earn <- lr[[paste0("earn_", hz)]]
      if (!is.null(e_emp))  emp_xs  <- c(emp_xs,  e_emp$ci.lb,  e_emp$ci.ub)
      if (!is.null(e_earn)) earn_xs <- c(earn_xs, e_earn$ci.lb, e_earn$ci.ub)
    }
  }
  emp_xlim   <- c(-5, 15)                   # clip wide CIs
  emp_xticks <- c(-5, 0, 5, 10, 15)
  earn_xlim  <- c(-1500, 9500)              # match 02f's clip
  earn_xticks <- c(0, 2000, 4000, 6000, 8000)

  fig_height <- max(7, 0.1125 * n_rows + 1.2)
  filename <- sprintf("output/forest_summary%s.png", filename_suffix)
  layout_widths <- c(0.85, rep(1.6, 4))             # labels + 4 equal forests
  oma_right_in  <- 0.4                              # space for earn_lt labels
  fig_width <- sum(layout_widths) + oma_right_in

  png(filename, width = fig_width, height = fig_height,
      units = "in", res = 300)
  on.exit(dev.off())
  showtext_opts(dpi = 300)
  # Top oma reserves room for single-line panel subtitles in the outer
  # margin (matching the bottom outer x-axis label setup). Right oma holds
  # the rightmost panel's right-edge labels (no next cell to spill into).
  par(family = "LM Roman 10",
      omi = c(1.7 * 0.17, 0, 1.4 * 0.17, oma_right_in),
      yaxs = "i", xaxs = "i")
  layout(matrix(seq_along(layout_widths), nrow = 1), widths = layout_widths)

  # No headroom above topmost data — subtitles live in the top oma.
  ylim <- c(0.5, n_rows + 0.5)

  # Panel 1: row labels.
  par(mar = c(1.1, 0.4, 0.1, 0))
  plot.new()
  plot.window(xlim = c(0, 1), ylim = ylim)
  for (lr in layout_rows) {
    if (lr$type == "header" || lr$section == "Overall") {
      label <- if (lr$type == "header") prettify(lr$section)
               else prettify(lr$level)
      text(0, lr$y, label, pos = 4, font = 2, cex = 1.0,
           offset = 0, xpd = NA)
    } else {
      lab <- sprintf("    %s", prettify(lr$level))
      text(0, lr$y, lab, pos = 4, cex = 1., offset = 0, xpd = NA)
    }
  }

  # Panels 2-5: emp_mt, emp_lt, earn_mt, earn_lt.
  # Inner margins: tight within an outcome pair, slightly larger between
  # pairs since emp and earn use different x-scales.
  # Uniform left/right margins so all four plot regions have equal width.
  # mar is in *lines* (~0.17"/line). right_mar is small because labels are
  # anchored to the plot's right edge (not the cell boundary) — labels can
  # overflow into the next cell's left_mar, which is sized to absorb them.
  panel_specs <- list(
    list(oc = "emp",  hz = "mt", xlim = emp_xlim,  xticks = emp_xticks,
         left_mar = 2.3, right_mar = 0.5, digits = 1L,
         subtitle = "Employment, medium-term"),
    list(oc = "emp",  hz = "lt", xlim = emp_xlim,  xticks = emp_xticks,
         left_mar = 2.3, right_mar = 0.5, digits = 1L,
         subtitle = "Employment, long-term"),
    list(oc = "earn", hz = "mt", xlim = earn_xlim, xticks = earn_xticks,
         left_mar = 2.3, right_mar = 0.5, digits = 0L,
         subtitle = "Earnings, medium-term"),
    list(oc = "earn", hz = "lt", xlim = earn_xlim, xticks = earn_xticks,
         left_mar = 2.3, right_mar = 0.5, digits = 0L,
         subtitle = "Earnings, long-term")
  )

  # Per-panel geometry captured here so subtitles below can be centered on
  # the *visual column* (plot region + data-label area to its right), not
  # the cell center. Without this, subtitles overlap the previous panel's
  # data labels because labels overflow into the next cell's left margin.
  panel_geom <- vector("list", length(panel_specs))
  for (i in seq_along(panel_specs)) {
    ps <- panel_specs[[i]]
    par(mar = c(1.1, ps$left_mar, 0.1, ps$right_mar))
    plot.new()
    plot.window(xlim = ps$xlim, ylim = ylim)
    for (lr in layout_rows) {
      if (lr$type != "level") next
      segments(0, lr$y - 0.5, 0, lr$y + 0.5, lty = 2, col = "gray50")
    }
    axis(1, at = ps$xlim, labels = FALSE, lwd = 1, lwd.ticks = 0,
         mgp = c(3, 0.3, 0))
    axis(1, at = ps$xticks, lwd = 0, lwd.ticks = 1, cex.axis = 1.,
         mgp = c(3, 0.5, 0))
    for (lr in layout_rows) {
      if (lr$type != "level") next
      e <- lr[[paste0(ps$oc, "_", ps$hz)]]
      if (is.null(e) || is.na(e$b)) next
      draw_point(e$b, e$ci.lb, e$ci.ub, lr$y)
    }
    # Right-edge point-estimate labels (no CIs). Anchor by the WIDEST
    # label's strwidth so the widest label sits a small pad past the plot
    # edge; all labels right-justify to the same column. Labels overflow
    # into the next cell's left_mar (which is sized to fit them) instead
    # of consuming this cell's right margin.
    labels <- vapply(seq_along(layout_rows), function(j) {
      lr <- layout_rows[[j]]
      if (lr$type != "level") return("")
      e <- lr[[paste0(ps$oc, "_", ps$hz)]]
      if (is.null(e) || is.na(e$b)) return("")
      gsub("-", "−", formatC(e$b, format = "f", digits = ps$digits))
    }, character(1))
    nonempty <- nzchar(labels)
    max_lw_in <- 0
    if (any(nonempty)) {
      max_lw_user <- max(strwidth(labels[nonempty], cex = 1.))
      usr <- par("usr"); pin <- par("pin")
      x_per_in <- (usr[2] - usr[1]) / pin[1]
      pad_user <- 0.02 * x_per_in
      x_anchor <- usr[2] + max_lw_user + pad_user
      for (j in which(nonempty)) {
        text(x_anchor, layout_rows[[j]]$y, labels[j],
             adj = c(1, 0.5), cex = 1., xpd = NA)
      }
      max_lw_in <- max_lw_user / x_per_in
    }
    mai <- par("mai")
    cell_left_in  <- sum(layout_widths[seq_len(i)])  # left edge of this cell
    plot_left_in  <- cell_left_in + mai[2]
    plot_right_in <- cell_left_in + layout_widths[i + 1] - mai[4]
    label_right_in <- plot_right_in + max_lw_in + 0.02
    panel_geom[[i]] <- list(visual_center_in = (plot_left_in + label_right_in) / 2)
  }

  # Reset par("cex") to 1 so the outer mtext calls below render at the
  # nominal cex without layout()'s panel-cex shrinkage.
  par(cex = 1)

  # Panel subtitles in the top oma — centered on each panel's *visual
  # column* (plot region + label area to its right). mtext(outer=TRUE)
  # treats `at` as NDC of the LAYOUT AREA (i.e., the device width minus
  # the outer margins), so divide by sum(layout_widths), NOT fig_width.
  layout_w <- sum(layout_widths)
  panel_centers <- vapply(panel_geom,
    function(g) g$visual_center_in / layout_w, numeric(1))
  for (i in seq_along(panel_specs)) {
    mtext(panel_specs[[i]]$subtitle, side = 3, line = 0.3,
          outer = TRUE, cex = 0.8, at = panel_centers[i])
  }

  # Outer x-axis labels — one per outcome pair, centered under it.
  total_w <- sum(layout_widths)
  emp_pair_center  <- (layout_widths[1] +
                        sum(layout_widths[2:3]) / 2) / total_w
  earn_pair_center <- (sum(layout_widths[1:3]) +
                        sum(layout_widths[4:5]) / 2) / total_w
  mtext("Employment impact (percentage points)", side = 1,
        line = 0.6, outer = TRUE, cex = .84, at = emp_pair_center)
  mtext("Earnings impact (2025$, annualized)", side = 1,
        line = 0.6, outer = TRUE, cex = .84, at = earn_pair_center)

  cat(sprintf("Saved %s (%d rows)\n", filename, n_rows))
}

make_combined_forest()
make_combined_forest(
  sample_idx = which(!is.na(dat$training_role) & dat$training_role == "primary"),
  filename_suffix = "_primary")
cat("\nDone.\n")
