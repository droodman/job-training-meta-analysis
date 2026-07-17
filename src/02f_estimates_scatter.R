###############################################################################
# 02f_estimates_scatter.R — Scatter plots of impact vs follow-up time
#
# Reads data/impact_estimates.rds (built by 01b_prep_estimates.R) and produces
# one scatter per outcome (earnings, employment): impact on the y-axis,
# time_point (years since random assignment) on the x-axis, every populated
# (study, time_point) point plotted.
#
# Earnings impacts are annualized (via the cadence column) and inflation-
# adjusted to 2025 $.
#
# Two small program subsets are highlighted (see `highlight_groups` below):
# national programs in shades of red, high-performing sector programs in
# shades of green. Everything else is plotted in a muted grey.
###############################################################################

options(error = function() {
  bar <- strrep("!", 78)
  cat("\n", bar, "\n", sep = "")
  cat("!!! SCRIPT FAILED — ", geterrmessage(), sep = "")
  cat(bar, "\n\n", sep = "")
  if (!interactive()) quit(status = 1, save = "no")
})

library(sysfonts)
library(showtext)

font_paths(c(font_paths(), file.path(Sys.getenv("LOCALAPPDATA"),
                                     "Microsoft/Windows/Fonts")))
# lmroman12 ships no bold-italic variant; the script never uses one.
font_add("LM Roman 12",
         regular = "lmroman12-regular.otf",
         bold    = "lmroman12-bold.otf",
         italic  = "lmroman12-italic.otf")
showtext_auto()

ie <- readRDS("data/impact_estimates.rds")
dir.create("output", showWarnings = FALSE)

# ── Highlight groups ─────────────────────────────────────────────────────────
# Two small program subsets get distinct shades; everything else is plotted in
# the muted base colour. `match` is a predicate over rows of a data frame.
# Per Scholas (Bronx) appears under two projects (WorkAdvance and the Sectoral
# Employment Impact Study) — it is the same program, so it gets one shade.
highlight_groups <- list(
  # National programs — shades of red
  list(label = "WIA Gold Standard",        col = "#a50f15",
       match = function(d) d$project == "WIA Gold Standard"),
  list(label = "National JTPA Study",      col = "#ef3b2c",
       match = function(d) d$project == "National JTPA Study"),
  list(label = "National Job Corps Study", col = "#fb9a99",
       match = function(d) d$project == "National Job Corps Study"),
  # High-performing sector programs — shades of green
  list(label = "PACE – Year Up",      col = "#005a32",
       match = function(d) d$project == "PACE – Year Up"),
  list(label = "Project QUEST",            col = "#41ab5d",
       match = function(d) d$project == "Project QUEST"),
  list(label = "Per Scholas (Bronx)",      col = "#a1d99b",
       match = function(d) grepl("Per Scholas \\(Bronx", d$site_subgroup))
)

# Index of the highlight group each row belongs to (0 = not highlighted).
highlight_index <- function(d) {
  idx <- integer(nrow(d))
  for (i in seq_along(highlight_groups)) {
    idx[highlight_groups[[i]]$match(d)] <- i
  }
  idx
}

# ── Direct labels (replace the legend) ───────────────────────────────────────
# One label per highlighted line, drawn in that line's colour. `place` rules,
# evaluated against the line's plotted points:
#   "right"             — right of the latest (max time_point) dot
#   "above_max"         — above the highest-impact dot
#   "above_x"           — above the dot nearest x = `x` years
# `dy` optionally nudges the label vertically, in character heights.
# WIA / JTPA placements are a first pass — their lines sit in the crowded
# centre, so labels there will likely need hand-tuning.
estimate_labels <- list(
  list(proj = "WIA Gold Standard",        site = "Adults",
       text = "WIA-low income",   place = "right"),
  list(proj = "WIA Gold Standard",        site = "Dislocated Workers",
       text = "WIA-dislocated",   place = "right"),
  list(proj = "National JTPA Study",      site = "Adult men",
       text = "JTPA-men",         place = "right"),
  list(proj = "National JTPA Study",      site = "Adult women",
       text = "JTPA-women",       place = "right", dy = 0.5),
  list(proj = "National JTPA Study",      site = "Male out-of-school youths",
       text = "JTPA-young men",   place = "right"),
  list(proj = "National JTPA Study",      site = "Female out-of-school youths",
       text = "JTPA-young women", place = "right", dy = -0.5),
  list(proj = "National Job Corps Study", site = NULL,
       text = "Job Corps",        place = "above_x", x = 19),
  list(proj = "PACE – Year Up",           site = NULL,
       text = "Year Up",          place = "right"),
  list(proj = "Project QUEST",            site = NULL,
       text = "Project QUEST",    place = "right"),
  list(proj = "WorkAdvance",              site = "Per Scholas (Bronx)",
       text = "Per Scholas",      place = "right"),
  list(proj = "Sectoral Employment Impact Study",
       site = "Per Scholas (Bronx, IT) [SEIS]",
       text = "Per Scholas",      place = "above_max")
)

draw_labels <- function(d) {
  for (L in estimate_labels) {
    sel <- d$project == L$proj
    if (!is.null(L$site)) sel <- sel & d$site_subgroup == L$site
    e <- d[sel, ]
    if (nrow(e) == 0) next
    e <- e[order(e$time_point), ]
    gi <- highlight_index(e)[1]
    col_i <- "black"  # col_i <- if (gi > 0) highlight_groups[[gi]]$col else "black"

    if (L$place == "above_max") {
      i <- which.max(e$impact); pos <- 3
    } else if (L$place == "above_x") {
      i <- which.min(abs(e$time_point - L$x)); pos <- 3
    } else {  # "right"
      i <- nrow(e); pos <- 4
    }
    dx <- if (is.null(L$dx)) 0 else L$dx
    dy <- if (is.null(L$dy)) 0 else L$dy
    char_w <- par("cxy")[1] * 0.85  # character width at the label's cex
    char_h <- par("cxy")[2] * 0.85  # character height at the label's cex
    text(e$time_point[i] + dx * char_w, e$impact[i] + dy * char_h,
         labels = L$text,
         pos = pos, offset = 0.5, col = col_i, font = 2, cex = 0.85,
         xpd = NA)
  }
}

# ymax: optional upper y-axis bound. Points above it are clipped (kept in the
#       k count but drawn off-panel) so the bulk of the data is legible.
make_estimates_scatter <- function(outcome_type, ylab, file_suffix,
                                   ymax = NULL) {
  d <- ie[!is.na(ie$outcome_type) & ie$outcome_type == outcome_type &
            !is.na(ie$impact) & !is.na(ie$time_point) &
            ie$time_point >= 0, ]  # drop pre-randomization estimates
  if (nrow(d) < 3) {
    cat(sprintf("  Skipping %s — too few observations\n", outcome_type))
    return(invisible(NULL))
  }

  ylim <- range(d$impact)
  if (!is.null(ymax)) {
    n_clip <- sum(d$impact > ymax)
    ylim[2] <- ymax
    if (n_clip > 0) {
      cat(sprintf("  %s: %d point(s) above y=%g clipped from view\n",
                  outcome_type, n_clip, ymax))
    }
  }

  # Anchor the x-axis at 0.
  xlim <- c(0, max(d$time_point))

  filename <- sprintf("output/scatter_estimates_%s.png", file_suffix)
  png(filename, width = 10, height = 7, units = "in", res = 150)
  on.exit(dev.off())
  showtext_opts(dpi = 150)
  par(mar = c(3.5, 5.8, 2.5, 1), family = "LM Roman 12", las = 1)

  n_studies <- length(unique(paste(d$project, d$site_subgroup, sep = "|")))
  hi <- highlight_index(d)

  # Axes first, then base points, then highlighted points on top.
  plot(d$time_point, d$impact, type = "n", xlim = xlim, ylim = ylim,
       xlab = "", ylab = "",
       main = sprintf("%s impact vs follow-up time (k=%d, %d experiments)",
                      tools::toTitleCase(outcome_type),
                      nrow(d), n_studies),
       cex.lab = 1.15, cex.axis = 1.1, cex.main = 1.15, bty = "l")
  # y-axis title one line further out than default, to clear the (horizontal)
  # tick labels; x-axis title pulled in closer to its tick labels.
  title(ylab = ylab, line = 4, cex.lab = 1.15)
  title(xlab = "Years since random assignment", line = 2.2, cex.lab = 1.15)
  abline(h = 0, lty = 3, col = "grey50")

  # Filled circles where training is the primary intervention, hollow
  # otherwise (training secondary / incidental / unknown).
  pch_role <- ifelse(!is.na(d$training_role) & d$training_role == "primary",
                     19, 1)

  # Filled base points stay light; hollow ones need a darker, thicker ring
  # or they vanish against the cloud.
  base <- hi == 0
  base_solid  <- base & pch_role == 19
  base_hollow <- base & pch_role == 1
  points(d$time_point[base_solid], d$impact[base_solid], pch = 19, cex = 0.8,
         col = adjustcolor("grey65", alpha.f = 0.40))
  points(d$time_point[base_hollow], d$impact[base_hollow], pch = 1, cex = 0.8,
         col = "grey65", lwd = 1)
  for (i in seq_along(highlight_groups)) {
    sel <- hi == i
    if (!any(sel)) next
    col_i <- highlight_groups[[i]]$col
    ds <- d[sel, ]
    # Connect each experiment's points with a thin line (Per Scholas spans
    # two experiments, so it gets two lines). Lines first, points on top.
    for (k in unique(paste(ds$project, ds$site_subgroup, sep = "|"))) {
      e <- ds[paste(ds$project, ds$site_subgroup, sep = "|") == k, ]
      e <- e[order(e$time_point), ]
      lines(e$time_point, e$impact, col = col_i, lwd = 1)
    }
    points(ds$time_point, ds$impact, pch = pch_role[sel], cex = 1.15,
           col = col_i, lwd = 1.6)
  }

  draw_labels(d)

  # Anchor at the top-right corner, but nudged 2 character widths to the left.
  usr <- par("usr")
  leg <- legend(x = usr[2] - 2 * par("cxy")[1], y = usr[4],
                xjust = 1, yjust = 1,
                legend = c("Training is primary in treatment",
                           "Training is secondary"),
                pch = c(19, 1), col = c("grey60", "grey40"),
                pt.cex = 1.1, pt.lwd = c(1, 1.6),
                bty = "n", cex = 1.0)

  # Two more legend rows: left-to-right gradient swatches spanning the colour
  # range of each program group. legend() can't draw gradients, so the
  # swatches are painted manually, aligned to the rows above. Reds are
  # highlight_groups 1-3, greens 4-6.
  cw <- par("cxy")[1]
  ch <- par("cxy")[2]
  pitch <- leg$text$y[1] - leg$text$y[2]   # row spacing
  txt_x <- leg$text$x[1]
  sw_x0 <- leg$rect$left
  sw_x1 <- txt_x - 0.4 * cw                # swatch fills the marker column
  sw_h  <- 0.65 * ch

  draw_swatch <- function(row, cols, label) {
    ymid <- leg$text$y[2] - row * pitch
    ramp <- colorRampPalette(cols)(60)
    xs   <- seq(sw_x0, sw_x1, length.out = 61)
    for (j in seq_len(60)) {
      rect(xs[j], ymid - sw_h / 2, xs[j + 1], ymid + sw_h / 2,
           col = ramp[j], border = NA, xpd = NA)
    }
    rect(sw_x0, ymid - sw_h / 2, sw_x1, ymid + sw_h / 2,
         border = "grey40", lwd = 0.6, xpd = NA)
    text(txt_x, ymid, label, adj = c(0, 0.5), cex = 1.0, xpd = NA)
  }
  draw_swatch(1, vapply(highlight_groups[1:3], `[[`, "", "col"),
              "Federal programs")
  draw_swatch(2, vapply(highlight_groups[4:6], `[[`, "", "col"),
              "Selected high-impact sector programs")

  cat(sprintf("Saved %s (k=%d points, %d experiments)\n",
              filename, nrow(d), n_studies))
}

make_estimates_scatter("earnings",
                       "Earnings impact (2025$, annualized)",
                       "earn", ymax = 13000)
make_estimates_scatter("employment",
                       "Employment impact (percentage points)",
                       "emp")

cat("\nAll impact-estimates scatter plots written.\n")