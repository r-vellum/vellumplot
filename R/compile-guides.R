#' @include classes.R compile-layout.R
NULL

# Short wrappers for the vellum primitives used throughout guide drawing. These
# are wrappers (not aliases like `.seg <- vellum::segments_grob`) so the vellum
# functions' bodies — which reference `vctrs::` — are not copied into this
# namespace (which would trip R CMD check's undeclared-import scan).
.vp <- function(...) vellum::viewport(...)
.rect <- function(...) vellum::rect_grob(...)
.seg <- function(...) vellum::segments_grob(...)
.text <- function(...) vellum::text_grob(...)
.np <- function(v) vellum::unit(v, "npc")
.na <- function(v) vellum::unit(v, "native")
.mm <- function(v) vellum::unit(v, "mm")
.g <- function(...) vellum::gpar(...)

PANEL_BG <- "grey92"
GRID_COL <- "white"
LABEL_COL <- "grey20"

# Panel background + gridlines, drawn while the scene is positioned inside the
# panel viewport (so `"native"` resolves against the trained scales).
.draw_panel_bg <- function(scene, scales) {
  scene <- vellum::draw(scene, .rect(gp = .g(fill = PANEL_BG, col = NA)))
  for (b in scales$x$breaks) {
    scene <- vellum::draw(scene, .seg(.na(b), .np(0), .na(b), .np(1),
                                      gp = .g(col = GRID_COL, lwd = 1)))
  }
  for (b in scales$y$breaks) {
    scene <- vellum::draw(scene, .seg(.np(0), .na(b), .np(1), .na(b),
                                      gp = .g(col = GRID_COL, lwd = 1)))
  }
  scene
}

# y-axis labels, right-aligned in the label gutter and aligned to the gridlines
# (the gutter viewport shares the panel's y native scale).
.draw_y_axis <- function(scene, cell, scales) {
  scene <- vellum::push(scene, .vp(row = cell$row, col = cell$col, yscale = scales$y$domain))
  for (i in seq_along(scales$y$breaks)) {
    scene <- vellum::draw(scene, .text(
      scales$y$labels[i], x = .np(0.96), y = .na(scales$y$breaks[i]),
      just = c("right", "centre"), gp = .g(fontsize = .AXIS_FS, col = LABEL_COL)))
  }
  vellum::pop(scene)
}

# x-axis labels, centred under each gridline (gutter shares the x native scale).
.draw_x_axis <- function(scene, cell, scales) {
  scene <- vellum::push(scene, .vp(row = cell$row, col = cell$col, xscale = scales$x$domain))
  for (i in seq_along(scales$x$breaks)) {
    scene <- vellum::draw(scene, .text(
      scales$x$labels[i], x = .na(scales$x$breaks[i]), y = .np(0.82),
      just = c("centre", "top"), gp = .g(fontsize = .AXIS_FS, col = LABEL_COL)))
  }
  vellum::pop(scene)
}

.draw_y_title <- function(scene, cell, scales) {
  scene <- vellum::push(scene, .vp(row = cell$row, col = cell$col))
  scene <- vellum::draw(scene, .text(scales$y$name, rot = 90,
                                     gp = .g(fontsize = .TITLE_FS)))
  vellum::pop(scene)
}

.draw_x_title <- function(scene, cell, scales) {
  scene <- vellum::push(scene, .vp(row = cell$row, col = cell$col))
  scene <- vellum::draw(scene, .text(scales$x$name, gp = .g(fontsize = .TITLE_FS)))
  vellum::pop(scene)
}

# --- legend -----------------------------------------------------------------

.draw_legend <- function(scene, cell, scales) {
  cl <- scales$color
  scene <- vellum::push(scene, .vp(row = cell$row, col = cell$col))
  # title (top, left-aligned)
  scene <- vellum::draw(scene, .text(cl$name, x = .np(0.06), y = .np(0.92),
                                     just = c("left", "centre"),
                                     gp = .g(fontsize = .LEGEND_TITLE_FS)))
  scene <- if (cl$kind == "continuous") {
    .draw_legend_continuous(scene, cl)
  } else {
    .draw_legend_discrete(scene, cl)
  }
  vellum::pop(scene)
}

.draw_legend_continuous <- function(scene, cl) {
  y_lo <- 0.12
  y_hi <- 0.72
  bar_x <- 0.18
  bar_w <- 0.22
  grad <- vellum::linear_gradient(cl$pal256, x1 = 0, y1 = 0, x2 = 0, y2 = 1)
  scene <- vellum::draw(scene, .rect(
    x = .np(bar_x), y = .np((y_lo + y_hi) / 2),
    width = .np(bar_w), height = .np(y_hi - y_lo),
    gp = .g(fill = grad, col = "grey50", lwd = 0.5)))
  for (i in seq_along(cl$legend_breaks)) {
    frac <- scales::rescale(cl$legend_breaks[i], from = cl$range)
    yy <- y_lo + frac * (y_hi - y_lo)
    scene <- vellum::draw(scene, .text(
      cl$legend_labels[i], x = .np(bar_x + bar_w / 2 + 0.06), y = .np(yy),
      just = c("left", "centre"), gp = .g(fontsize = .LEGEND_FS, col = LABEL_COL)))
  }
  scene
}

.draw_legend_discrete <- function(scene, cl) {
  k <- length(cl$levels)
  top <- 0.78
  step <- min(0.12, top / max(k, 1))
  for (i in seq_len(k)) {
    yy <- top - (i - 1) * step
    scene <- vellum::draw(scene, .rect(
      x = .np(0.14), y = .np(yy), width = .mm(.LEGEND_SWATCH_MM), height = .mm(.LEGEND_SWATCH_MM),
      gp = .g(fill = cl$colors[i], col = NA)))
    scene <- vellum::draw(scene, .text(
      cl$levels[i], x = .np(0.30), y = .np(yy), just = c("left", "centre"),
      gp = .g(fontsize = .LEGEND_FS, col = LABEL_COL)))
  }
  scene
}
