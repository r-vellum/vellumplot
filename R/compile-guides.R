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
.pts <- function(...) vellum::points_grob(...)
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

# The list of guides a plot needs, in draw order: colour first, then size. Each
# guide is `list(kind, sc)` where `sc` is the trained scale.
.legend_guides <- function(scales) {
  out <- list()
  if (!is.null(scales$color)) {
    out <- c(out, list(list(kind = paste0("color_", scales$color$kind), sc = scales$color)))
  }
  if (!is.null(scales$size)) {
    out <- c(out, list(list(kind = "size", sc = scales$size)))
  }
  out
}

# Stack the guides vertically in the legend cell: each gets an equal slot, drawn
# in its own 0..1 sub-viewport so the per-guide drawers share one coordinate
# frame.
.draw_legends <- function(scene, cell, guides) {
  scene <- vellum::push(scene, .vp(row = cell$row, col = cell$col))
  n <- length(guides)
  for (i in seq_along(guides)) {
    yc <- 1 - (i - 0.5) / n
    scene <- vellum::push(scene, .vp(y = .np(yc), height = .np(1 / n)))
    g <- guides[[i]]
    scene <- switch(g$kind,
      color_continuous = .guide_color_continuous(scene, g$sc),
      color_discrete = .guide_color_discrete(scene, g$sc),
      size = .guide_size(scene, g$sc)
    )
    scene <- vellum::pop(scene)
  }
  vellum::pop(scene)
}

.guide_title <- function(scene, name) {
  vellum::draw(scene, .text(name, x = .np(0.06), y = .np(0.92),
                            just = c("left", "centre"), gp = .g(fontsize = .LEGEND_TITLE_FS)))
}

.guide_color_continuous <- function(scene, cl) {
  scene <- .guide_title(scene, cl$name)
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

.guide_color_discrete <- function(scene, cl) {
  scene <- .guide_title(scene, cl$name)
  .draw_key_rows(scene, cl$levels, function(scene, yy, i) {
    vellum::draw(scene, .rect(
      x = .np(0.14), y = .np(yy), width = .mm(.LEGEND_SWATCH_MM), height = .mm(.LEGEND_SWATCH_MM),
      gp = .g(fill = cl$colors[i], col = NA)))
  })
}

.guide_size <- function(scene, sc) {
  scene <- .guide_title(scene, sc$name)
  .draw_key_rows(scene, sc$legend_labels, function(scene, yy, i) {
    vellum::draw(scene, .pts(
      .np(0.16), .np(yy), size = .mm(sc$legend_sizes[i]), shape = "circle",
      gp = .g(fill = "grey35", col = "grey35")))
  })
}

# Shared key-row layout for discrete/size legends: a column of `keys` (drawn by
# `draw_key(scene, y, i)`) with labels to the right.
.draw_key_rows <- function(scene, labels, draw_key) {
  k <- length(labels)
  top <- 0.78
  step <- min(0.14, top / max(k, 1))
  for (i in seq_len(k)) {
    yy <- top - (i - 1) * step
    scene <- draw_key(scene, yy, i)
    scene <- vellum::draw(scene, .text(
      labels[i], x = .np(0.32), y = .np(yy), just = c("left", "centre"),
      gp = .g(fontsize = .LEGEND_FS, col = LABEL_COL)))
  }
  scene
}
