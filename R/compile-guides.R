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

# A colour that may be NA ("draw nothing").
.has_col <- function(x) !is.null(x) && !is.na(x)

# Panel background + gridlines, drawn while the scene is positioned inside the
# panel viewport (so `"native"` resolves against the panel's trained scales).
.draw_panel_bg <- function(scene, x_sc, y_sc, thm) {
  if (.has_col(thm$panel_bg)) {
    scene <- vellum::draw(scene, .rect(gp = .g(fill = thm$panel_bg, col = NA)))
  }
  if (.has_col(thm$grid_col)) {
    for (b in x_sc$breaks) {
      scene <- vellum::draw(scene, .seg(.na(b), .np(0), .na(b), .np(1),
                                        gp = .g(col = thm$grid_col, lwd = 1)))
    }
    for (b in y_sc$breaks) {
      scene <- vellum::draw(scene, .seg(.np(0), .na(b), .np(1), .na(b),
                                        gp = .g(col = thm$grid_col, lwd = 1)))
    }
  }
  scene
}

# y-axis labels for `y_sc`, right-aligned in the gutter cell and aligned to the
# gridlines (the gutter viewport shares the panel's y native scale).
.draw_y_axis <- function(scene, row, col, y_sc, thm) {
  scene <- vellum::push(scene, .vp(row = row, col = col, yscale = y_sc$domain))
  for (i in seq_along(y_sc$breaks)) {
    scene <- vellum::draw(scene, .text(
      y_sc$labels[i], x = .np(0.96), y = .na(y_sc$breaks[i]),
      just = c("right", "centre"), gp = .g(fontsize = .AXIS_FS, col = thm$label_col)))
  }
  vellum::pop(scene)
}

# x-axis labels for `x_sc`, centred under each gridline.
.draw_x_axis <- function(scene, row, col, x_sc, thm) {
  scene <- vellum::push(scene, .vp(row = row, col = col, xscale = x_sc$domain))
  for (i in seq_along(x_sc$breaks)) {
    scene <- vellum::draw(scene, .text(
      x_sc$labels[i], x = .na(x_sc$breaks[i]), y = .np(0.82),
      just = c("centre", "top"), gp = .g(fontsize = .AXIS_FS, col = thm$label_col)))
  }
  vellum::pop(scene)
}

# A facet strip: an optional filled background plus a centred label. `rot = 90`
# draws a right-side (row) strip.
.draw_strip <- function(scene, row, col, label, thm, rot = 0, rowspan = 1, colspan = 1) {
  scene <- vellum::push(scene, .vp(row = row, col = col, rowspan = rowspan, colspan = colspan))
  if (.has_col(thm$strip_bg)) {
    scene <- vellum::draw(scene, .rect(gp = .g(fill = thm$strip_bg, col = NA)))
  }
  scene <- vellum::draw(scene, .text(label, rot = rot, gp = .g(fontsize = .STRIP_FS, col = "grey10")))
  vellum::pop(scene)
}

.draw_y_title <- function(scene, row, col, name, rowspan = 1) {
  scene <- vellum::push(scene, .vp(row = row, col = col, rowspan = rowspan))
  scene <- vellum::draw(scene, .text(name, rot = 90, gp = .g(fontsize = .TITLE_FS)))
  vellum::pop(scene)
}

.draw_x_title <- function(scene, row, col, name, colspan = 1) {
  scene <- vellum::push(scene, .vp(row = row, col = col, colspan = colspan))
  scene <- vellum::draw(scene, .text(name, gp = .g(fontsize = .TITLE_FS)))
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
.draw_legends <- function(scene, cell, guides, thm) {
  scene <- vellum::push(scene, .vp(row = cell$row, col = cell$col,
                                   rowspan = cell$rowspan %||% 1))
  n <- length(guides)
  for (i in seq_along(guides)) {
    yc <- 1 - (i - 0.5) / n
    scene <- vellum::push(scene, .vp(y = .np(yc), height = .np(1 / n)))
    g <- guides[[i]]
    scene <- switch(g$kind,
      color_continuous = .guide_color_continuous(scene, g$sc, thm),
      color_discrete = .guide_color_discrete(scene, g$sc, thm),
      size = .guide_size(scene, g$sc, thm)
    )
    scene <- vellum::pop(scene)
  }
  vellum::pop(scene)
}

.guide_title <- function(scene, name) {
  vellum::draw(scene, .text(name, x = .np(0.06), y = .np(0.92),
                            just = c("left", "centre"), gp = .g(fontsize = .LEGEND_TITLE_FS)))
}

.guide_color_continuous <- function(scene, cl, thm) {
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
      just = c("left", "centre"), gp = .g(fontsize = .LEGEND_FS, col = thm$label_col)))
  }
  scene
}

.guide_color_discrete <- function(scene, cl, thm) {
  scene <- .guide_title(scene, cl$name)
  .draw_key_rows(scene, cl$levels, thm, function(scene, yy, i) {
    vellum::draw(scene, .rect(
      x = .np(0.14), y = .np(yy), width = .mm(.LEGEND_SWATCH_MM), height = .mm(.LEGEND_SWATCH_MM),
      gp = .g(fill = cl$colors[i], col = NA)))
  })
}

.guide_size <- function(scene, sc, thm) {
  scene <- .guide_title(scene, sc$name)
  .draw_key_rows(scene, sc$legend_labels, thm, function(scene, yy, i) {
    vellum::draw(scene, .pts(
      .np(0.16), .np(yy), size = .mm(sc$legend_sizes[i]), shape = "circle",
      gp = .g(fill = "grey35", col = "grey35")))
  })
}

# Shared key-row layout for discrete/size legends: a column of `keys` (drawn by
# `draw_key(scene, y, i)`) with labels to the right.
.draw_key_rows <- function(scene, labels, thm, draw_key) {
  k <- length(labels)
  top <- 0.78
  step <- min(0.14, top / max(k, 1))
  for (i in seq_len(k)) {
    yy <- top - (i - 1) * step
    scene <- draw_key(scene, yy, i)
    scene <- vellum::draw(scene, .text(
      labels[i], x = .np(0.32), y = .np(yy), just = c("left", "centre"),
      gp = .g(fontsize = .LEGEND_FS, col = thm$label_col)))
  }
  scene
}
