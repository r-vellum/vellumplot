#' @include classes.R compile-layout.R
NULL

# A colour that may be NA ("draw nothing").
.has_col <- function(x) !is.null(x) && !is.na(x)

# Panel background + gridlines, drawn while the scene is positioned inside the
# panel viewport (so `"native"` resolves against the panel's trained scales).
.draw_panel_bg <- function(scene, x_sc, y_sc, thm) {
  if (.has_col(thm$panel_bg)) {
    scene <- vellum::draw(
      scene,
      vellum::rect_grob(gp = vellum::gpar(fill = thm$panel_bg, col = NA))
    )
  }
  if (.has_col(thm$grid_col)) {
    for (b in x_sc$breaks) {
      scene <- vellum::draw(
        scene,
        vellum::segments_grob(
          vellum::unit(b, "native"),
          vellum::unit(0, "npc"),
          vellum::unit(b, "native"),
          vellum::unit(1, "npc"),
          gp = vellum::gpar(col = thm$grid_col, lwd = 1)
        )
      )
    }
    for (b in y_sc$breaks) {
      scene <- vellum::draw(
        scene,
        vellum::segments_grob(
          vellum::unit(0, "npc"),
          vellum::unit(b, "native"),
          vellum::unit(1, "npc"),
          vellum::unit(b, "native"),
          gp = vellum::gpar(col = thm$grid_col, lwd = 1)
        )
      )
    }
  }
  scene
}

# y-axis labels for `y_sc`, right-aligned in the gutter cell and aligned to the
# gridlines (the gutter viewport shares the panel's y native scale).
.draw_y_axis <- function(scene, row, col, y_sc, thm) {
  scene <- vellum::push(
    scene,
    vellum::viewport(row = row, col = col, yscale = y_sc$domain)
  )
  for (i in seq_along(y_sc$breaks)) {
    scene <- vellum::draw(
      scene,
      vellum::text_grob(
        y_sc$labels[i],
        x = vellum::unit(0.96, "npc"),
        y = vellum::unit(y_sc$breaks[i], "native"),
        just = c("right", "centre"),
        gp = vellum::gpar(fontsize = .AXIS_FS, col = thm$label_col)
      )
    )
  }
  vellum::pop(scene)
}

# x-axis labels for `x_sc`, centred under each gridline.
.draw_x_axis <- function(scene, row, col, x_sc, thm) {
  scene <- vellum::push(
    scene,
    vellum::viewport(row = row, col = col, xscale = x_sc$domain)
  )
  for (i in seq_along(x_sc$breaks)) {
    scene <- vellum::draw(
      scene,
      vellum::text_grob(
        x_sc$labels[i],
        x = vellum::unit(x_sc$breaks[i], "native"),
        y = vellum::unit(0.82, "npc"),
        just = c("centre", "top"),
        gp = vellum::gpar(fontsize = .AXIS_FS, col = thm$label_col)
      )
    )
  }
  vellum::pop(scene)
}

# A facet strip: an optional filled background plus a centred label. `rot = 90`
# draws a right-side (row) strip.
.draw_strip <- function(
  scene,
  row,
  col,
  label,
  thm,
  rot = 0,
  rowspan = 1,
  colspan = 1
) {
  scene <- vellum::push(
    scene,
    vellum::viewport(row = row, col = col, rowspan = rowspan, colspan = colspan)
  )
  if (.has_col(thm$strip_bg)) {
    scene <- vellum::draw(
      scene,
      vellum::rect_grob(gp = vellum::gpar(fill = thm$strip_bg, col = NA))
    )
  }
  scene <- vellum::draw(
    scene,
    vellum::text_grob(
      label,
      rot = rot,
      gp = vellum::gpar(fontsize = .STRIP_FS, col = "grey10")
    )
  )
  vellum::pop(scene)
}

.draw_y_title <- function(scene, row, col, name, rowspan = 1) {
  scene <- vellum::push(
    scene,
    vellum::viewport(row = row, col = col, rowspan = rowspan)
  )
  scene <- vellum::draw(
    scene,
    vellum::text_grob(name, rot = 90, gp = vellum::gpar(fontsize = .TITLE_FS))
  )
  vellum::pop(scene)
}

.draw_x_title <- function(scene, row, col, name, colspan = 1) {
  scene <- vellum::push(
    scene,
    vellum::viewport(row = row, col = col, colspan = colspan)
  )
  scene <- vellum::draw(
    scene,
    vellum::text_grob(name, gp = vellum::gpar(fontsize = .TITLE_FS))
  )
  vellum::pop(scene)
}

# --- plot title / subtitle / caption / tag bands ----------------------------
# Each spans the full page width (col = 1, colspan = ncol). Justification mirrors
# ggplot2: title/subtitle flush left, caption flush right, tag in the top-left
# corner of the title band.
.draw_title <- function(scene, row, ncol, text) {
  scene <- vellum::push(
    scene,
    vellum::viewport(row = row, col = 1, colspan = ncol)
  )
  scene <- vellum::draw(
    scene,
    vellum::text_grob(
      text,
      x = vellum::unit(0.01, "npc"),
      just = c("left", "centre"),
      gp = vellum::gpar(fontsize = .PLOT_TITLE_FS, col = "grey10")
    )
  )
  vellum::pop(scene)
}

.draw_subtitle <- function(scene, row, ncol, text) {
  scene <- vellum::push(
    scene,
    vellum::viewport(row = row, col = 1, colspan = ncol)
  )
  scene <- vellum::draw(
    scene,
    vellum::text_grob(
      text,
      x = vellum::unit(0.01, "npc"),
      just = c("left", "centre"),
      gp = vellum::gpar(fontsize = .PLOT_SUBTITLE_FS, col = "grey30")
    )
  )
  vellum::pop(scene)
}

.draw_caption <- function(scene, row, ncol, text) {
  scene <- vellum::push(
    scene,
    vellum::viewport(row = row, col = 1, colspan = ncol)
  )
  scene <- vellum::draw(
    scene,
    vellum::text_grob(
      text,
      x = vellum::unit(0.99, "npc"),
      just = c("right", "centre"),
      gp = vellum::gpar(fontsize = .PLOT_CAPTION_FS, col = "grey30")
    )
  )
  vellum::pop(scene)
}

.draw_tag <- function(scene, row, ncol, text) {
  scene <- vellum::push(
    scene,
    vellum::viewport(row = row, col = 1, colspan = ncol)
  )
  scene <- vellum::draw(
    scene,
    vellum::text_grob(
      text,
      x = vellum::unit(0.01, "npc"),
      just = c("left", "centre"),
      gp = vellum::gpar(fontsize = .PLOT_TAG_FS, col = "grey10")
    )
  )
  vellum::pop(scene)
}

# --- legend -----------------------------------------------------------------

# The list of guides a plot needs, in draw order: colour first, then size. Each
# guide is `list(kind, sc)` where `sc` is the trained scale.
.legend_guides <- function(scales) {
  out <- list()
  if (!is.null(scales$color)) {
    out <- c(
      out,
      list(list(kind = paste0("color_", scales$color$kind), sc = scales$color))
    )
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
  scene <- vellum::push(
    scene,
    vellum::viewport(
      row = cell$row,
      col = cell$col,
      rowspan = cell$rowspan %||% 1
    )
  )
  n <- length(guides)
  for (i in seq_along(guides)) {
    yc <- 1 - (i - 0.5) / n
    scene <- vellum::push(
      scene,
      vellum::viewport(
        y = vellum::unit(yc, "npc"),
        height = vellum::unit(1 / n, "npc")
      )
    )
    g <- guides[[i]]
    scene <- switch(
      g$kind,
      color_continuous = .guide_color_continuous(scene, g$sc, thm),
      color_discrete = .guide_color_discrete(scene, g$sc, thm),
      size = .guide_size(scene, g$sc, thm)
    )
    scene <- vellum::pop(scene)
  }
  vellum::pop(scene)
}

.guide_title <- function(scene, name) {
  vellum::draw(
    scene,
    vellum::text_grob(
      name,
      x = vellum::unit(0.06, "npc"),
      y = vellum::unit(0.92, "npc"),
      just = c("left", "centre"),
      gp = vellum::gpar(fontsize = .LEGEND_TITLE_FS)
    )
  )
}

.guide_color_continuous <- function(scene, cl, thm) {
  scene <- .guide_title(scene, cl$name)
  y_lo <- 0.12
  y_hi <- 0.72
  bar_x <- 0.18
  bar_w <- 0.22
  grad <- vellum::linear_gradient(cl$pal256, x1 = 0, y1 = 0, x2 = 0, y2 = 1)
  scene <- vellum::draw(
    scene,
    vellum::rect_grob(
      x = vellum::unit(bar_x, "npc"),
      y = vellum::unit((y_lo + y_hi) / 2, "npc"),
      width = vellum::unit(bar_w, "npc"),
      height = vellum::unit(y_hi - y_lo, "npc"),
      gp = vellum::gpar(fill = grad, col = "grey50", lwd = 0.5)
    )
  )
  for (i in seq_along(cl$legend_breaks)) {
    frac <- scales::rescale(cl$legend_breaks[i], from = cl$range)
    yy <- y_lo + frac * (y_hi - y_lo)
    scene <- vellum::draw(
      scene,
      vellum::text_grob(
        cl$legend_labels[i],
        x = vellum::unit(bar_x + bar_w / 2 + 0.06, "npc"),
        y = vellum::unit(yy, "npc"),
        just = c("left", "centre"),
        gp = vellum::gpar(fontsize = .LEGEND_FS, col = thm$label_col)
      )
    )
  }
  scene
}

.guide_color_discrete <- function(scene, cl, thm) {
  scene <- .guide_title(scene, cl$name)
  .draw_key_rows(scene, cl$levels, thm, function(scene, yy, i) {
    vellum::draw(
      scene,
      vellum::rect_grob(
        x = vellum::unit(0.14, "npc"),
        y = vellum::unit(yy, "npc"),
        width = vellum::unit(.LEGEND_SWATCH_MM, "mm"),
        height = vellum::unit(.LEGEND_SWATCH_MM, "mm"),
        gp = vellum::gpar(fill = cl$colors[i], col = NA)
      )
    )
  })
}

.guide_size <- function(scene, sc, thm) {
  scene <- .guide_title(scene, sc$name)
  .draw_key_rows(scene, sc$legend_labels, thm, function(scene, yy, i) {
    vellum::draw(
      scene,
      vellum::points_grob(
        vellum::unit(0.16, "npc"),
        vellum::unit(yy, "npc"),
        size = vellum::unit(sc$legend_sizes[i], "mm"),
        shape = "circle",
        gp = vellum::gpar(fill = "grey35", col = "grey35")
      )
    )
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
    scene <- vellum::draw(
      scene,
      vellum::text_grob(
        labels[i],
        x = vellum::unit(0.32, "npc"),
        y = vellum::unit(yy, "npc"),
        just = c("left", "centre"),
        gp = vellum::gpar(fontsize = .LEGEND_FS, col = thm$label_col)
      )
    )
  }
  scene
}
