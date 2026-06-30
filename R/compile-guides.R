#' @include classes.R elements.R theme-tree.R compile-layout.R
NULL

# Minor gridline positions: midpoints between sorted finite breaks, plus one
# extrapolated half-step past each end so minor lines also appear outside the
# outer majors (the panel viewport clips any that fall beyond the scale). Empty
# when there are fewer than two breaks.
.minor_breaks <- function(b) {
  b <- sort(b[is.finite(b)])
  if (length(b) < 2) {
    return(numeric(0))
  }
  mids <- (utils::head(b, -1) + utils::tail(b, -1)) / 2
  step <- b[2] - b[1]
  c(b[1] - step / 2, mids, b[length(b)] + step / 2)
}

# Vertical gridlines at `xs` (native), spanning the panel height. Drawn as one
# batched segments_grob (vellum splits it per element only where gpar differs).
.vlines <- function(scene, xs, gp) {
  if (!length(xs)) {
    return(scene)
  }
  k <- length(xs)
  vellum::draw(
    scene,
    vellum::segments_grob(
      vellum::unit(xs, "native"),
      vellum::unit(rep(0, k), "npc"),
      vellum::unit(xs, "native"),
      vellum::unit(rep(1, k), "npc"),
      gp = gp
    )
  )
}

# Horizontal gridlines at `ys` (native), spanning the panel width.
.hlines <- function(scene, ys, gp) {
  if (!length(ys)) {
    return(scene)
  }
  k <- length(ys)
  vellum::draw(
    scene,
    vellum::segments_grob(
      vellum::unit(rep(0, k), "npc"),
      vellum::unit(ys, "native"),
      vellum::unit(rep(1, k), "npc"),
      vellum::unit(ys, "native"),
      gp = gp
    )
  )
}

# Panel background + gridlines + axis ticks, drawn while the scene is positioned
# inside the panel viewport (so `"native"` resolves against the panel's trained
# scales). `rt` is the resolved theme. Minor gridlines sit under major ones;
# ticks point inward from the panel edges (their length is in mm from the edge,
# which avoids mixed npc/mm unit arithmetic vellum disallows).
.draw_panel_bg <- function(scene, x_sc, y_sc, rt) {
  pb <- rt[["panel.background"]]
  if (!.is_blank(pb)) {
    scene <- vellum::draw(scene, vellum::rect_grob(gp = .el_gpar_rect(pb)))
  }
  mnx <- rt[["panel.grid.minor.x"]]
  if (!.is_blank(mnx)) {
    scene <- .vlines(scene, .minor_breaks(x_sc$breaks), .el_gpar_line(mnx))
  }
  mny <- rt[["panel.grid.minor.y"]]
  if (!.is_blank(mny)) {
    scene <- .hlines(scene, .minor_breaks(y_sc$breaks), .el_gpar_line(mny))
  }
  gx <- rt[["panel.grid.major.x"]]
  if (!.is_blank(gx)) {
    scene <- .vlines(scene, x_sc$breaks, .el_gpar_line(gx))
  }
  gy <- rt[["panel.grid.major.y"]]
  if (!.is_blank(gy)) {
    scene <- .hlines(scene, y_sc$breaks, .el_gpar_line(gy))
  }
  tlen <- rt[["axis.ticks.length"]]
  tx <- rt[["axis.ticks.x"]]
  if (!.is_blank(tx) && length(x_sc$breaks)) {
    b <- x_sc$breaks
    k <- length(b)
    scene <- vellum::draw(
      scene,
      vellum::segments_grob(
        vellum::unit(b, "native"),
        vellum::unit(rep(0, k), "npc"),
        vellum::unit(b, "native"),
        vellum::unit(rep(tlen, k), "mm"),
        gp = .el_gpar_line(tx)
      )
    )
  }
  ty <- rt[["axis.ticks.y"]]
  if (!.is_blank(ty) && length(y_sc$breaks)) {
    b <- y_sc$breaks
    k <- length(b)
    scene <- vellum::draw(
      scene,
      vellum::segments_grob(
        vellum::unit(rep(0, k), "npc"),
        vellum::unit(b, "native"),
        vellum::unit(rep(tlen, k), "mm"),
        vellum::unit(b, "native"),
        gp = .el_gpar_line(ty)
      )
    )
  }
  scene
}

# Points around a circle of radius `r` (closed; native units), for polar
# backgrounds and concentric gridlines.
.circle_pts <- function(r, n = 120L) {
  t <- seq(0, 2 * pi, length.out = n + 1L)
  list(x = r * cos(t), y = r * sin(t))
}

# Polar panel background + gridlines + axis labels, drawn inside the square
# [-1, 1] panel viewport (polar plots have no gutters). The theta aesthetic's
# breaks become radial spokes + rim labels; the radius aesthetic's breaks become
# concentric circles + labels up the top-centre. Theme elements follow the
# underlying aesthetic, so theming flows as in the cartesian case.
.draw_panel_polar <- function(scene, ctx, rt) {
  theta_aes <- ctx$theta_aes
  r_aes <- if (theta_aes == "x") "y" else "x"

  # background disk
  pb <- rt[["panel.background"]]
  if (!.is_blank(pb)) {
    cp <- .circle_pts(ctx$rmax)
    scene <- vellum::draw(
      scene,
      vellum::polygon_grob(
        vellum::unit(cp$x, "native"),
        vellum::unit(cp$y, "native"),
        gp = .el_gpar_rect(pb)
      )
    )
  }

  # concentric circles at the radius breaks
  r_grid <- rt[[paste0("panel.grid.major.", r_aes)]]
  rbreaks <- ctx$r_sc$breaks
  if (!.is_blank(r_grid) && length(rbreaks)) {
    gp <- .el_gpar_line(r_grid)
    for (b in rbreaks) {
      rr <- ctx$r_map(b)
      if (rr <= 0) {
        next
      }
      cp <- .circle_pts(rr)
      scene <- vellum::draw(
        scene,
        vellum::lines_grob(
          vellum::unit(cp$x, "native"),
          vellum::unit(cp$y, "native"),
          gp = gp
        )
      )
    }
  }

  # radial spokes at the theta breaks
  theta_grid <- rt[[paste0("panel.grid.major.", theta_aes)]]
  tbreaks <- ctx$theta_sc$breaks
  if (!.is_blank(theta_grid) && length(tbreaks)) {
    ang <- ctx$theta_map(tbreaks)
    k <- length(ang)
    scene <- vellum::draw(
      scene,
      vellum::segments_grob(
        vellum::unit(rep(0, k), "native"),
        vellum::unit(rep(0, k), "native"),
        vellum::unit(ctx$rmax * cos(ang), "native"),
        vellum::unit(ctx$rmax * sin(ang), "native"),
        gp = .el_gpar_line(theta_grid)
      )
    )
  }

  # angular tick labels just outside the rim
  tel <- rt[[paste0("axis.text.", theta_aes)]]
  if (!.is_blank(tel) && length(tbreaks)) {
    gp <- .el_gpar_text(tel)
    ang <- ctx$theta_map(tbreaks)
    rl <- ctx$rmax + 0.12
    labs <- ctx$theta_sc$labels
    for (i in seq_along(ang)) {
      scene <- vellum::draw(
        scene,
        vellum::text_grob(
          labs[i],
          x = vellum::unit(rl * cos(ang[i]), "native"),
          y = vellum::unit(rl * sin(ang[i]), "native"),
          just = c("centre", "centre"),
          gp = gp
        )
      )
    }
  }

  # radial tick labels up the top-centre spoke
  rel <- rt[[paste0("axis.text.", r_aes)]]
  if (!.is_blank(rel) && length(rbreaks)) {
    gp <- .el_gpar_text(rel)
    labs <- ctx$r_sc$labels
    for (i in seq_along(rbreaks)) {
      rr <- ctx$r_map(rbreaks[i])
      scene <- vellum::draw(
        scene,
        vellum::text_grob(
          labs[i],
          x = vellum::unit(0.02, "native"),
          y = vellum::unit(rr, "native"),
          just = c("left", "centre"),
          gp = gp
        )
      )
    }
  }
  scene
}

# y-axis labels for `y_sc`, right-aligned in the gutter cell and aligned to the
# gridlines (the gutter viewport shares the panel's y native scale).
.draw_y_axis <- function(scene, row, col, y_sc, rt) {
  el <- rt[["axis.text.y"]]
  aline <- rt[["axis.line.y"]]
  if (.is_blank(el) && .is_blank(aline)) {
    return(scene)
  }
  scene <- vellum::push(
    scene,
    vellum::viewport(row = row, col = col, yscale = y_sc$domain)
  )
  # axis line along the panel-adjacent (right) edge of the gutter
  if (!.is_blank(aline)) {
    scene <- vellum::draw(
      scene,
      vellum::segments_grob(
        vellum::unit(1, "npc"),
        vellum::unit(0, "npc"),
        vellum::unit(1, "npc"),
        vellum::unit(1, "npc"),
        gp = .el_gpar_line(aline)
      )
    )
  }
  if (!.is_blank(el)) {
    gp <- .el_gpar_text(el)
    for (i in seq_along(y_sc$breaks)) {
      scene <- vellum::draw(
        scene,
        vellum::text_grob(
          y_sc$labels[i],
          x = vellum::unit(0.96, "npc"),
          y = vellum::unit(y_sc$breaks[i], "native"),
          just = c("right", "centre"),
          gp = gp
        )
      )
    }
  }
  vellum::pop(scene)
}

# x-axis labels for `x_sc`, centred under each gridline.
.draw_x_axis <- function(scene, row, col, x_sc, rt) {
  el <- rt[["axis.text.x"]]
  aline <- rt[["axis.line.x"]]
  if (.is_blank(el) && .is_blank(aline)) {
    return(scene)
  }
  scene <- vellum::push(
    scene,
    vellum::viewport(row = row, col = col, xscale = x_sc$domain)
  )
  # axis line along the panel-adjacent (top) edge of the gutter
  if (!.is_blank(aline)) {
    scene <- vellum::draw(
      scene,
      vellum::segments_grob(
        vellum::unit(0, "npc"),
        vellum::unit(1, "npc"),
        vellum::unit(1, "npc"),
        vellum::unit(1, "npc"),
        gp = .el_gpar_line(aline)
      )
    )
  }
  if (!.is_blank(el)) {
    gp <- .el_gpar_text(el)
    for (i in seq_along(x_sc$breaks)) {
      scene <- vellum::draw(
        scene,
        vellum::text_grob(
          x_sc$labels[i],
          x = vellum::unit(x_sc$breaks[i], "native"),
          y = vellum::unit(0.82, "npc"),
          just = c("centre", "top"),
          gp = gp
        )
      )
    }
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
  rt,
  rot = 0,
  rowspan = 1,
  colspan = 1
) {
  scene <- vellum::push(
    scene,
    vellum::viewport(row = row, col = col, rowspan = rowspan, colspan = colspan)
  )
  sb <- rt[["strip.background"]]
  if (!.is_blank(sb)) {
    scene <- vellum::draw(scene, vellum::rect_grob(gp = .el_gpar_rect(sb)))
  }
  st <- rt[["strip.text"]]
  if (!.is_blank(st)) {
    scene <- vellum::draw(
      scene,
      vellum::text_grob(label, rot = rot, gp = .el_gpar_text(st))
    )
  }
  vellum::pop(scene)
}

.draw_y_title <- function(scene, row, col, name, rt, rowspan = 1) {
  el <- rt[["axis.title.y"]]
  if (.is_blank(el)) {
    return(scene)
  }
  scene <- vellum::push(
    scene,
    vellum::viewport(row = row, col = col, rowspan = rowspan)
  )
  scene <- vellum::draw(
    scene,
    vellum::text_grob(name, rot = 90, gp = .el_gpar_text(el))
  )
  vellum::pop(scene)
}

.draw_x_title <- function(scene, row, col, name, rt, colspan = 1) {
  el <- rt[["axis.title.x"]]
  if (.is_blank(el)) {
    return(scene)
  }
  scene <- vellum::push(
    scene,
    vellum::viewport(row = row, col = col, colspan = colspan)
  )
  scene <- vellum::draw(scene, vellum::text_grob(name, gp = .el_gpar_text(el)))
  vellum::pop(scene)
}

# --- plot title / subtitle / caption / tag bands ----------------------------
# Each spans the full page width (col = 1, colspan = ncol). `default_hjust`
# reproduces the ggplot2 default placement (title/subtitle/tag flush left,
# caption flush right); a theme `hjust` overrides it.
.draw_band <- function(scene, row, ncol, text, el, default_hjust) {
  if (.is_blank(el)) {
    return(scene)
  }
  hj <- el@hjust %||% default_hjust
  x_npc <- 0.01 + hj * 0.98
  just_h <- if (hj < 0.25) {
    "left"
  } else if (hj > 0.75) {
    "right"
  } else {
    "centre"
  }
  scene <- vellum::push(
    scene,
    vellum::viewport(row = row, col = 1, colspan = ncol)
  )
  scene <- vellum::draw(
    scene,
    vellum::text_grob(
      text,
      x = vellum::unit(x_npc, "npc"),
      just = c(just_h, "centre"),
      rot = .el_rot(el),
      gp = .el_gpar_text(el)
    )
  )
  vellum::pop(scene)
}

.draw_title <- function(scene, row, ncol, text, rt) {
  .draw_band(scene, row, ncol, text, rt[["plot.title"]], 0)
}

.draw_subtitle <- function(scene, row, ncol, text, rt) {
  .draw_band(scene, row, ncol, text, rt[["plot.subtitle"]], 0)
}

.draw_caption <- function(scene, row, ncol, text, rt) {
  .draw_band(scene, row, ncol, text, rt[["plot.caption"]], 1)
}

.draw_tag <- function(scene, row, ncol, text, rt) {
  .draw_band(scene, row, ncol, text, rt[["plot.tag"]], 0)
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
  if (!is.null(scales$shape)) {
    out <- c(out, list(list(kind = "shape", sc = scales$shape)))
  }
  out
}

# Lay the guides out in the legend cell, each in its own 0..1 sub-viewport so the
# per-guide drawers share one coordinate frame. A vertical legend stacks the
# guides top-to-bottom (each an equal-height slot, vertical key drawers); a
# horizontal legend spreads them left-to-right (each an equal-width slot,
# horizontal key drawers).
.draw_legends <- function(scene, cell, guides, rt, orient = "vertical") {
  scene <- vellum::push(
    scene,
    vellum::viewport(
      row = cell$row,
      col = cell$col,
      rowspan = cell$rowspan %||% 1,
      colspan = cell$colspan %||% 1
    )
  )
  lb <- rt[["legend.background"]]
  if (!.is_blank(lb)) {
    scene <- vellum::draw(scene, vellum::rect_grob(gp = .el_gpar_rect(lb)))
  }
  n <- length(guides)
  for (i in seq_along(guides)) {
    g <- guides[[i]]
    if (orient == "horizontal") {
      scene <- vellum::push(
        scene,
        vellum::viewport(
          x = vellum::unit((i - 0.5) / n, "npc"),
          width = vellum::unit(1 / n, "npc")
        )
      )
      scene <- switch(
        g$kind,
        color_continuous = .guide_color_continuous_h(scene, g$sc, rt),
        color_discrete = .guide_color_discrete_h(scene, g$sc, rt),
        size = .guide_size_h(scene, g$sc, rt),
        shape = .guide_shape_h(scene, g$sc, rt)
      )
    } else {
      scene <- vellum::push(
        scene,
        vellum::viewport(
          y = vellum::unit(1 - (i - 0.5) / n, "npc"),
          height = vellum::unit(1 / n, "npc")
        )
      )
      scene <- switch(
        g$kind,
        color_continuous = .guide_color_continuous(scene, g$sc, rt),
        color_discrete = .guide_color_discrete(scene, g$sc, rt),
        size = .guide_size(scene, g$sc, rt),
        shape = .guide_shape(scene, g$sc, rt)
      )
    }
    scene <- vellum::pop(scene)
  }
  vellum::pop(scene)
}

.guide_title <- function(scene, name, rt) {
  el <- rt[["legend.title"]]
  if (.is_blank(el)) {
    return(scene)
  }
  vellum::draw(
    scene,
    vellum::text_grob(
      name,
      x = vellum::unit(0.06, "npc"),
      y = vellum::unit(0.92, "npc"),
      just = c("left", "centre"),
      gp = .el_gpar_text(el)
    )
  )
}

.guide_color_continuous <- function(scene, cl, rt) {
  scene <- .guide_title(scene, cl$name, rt)
  txt <- .el_gpar_text(rt[["legend.text"]])
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
        gp = txt
      )
    )
  }
  scene
}

.guide_color_discrete <- function(scene, cl, rt) {
  scene <- .guide_title(scene, cl$name, rt)
  .draw_key_rows(scene, cl$labels %||% cl$levels, rt, function(scene, yy, i) {
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

.guide_size <- function(scene, sc, rt) {
  scene <- .guide_title(scene, sc$name, rt)
  .draw_key_rows(scene, sc$legend_labels, rt, function(scene, yy, i) {
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

.guide_shape <- function(scene, sc, rt) {
  scene <- .guide_title(scene, sc$name, rt)
  .draw_key_rows(scene, sc$levels, rt, function(scene, yy, i) {
    vellum::draw(
      scene,
      vellum::points_grob(
        vellum::unit(0.16, "npc"),
        vellum::unit(yy, "npc"),
        size = vellum::unit(3, "mm"),
        shape = sc$shapes[i],
        gp = vellum::gpar(fill = "grey35", col = "grey35")
      )
    )
  })
}

# --- horizontal guide drawers (top/bottom legend) ---------------------------
# Each draws inside a guide's full-height slot: the title on the upper-left, a
# single row of keys/labels below it flowing left-to-right.

.guide_title_h <- function(scene, name, rt) {
  el <- rt[["legend.title"]]
  if (.is_blank(el)) {
    return(scene)
  }
  vellum::draw(
    scene,
    vellum::text_grob(
      name,
      x = vellum::unit(0.04, "npc"),
      y = vellum::unit(0.8, "npc"),
      just = c("left", "centre"),
      gp = .el_gpar_text(el)
    )
  )
}

.guide_color_continuous_h <- function(scene, cl, rt) {
  scene <- .guide_title_h(scene, cl$name, rt)
  txt <- .el_gpar_text(rt[["legend.text"]])
  x_lo <- 0.1
  x_hi <- 0.7
  bar_y <- 0.42
  bar_h <- 0.24
  grad <- vellum::linear_gradient(cl$pal256, x1 = 0, y1 = 0, x2 = 1, y2 = 0)
  scene <- vellum::draw(
    scene,
    vellum::rect_grob(
      x = vellum::unit((x_lo + x_hi) / 2, "npc"),
      y = vellum::unit(bar_y, "npc"),
      width = vellum::unit(x_hi - x_lo, "npc"),
      height = vellum::unit(bar_h, "npc"),
      gp = vellum::gpar(fill = grad, col = "grey50", lwd = 0.5)
    )
  )
  for (i in seq_along(cl$legend_breaks)) {
    frac <- scales::rescale(cl$legend_breaks[i], from = cl$range)
    xx <- x_lo + frac * (x_hi - x_lo)
    scene <- vellum::draw(
      scene,
      vellum::text_grob(
        cl$legend_labels[i],
        x = vellum::unit(xx, "npc"),
        y = vellum::unit(bar_y - bar_h / 2 - 0.12, "npc"),
        just = c("centre", "top"),
        gp = txt
      )
    )
  }
  scene
}

.guide_color_discrete_h <- function(scene, cl, rt) {
  scene <- .guide_title_h(scene, cl$name, rt)
  .draw_key_row_h(scene, cl$labels %||% cl$levels, rt, function(scene, x, y, i) {
    vellum::draw(
      scene,
      vellum::rect_grob(
        x = vellum::unit(x, "npc"),
        y = vellum::unit(y, "npc"),
        width = vellum::unit(.LEGEND_SWATCH_MM, "mm"),
        height = vellum::unit(.LEGEND_SWATCH_MM, "mm"),
        gp = vellum::gpar(fill = cl$colors[i], col = NA)
      )
    )
  })
}

.guide_size_h <- function(scene, sc, rt) {
  scene <- .guide_title_h(scene, sc$name, rt)
  .draw_key_row_h(scene, sc$legend_labels, rt, function(scene, x, y, i) {
    vellum::draw(
      scene,
      vellum::points_grob(
        vellum::unit(x, "npc"),
        vellum::unit(y, "npc"),
        size = vellum::unit(sc$legend_sizes[i], "mm"),
        shape = "circle",
        gp = vellum::gpar(fill = "grey35", col = "grey35")
      )
    )
  })
}

.guide_shape_h <- function(scene, sc, rt) {
  scene <- .guide_title_h(scene, sc$name, rt)
  .draw_key_row_h(scene, sc$levels, rt, function(scene, x, y, i) {
    vellum::draw(
      scene,
      vellum::points_grob(
        vellum::unit(x, "npc"),
        vellum::unit(y, "npc"),
        size = vellum::unit(3, "mm"),
        shape = sc$shapes[i],
        gp = vellum::gpar(fill = "grey35", col = "grey35")
      )
    )
  })
}

# Shared horizontal key layout: `keys` (drawn by `draw_key(scene, x, y, i)`) in a
# row, each followed by its label, split into equal-width cells across the slot.
.draw_key_row_h <- function(scene, labels, rt, draw_key) {
  txt <- .el_gpar_text(rt[["legend.text"]])
  key_bg <- rt[["legend.key"]]
  key_side <- .LEGEND_SWATCH_MM + .PAD_MM
  k <- length(labels)
  y_key <- 0.32
  cellw <- 1 / k
  for (i in seq_len(k)) {
    xk <- (i - 1) * cellw + 0.06
    if (!.is_blank(key_bg)) {
      scene <- vellum::draw(
        scene,
        vellum::rect_grob(
          x = vellum::unit(xk, "npc"),
          y = vellum::unit(y_key, "npc"),
          width = vellum::unit(key_side, "mm"),
          height = vellum::unit(key_side, "mm"),
          gp = .el_gpar_rect(key_bg)
        )
      )
    }
    scene <- draw_key(scene, xk, y_key, i)
    scene <- vellum::draw(
      scene,
      vellum::text_grob(
        labels[i],
        x = vellum::unit(xk + 0.08, "npc"),
        y = vellum::unit(y_key, "npc"),
        just = c("left", "centre"),
        gp = txt
      )
    )
  }
  scene
}

# Shared key-row layout for discrete/size legends: a column of `keys` (drawn by
# `draw_key(scene, y, i)`) with labels to the right. Each key sits on an optional
# `legend.key` background (blank by default, so nothing is drawn).
.draw_key_rows <- function(scene, labels, rt, draw_key) {
  txt <- .el_gpar_text(rt[["legend.text"]])
  key_bg <- rt[["legend.key"]]
  key_side <- .LEGEND_SWATCH_MM + .PAD_MM
  k <- length(labels)
  top <- 0.78
  step <- min(0.14, top / max(k, 1))
  for (i in seq_len(k)) {
    yy <- top - (i - 1) * step
    if (!.is_blank(key_bg)) {
      scene <- vellum::draw(
        scene,
        vellum::rect_grob(
          x = vellum::unit(0.16, "npc"),
          y = vellum::unit(yy, "npc"),
          width = vellum::unit(key_side, "mm"),
          height = vellum::unit(key_side, "mm"),
          gp = .el_gpar_rect(key_bg)
        )
      )
    }
    scene <- draw_key(scene, yy, i)
    scene <- vellum::draw(
      scene,
      vellum::text_grob(
        labels[i],
        x = vellum::unit(0.32, "npc"),
        y = vellum::unit(yy, "npc"),
        just = c("left", "centre"),
        gp = txt
      )
    )
  }
  scene
}
