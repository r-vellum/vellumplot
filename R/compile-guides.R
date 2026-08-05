#' @include classes.R elements.R theme-tree.R compile-layout.R
NULL

# Tabular-figures OpenType feature, for axis tick labels (constant digit width).
.TNUM <- c(tnum = 1L)

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
  n <- length(b)
  # Extrapolate half of the *local* gap at each end; using the first gap for both
  # misplaces the top minor line when breaks are unevenly spaced.
  lo_gap <- b[2] - b[1]
  hi_gap <- b[n] - b[n - 1]
  c(b[1] - lo_gap / 2, mids, b[n] + hi_gap / 2)
}

# Vertical gridlines at `xs` (native), spanning the panel height. Drawn as one
# batched segments_grob (vellum splits it per element only where gpar differs).
.vlines <- function(scene, xs, gp, sketch = NULL) {
  if (!length(xs)) {
    return(scene)
  }
  k <- length(xs)
  g <- vellum::segments_grob(
    vellum::vl_unit(xs, "native"),
    vellum::vl_unit(rep(0, k), "npc"),
    vellum::vl_unit(xs, "native"),
    vellum::vl_unit(rep(1, k), "npc"),
    sketch = sketch,
    gp = gp
  )
  g@role <- "grid" # so an interactive host can hide + re-draw gridlines on zoom
  vellum::draw(scene, g)
}

# Horizontal gridlines at `ys` (native), spanning the panel width.
.hlines <- function(scene, ys, gp, sketch = NULL) {
  if (!length(ys)) {
    return(scene)
  }
  k <- length(ys)
  g <- vellum::segments_grob(
    vellum::vl_unit(rep(0, k), "npc"),
    vellum::vl_unit(ys, "native"),
    vellum::vl_unit(rep(1, k), "npc"),
    vellum::vl_unit(ys, "native"),
    sketch = sketch,
    gp = gp
  )
  g@role <- "grid" # so an interactive host can hide + re-draw gridlines on zoom
  vellum::draw(scene, g)
}

# Panel background + gridlines + axis ticks, drawn while the scene is positioned
# inside the panel viewport (so `"native"` resolves against the panel's trained
# scales). `rt` is the resolved theme. Minor gridlines sit under major ones;
# ticks point inward from the panel edges (their length is in mm from the edge,
# which avoids mixed npc/mm unit arithmetic vellum disallows).
.draw_panel_bg <- function(scene, x_sc, y_sc, rt) {
  # Panel furniture is decorative in a tagged PDF: ticks below carry `role =
  # "presentation"` (a screen-reader artifact), and gridlines carry `role =
  # "grid"` (which the engine also treats as decorative, so the interactive-host
  # selector survives). The panel *background* is deliberately left un-roled: it
  # is the grob that carries the panel viewport's clip, and tagging it moves the
  # SVG clip off the panel group. It is a single rect, so the cost of a screen
  # reader naming it once is negligible next to thousands of gridlines/ticks.
  pb <- rt[["panel.background"]]
  if (!.is_blank(pb)) {
    scene <- vellum::draw(
      scene,
      vellum::rect_grob(gp = .el_gpar_rect(pb), sketch = .el_sketch(pb, 1L))
    )
  }
  # Major gridlines and ticks sit at the finite breaks; a non-finite break (e.g. a
  # degenerate single-value domain) would anchor a line/tick at a bad coordinate.
  xb <- x_sc$breaks[is.finite(x_sc$breaks)]
  yb <- y_sc$breaks[is.finite(y_sc$breaks)]
  mnx <- rt[["panel.grid.minor.x"]]
  # A discrete position scale has no minor gridlines: its breaks sit at category
  # centres, so `.minor_breaks()` would draw lines *between and outside* the
  # categories (ggplot2 draws none).
  if (!.is_blank(mnx) && !isTRUE(x_sc$discrete)) {
    scene <- .vlines(
      scene,
      .minor_breaks(x_sc$breaks),
      .el_gpar_line(mnx),
      .el_sketch(mnx, 2L)
    )
  }
  mny <- rt[["panel.grid.minor.y"]]
  if (!.is_blank(mny) && !isTRUE(y_sc$discrete)) {
    scene <- .hlines(
      scene,
      .minor_breaks(y_sc$breaks),
      .el_gpar_line(mny),
      .el_sketch(mny, 3L)
    )
  }
  gx <- rt[["panel.grid.major.x"]]
  if (!.is_blank(gx)) {
    scene <- .vlines(scene, xb, .el_gpar_line(gx), .el_sketch(gx, 4L))
  }
  gy <- rt[["panel.grid.major.y"]]
  if (!.is_blank(gy)) {
    scene <- .hlines(scene, yb, .el_gpar_line(gy), .el_sketch(gy, 5L))
  }
  tlen <- rt[["axis.ticks.length"]]
  tx <- rt[["axis.ticks.x"]]
  if (!.is_blank(tx) && length(xb)) {
    b <- xb
    k <- length(b)
    scene <- vellum::draw(
      scene,
      vellum::segments_grob(
        vellum::vl_unit(b, "native"),
        vellum::vl_unit(rep(0, k), "npc"),
        vellum::vl_unit(b, "native"),
        vellum::vl_unit(rep(tlen, k), "mm"),
        sketch = .el_sketch(tx, 6L),
        gp = .el_gpar_line(tx),
        role = "presentation"
      )
    )
  }
  ty <- rt[["axis.ticks.y"]]
  if (!.is_blank(ty) && length(yb)) {
    b <- yb
    k <- length(b)
    scene <- vellum::draw(
      scene,
      vellum::segments_grob(
        vellum::vl_unit(rep(0, k), "npc"),
        vellum::vl_unit(b, "native"),
        vellum::vl_unit(rep(tlen, k), "mm"),
        vellum::vl_unit(b, "native"),
        sketch = .el_sketch(ty, 7L),
        gp = .el_gpar_line(ty),
        role = "presentation"
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

  # background disk (an annulus for a donut, so the hole stays empty rather than
  # filled with panel colour and crossed by spokes)
  pb <- rt[["panel.background"]]
  if (!.is_blank(pb)) {
    co <- .circle_pts(ctx$rmax)
    if (ctx$rmin > 0) {
      ci <- .circle_pts(ctx$rmin)
      bx <- c(co$x, NA, rev(ci$x))
      by <- c(co$y, NA, rev(ci$y))
    } else {
      bx <- co$x
      by <- co$y
    }
    scene <- vellum::draw(
      scene,
      vellum::polygon_grob(
        vellum::vl_unit(bx, "native"),
        vellum::vl_unit(by, "native"),
        sketch = .el_sketch(pb, 1L),
        gp = .el_gpar_rect(pb)
      )
    )
  }

  # concentric circles at the radius breaks
  r_grid <- rt[[paste0("panel.grid.major.", r_aes)]]
  rbreaks <- ctx$r_sc$breaks
  if (!.is_blank(r_grid) && length(rbreaks)) {
    gp <- .el_gpar_line(r_grid)
    rsk <- .el_sketch(r_grid, 5L)
    for (b in rbreaks) {
      rr <- ctx$r_map(b)
      if (rr <= 0) {
        next
      }
      cp <- .circle_pts(rr)
      scene <- vellum::draw(
        scene,
        vellum::lines_grob(
          vellum::vl_unit(cp$x, "native"),
          vellum::vl_unit(cp$y, "native"),
          sketch = rsk,
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
    # Spokes run from the inner radius to the rim, so a donut's hole is not
    # crossed by lines converging at the centre.
    scene <- vellum::draw(
      scene,
      vellum::segments_grob(
        vellum::vl_unit(ctx$rmin * cos(ang), "native"),
        vellum::vl_unit(ctx$rmin * sin(ang), "native"),
        vellum::vl_unit(ctx$rmax * cos(ang), "native"),
        vellum::vl_unit(ctx$rmax * sin(ang), "native"),
        sketch = .el_sketch(theta_grid, 4L),
        gp = .el_gpar_line(theta_grid)
      )
    )
  }

  # angular tick labels just outside the rim
  tel <- rt[[paste0("axis.text.", theta_aes)]]
  if (!.is_blank(tel) && length(tbreaks)) {
    gp <- .el_gpar_text(tel, features = .TNUM)
    ang <- ctx$theta_map(tbreaks)
    rl <- ctx$rmax + 0.12
    labs <- ctx$theta_sc$labels
    for (i in seq_along(ang)) {
      scene <- vellum::draw(
        scene,
        vellum::text_grob(
          labs[i],
          x = vellum::vl_unit(rl * cos(ang[i]), "native"),
          y = vellum::vl_unit(rl * sin(ang[i]), "native"),
          just = c("centre", "centre"),
          gp = gp
        )
      )
    }
  }

  # radial tick labels up the top-centre spoke
  rel <- rt[[paste0("axis.text.", r_aes)]]
  if (!.is_blank(rel) && length(rbreaks)) {
    gp <- .el_gpar_text(rel, features = .TNUM)
    labs <- ctx$r_sc$labels
    for (i in seq_along(rbreaks)) {
      rr <- ctx$r_map(rbreaks[i])
      scene <- vellum::draw(
        scene,
        vellum::text_grob(
          labs[i],
          x = vellum::vl_unit(0.02, "native"),
          y = vellum::vl_unit(rr, "native"),
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
# One axis-label track. `axis` is "x"/"y"; `sec = TRUE` is the secondary
# (opposite-gutter) variant. The four former .draw_{x,y}_axis[_sec] drawers were
# byte-identical but for the small geometry table below (viewport name + scale,
# the axis-line segment endpoints and its sketch seed, and the label anchor +
# justification), so they are now thin wrappers over this.
# hjust/vjust (0..1) bucketed to a justification word, matching `.band_anchor`.
.just_h <- function(h) {
  if (h < 0.25) {
    "left"
  } else if (h > 0.75) {
    "right"
  } else {
    "centre"
  }
}
.just_v <- function(v) {
  if (v < 0.25) {
    "bottom"
  } else if (v > 0.75) {
    "top"
  } else {
    "centre"
  }
}

# The justification a tick label uses. Unrotated labels keep the track's fixed
# anchor (`geom$just`). A rotated label honours the element's own `hjust`/`vjust`
# when set; otherwise it anchors the *end* of the run at the tick so the slanted
# text hangs clear of the panel (down-outward along the bottom, up-outward along
# the top).
.axis_text_just <- function(geom, el, rot) {
  if (rot == 0) {
    return(geom$just)
  }
  hj <- el@hjust
  vj <- el@vjust
  if (!is.null(hj) || !is.null(vj)) {
    return(c(
      if (!is.null(hj)) .just_h(hj) else geom$just[1],
      if (!is.null(vj)) .just_v(vj) else geom$just[2]
    ))
  }
  if (identical(geom$just[2], "top")) {
    c(if (rot > 0) "right" else "left", "top")
  } else if (identical(geom$just[2], "bottom")) {
    c(if (rot > 0) "left" else "right", "bottom")
  } else {
    geom$just
  }
}

# The `text_grob` for one tick label. When `wrap_mm` (the mm width a single tick
# gets) is finite and the label is unrotated, the text wraps to that width and
# its lines centre under the tick; otherwise it is an ordinary single line drawn
# exactly where the old code placed it. Shared by the draw and the row/gutter
# height measurement (`.track_h_axis`) so the reserved track always matches.
.axis_text_grob <- function(
  label,
  el,
  just,
  x = vellum::vl_unit(0.5, "npc"),
  y = vellum::vl_unit(0.5, "npc"),
  rot = 0,
  wrap_mm = NA_real_
) {
  wrap <- length(wrap_mm) == 1L && is.finite(wrap_mm) && wrap_mm > 0 && rot == 0
  # Tabular figures on tick labels: digits keep a constant advance width, so a
  # column of numbers does not jitter left/right from tick to tick. Harmless on
  # non-numeric labels (only digit glyphs are affected).
  vellum::text_grob(
    label,
    x = x,
    y = y,
    just = just,
    rot = rot,
    width = if (wrap) vellum::vl_unit(wrap_mm, "mm") else NULL,
    align = if (wrap) "centre" else "left",
    gp = .el_gpar_text(el, features = .TNUM)
  )
}

.draw_axis <- function(
  scene,
  row,
  col,
  sc,
  rt,
  axis,
  sec = FALSE,
  wrap_mm = NA_real_
) {
  is_y <- identical(axis, "y")
  el <- rt[[if (is_y) "axis.text.y" else "axis.text.x"]]
  aline <- rt[[if (is_y) "axis.line.y" else "axis.line.x"]]
  if (.is_blank(el) && .is_blank(aline)) {
    return(scene)
  }
  geom <- if (is_y && !sec) {
    # panel-adjacent (right) edge; labels right-justified in the left gutter
    list(
      nm = sprintf("axis-y-%d", row),
      seg = c(1, 0, 1, 1),
      seed = 9L,
      lx = 0.96,
      just = c("right", "centre")
    )
  } else if (is_y && sec) {
    # panel-adjacent (left) edge; labels left-justified in the right gutter
    list(
      nm = sprintf("axis-y2-%d", row),
      seg = c(0, 0, 0, 1),
      seed = 11L,
      lx = 0.04,
      just = c("left", "centre")
    )
  } else if (!is_y && !sec) {
    # panel-adjacent (top) edge; labels top-justified in the bottom gutter
    list(
      nm = sprintf("axis-x-%d", col),
      seg = c(0, 1, 1, 1),
      seed = 8L,
      ly = 0.82,
      just = c("centre", "top")
    )
  } else {
    # panel-adjacent (bottom) edge; labels bottom-justified in the top gutter
    list(
      nm = sprintf("axis-x2-%d", col),
      seg = c(0, 0, 1, 0),
      seed = 12L,
      ly = 0.18,
      just = c("centre", "bottom")
    )
  }
  vp <- if (is_y) {
    vellum::vl_viewport(
      row = row,
      col = col,
      yscale = sc$domain,
      name = geom$nm
    )
  } else {
    vellum::vl_viewport(
      row = row,
      col = col,
      xscale = sc$domain,
      name = geom$nm
    )
  }
  scene <- vellum::push(scene, vp)
  if (!.is_blank(aline)) {
    s <- geom$seg
    scene <- vellum::draw(
      scene,
      vellum::segments_grob(
        vellum::vl_unit(s[1], "npc"),
        vellum::vl_unit(s[2], "npc"),
        vellum::vl_unit(s[3], "npc"),
        vellum::vl_unit(s[4], "npc"),
        sketch = .el_sketch(aline, geom$seed),
        gp = .el_gpar_line(aline)
      )
    )
  }
  if (!.is_blank(el)) {
    rot <- .el_rot(el)
    just <- .axis_text_just(geom, el, rot)
    for (i in seq_along(sc$breaks)) {
      if (!is.finite(sc$breaks[i])) {
        next # a non-finite break has no position to anchor its label
      }
      x <- if (is_y) {
        vellum::vl_unit(geom$lx, "npc")
      } else {
        vellum::vl_unit(sc$breaks[i], "native")
      }
      y <- if (is_y) {
        vellum::vl_unit(sc$breaks[i], "native")
      } else {
        vellum::vl_unit(geom$ly, "npc")
      }
      scene <- vellum::draw(
        scene,
        .axis_text_grob(
          sc$labels[i],
          el,
          just,
          x = x,
          y = y,
          rot = rot,
          wrap_mm = wrap_mm
        )
      )
    }
  }
  vellum::pop(scene)
}

# y-axis labels for `y_sc`, right-justified against each gridline.
.draw_y_axis <- function(scene, row, col, y_sc, rt) {
  .draw_axis(scene, row, col, y_sc, rt, "y")
}

# x-axis labels for `x_sc`, centred under each gridline. `wrap_mm` is the mm
# width one tick gets, so a long label wraps to its column instead of colliding
# with its neighbour; `NA` (the default, and every unknown-width path) keeps
# single-line labels.
.draw_x_axis <- function(scene, row, col, x_sc, rt, wrap_mm = NA_real_) {
  .draw_axis(scene, row, col, x_sc, rt, "x", wrap_mm = wrap_mm)
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
  colspan = 1,
  name = sprintf("strip-%d-%d", row, col)
) {
  scene <- vellum::push(
    scene,
    vellum::vl_viewport(
      row = row,
      col = col,
      rowspan = rowspan,
      colspan = colspan,
      name = name
    )
  )
  sb <- rt[["strip.background"]]
  if (!.is_blank(sb)) {
    scene <- vellum::draw(
      scene,
      vellum::rect_grob(gp = .el_gpar_rect(sb), sketch = .el_sketch(sb, 10L))
    )
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

# One axis-title band. `axis` is "x"/"y"; `sec = TRUE` is the secondary variant
# (right/top, and skipped when `name` is NULL). The four former
# .draw_{x,y}_title[_sec] drawers differed only in rotation, the viewport name,
# the row/col span axis, and the secondary NULL-name guard.
.draw_axis_title <- function(
  scene,
  row,
  col,
  name,
  rt,
  axis,
  sec = FALSE,
  span = 1
) {
  is_y <- identical(axis, "y")
  el <- rt[[if (is_y) "axis.title.y" else "axis.title.x"]]
  if (.is_blank(el) || (sec && is.null(name))) {
    return(scene)
  }
  vp_name <- paste0("axis-title-", axis, if (sec) "2" else "")
  rot <- if (!is_y) {
    0
  } else if (sec) {
    -90
  } else {
    90
  }
  vp <- if (is_y) {
    vellum::vl_viewport(row = row, col = col, rowspan = span, name = vp_name)
  } else {
    vellum::vl_viewport(row = row, col = col, colspan = span, name = vp_name)
  }
  scene <- vellum::push(scene, vp)
  scene <- vellum::draw(
    scene,
    vellum::text_grob(name, rot = rot, gp = .el_gpar_text(el))
  )
  vellum::pop(scene)
}

.draw_y_title <- function(scene, row, col, name, rt, rowspan = 1) {
  .draw_axis_title(scene, row, col, name, rt, "y", span = rowspan)
}

.draw_x_title <- function(scene, row, col, name, rt, colspan = 1) {
  .draw_axis_title(scene, row, col, name, rt, "x", span = colspan)
}

# Secondary y-axis: mirror of `.draw_y_axis` on the RIGHT gutter. The axis line
# sits on the panel-adjacent (left) edge and labels are left-justified.
.draw_y_axis_sec <- function(scene, row, col, y_sc, rt) {
  .draw_axis(scene, row, col, y_sc, rt, "y", sec = TRUE)
}

# Secondary x-axis: mirror of `.draw_x_axis` on the TOP gutter. The axis line
# sits on the panel-adjacent (bottom) edge and labels are bottom-justified.
.draw_x_axis_sec <- function(scene, row, col, x_sc, rt) {
  .draw_axis(scene, row, col, x_sc, rt, "x", sec = TRUE)
}

.draw_y_title_sec <- function(scene, row, col, name, rt, rowspan = 1) {
  .draw_axis_title(scene, row, col, name, rt, "y", sec = TRUE, span = rowspan)
}

.draw_x_title_sec <- function(scene, row, col, name, rt, colspan = 1) {
  .draw_axis_title(scene, row, col, name, rt, "x", sec = TRUE, span = colspan)
}

# --- plot title / subtitle / caption / tag bands ----------------------------
# Each spans the full page width (col = 1, colspan = ncol). `default_hjust`
# reproduces the ggplot2 default placement (title/subtitle/tag flush left,
# caption flush right); a theme `hjust` overrides it.

# The anchor (x position + horizontal justification) a band uses for a given
# hjust. Kept as a single source so the height measurement (layout) and the draw
# stay in lockstep. `hjust` buckets to left / centre / right, exactly as the
# original point-anchored placement did.
.band_anchor <- function(hj) {
  if (hj < 0.25) {
    list(x = 0.01, just = "left")
  } else if (hj > 0.75) {
    list(x = 0.99, just = "right")
  } else {
    list(x = 0.5, just = "centre")
  }
}

# The `text_grob` for a title/subtitle/caption/tag band. When `band_w` (the mm
# width the band spans) is finite and the band is unrotated, the text wraps to
# that width and its lines are aligned by the element's hjust (`align=`); a label
# that fits on one line is drawn exactly where the old point-anchored code put it
# (the box is anchored the same way, so a single line lands on the same edge).
# With `band_w` unset (the composition path, which does not know its page width
# here) it is an ordinary single-line label. Shared by the layout height
# measurement and the draw so the reserved height always matches the drawn text.
.band_text_grob <- function(text, el, band_w = NA_real_, default_hjust = 0) {
  hj <- el@hjust %||% default_hjust
  a <- .band_anchor(hj)
  rot <- .el_rot(el)
  # Wrap only for an unrotated band: a rotated box's width is along the rotated
  # axis, which is not the page-width wrapping the caller measured.
  wrap <- is.finite(band_w) && band_w > 0 && isTRUE(rot == 0)
  vellum::text_grob(
    text,
    x = vellum::vl_unit(a$x, "npc"),
    just = c(a$just, "centre"),
    rot = rot,
    width = if (wrap) vellum::vl_unit(band_w, "mm") else NULL,
    align = if (wrap) a$just else "left",
    gp = .el_gpar_text(el)
  )
}

.draw_band <- function(
  scene,
  row,
  ncol,
  text,
  el,
  default_hjust,
  band_w = NA_real_
) {
  if (.is_blank(el)) {
    return(scene)
  }
  scene <- vellum::push(
    scene,
    vellum::vl_viewport(row = row, col = 1, colspan = ncol)
  )
  scene <- vellum::draw(scene, .band_text_grob(text, el, band_w, default_hjust))
  vellum::pop(scene)
}

.draw_title <- function(scene, row, ncol, text, rt, band_w = NA_real_) {
  .draw_band(scene, row, ncol, text, rt[["plot.title"]], 0, band_w)
}

.draw_subtitle <- function(scene, row, ncol, text, rt, band_w = NA_real_) {
  .draw_band(scene, row, ncol, text, rt[["plot.subtitle"]], 0, band_w)
}

.draw_caption <- function(scene, row, ncol, text, rt, band_w = NA_real_) {
  .draw_band(scene, row, ncol, text, rt[["plot.caption"]], 1, band_w)
}

.draw_tag <- function(scene, row, ncol, text, rt, band_w = NA_real_) {
  # The tag never wraps -- it is a short corner mark, and its track is sized to a
  # single line -- so it keeps the single-line box.
  .draw_band(scene, row, ncol, text, rt[["plot.tag"]], 0)
}

# --- legend -----------------------------------------------------------------

# The list of guides a plot needs, in draw order: colour, size, shape, edge
# width, alpha, then linetype. Each guide is `list(kind, sc)` where `sc` is the
# trained scale (or, for a merged guide, a synthesized pseudo-scale).
#
# When one variable drives two aesthetics, the colour guide merges with its
# partner into a single guide whose keys carry both encodings (ggplot2's rule):
# a discrete colour scale merges with `shape`; a continuous colour scale merges
# with `size`. They merge only when the two scales share a title *and* a
# compatible break/level set -- give one scale a different `name=` to keep them
# apart. `merged` marks which partner (if any) was folded into the colour guide.
.legend_guides <- function(scales) {
  out <- list()
  merged <- NULL
  # A scale can opt out of its legend (an identity scale, or `guide = "none"`):
  # the mapping still applies to the marks, but no guide is drawn. Drop it here
  # so neither the standalone nor the merge path picks it up. This is a local
  # copy, so the emitters' own scales are unaffected.
  for (k in c(
    "color",
    "size",
    "shape",
    "pattern",
    "edge_width",
    "alpha",
    "linetype",
    "edge_color",
    "edge_alpha",
    "edge_linetype"
  )) {
    if (!is.null(scales[[k]]) && isTRUE(scales[[k]]$no_guide)) {
      scales[[k]] <- NULL
    }
  }
  if (!is.null(scales$color)) {
    gk <- scales$color$kind
    # A binned (classed) colour scale draws as discrete swatches with interval
    # labels, so it reuses the discrete guide.
    discrete <- gk %in% c("discrete", "binned")
    if (discrete && .can_merge_shape(scales$color, scales$shape)) {
      out <- c(
        out,
        list(list(
          kind = "merged",
          sc = .merged_color_shape(scales$color, scales$shape)
        ))
      )
      merged <- "shape"
    } else if (
      identical(gk, "continuous") && .can_merge_size(scales$color, scales$size)
    ) {
      out <- c(
        out,
        list(list(
          kind = "merged",
          sc = .merged_color_size(scales$color, scales$size)
        ))
      )
      merged <- "size"
    } else {
      k <- if (discrete) "discrete" else gk
      out <- c(out, list(list(kind = paste0("color_", k), sc = scales$color)))
    }
  }
  if (!is.null(scales$size) && !identical(merged, "size")) {
    out <- c(out, list(list(kind = "size", sc = scales$size)))
  }
  if (!is.null(scales$shape) && !identical(merged, "shape")) {
    out <- c(out, list(list(kind = "shape", sc = scales$shape)))
  }
  if (!is.null(scales$pattern)) {
    out <- c(out, list(list(kind = "pattern", sc = scales$pattern)))
  }
  if (!is.null(scales$edge_width)) {
    out <- c(out, list(list(kind = "edge_width", sc = scales$edge_width)))
  }
  if (!is.null(scales$alpha)) {
    out <- c(out, list(list(kind = "alpha", sc = scales$alpha)))
  }
  if (!is.null(scales$linetype)) {
    out <- c(out, list(list(kind = "linetype", sc = scales$linetype)))
  }
  # Edge colour / alpha / linetype guides reuse the node guide renderers (the
  # scale objects are structurally identical), but never merge with shape/size --
  # they belong to the edge layer, whose key glyph is a line. A discrete edge
  # colour scale draws as swatches; a continuous one as a colourbar. When the
  # edge colour scale is the same encoding as the node colour scale (one variable
  # colouring both), its guide would duplicate the node swatch, so it folds into
  # the node guide instead of drawing a second, redundant legend.
  if (
    !is.null(scales$edge_color) &&
      !.can_merge_edge_color(scales$color, scales$edge_color)
  ) {
    gk <- scales$edge_color$kind
    k <- if (gk %in% c("discrete", "binned")) "discrete" else gk
    out <- c(
      out,
      list(list(kind = paste0("color_", k), sc = scales$edge_color))
    )
  }
  if (!is.null(scales$edge_alpha)) {
    out <- c(out, list(list(kind = "alpha", sc = scales$edge_alpha)))
  }
  if (!is.null(scales$edge_linetype)) {
    out <- c(out, list(list(kind = "linetype", sc = scales$edge_linetype)))
  }
  out
}

# A discrete colour scale and a shape scale merge when they carry the same title
# and the identical set of levels (so the same variable, encoded the same way).
.can_merge_shape <- function(color, shape) {
  !is.null(color) &&
    !is.null(shape) &&
    identical(color$name, shape$name) &&
    identical(color$levels, shape$levels)
}

# A continuous colour scale and a size scale merge when they carry the same
# title (they then share the variable, hence its data range and breaks).
.can_merge_size <- function(color, size) {
  !is.null(color) && !is.null(size) && identical(color$name, size$name)
}

# A node colour scale and an edge colour scale describe the same encoding when
# one variable colours both nodes/text and edges (e.g. `vdendrogram(k=)`). Both
# come from the same `.train_colour`, so their palette-defining fields are
# directly comparable -- they match when title, kind, and the fields that fix
# the palette and its breaks agree. `key_glyph` (a swatch for nodes, a line for
# edges) and `map` (a closure) are expected to differ and are not compared. When
# they match, the node colour guide already carries the whole encoding, so the
# edge guide is dropped rather than drawn as a redundant twin.
.can_merge_edge_color <- function(color, edge_color) {
  if (is.null(color) || is.null(edge_color)) {
    return(FALSE)
  }
  if (
    !identical(color$kind, edge_color$kind) ||
      !identical(color$name, edge_color$name)
  ) {
    return(FALSE)
  }
  if (identical(color$kind, "continuous")) {
    identical(color$range, edge_color$range) &&
      identical(color$pal256, edge_color$pal256) &&
      identical(color$midpoint, edge_color$midpoint)
  } else {
    # discrete / binned: same shown levels drawn in the same colours.
    identical(color$levels, edge_color$levels) &&
      identical(color$colors, edge_color$colors)
  }
}

# Pseudo-scale for a merged colour+shape guide: one row per shared level, each a
# point drawn in the level's colour with the level's shape. `fill` and `col` are
# both set so stroke-only shapes (plus/cross) still take the colour.
.merged_color_shape <- function(color, shape) {
  cols <- color$colors
  list(
    name = color$name,
    # `levels` is the shared discrete key; it lets `.legend_series_key` tag the
    # merged swatches for interactive highlight (marks carry `color:<level>`).
    levels = color$levels,
    labels = color$labels %||% color$levels,
    fills = cols,
    cols = cols,
    shapes = shape$shapes,
    sizes_mm = NULL
  )
}

# Pseudo-scale for a merged colour+size guide: the size scale drives the rows
# (breaks -> radii), and each key is coloured by mapping its break through the
# continuous colour scale. Same variable, so the size breaks lie in the colour
# scale's data range.
.merged_color_size <- function(color, size) {
  cols <- color$map(size$legend_breaks)
  # Sized keys are drawn as circles (the colour scale's own key glyph may be a
  # line/square, which are not point shapes).
  list(
    name = color$name,
    labels = size$legend_labels,
    fills = cols,
    cols = cols,
    shapes = rep_len("circle", length(cols)),
    sizes_mm = size$legend_sizes
  )
}

# --- legend geometry --------------------------------------------------------
# Legends are laid out in absolute millimetres, never in fractions of the
# (variable-height) legend cell. Each guide is measured to its content height
# (a title line + one row per key, the row pitch driven by the key's drawn
# size), guides are stacked with a fixed inter-guide gap, and the whole block is
# centred in the legend track. Everything is placed through nested
# `grid_layout()` cells so a key and its label land in their own mm-sized boxes
# and no npc/mm unit arithmetic (which vellum disallows) is ever needed.

# Millimetre text metrics at a given point size (device-space, resize-stable).
.mm_th <- function(fs) vellum::vl_strheight("Ag", "", "plain", fs, unit = "mm")
.mm_tw <- function(s, fs) {
  s <- as.character(s)
  s <- s[!is.na(s)]
  if (!length(s)) {
    return(0)
  }
  s[!nzchar(s)] <- " "
  max(vellum::vl_strwidth(s, "", "plain", fs, unit = "mm"))
}

# Resolved legend geometry (all mm, except the two font sizes). Phase 3 lets the
# theme override the key size / spacings; the defaults live in `.SETTINGS_DEFAULTS`.
.legend_metrics <- function(rt) {
  list(
    text_fs = rt[["legend.text"]]@size,
    title_fs = rt[["legend.title"]]@size,
    text_h = .mm_th(rt[["legend.text"]]@size),
    title_h = .mm_th(rt[["legend.title"]]@size),
    key = rt[["legend.key.size"]],
    bar_w = .LEGEND_BAR_MM,
    row_gap = .LEGEND_ROW_GAP_MM,
    title_gap = .LEGEND_TITLE_GAP_MM,
    spacing = rt[["legend.spacing"]],
    lab_gap = .LEGEND_KEY_LABEL_GAP_MM,
    pad = .LEGEND_INNER_PAD_MM,
    # inset (t, r, b, l) around the whole legend block
    margin = rep_len(rt[["legend.margin"]], 4L),
    show_title = !.is_blank(rt[["legend.title"]])
  )
}

# The row labels a guide shows. A discrete/binned colour, size, or shape guide
# appends an "NA" row where the data has missing values (its key is drawn in the
# NA colour / neutral glyph).
.guide_labels <- function(g) {
  sc <- g$sc
  switch(
    g$kind,
    color_continuous = sc$legend_labels,
    color_discrete = {
      l <- sc$labels %||% sc$levels
      if (isTRUE(sc$na)) c(l, "NA") else l
    },
    size = {
      l <- sc$legend_labels
      if (isTRUE(sc$na)) c(l, "NA") else l
    },
    shape = {
      l <- sc$levels
      if (isTRUE(sc$na)) c(l, "NA") else l
    },
    pattern = sc$levels,
    edge_width = sc$legend_labels,
    alpha = sc$legend_labels,
    linetype = sc$levels,
    merged = sc$labels
  )
}

# The drawn diameter (mm) of a guide's key, used to size the row pitch / key
# column so the largest key never collides with its neighbours. `points_grob`
# sizes are radii, so a size key of radius r is 2r across.
.guide_key_d <- function(g, m) {
  sizes <- if (g$kind == "size") {
    g$sc$legend_sizes
  } else if (g$kind == "merged") {
    g$sc$sizes_mm
  } else {
    NULL
  }
  if (length(sizes)) max(2 * max(sizes), m$key) else m$key
}

# The continuous colour-bar length (mm) for a vertical guide: long enough that
# consecutive break labels never touch.
.bar_len_mm <- function(g, m) {
  k <- length(g$sc$legend_labels)
  max(.LEGEND_MIN_BAR_MM, k * (m$text_h + m$lab_gap))
}

# Measured height (mm) of a vertical guide: title line + key rows (uniform pitch
# = max(key diameter, text height) + gap), or title + colour bar (+ NA row).
.guide_height_mm <- function(g, m) {
  # A guide only reserves a title band when the theme shows titles AND this guide
  # actually has a name; otherwise a titleless guide would reserve an empty band
  # and its keys would sit lower than a titled sibling's (misaligned legends).
  m$show_title <- isTRUE(m$show_title) && !is.null(g$sc$name)
  th <- if (m$show_title) m$title_h + m$title_gap else 0
  if (g$kind == "color_continuous") {
    na <- if (isTRUE(g$sc$na)) max(m$key, m$text_h) + m$row_gap else 0
    th + .bar_len_mm(g, m) + na
  } else {
    k <- length(.guide_labels(g))
    pitch <- max(.guide_key_d(g, m), m$text_h) + m$row_gap
    th + k * pitch
  }
}

# Measured width (mm) of a horizontal guide: the wider of its title and its key
# row (keys packed left-to-right, each followed by its own label).
.guide_width_mm <- function(g, m) {
  labs <- .guide_labels(g)
  tw <- if (m$show_title && !is.null(g$sc$name)) {
    .mm_tw_any(g$sc$name, m$title_fs)
  } else {
    0
  }
  if (g$kind == "color_continuous") {
    na <- if (isTRUE(g$sc$na)) {
      m$spacing + max(m$key, .mm_tw("NA", fs = m$text_fs))
    } else {
      0
    }
    body <- max(
      .LEGEND_MIN_BAR_MM,
      sum(vapply(labs, .mm_tw, 0, fs = m$text_fs) + m$lab_gap)
    ) +
      na
  } else {
    key_d <- .guide_key_d(g, m)
    body <- sum(
      key_d +
        m$lab_gap +
        vapply(labs, .mm_tw, 0, fs = m$text_fs) +
        m$spacing
    )
  }
  max(tw, body)
}

# Title width in mm. A rich md() title is measured through vl_strwidth()'s rich
# path (the same run composition the renderer draws), so super/subscripts and
# bold runs reserve the space they actually occupy — otherwise the legend column
# is sized as if the title were empty and the drawn title clips.
.mm_tw_any <- function(name, fs) {
  if (is.character(name)) {
    return(.mm_tw(name, fs))
  }
  vellum::vl_strwidth(name, "", "plain", fs, unit = "mm")
}

# A unit vector of `sizes` (mm) separated by `gap` (mm) spacers, for a stacking
# grid_layout: c(size1, gap, size2, gap, ..., sizeN). Guides live in the odd
# tracks (2*i - 1).
.interleave_mm <- function(sizes, gap) {
  n <- length(sizes)
  if (!n) {
    return(vellum::vl_unit(numeric(0), "mm"))
  }
  parts <- vector("list", 2L * n - 1L)
  for (i in seq_len(n)) {
    parts[[2L * i - 1L]] <- vellum::vl_unit(sizes[i], "mm")
    if (i < n) {
      parts[[2L * i]] <- vellum::vl_unit(gap, "mm")
    }
  }
  do.call(c, parts)
}

# Concatenate a list of units, dropping NULL entries (conditional tracks).
.c_units <- function(...) {
  do.call(c, Filter(Negate(is.null), list(...)))
}

# --- key glyphs -------------------------------------------------------------
# A single key glyph, drawn centred in the current (cell) viewport. Phase 2
# swaps the discrete-colour swatch for a mark-matched glyph via `sc$key_glyph`.

# `sketch` is the plot-wide hand-drawn spec (from theme_sketch()), or NULL: it
# makes the legend keys match a hand-drawn plot. A per-key seed bump keeps
# stacked keys from sharing an identical wobble.
# The key for an NA row of a size/shape guide: a neutral grey circle (mirrors the
# grey NA swatch a colour guide draws), signalling "missing" without a value.
.na_key_grob <- function(m, sketch = NULL) {
  vellum::points_grob(
    vellum::vl_unit(0.5, "npc"),
    vellum::vl_unit(0.5, "npc"),
    size = vellum::vl_unit(m$key / 2, "mm"),
    shape = "circle",
    sketch = sketch,
    gp = vellum::vl_gpar(fill = "grey70", col = "grey50")
  )
}

# A legend key glyph for a `shape`: an `svg_grob()` icon when the shape is an
# SVG (a `d` path or `.svg` file), otherwise the usual `points_grob()` marker.
# `size_mm` is the marker size for the built-in case (the icon uses a slightly
# larger longer-side so it reads at legend scale).
.shape_key_grob <- function(shape, size_mm, fill, col, sk) {
  d <- .shape_svg_d(shape)
  if (!is.na(d)) {
    return(vellum::svg_grob(
      d,
      vellum::vl_unit(0.5, "npc"),
      vellum::vl_unit(0.5, "npc"),
      size = vellum::vl_unit(2 * size_mm, "mm"),
      gp = vellum::vl_gpar(fill = fill, col = col)
    ))
  }
  vellum::points_grob(
    vellum::vl_unit(0.5, "npc"),
    vellum::vl_unit(0.5, "npc"),
    size = vellum::vl_unit(size_mm, "mm"),
    shape = shape,
    sketch = sk,
    gp = vellum::vl_gpar(fill = fill, col = col)
  )
}

.key_grob <- function(g, i, m, sketch = NULL) {
  sc <- g$sc
  sk <- .sketch_bump(sketch, 200L + i)
  switch(
    g$kind,
    color_discrete = {
      cols <- sc$colors
      if (isTRUE(sc$na)) {
        cols <- c(cols, sc$na_value)
      }
      .colour_key_grob(sc$key_glyph, cols[i], m, sk)
    },
    size = if (isTRUE(sc$na) && i > length(sc$legend_sizes)) {
      .na_key_grob(m, sk)
    } else {
      vellum::points_grob(
        vellum::vl_unit(0.5, "npc"),
        vellum::vl_unit(0.5, "npc"),
        size = vellum::vl_unit(sc$legend_sizes[i], "mm"),
        shape = sc$key_glyph %||% "circle",
        sketch = sk,
        gp = vellum::vl_gpar(fill = "grey35", col = "grey35")
      )
    },
    shape = if (isTRUE(sc$na) && i > length(sc$shapes)) {
      .na_key_grob(m, sk)
    } else {
      .shape_key_grob(sc$shapes[i], m$key / 2, "grey35", "grey35", sk)
    },
    pattern = vellum::rect_grob(
      vellum::vl_unit(0.5, "npc"),
      vellum::vl_unit(0.5, "npc"),
      width = vellum::vl_unit(0.86, "npc"),
      height = vellum::vl_unit(0.86, "npc"),
      sketch = sk,
      gp = vellum::vl_gpar(fill = sc$patterns[[i]], col = "grey55")
    ),
    edge_width = vellum::segments_grob(
      vellum::vl_unit(0.12, "npc"),
      vellum::vl_unit(0.5, "npc"),
      vellum::vl_unit(0.88, "npc"),
      vellum::vl_unit(0.5, "npc"),
      sketch = sk,
      gp = vellum::vl_gpar(col = "grey35", lwd = sc$legend_widths[i])
    ),
    alpha = vellum::points_grob(
      vellum::vl_unit(0.5, "npc"),
      vellum::vl_unit(0.5, "npc"),
      size = vellum::vl_unit(m$key / 2, "mm"),
      shape = "circle",
      sketch = sk,
      gp = vellum::vl_gpar(
        fill = "grey20",
        col = NA,
        alpha = sc$legend_alphas[i]
      )
    ),
    linetype = vellum::segments_grob(
      vellum::vl_unit(0.12, "npc"),
      vellum::vl_unit(0.5, "npc"),
      vellum::vl_unit(0.88, "npc"),
      vellum::vl_unit(0.5, "npc"),
      sketch = sk,
      gp = vellum::vl_gpar(col = "grey35", lwd = 1.5, lty = sc$linetypes[i])
    ),
    # A merged guide's key carries both encodings in one point: the shared
    # variable's colour (fill + stroke) and shape, sized when size is merged in.
    merged = .shape_key_grob(
      sc$shapes[i] %||% "circle",
      sc$sizes_mm[i] %||% (m$key / 2),
      sc$fills[i],
      sc$cols[i],
      sk
    )
  )
}

# A discrete-colour key drawn as the mark's glyph: a filled point for point/glyph
# marks, a short line for line marks, else the default filled square swatch.
.colour_key_grob <- function(glyph, col, m, sketch = NULL) {
  switch(
    glyph %||% "square",
    point = ,
    circle = vellum::points_grob(
      vellum::vl_unit(0.5, "npc"),
      vellum::vl_unit(0.5, "npc"),
      size = vellum::vl_unit(m$key / 2, "mm"),
      shape = "circle",
      sketch = sketch,
      gp = vellum::vl_gpar(fill = col, col = NA)
    ),
    line = vellum::segments_grob(
      vellum::vl_unit(0.12, "npc"),
      vellum::vl_unit(0.5, "npc"),
      vellum::vl_unit(0.88, "npc"),
      vellum::vl_unit(0.5, "npc"),
      sketch = sketch,
      gp = vellum::vl_gpar(col = col, lwd = 2)
    ),
    vellum::rect_grob(
      width = vellum::vl_unit(m$key, "mm"),
      height = vellum::vl_unit(m$key, "mm"),
      sketch = sketch,
      gp = vellum::vl_gpar(fill = col, col = NA)
    )
  )
}

# --- guide drawers ----------------------------------------------------------

.draw_guide_title <- function(scene, name, rt) {
  el <- rt[["legend.title"]]
  if (.is_blank(el) || is.null(name)) {
    return(scene)
  }
  vellum::draw(
    scene,
    vellum::text_grob(
      name,
      x = vellum::vl_unit(0, "npc"),
      y = vellum::vl_unit(0.4, "npc"),
      just = c("left", "centre"),
      gp = .el_gpar_text(el)
    )
  )
}

# Draw a vertical guide into the current viewport (its exact measured mm height).
# Is any layer of `spec` interactive (declares a `data_id`/`tooltip`/... arg)?
# When TRUE, discrete legend swatches are tagged so a host can drive series
# highlight/select from the legend.
.spec_interactive <- function(spec) {
  any(vapply(spec@layers, function(l) length(l@interactivity) > 0L, logical(1)))
}

# The series key a discrete legend key `i` represents, as "<aes>:<level>" — the
# join value between a legend swatch (`legend_for`) and the marks of that series
# (their `legend` membership, set in `.compile_marks`). NULL for guide kinds that
# are not a discrete colour/fill/shape legend (continuous colourbars, size, etc.).
.legend_series_key <- function(g, i) {
  aes <- switch(
    g$kind,
    color_discrete = "color",
    shape = "shape",
    # A merged colour+shape guide shares one discrete variable, so its swatches
    # join the colour series (marks carry `color:<value>`). A merged colour+size
    # guide has no discrete series (`levels` is NULL) and stays untagged.
    merged = if (!is.null(g$sc$levels)) "color" else NULL,
    NULL
  )
  if (is.null(aes)) {
    return(NULL)
  }
  paste0(aes, ":", as.character(g$sc$levels[i]))
}

# Tag a legend swatch grob so it becomes a keyed, hoverable element: `data_id` a
# stable per-series id, `meta$legend_for` the series key (vellumwidget highlights every
# mark whose `legend` contains it), `meta$tooltip` the level's label. A no-op when
# the plot is not interactive or the guide is not a discrete colour/shape legend,
# so a static plot's legend is unchanged.
.tag_legend_swatch <- function(swatch, g, i, label, interactive) {
  if (!isTRUE(interactive)) {
    return(swatch)
  }
  key <- .legend_series_key(g, i)
  if (is.null(key)) {
    return(swatch)
  }
  swatch@keys <- paste0("legend:", key)
  swatch@meta <- list(list(legend_for = key, tooltip = as.character(label)))
  swatch
}

.draw_guide_v <- function(scene, g, m, rt) {
  # Reserve the title band only when this guide has a name (see .guide_height_mm);
  # the normalised `m` also flows to the continuous drawer and its `th`.
  m$show_title <- isTRUE(m$show_title) && !is.null(g$sc$name)
  txt <- .el_gpar_text(rt[["legend.text"]])
  th <- if (m$show_title) m$title_h + m$title_gap else 0
  if (g$kind == "color_continuous") {
    return(.draw_guide_continuous_v(scene, g, m, rt, txt, th))
  }
  labs <- .guide_labels(g)
  k <- length(labs)
  key_d <- .guide_key_d(g, m)
  pitch <- max(key_d, m$text_h) + m$row_gap
  key_w <- m$pad + key_d + m$lab_gap
  heights <- .c_units(
    if (m$show_title) vellum::vl_unit(th, "mm"),
    vellum::vl_unit(rep(pitch, k), "mm")
  )
  scene <- vellum::push(
    scene,
    vellum::vl_viewport(
      layout = vellum::grid_layout(
        heights = heights,
        widths = c(vellum::vl_unit(key_w, "mm"), vellum::vl_unit(1, "null"))
      )
    )
  )
  off <- if (m$show_title) 1L else 0L
  if (m$show_title) {
    scene <- vellum::push(
      scene,
      vellum::vl_viewport(row = 1, col = 1, colspan = 2)
    )
    scene <- .draw_guide_title(scene, g$sc$name, rt)
    scene <- vellum::pop(scene)
  }
  for (i in seq_len(k)) {
    scene <- vellum::push(scene, vellum::vl_viewport(row = off + i, col = 1))
    scene <- vellum::draw(
      scene,
      .tag_legend_swatch(
        .key_grob(g, i, m, rt[[".sketch"]]),
        g,
        i,
        labs[i],
        rt[[".interactive"]]
      )
    )
    scene <- vellum::pop(scene)
    scene <- vellum::push(scene, vellum::vl_viewport(row = off + i, col = 2))
    scene <- vellum::draw(
      scene,
      vellum::text_grob(
        labs[i],
        x = vellum::vl_unit(0, "npc"),
        y = vellum::vl_unit(0.5, "npc"),
        just = c("left", "centre"),
        gp = txt
      )
    )
    scene <- vellum::pop(scene)
  }
  vellum::pop(scene)
}

# The per-element `meta` for a continuous colorbar's gradient-bar rect: a
# `colorbar` descriptor a host reads (with the rect's device-px bbox from
# `scene_model()`) to overlay an interactive value-range filter. `orientation` is
# "v"/"h"; `reverse` flags a reversed bar (high value at the low-position end).
# A diverging scale (scale_*_gradient2()) also reports `midpoint` + `diverging`,
# so a host can centre a value-range filter (visualMap) on the neutral value.
.colorbar_meta <- function(cl, revb, orientation) {
  desc <- list(
    aesthetic = "color",
    lo = as.numeric(cl$range[1]),
    hi = as.numeric(cl$range[2]),
    orientation = orientation,
    reverse = isTRUE(revb)
  )
  if (!is.null(cl$midpoint)) {
    desc$diverging <- TRUE
    desc$midpoint <- as.numeric(cl$midpoint)
  }
  list(list(colorbar = desc))
}

.draw_guide_continuous_v <- function(scene, g, m, rt, txt, th) {
  cl <- g$sc
  has_na <- isTRUE(cl$na)
  na_h <- max(m$key, m$text_h) + m$row_gap
  heights <- .c_units(
    if (m$show_title) vellum::vl_unit(th, "mm"),
    vellum::vl_unit(1, "null"),
    if (has_na) vellum::vl_unit(na_h, "mm")
  )
  scene <- vellum::push(
    scene,
    vellum::vl_viewport(
      layout = vellum::grid_layout(
        heights = heights,
        widths = vellum::vl_unit(1, "null")
      )
    )
  )
  row_bar <- if (m$show_title) 2L else 1L
  if (m$show_title) {
    scene <- vellum::push(scene, vellum::vl_viewport(row = 1, col = 1))
    scene <- .draw_guide_title(scene, cl$name, rt)
    scene <- vellum::pop(scene)
  }
  scene <- vellum::push(scene, vellum::vl_viewport(row = row_bar, col = 1))
  # guide_legend(reverse = TRUE) flips a continuous bar by reversing the gradient
  # and mirroring every value position, keeping each value paired with its label.
  revb <- isTRUE(cl$reverse_bar)
  pal <- if (revb) rev(cl$pal256) else cl$pal256
  grad <- vellum::linear_gradient(pal, x1 = 0, y1 = 0, x2 = 0, y2 = 1)
  bar <- vellum::rect_grob(
    x = vellum::vl_unit(m$pad + m$bar_w / 2, "mm"),
    y = vellum::vl_unit(0.5, "npc"),
    width = vellum::vl_unit(m$bar_w, "mm"),
    height = vellum::vl_unit(1, "npc"),
    gp = vellum::vl_gpar(fill = grad, col = "grey50", lwd = 0.5)
  )
  # Carry the colorbar descriptor so a host can overlay an interactive value
  # filter; it surfaces (with the bar's device-px bbox) via scene_model().
  bar@meta <- .colorbar_meta(cl, revb, "v")
  scene <- vellum::draw(scene, bar)
  for (i in seq_along(cl$legend_breaks)) {
    frac <- scales::rescale(cl$legend_breaks[i], from = cl$range)
    if (revb) {
      frac <- 1 - frac
    }
    # A white break tick reaching in from the bar's right (label-side) edge.
    scene <- vellum::draw(
      scene,
      vellum::segments_grob(
        vellum::vl_unit(m$pad + m$bar_w - .LEGEND_TICK_MM, "mm"),
        vellum::vl_unit(frac, "npc"),
        vellum::vl_unit(m$pad + m$bar_w, "mm"),
        vellum::vl_unit(frac, "npc"),
        gp = vellum::vl_gpar(col = "white", lwd = 0.8)
      )
    )
    scene <- vellum::draw(
      scene,
      vellum::text_grob(
        cl$legend_labels[i],
        x = vellum::vl_unit(m$pad + m$bar_w + m$lab_gap, "mm"),
        y = vellum::vl_unit(frac, "npc"),
        just = c("left", "centre"),
        gp = txt
      )
    )
  }
  scene <- vellum::pop(scene)
  if (has_na) {
    scene <- vellum::push(
      scene,
      vellum::vl_viewport(row = row_bar + 1L, col = 1)
    )
    scene <- vellum::draw(
      scene,
      vellum::rect_grob(
        x = vellum::vl_unit(m$pad + m$bar_w / 2, "mm"),
        y = vellum::vl_unit(0.5, "npc"),
        width = vellum::vl_unit(m$key, "mm"),
        height = vellum::vl_unit(m$key, "mm"),
        gp = vellum::vl_gpar(fill = cl$na_value, col = NA)
      )
    )
    scene <- vellum::draw(
      scene,
      vellum::text_grob(
        "NA",
        x = vellum::vl_unit(m$pad + m$bar_w + m$lab_gap, "mm"),
        y = vellum::vl_unit(0.5, "npc"),
        just = c("left", "centre"),
        gp = txt
      )
    )
    scene <- vellum::pop(scene)
  }
  vellum::pop(scene)
}

# --- horizontal guide drawers (top/bottom legend) ---------------------------

# Draw a horizontal guide into the current viewport (its exact measured mm width).
.draw_guide_h <- function(scene, g, m, rt) {
  # Reserve the title band only when this guide has a name (see .guide_height_mm).
  m$show_title <- isTRUE(m$show_title) && !is.null(g$sc$name)
  txt <- .el_gpar_text(rt[["legend.text"]])
  th <- if (m$show_title) m$title_h + m$title_gap else 0
  if (g$kind == "color_continuous") {
    return(.draw_guide_continuous_h(scene, g, m, rt, txt, th))
  }
  labs <- .guide_labels(g)
  k <- length(labs)
  key_d <- .guide_key_d(g, m)
  # per-key cell width: key + gap + label + trailing spacer
  cellw <- key_d +
    m$lab_gap +
    vapply(labs, .mm_tw, 0, fs = m$text_fs) +
    m$spacing
  heights <- .c_units(
    if (m$show_title) vellum::vl_unit(th, "mm"),
    vellum::vl_unit(1, "null")
  )
  scene <- vellum::push(
    scene,
    vellum::vl_viewport(
      layout = vellum::grid_layout(
        heights = heights,
        widths = vellum::vl_unit(1, "null")
      )
    )
  )
  key_row <- if (m$show_title) 2L else 1L
  if (m$show_title) {
    scene <- vellum::push(scene, vellum::vl_viewport(row = 1, col = 1))
    scene <- .draw_guide_title(scene, g$sc$name, rt)
    scene <- vellum::pop(scene)
  }
  scene <- vellum::push(
    scene,
    vellum::vl_viewport(
      row = key_row,
      col = 1,
      layout = vellum::grid_layout(
        widths = vellum::vl_unit(cellw, "mm"),
        heights = vellum::vl_unit(1, "null")
      )
    )
  )
  for (i in seq_len(k)) {
    scene <- vellum::push(scene, vellum::vl_viewport(row = 1, col = i))
    scene <- vellum::push(
      scene,
      vellum::vl_viewport(
        x = vellum::vl_unit(key_d / 2, "mm"),
        width = vellum::vl_unit(key_d, "mm")
      )
    )
    scene <- vellum::draw(
      scene,
      .tag_legend_swatch(
        .key_grob(g, i, m, rt[[".sketch"]]),
        g,
        i,
        labs[i],
        rt[[".interactive"]]
      )
    )
    scene <- vellum::pop(scene)
    scene <- vellum::draw(
      scene,
      vellum::text_grob(
        labs[i],
        x = vellum::vl_unit(key_d + m$lab_gap, "mm"),
        y = vellum::vl_unit(0.5, "npc"),
        just = c("left", "centre"),
        gp = txt
      )
    )
    scene <- vellum::pop(scene)
  }
  scene <- vellum::pop(scene)
  vellum::pop(scene)
}

.draw_guide_continuous_h <- function(scene, g, m, rt, txt, th) {
  cl <- g$sc
  # Reserve a column at the right end for the NA key (mirrors the vertical guide,
  # which reserves a row); the gradient bar takes the remaining `null` width.
  has_na <- isTRUE(cl$na)
  na_w <- if (has_na) max(m$key, .mm_tw("NA", fs = m$text_fs)) else 0
  heights <- .c_units(
    if (m$show_title) vellum::vl_unit(th, "mm"),
    vellum::vl_unit(m$bar_w, "mm"),
    vellum::vl_unit(m$text_h + m$lab_gap, "mm")
  )
  widths <- if (has_na) {
    .c_units(
      vellum::vl_unit(1, "null"),
      vellum::vl_unit(m$spacing + na_w, "mm")
    )
  } else {
    vellum::vl_unit(1, "null")
  }
  scene <- vellum::push(
    scene,
    vellum::vl_viewport(
      layout = vellum::grid_layout(heights = heights, widths = widths)
    )
  )
  off <- if (m$show_title) 1L else 0L
  if (m$show_title) {
    scene <- vellum::push(scene, vellum::vl_viewport(row = 1, col = 1))
    scene <- .draw_guide_title(scene, cl$name, rt)
    scene <- vellum::pop(scene)
  }
  revb <- isTRUE(cl$reverse_bar)
  pal <- if (revb) rev(cl$pal256) else cl$pal256
  scene <- vellum::push(scene, vellum::vl_viewport(row = off + 1L, col = 1))
  grad <- vellum::linear_gradient(pal, x1 = 0, y1 = 0, x2 = 1, y2 = 0)
  bar <- vellum::rect_grob(
    x = vellum::vl_unit(0.5, "npc"),
    y = vellum::vl_unit(0.5, "npc"),
    width = vellum::vl_unit(1, "npc"),
    height = vellum::vl_unit(1, "npc"),
    gp = vellum::vl_gpar(fill = grad, col = "grey50", lwd = 0.5)
  )
  bar@meta <- .colorbar_meta(cl, revb, "h")
  scene <- vellum::draw(scene, bar)
  # White break ticks reaching up from the bar's bottom (label-side) edge.
  for (i in seq_along(cl$legend_breaks)) {
    frac <- scales::rescale(cl$legend_breaks[i], from = cl$range)
    if (revb) {
      frac <- 1 - frac
    }
    scene <- vellum::draw(
      scene,
      vellum::segments_grob(
        vellum::vl_unit(frac, "npc"),
        vellum::vl_unit(0, "npc"),
        vellum::vl_unit(frac, "npc"),
        vellum::vl_unit(.LEGEND_TICK_MM, "mm"),
        gp = vellum::vl_gpar(col = "white", lwd = 0.8)
      )
    )
  }
  scene <- vellum::pop(scene)
  scene <- vellum::push(scene, vellum::vl_viewport(row = off + 2L, col = 1))
  for (i in seq_along(cl$legend_breaks)) {
    frac <- scales::rescale(cl$legend_breaks[i], from = cl$range)
    if (revb) {
      frac <- 1 - frac
    }
    # Justify the end labels inward so they never spill past the bar ends.
    hjust <- if (frac <= 0.01) {
      "left"
    } else if (frac >= 0.99) {
      "right"
    } else {
      "centre"
    }
    scene <- vellum::draw(
      scene,
      vellum::text_grob(
        cl$legend_labels[i],
        x = vellum::vl_unit(frac, "npc"),
        y = vellum::vl_unit(0.5, "npc"),
        just = c(hjust, "centre"),
        gp = txt
      )
    )
  }
  scene <- vellum::pop(scene)
  if (has_na) {
    # NA swatch in the reserved column, on the bar row; "NA" label below it.
    cx <- vellum::vl_unit(m$spacing + m$key / 2, "mm")
    scene <- vellum::push(scene, vellum::vl_viewport(row = off + 1L, col = 2))
    scene <- vellum::draw(
      scene,
      vellum::rect_grob(
        x = cx,
        y = vellum::vl_unit(0.5, "npc"),
        width = vellum::vl_unit(m$key, "mm"),
        height = vellum::vl_unit(m$key, "mm"),
        gp = vellum::vl_gpar(fill = cl$na_value, col = NA)
      )
    )
    scene <- vellum::pop(scene)
    scene <- vellum::push(scene, vellum::vl_viewport(row = off + 2L, col = 2))
    scene <- vellum::draw(
      scene,
      vellum::text_grob(
        "NA",
        x = cx,
        y = vellum::vl_unit(0.5, "npc"),
        just = c("centre", "centre"),
        gp = txt
      )
    )
    scene <- vellum::pop(scene)
  }
  vellum::pop(scene)
}

# --- legend assembly --------------------------------------------------------
# Lay the guides out in the legend cell. A vertical (left/right) legend stacks
# the guides top-to-bottom in a mm-height block centred in the cell; a horizontal
# (top/bottom) legend packs them left-to-right in a mm-width block centred in the
# cell. Each guide is drawn to its own measured extent, so spacing is uniform and
# resize-stable regardless of how many guides share the legend.
.draw_legends <- function(
  scene,
  cell,
  guides,
  rt,
  orient = "vertical",
  avail_h = Inf
) {
  scene <- vellum::push(
    scene,
    vellum::vl_viewport(
      row = cell$row,
      col = cell$col,
      rowspan = cell$rowspan %||% 1,
      colspan = cell$colspan %||% 1,
      name = "legend"
    )
  )
  lb <- rt[["legend.background"]]
  if (!.is_blank(lb)) {
    scene <- vellum::draw(
      scene,
      vellum::rect_grob(gp = .el_gpar_rect(lb), sketch = .el_sketch(lb, 11L))
    )
  }
  m <- .legend_metrics(rt)
  n <- length(guides)
  if (!n) {
    return(vellum::pop(scene))
  }
  # Inset the content block by `legend.margin` (t, r, b, l) via a margin grid, so
  # everything below draws inside the middle (2, 2) cell.
  scene <- vellum::push(
    scene,
    vellum::vl_viewport(
      layout = vellum::grid_layout(
        widths = c(
          vellum::vl_unit(m$margin[4], "mm"),
          vellum::vl_unit(1, "null"),
          vellum::vl_unit(m$margin[2], "mm")
        ),
        heights = c(
          vellum::vl_unit(m$margin[1], "mm"),
          vellum::vl_unit(1, "null"),
          vellum::vl_unit(m$margin[3], "mm")
        )
      )
    )
  )
  scene <- vellum::push(scene, vellum::vl_viewport(row = 2, col = 2))
  if (orient == "horizontal") {
    ws <- vapply(guides, .guide_width_mm, 0, m = m)
    total <- sum(ws) + (n - 1) * m$spacing
    scene <- vellum::push(
      scene,
      vellum::vl_viewport(
        x = vellum::vl_unit(0.5, "npc"),
        width = vellum::vl_unit(total, "mm"),
        height = vellum::vl_unit(1, "npc"),
        layout = vellum::grid_layout(
          widths = .interleave_mm(ws, m$spacing),
          heights = vellum::vl_unit(1, "null")
        )
      )
    )
    for (i in seq_len(n)) {
      scene <- vellum::push(
        scene,
        vellum::vl_viewport(row = 1, col = 2L * i - 1L)
      )
      scene <- .draw_guide_h(scene, guides[[i]], m, rt)
      scene <- vellum::pop(scene)
    }
    scene <- vellum::pop(scene)
  } else {
    # Pack guides into as many columns as it takes to fit `avail_h` (the figure
    # height), so a tall stack wraps sideways instead of spilling off the top and
    # bottom of the page (#80). One column when avail_h is Inf -> unchanged.
    cols <- .legend_columns(guides, m, avail_h)
    ncol <- length(cols)
    colw <- vapply(
      cols,
      function(idx) max(vapply(guides[idx], .guide_col_width, 0, m = m)),
      0
    )
    scene <- vellum::push(
      scene,
      vellum::vl_viewport(
        layout = vellum::grid_layout(
          widths = .interleave_mm(colw, m$spacing),
          heights = vellum::vl_unit(1, "null")
        )
      )
    )
    for (c in seq_len(ncol)) {
      idx <- cols[[c]]
      hs <- vapply(guides[idx], .guide_height_mm, 0, m = m)
      total <- sum(hs) + (length(idx) - 1L) * m$spacing
      scene <- vellum::push(
        scene,
        vellum::vl_viewport(row = 1, col = 2L * c - 1L)
      )
      scene <- vellum::push(
        scene,
        vellum::vl_viewport(
          y = vellum::vl_unit(0.5, "npc"),
          width = vellum::vl_unit(1, "npc"),
          height = vellum::vl_unit(total, "mm"),
          layout = vellum::grid_layout(
            heights = .interleave_mm(hs, m$spacing),
            widths = vellum::vl_unit(1, "null")
          )
        )
      )
      for (i in seq_along(idx)) {
        scene <- vellum::push(
          scene,
          vellum::vl_viewport(row = 2L * i - 1L, col = 1)
        )
        scene <- .draw_guide_v(scene, guides[[idx[i]]], m, rt)
        scene <- vellum::pop(scene)
      }
      scene <- vellum::pop(scene) # this column's stack
      scene <- vellum::pop(scene) # this column's cell
    }
    scene <- vellum::pop(scene) # columns grid
  }
  scene <- vellum::pop(scene) # content (2, 2) cell
  scene <- vellum::pop(scene) # margin grid
  vellum::pop(scene) # legend cell
}
