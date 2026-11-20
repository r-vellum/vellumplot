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
  # `guides(x = "none")` drops the ticks; `guide_axis(minor.ticks = TRUE)` adds
  # shorter ones between the breaks.
  gxa <- .axis_guide_of(x_sc)
  gya <- .axis_guide_of(y_sc)
  if (identical(gxa$kind, "none")) {
    xb <- numeric(0)
  }
  if (identical(gya$kind, "none")) {
    yb <- numeric(0)
  }
  tx <- rt[["axis.ticks.x"]]
  ty0 <- rt[["axis.ticks.y"]]
  # guide_axis_logticks(): log10 ticks replace the plain major ticks.
  if (!is.null(gxa$logticks) && !isTRUE(x_sc$discrete)) {
    if (!.is_blank(tx)) {
      scene <- .draw_log_ticks(scene, x_sc, gxa$logticks, tlen, tx, "x")
    }
    xb <- numeric(0)
  }
  if (!is.null(gya$logticks) && !isTRUE(y_sc$discrete)) {
    if (!.is_blank(ty0)) {
      scene <- .draw_log_ticks(scene, y_sc, gya$logticks, tlen, ty0, "y")
    }
    yb <- numeric(0)
  }
  if (!.is_blank(tx) && isTRUE(gxa$minor.ticks) && !isTRUE(x_sc$discrete)) {
    scene <- .minor_ticks(scene, x_sc, tlen, tx, "x")
  }
  if (!.is_blank(ty0) && isTRUE(gya$minor.ticks) && !isTRUE(y_sc$discrete)) {
    scene <- .minor_ticks(scene, y_sc, tlen, ty0, "y")
  }
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

# Minor axis ticks (`guide_axis(minor.ticks = TRUE)`): three quarters of the
# major tick length, halfway between the breaks, inside the panel like the
# major ones.
.minor_ticks <- function(scene, sc, tlen, el, axis) {
  m <- .minor_breaks(sc$breaks)
  dom <- range(sc$domain)
  m <- m[m >= dom[1] & m <= dom[2]]
  k <- length(m)
  if (!k) {
    return(scene)
  }
  len <- vellum::vl_unit(rep(0.75 * tlen, k), "mm")
  pos <- vellum::vl_unit(m, "native")
  zero <- vellum::vl_unit(rep(0, k), "npc")
  g <- if (axis == "x") {
    vellum::segments_grob(
      pos,
      zero,
      pos,
      len,
      gp = .el_gpar_line(el),
      role = "presentation"
    )
  } else {
    vellum::segments_grob(
      zero,
      pos,
      len,
      pos,
      gp = .el_gpar_line(el),
      role = "presentation"
    )
  }
  vellum::draw(scene, g)
}

# Log10 axis ticks (`guide_axis_logticks()`), each `len` x the theme tick
# length, inside the panel like the plain ticks.
.draw_log_ticks <- function(scene, sc, lt, tlen, el, axis) {
  t <- .log_ticks(sc, lt)
  k <- length(t$pos)
  if (!k) {
    return(scene)
  }
  len <- vellum::vl_unit(t$len * tlen, "mm")
  pos <- vellum::vl_unit(t$pos, "native")
  zero <- vellum::vl_unit(rep(0, k), "npc")
  g <- if (axis == "x") {
    vellum::segments_grob(
      pos,
      zero,
      pos,
      len,
      gp = .el_gpar_line(el),
      role = "presentation"
    )
  } else {
    vellum::segments_grob(
      zero,
      pos,
      len,
      pos,
      gp = .el_gpar_line(el),
      role = "presentation"
    )
  }
  vellum::draw(scene, g)
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
  wrap_mm = NA_real_,
  keep = NULL
) {
  # A secondary axis keeps the plain guide; the primary one honours its
  # `guide_axis()` (angle, dodging, overlap, cap) or `guide_none()`.
  guide <- if (sec) .axis_guide_of(list()) else .axis_guide_of(sc)
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
    s <- .axis_line_seg(geom$seg, sc, guide$cap, is_y)
    scene <- vellum::draw(
      scene,
      vellum::segments_grob(
        s[[1]],
        s[[2]],
        s[[3]],
        s[[4]],
        sketch = .el_sketch(aline, geom$seed),
        gp = .el_gpar_line(aline)
      )
    )
  }
  if (!.is_blank(el) && !identical(guide$kind, "none")) {
    rot <- guide$angle %||% .el_rot(el)
    # A guide's angle sets the justification itself (the label's end meets the
    # tick), as ggplot2's guide_axis(angle =) does, rather than the element's.
    just <- if (is.null(guide$angle)) {
      .axis_text_just(geom, el, rot)
    } else {
      .axis_text_just(geom, .el_unjust(el), rot)
    }
    nd <- guide$n.dodge %||% 1L
    # Staggered rows / columns: row k sits (k - 1) label extents further from
    # the panel, measured as the layout measured the gutter.
    step_mm <- 0
    base_mm <- 0
    if (nd > 1L) {
      tick <- rt[["axis.ticks.length"]]
      probe <- .axis_text_grob(.longest(sc$labels), el, just, rot = rot)
      ext <- vellum::vl_convert(
        if (is_y) vellum::grobwidth(probe) else vellum::grobheight(probe),
        "mm"
      )
      step_mm <- ext + .DODGE_GAP_MM
      # the single-row gutter the default anchor fraction is relative to
      base_mm <- ext + tick + .PAD_MM
    }
    for (i in seq_along(sc$breaks)) {
      if (!is.finite(sc$breaks[i])) {
        next # a non-finite break has no position to anchor its label
      }
      if (!is.null(keep) && !isTRUE(keep[i])) {
        next # dropped by check.overlap
      }
      k <- (i - 1L) %% nd
      off <- if (is_y) {
        .axis_dodge_pos(geom$lx, k, base_mm, step_mm)
      } else {
        .axis_dodge_pos(geom$ly, k, base_mm, step_mm)
      }
      x <- if (is_y) off else vellum::vl_unit(sc$breaks[i], "native")
      y <- if (is_y) vellum::vl_unit(sc$breaks[i], "native") else off
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

# The anchor of a tick label in staggered row / column `k` (0-based) of a
# gutter whose labels sit at npc fraction `a`: `a` itself for the first row (the
# undodged position, unchanged), else measured in mm from the gutter edge the
# labels hang from (the high edge when `a > 0.5`), `k` steps further out.
.axis_dodge_pos <- function(a, k, base_mm, step_mm) {
  if (k == 0L || step_mm == 0) {
    return(vellum::vl_unit(a, "npc"))
  }
  if (a > 0.5) {
    vellum::vl_unit(1, "npc") -
      vellum::vl_unit((1 - a) * base_mm + k * step_mm, "mm")
  } else {
    vellum::vl_unit(a * base_mm + k * step_mm, "mm")
  }
}

# The axis line's endpoints (npc, or native along the axis when capped at the
# outermost breaks by `guide_axis(cap =)`).
.axis_line_seg <- function(seg, sc, cap, is_y) {
  u <- lapply(seg, vellum::vl_unit, units = "npc")
  b <- sc$breaks[is.finite(sc$breaks)]
  if (is.null(cap) || identical(cap, "none") || !length(b)) {
    return(u)
  }
  # the coordinate that runs along the axis: y (2, 4) on a y axis, else x (1, 3)
  lo_i <- if (is_y) 2L else 1L
  hi_i <- if (is_y) 4L else 3L
  dom <- sc$domain
  rev_dom <- length(dom) == 2L && is.numeric(dom) && dom[1] > dom[2]
  lo <- if (rev_dom) max(b) else min(b)
  hi <- if (rev_dom) min(b) else max(b)
  if (cap %in% c("both", "lower")) {
    u[[lo_i]] <- vellum::vl_unit(lo, "native")
  }
  if (cap %in% c("both", "upper")) {
    u[[hi_i]] <- vellum::vl_unit(hi, "native")
  }
  u
}

# A copy of a text element without its own hjust / vjust.
.el_unjust <- function(el) {
  el@hjust <- NULL
  el@vjust <- NULL
  el
}

# y-axis labels for `y_sc`, right-justified against each gridline.
.draw_y_axis <- function(scene, row, col, y_sc, rt, keep = NULL) {
  .draw_axis(scene, row, col, y_sc, rt, "y", keep = keep)
}

# x-axis labels for `x_sc`, centred under each gridline. `wrap_mm` is the mm
# width one tick gets, so a long label wraps to its column instead of colliding
# with its neighbour; `NA` (the default, and every unknown-width path) keeps
# single-line labels.
.draw_x_axis <- function(
  scene,
  row,
  col,
  x_sc,
  rt,
  wrap_mm = NA_real_,
  keep = NULL
) {
  .draw_axis(scene, row, col, x_sc, rt, "x", wrap_mm = wrap_mm, keep = keep)
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
      .strip_text_grob(label, rot, .el_gpar_text(st))
    )
  }
  vellum::pop(scene)
}

# Strip text. A multi-line label (from a labeller) centres each line: lines
# align within a text box, so the box is the label's own measured width (no
# re-wrapping).
.strip_text_grob <- function(label, rot, gp) {
  if (!grepl("\n", label, fixed = TRUE)) {
    return(vellum::text_grob(label, rot = rot, gp = gp))
  }
  w <- vellum::grobwidth(vellum::text_grob(label, gp = gp))
  vellum::text_grob(label, rot = rot, width = w, align = "centre", gp = gp)
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

# The list of guides a plot needs, in draw order -- the order of the legend rows
# of `.AES_REGISTRY` (colour, size, shape, pattern, edge width, line width,
# alpha, linetype, then the edge colour / alpha / linetype). Each guide is
# `list(kind, sc, key)` where `sc` is the trained scale (or, for a merged guide,
# a synthesized pseudo-scale), `kind` is the registry row's `guide`, resolved
# per scale kind for the colour guides, and `key` the registry row.
#
# Legends then merge as in ggplot2 (`.merge_legend_guides()`): two legends of
# registry rows marked `legend_merge` that share a title and identical keys
# draw as one legend whose keys carry every encoding. Give one scale a
# different `name=` to keep them apart.
.legend_guides <- function(scales) {
  out <- list()
  keys <- .aes_legend_keys()
  # A scale can opt out of its legend (an identity scale, or `guide = "none"`):
  # the mapping still applies to the marks, but no guide is drawn. Drop it here
  # so neither the standalone nor the merge path picks it up. This is a local
  # copy, so the emitters' own scales are unaffected.
  for (k in keys) {
    if (!is.null(scales[[k]]) && isTRUE(scales[[k]]$no_guide)) {
      scales[k] <- list(NULL)
    }
  }
  add <- function(kind, sc, key) {
    out <<- c(out, list(list(kind = kind, sc = sc, key = key)))
  }
  for (k in keys) {
    sc <- scales[[k]]
    if (is.null(sc)) {
      next
    }
    guide <- .AES_REGISTRY[[k]]$guide
    if (k %in% c("color", "fill")) {
      gk <- sc$type
      # A binned (classed) colour scale draws as discrete swatches with interval
      # labels, so it reuses the discrete guide -- unless guide_coloursteps()
      # asked for a segmented bar, which routes to its own stepped-bar drawer.
      discrete <- gk %in% c("discrete", "binned")
      if (!is.null(sc$bivariate)) {
        # A bivariate scale draws its 2-D grid and never merges with a partner.
        add("color_bivariate", sc, k)
      } else if (identical(gk, "binned") && isTRUE(sc$stepped)) {
        add("color_steps", sc, k)
      } else {
        add(paste0("color_", if (discrete) "discrete" else gk), sc, k)
      }
    } else if (k == "size" && isTRUE(sc$nested)) {
      # guide_legend(nested = TRUE) draws the size keys as concentric circles.
      add("size_nested", sc, k)
    } else if (identical(guide, "color")) {
      # Edge colour reuses the node colour renderers (the scale objects are
      # structurally identical) but never merges with shape/size -- it belongs to
      # the edge layer, whose key glyph is a line. When it is the same encoding
      # as the node colour scale (one variable colouring both), its guide would
      # duplicate the node swatch, so it folds into the node guide instead.
      if (.can_merge_edge_color(scales$color, sc)) {
        next
      }
      gk <- sc$type
      add(
        paste0(
          "color_",
          if (gk %in% c("discrete", "binned")) "discrete" else gk
        ),
        sc,
        k
      )
    } else {
      add(guide, sc, k)
    }
  }
  .merge_legend_guides(out)
}

# A node colour scale and an edge colour scale describe the same encoding when
# one variable colours both nodes/text and edges (e.g. `vdendrogram(k=)`). Both
# come from the same `.train_colour`, so their palette-defining fields are
# directly comparable; the registry names them (title, kind, and the fields that
# fix the palette and its breaks). When they match, the node colour guide
# already carries the whole encoding, so the edge guide is dropped rather than
# drawn as a redundant twin.
.can_merge_edge_color <- function(color, edge_color) {
  .aes_can_merge(edge_color, color, "edge_color", "color")
}

# The guide kinds that can join a merged legend: keyed legends drawn one key
# per row (a stepped bar, bubble stack or bivariate grid never merges). A
# continuous colour bar only finds a partner among the continuous keyed
# legends, since its keys are numeric breaks.
.MERGEABLE_GUIDE_KINDS <- c(
  "color_discrete",
  "color_continuous",
  "shape",
  "linetype",
  "size",
  "alpha",
  "linewidth"
)

# What two legends must share to merge (ggplot2's rule): the title, the key
# values (discrete levels or numeric breaks) and the labels shown, NA row
# included. NULL for a guide that cannot merge.
.legend_merge_sig <- function(g) {
  if (
    !isTRUE(.AES_REGISTRY[[g$key]]$legend_merge) ||
      !g$kind %in% .MERGEABLE_GUIDE_KINDS ||
      isTRUE(g$sc$bins)
  ) {
    return(NULL)
  }
  sc <- g$sc
  labels <- .guide_labels(g)
  if (identical(g$kind, "color_continuous") && isTRUE(sc$na)) {
    labels <- c(labels, "NA")
  }
  list(
    name = sc$name,
    values = sc$levels %||% sc$legend_breaks,
    labels = labels,
    # legends placed at different positions (`guide_legend(position =)`)
    # stay apart
    position = sc$guide_position
  )
}

# Merge legends that share a title and identical keys into one guide, placed
# where the first of them would have been drawn. Legends that differ only in
# title stay apart, and so do legends that share a title but not their keys
# (two variables given the same `labs()` title).
.merge_legend_guides <- function(guides) {
  sigs <- lapply(guides, .legend_merge_sig)
  used <- logical(length(guides))
  out <- list()
  for (i in seq_along(guides)) {
    if (used[i]) {
      next
    }
    members <- i
    if (!is.null(sigs[[i]])) {
      for (j in seq_along(guides)[-seq_len(i)]) {
        if (
          !used[j] &&
            identical(sigs[[i]], sigs[[j]]) &&
            .same_colourbar(guides[[i]], guides[[j]])
        ) {
          members <- c(members, j)
        }
      }
    }
    used[members] <- TRUE
    kinds <- vapply(guides[members], function(g) g$kind, character(1))
    out <- c(
      out,
      # A colour bar and a fill bar of one variable, title and palette are
      # one colour bar, as in ggplot2.
      if (length(members) == 1L || all(kinds == "color_continuous")) {
        guides[i]
      } else {
        list(list(
          kind = "merged",
          sc = .merged_legend(guides[members], sigs[[i]]),
          key = guides[[i]]$key,
          position = sigs[[i]]$position
        ))
      }
    )
  }
  out
}

# Two continuous colour legends (a colour and a fill bar) fold into one only when
# they also share their palette; any other pair is decided by the signature.
.same_colourbar <- function(a, b) {
  if (!identical(a$kind, "color_continuous") || a$kind != b$kind) {
    return(TRUE)
  }
  identical(a$sc$pal256, b$sc$pal256) && identical(a$sc$range, b$sc$range)
}

# Pseudo-scale for a merged legend: one row per shared key, carrying each
# member's per-key style -- `fills` and `cols` (the fill and the stroke colour),
# `shapes`, `sizes_mm`, `linetypes`, `alphas`, `linewidths` -- aligned to
# `labels` (NA rows padded with neutral values). A member that does not set a
# style leaves it NULL. The fill scale fills and the colour scale strokes; a
# lone colour member does both, unless its filled mark's constant other half
# (`key_style`, VP-46) supplies the outline or the fill. `outlined` / `filled`
# say the stroke / fill is its own colour. `glyph` picks the key drawing: a
# point, a line, both, a swatch under a line, or the colour scales' own swatch
# when nothing asks for a point or a line.
.merged_legend <- function(members, sig) {
  labels <- sig$labels
  k <- length(labels)
  pad <- function(v, fill) c(v, rep(fill, k - length(v)))
  style <- list()
  colour_glyph <- NULL
  fill_glyph <- NULL
  ks <- list()
  series_aes <- NULL
  levels <- NULL
  override <- list()
  for (g in members) {
    sc <- g$sc
    is_fill <- identical(g$key, "fill")
    paint <- switch(
      g$kind,
      color_discrete = pad(
        c(sc$colors, if (isTRUE(sc$na)) sc$na_value),
        "grey50"
      ),
      color_continuous = pad(
        c(sc$map(sig$values), if (isTRUE(sc$na)) sc$na_value),
        "grey50"
      )
    )
    if (!is.null(paint)) {
      glyph <- if (identical(g$kind, "color_continuous")) {
        "point"
      } else {
        sc$key_glyph
      }
      if (is_fill) {
        style$fills <- paint
        fill_glyph <- glyph
        ks$col <- ks$col %||% sc$key_style$col
      } else {
        style$cols <- paint
        colour_glyph <- glyph
        ks$fill <- ks$fill %||% sc$key_style$fill
      }
      if (identical(g$kind, "color_discrete")) {
        series_aes <- series_aes %||% (sc$series_aes %||% "color")
      }
    }
    switch(
      g$kind,
      shape = {
        style$shapes <- pad(sc$shapes, "circle")
        series_aes <- series_aes %||% "shape"
      },
      linetype = {
        style$linetypes <- pad(sc$linetypes, "solid")
      },
      size = {
        style$sizes_mm <- pad(sc$legend_sizes, NA)
      },
      alpha = {
        style$alphas <- pad(sc$legend_alphas, NA)
      },
      linewidth = {
        style$linewidths <- pad(sc$legend_widths, NA)
      }
    )
    if (is.character(sc$levels)) {
      levels <- levels %||% sc$levels
    }
    # guide_legend(override.aes=) of every member applies; the first to name
    # an aesthetic wins (the colour legend comes first).
    ov <- sc$override_aes %||% list()
    override <- c(override, ov[setdiff(names(ov), names(override))])
  }
  glyphs <- c(colour_glyph, fill_glyph)
  has_line <- !is.null(style$linetypes) ||
    !is.null(style$linewidths) ||
    "line" %in% glyphs
  has_point <- !is.null(style$shapes) ||
    !is.null(style$sizes_mm) ||
    any(glyphs %in% c("point", "circle")) ||
    (!length(glyphs) && !has_line)
  # the swatch the colour scales draw on their own: the fill's, when it has one
  swatch <- fill_glyph %||% colour_glyph
  glyph <- if (has_line && has_point) {
    "line+point"
  } else if (has_line && identical(fill_glyph, "square")) {
    "square+line"
  } else if (has_line) {
    "line"
  } else if (has_point) {
    "point"
  } else {
    swatch
  }
  outlined <- !is.null(style$cols) && !is.null(style$fills) ||
    is.null(style$cols) && !is.null(ks$col)
  filled <- !is.null(style$fills) || !is.null(ks$fill)
  fills <- style$fills %||%
    (if (!is.null(ks$fill)) rep_len(ks$fill, k)) %||%
    style$cols %||%
    rep_len(.geom_paint("grey35", "fill"), k)
  cols <- style$cols %||%
    (if (!is.null(ks$col)) rep_len(ks$col, k)) %||%
    fills
  list(
    name = sig$name,
    # The shared discrete key: `.legend_series_key()` tags the merged swatches
    # for interactive highlight (marks carry `color:<level>`/`shape:<level>`).
    levels = if (!is.null(series_aes)) levels,
    series_aes = series_aes,
    labels = labels,
    fills = fills,
    cols = cols,
    outlined = outlined,
    filled = filled && !is.null(style$cols),
    shapes = style$shapes,
    sizes_mm = style$sizes_mm,
    linetypes = style$linetypes,
    alphas = style$alphas,
    linewidths = style$linewidths,
    glyph = glyph,
    override_aes = if (length(override)) override
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
    # A stepped colour bar labels the N+1 bin boundaries (the numeric breaks),
    # not the interval strings a discrete binned legend shows.
    color_steps = .steps_break_labels(sc),
    color_discrete = {
      l <- sc$labels %||% sc$levels
      if (isTRUE(sc$na)) c(l, "NA") else l
    },
    # The bubble legend draws no NA key, so it reserves no "NA" label either.
    size_nested = sc$legend_labels,
    size = {
      l <- sc$legend_labels
      if (isTRUE(sc$na)) c(l, "NA") else l
    },
    shape = {
      l <- sc$labels %||% sc$levels
      if (isTRUE(sc$na)) c(l, "NA") else l
    },
    pattern = sc$levels,
    edge_width = ,
    linewidth = sc$legend_labels,
    alpha = sc$legend_labels,
    linetype = sc$labels %||% sc$levels,
    merged = sc$labels,
    color_bivariate = c(sc$bivariate$xlab, sc$bivariate$ylab)
  )
}

# The drawn diameter (mm) of a guide's key, used to size the row pitch / key
# column so the largest key never collides with its neighbours. `points_grob`
# sizes are radii, so a size key of radius r is 2r across.
.guide_key_d <- function(g, m) {
  sizes <- if (g$kind %in% c("size", "size_nested")) {
    g$sc$legend_sizes
  } else if (g$kind == "merged") {
    g$sc$sizes_mm
  } else {
    NULL
  }
  # A `guide_legend(override.aes = list(size = ))` forces the key marker size, so
  # the key cell must grow to hold it (else large keys overflow their rows).
  ov_size <- g$sc$override_aes[["size"]]
  cand <- c(
    if (length(sizes)) 2 * max(sizes),
    if (!is.null(ov_size)) 2 * ov_size,
    m$key
  )
  max(cand)
}

# The continuous colour-bar length (mm) for a vertical guide: long enough that
# consecutive break labels never touch.
.bar_len_mm <- function(g, m) {
  # guide_colourbar(barheight = ) fixes the bar length; else it auto-sizes so
  # consecutive break labels never touch.
  if (!is.null(g$sc$bar_height)) {
    return(g$sc$bar_height)
  }
  k <- length(.guide_labels(g))
  max(.LEGEND_MIN_BAR_MM, k * (m$text_h + m$lab_gap))
}

# The colourbar's thickness (mm): guide_colourbar(barwidth = ) or the theme default.
.bar_w_mm <- function(g, m) g$sc$bar_width %||% m$bar_w

# Head/foot room (mm) above and below a vertical colour bar. Break labels are
# centred on their ticks, so a label at a bar end overhangs it by half a text
# line -- without this the top label ran into the legend title, and the bottom
# one into the NA key or whatever follows the legend.
.bar_end_pad_mm <- function(m) m$text_h / 2

# The N+1 bin boundaries of a stepped colour bar, as labels. Each break is
# formatted on its own, in the colourbar's plain number style (no big mark, no
# scientific notation), so quantile breaks spanning orders of magnitude read
# "0.0002, 0.066, 2.32, 193, 806" rather than all padded to the smallest one's
# precision. Three significant digits, more only where two labels would
# otherwise read the same.
.steps_break_labels <- function(sc) {
  b <- as.numeric(sc$breaks)
  fmt <- function(digits) {
    vapply(
      b,
      function(x) {
        format(x, digits = digits, scientific = FALSE, trim = TRUE)
      },
      character(1)
    )
  }
  labs <- fmt(3L)
  for (d in 4:15) {
    if (!anyDuplicated(labs[!is.na(b)])) {
      break
    }
    labs <- fmt(d)
  }
  labs
}

# Measured height (mm) of a vertical guide: title line + key rows (uniform pitch
# = max(key diameter, text height) + gap), or title + colour bar (+ NA row).
.guide_height_mm <- function(g, m) {
  # A guide only reserves a title band when the theme shows titles AND this guide
  # actually has a name; otherwise a titleless guide would reserve an empty band
  # and its keys would sit lower than a titled sibling's (misaligned legends).
  if (g$kind == "color_bivariate") {
    return(.bi_geom(g, m)$height)
  }
  if (isTRUE(g$sc$bins)) {
    return(.bins_geom(g, m)$height)
  }
  m$show_title <- isTRUE(m$show_title) && !is.null(g$sc$name)
  th <- if (m$show_title) m$title_h + m$title_gap else 0
  if (g$kind %in% c("color_continuous", "color_steps")) {
    na <- if (isTRUE(g$sc$na)) max(m$key, m$text_h) + m$row_gap else 0
    th + .bar_len_mm(g, m) + 2 * .bar_end_pad_mm(m) + na
  } else if (g$kind == "size_nested") {
    # Concentric circles share one bottom baseline, so the stack is as tall as the
    # largest diameter, plus headroom for the top circle's label.
    th + 2 * max(g$sc$legend_sizes) + m$text_h
  } else {
    k <- length(.guide_labels(g))
    pitch <- max(.guide_key_d(g, m), m$text_h) + m$row_gap
    th + k * pitch
  }
}

# Measured width (mm) of a horizontal guide: the wider of its title and its key
# row (keys packed left-to-right, each followed by its own label).
.guide_width_mm <- function(g, m) {
  if (g$kind == "color_bivariate") {
    return(.bi_geom(g, m)$width)
  }
  if (isTRUE(g$sc$bins)) {
    return(.bins_geom(g, m, horizontal = TRUE)$width)
  }
  labs <- .guide_labels(g)
  tw <- if (m$show_title && !is.null(g$sc$name)) {
    .mm_tw_any(g$sc$name, m$title_fs)
  } else {
    0
  }
  if (g$kind %in% c("color_continuous", "color_steps")) {
    na <- if (isTRUE(g$sc$na)) {
      m$spacing + max(m$key, .mm_tw("NA", fs = m$text_fs))
    } else {
      0
    }
    # On a horizontal legend the bar length is `barwidth`; without one it auto-
    # sizes so the boundary labels never touch.
    body <- max(
      .LEGEND_MIN_BAR_MM,
      g$sc$bar_width %||%
        max(
          sum(vapply(labs, .mm_tw, 0, fs = m$text_fs) + m$lab_gap),
          .hbar_min_len_mm(g, m)
        )
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

# The shortest horizontal colour bar (mm) on which no two adjacent break labels
# touch. Labels sit at their values' positions along the bar (end labels
# justified inward, as the drawers do), so unevenly spaced breaks -- or an end
# label pulled inward next to its neighbour -- need more length than the summed
# label widths.
.hbar_min_len_mm <- function(g, m) {
  sc <- g$sc
  if (g$kind == "color_steps") {
    labs <- .steps_break_labels(sc)
    fr <- seq(0, 1, length.out = length(labs))
    ends <- c("left", rep("centre", max(length(labs) - 2L, 0L)), "right")
  } else {
    labs <- sc$legend_labels
    fr <- scales::rescale(sc$legend_breaks, from = sc$range)
    if (isTRUE(sc$reverse_bar)) {
      fr <- 1 - fr
    }
    ends <- ifelse(fr <= 0.01, "left", ifelse(fr >= 0.99, "right", "centre"))
  }
  if (length(labs) < 2L) {
    return(0)
  }
  w <- vapply(as.character(labs), .mm_tw, 0, fs = m$text_fs)
  o <- order(fr)
  fr <- fr[o]
  w <- w[o]
  ends <- ends[o]
  # Each label's extent either side of its anchor.
  lo <- ifelse(ends == "left", 0, ifelse(ends == "right", w, w / 2))
  hi <- w - lo
  i <- seq_len(length(fr) - 1L)
  gap <- fr[i + 1L] - fr[i]
  need <- (hi[i] + lo[i + 1L] + m$lab_gap) / gap
  max(need[is.finite(need) & gap > 0], 0)
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
.shape_key_grob <- function(shape, size_mm, fill, col, sk, alpha = NULL) {
  d <- .shape_svg_d(shape)
  if (!is.na(d)) {
    return(vellum::svg_grob(
      d,
      vellum::vl_unit(0.5, "npc"),
      vellum::vl_unit(0.5, "npc"),
      size = vellum::vl_unit(2 * size_mm, "mm"),
      gp = vellum::vl_gpar(fill = fill, col = col, alpha = alpha)
    ))
  }
  vellum::points_grob(
    vellum::vl_unit(0.5, "npc"),
    vellum::vl_unit(0.5, "npc"),
    size = vellum::vl_unit(size_mm, "mm"),
    shape = shape,
    sketch = sk,
    gp = vellum::vl_gpar(fill = fill, col = col, alpha = alpha)
  )
}

.key_grob <- function(g, i, m, sketch = NULL) {
  sc <- g$sc
  sk <- .sketch_bump(sketch, 200L + i)
  # `override.aes` (from guide_legend(override.aes=)) forces aesthetics on the
  # swatch only. Read each named entry with `%||%` so an absent one keeps the
  # data-driven default. `colour` was canonicalised to `color` in guide_legend().
  ov <- sc$override_aes %||% list()
  switch(
    g$kind,
    color_discrete = {
      cols <- sc$colors
      if (isTRUE(sc$na)) {
        cols <- c(cols, sc$na_value)
      }
      .colour_key_grob(sc$key_glyph, cols[i], m, sk, ov, sc$key_style)
    },
    size_nested = ,
    size = if (isTRUE(sc$na) && i > length(sc$legend_sizes)) {
      .na_key_grob(m, sk)
    } else {
      fill <- ov$fill %||% ov$color %||% .geom_paint("grey35", "fill")
      vellum::points_grob(
        vellum::vl_unit(0.5, "npc"),
        vellum::vl_unit(0.5, "npc"),
        size = vellum::vl_unit(ov$size %||% sc$legend_sizes[i], "mm"),
        shape = ov$shape %||% sc$key_glyph %||% "circle",
        sketch = sk,
        gp = vellum::vl_gpar(
          fill = fill,
          col = ov$color %||% .geom_paint("grey35"),
          alpha = ov$alpha
        )
      )
    },
    shape = if (isTRUE(sc$na) && i > length(sc$shapes)) {
      .na_key_grob(m, sk)
    } else {
      .shape_key_grob(
        ov$shape %||% sc$shapes[i],
        ov$size %||% (m$key / 2),
        ov$fill %||% ov$color %||% .geom_paint("grey35", "fill"),
        ov$color %||% .geom_paint("grey35"),
        sk,
        ov$alpha
      )
    },
    pattern = vellum::rect_grob(
      vellum::vl_unit(0.5, "npc"),
      vellum::vl_unit(0.5, "npc"),
      width = vellum::vl_unit(0.86, "npc"),
      height = vellum::vl_unit(0.86, "npc"),
      sketch = sk,
      gp = vellum::vl_gpar(fill = sc$patterns[[i]], col = "grey55")
    ),
    edge_width = ,
    linewidth = vellum::segments_grob(
      vellum::vl_unit(0.12, "npc"),
      vellum::vl_unit(0.5, "npc"),
      vellum::vl_unit(0.88, "npc"),
      vellum::vl_unit(0.5, "npc"),
      sketch = sk,
      gp = vellum::vl_gpar(
        col = ov$color %||% .geom_paint("grey35"),
        lwd = sc$legend_widths[i],
        alpha = ov$alpha
      )
    ),
    alpha = vellum::points_grob(
      vellum::vl_unit(0.5, "npc"),
      vellum::vl_unit(0.5, "npc"),
      size = vellum::vl_unit(ov$size %||% (m$key / 2), "mm"),
      shape = ov$shape %||% "circle",
      sketch = sk,
      gp = vellum::vl_gpar(
        fill = ov$fill %||% ov$color %||% .geom_paint("grey20", "fill"),
        col = NA,
        alpha = ov$alpha %||% sc$legend_alphas[i]
      )
    ),
    linetype = vellum::segments_grob(
      vellum::vl_unit(0.12, "npc"),
      vellum::vl_unit(0.5, "npc"),
      vellum::vl_unit(0.88, "npc"),
      vellum::vl_unit(0.5, "npc"),
      sketch = sk,
      gp = vellum::vl_gpar(
        col = ov$color %||% .geom_paint("grey35"),
        lwd = ov$linewidth %||% 1.5,
        lty = ov$linetype %||% sc$linetypes[i],
        alpha = ov$alpha
      )
    ),
    merged = .merged_key_grob(sc, i, m, sk, ov)
  )
}

# The key of a merged legend row: every member's encoding in one key, drawn as
# a point (colour, shape, size), a line (colour, linetype, linewidth), both
# stacked, or the colour scale's own swatch. Alpha applies to whichever is
# drawn. Returns one grob, or a list of grobs for a line-and-point key.
.merged_key_grob <- function(sc, i, m, sk, ov) {
  pick <- function(v, default) {
    x <- if (length(v) >= i) v[[i]] else NULL
    if (is.null(x) || is.na(x)) default else x
  }
  alpha <- ov$alpha %||% pick(sc$alphas, NULL)
  col <- ov$color %||% sc$cols[i]
  point <- function() {
    .shape_key_grob(
      ov$shape %||% pick(sc$shapes, "circle"),
      ov$size %||% pick(sc$sizes_mm, m$key / 2),
      ov$fill %||% ov$color %||% sc$fills[i],
      col,
      sk,
      alpha
    )
  }
  line <- function() {
    vellum::segments_grob(
      vellum::vl_unit(0.12, "npc"),
      vellum::vl_unit(0.5, "npc"),
      vellum::vl_unit(0.88, "npc"),
      vellum::vl_unit(0.5, "npc"),
      sketch = sk,
      gp = vellum::vl_gpar(
        col = col,
        lwd = ov$linewidth %||% pick(sc$linewidths, 1.5),
        lty = ov$linetype %||% pick(sc$linetypes, "solid"),
        alpha = alpha
      )
    )
  }
  # the colour scales' own swatch: an outlined box keeps its stroke colour
  # (and a fill of its own), any other glyph is filled and maybe outlined
  swatch <- function(glyph) {
    ova <- utils::modifyList(ov, list(alpha = alpha))
    if (identical(glyph, "outline")) {
      ks <- if (isTRUE(sc$filled)) list(fill = sc$fills[i])
      .colour_key_grob(glyph, sc$cols[i], m, sk, ova, ks)
    } else {
      ks <- if (isTRUE(sc$outlined)) list(col = sc$cols[i])
      .colour_key_grob(glyph, sc$fills[i], m, sk, ova, ks)
    }
  }
  switch(
    sc$glyph %||% "point",
    point = point(),
    line = line(),
    `line+point` = list(line(), point()),
    `square+line` = list(swatch("square"), line()),
    swatch(sc$glyph)
  )
}

# A discrete-colour key drawn as the mark's glyph: a filled point for point/glyph
# marks, a short line for line marks, else the default filled square swatch.
# `ks` (`.key_style_for_aes()`) is a filled mark's constant other half: a
# `col` outlines the square / point swatch, a `fill` fills the outlined box.
.colour_key_grob <- function(
  glyph,
  col,
  m,
  sketch = NULL,
  ov = list(),
  ks = NULL
) {
  # `override.aes` forces key aesthetics: fill/stroke take the overridden colour,
  # a point key its overridden size/shape/alpha, a line key its linewidth. The
  # colour itself defaults to the data-driven `col`.
  fill <- ov$fill %||% ov$color %||% ks$fill %||% col
  line_col <- ov$color %||% col
  # the swatch outline: a constant `color` around a mapped fill, or the mapped
  # colour around a constant fill (a point key; the square becomes "outline")
  edge <- ov$color %||% ks$col %||% (if (!is.null(ks$fill)) col)
  alpha <- ov$alpha
  glyph <- if (!is.null(ov$shape)) "point" else (glyph %||% "square")
  switch(
    glyph,
    point = ,
    circle = vellum::points_grob(
      vellum::vl_unit(0.5, "npc"),
      vellum::vl_unit(0.5, "npc"),
      size = vellum::vl_unit(ov$size %||% (m$key / 2), "mm"),
      shape = ov$shape %||% "circle",
      sketch = sketch,
      gp = vellum::vl_gpar(
        fill = fill,
        col = if (is.null(ks)) NA else edge,
        alpha = alpha
      )
    ),
    line = vellum::segments_grob(
      vellum::vl_unit(0.12, "npc"),
      vellum::vl_unit(0.5, "npc"),
      vellum::vl_unit(0.88, "npc"),
      vellum::vl_unit(0.5, "npc"),
      sketch = sketch,
      gp = vellum::vl_gpar(
        col = line_col,
        lwd = ov$linewidth %||% 2,
        alpha = alpha
      )
    ),
    # An outlined box: the key of a box plot / violin coloured through `color`.
    outline = vellum::rect_grob(
      width = vellum::vl_unit(m$key * 0.8, "mm"),
      height = vellum::vl_unit(m$key * 0.8, "mm"),
      sketch = sketch,
      gp = vellum::vl_gpar(
        fill = ov$fill %||% ks$fill %||% .geom_paint("white", "fill"),
        col = line_col,
        lwd = ov$linewidth %||% 1.5,
        alpha = alpha
      )
    ),
    vellum::rect_grob(
      width = vellum::vl_unit(m$key, "mm"),
      height = vellum::vl_unit(m$key, "mm"),
      sketch = sketch,
      gp = if (is.null(ks$col)) {
        vellum::vl_gpar(fill = fill, col = NA, alpha = alpha)
      } else {
        vellum::vl_gpar(fill = fill, col = edge, lwd = 1, alpha = alpha)
      }
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
    # a fill legend beside a separate colour legend keys "fill:<level>"
    color_discrete = ,
    color_bivariate = g$sc$series_aes %||% "color",
    shape = "shape",
    # A merged legend over a discrete colour or shape scale joins that series
    # (marks carry `color:<value>`/`shape:<value>`); one with neither (say,
    # continuous colour + size) has no discrete series and stays untagged.
    merged = g$sc$series_aes,
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

# Draw legend key `i` of guide `g` into the current viewport, tagging each of
# its grobs (a merged key may be a line and a point) for interactive highlight.
.draw_key <- function(scene, g, i, m, rt, label) {
  grobs <- .key_grob(g, i, m, rt[[".sketch"]])
  if (S7::S7_inherits(grobs)) {
    grobs <- list(grobs)
  }
  for (grob in grobs) {
    scene <- vellum::draw(
      scene,
      .tag_legend_swatch(grob, g, i, label, rt[[".interactive"]])
    )
  }
  scene
}

.draw_guide_v <- function(scene, g, m, rt) {
  # Reserve the title band only when this guide has a name (see .guide_height_mm);
  # the normalised `m` also flows to the continuous drawer and its `th`.
  m$show_title <- isTRUE(m$show_title) && !is.null(g$sc$name)
  txt <- .el_gpar_text(rt[["legend.text"]])
  th <- if (m$show_title) m$title_h + m$title_gap else 0
  if (isTRUE(g$sc$bins)) {
    return(.draw_guide_bins_v(scene, g, m, rt))
  }
  if (g$kind == "color_continuous") {
    return(.draw_guide_continuous_v(scene, g, m, rt, txt, th))
  }
  if (g$kind == "color_steps") {
    return(.draw_guide_steps_v(scene, g, m, rt, txt, th))
  }
  if (g$kind == "size_nested") {
    return(.draw_guide_bubbles_v(scene, g, m, rt, txt, th))
  }
  if (g$kind == "color_bivariate") {
    return(.draw_guide_bivariate(scene, g, m, rt))
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
    scene <- .draw_key(scene, g, i, m, rt, labs[i])
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
  pad <- vellum::vl_unit(.bar_end_pad_mm(m), "mm")
  heights <- .c_units(
    if (m$show_title) vellum::vl_unit(th, "mm"),
    pad,
    vellum::vl_unit(1, "null"),
    pad,
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
  row_bar <- if (m$show_title) 3L else 2L
  if (m$show_title) {
    scene <- vellum::push(scene, vellum::vl_viewport(row = 1, col = 1))
    scene <- .draw_guide_title(scene, cl$name, rt)
    scene <- vellum::pop(scene)
  }
  scene <- vellum::push(scene, vellum::vl_viewport(row = row_bar, col = 1))
  # guide_colourbar() tunables: bar thickness, whether/what-colour the break ticks
  # are, and which side the labels sit. `%||% <default>` so a bar with no explicit
  # guide_colourbar() renders exactly as before.
  bw <- cl$bar_width %||% m$bar_w
  ticks_on <- cl$bar_ticks %||% TRUE
  tick_col <- cl$bar_ticks_colour %||% "white"
  left_lab <- identical(cl$bar_label_pos, "left")
  lw <- if (left_lab) .mm_tw(cl$legend_labels, m$text_fs) else 0
  x0 <- m$pad + if (left_lab) lw + m$lab_gap else 0 # bar's left edge (mm)
  lab_x <- if (left_lab) x0 - m$lab_gap else x0 + bw + m$lab_gap
  lab_just <- c(if (left_lab) "right" else "left", "centre")
  # guide_legend(reverse = TRUE) flips a continuous bar by reversing the gradient
  # and mirroring every value position, keeping each value paired with its label.
  revb <- isTRUE(cl$reverse_bar)
  pal <- if (revb) rev(cl$pal256) else cl$pal256
  grad <- vellum::linear_gradient(pal, x1 = 0, y1 = 0, x2 = 0, y2 = 1)
  bar <- vellum::rect_grob(
    x = vellum::vl_unit(x0 + bw / 2, "mm"),
    y = vellum::vl_unit(0.5, "npc"),
    width = vellum::vl_unit(bw, "mm"),
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
    # A break tick reaching in from the bar's label-side edge.
    if (ticks_on) {
      tx <- if (left_lab) {
        c(x0, x0 + .LEGEND_TICK_MM)
      } else {
        c(x0 + bw - .LEGEND_TICK_MM, x0 + bw)
      }
      scene <- vellum::draw(
        scene,
        vellum::segments_grob(
          vellum::vl_unit(tx[1], "mm"),
          vellum::vl_unit(frac, "npc"),
          vellum::vl_unit(tx[2], "mm"),
          vellum::vl_unit(frac, "npc"),
          gp = vellum::vl_gpar(col = tick_col, lwd = 0.8)
        )
      )
    }
    scene <- vellum::draw(
      scene,
      vellum::text_grob(
        cl$legend_labels[i],
        x = vellum::vl_unit(lab_x, "mm"),
        y = vellum::vl_unit(frac, "npc"),
        just = lab_just,
        gp = txt
      )
    )
  }
  scene <- vellum::pop(scene)
  if (has_na) {
    scene <- vellum::push(
      scene,
      vellum::vl_viewport(row = row_bar + 2L, col = 1)
    )
    scene <- vellum::draw(
      scene,
      vellum::rect_grob(
        x = vellum::vl_unit(x0 + bw / 2, "mm"),
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
        x = vellum::vl_unit(lab_x, "mm"),
        y = vellum::vl_unit(0.5, "npc"),
        just = lab_just,
        gp = txt
      )
    )
    scene <- vellum::pop(scene)
  }
  vellum::pop(scene)
}

# A stepped (segmented) colour bar for a binned scale under guide_coloursteps():
# N equal segments in the bin colours, the N+1 bin boundaries labelled at the
# segment edges. Honours the same barwidth / barheight / ticks / label.position
# tunables as the smooth bar (ticks default off for a stepped bar).
.draw_guide_steps_v <- function(scene, g, m, rt, txt, th) {
  cl <- g$sc
  revb <- isTRUE(cl$reverse_bar)
  cols <- if (revb) rev(cl$colors) else cl$colors
  brk <- if (revb) rev(cl$breaks) else cl$breaks
  blabs <- .steps_break_labels(list(breaks = brk))
  n <- length(cols)
  has_na <- isTRUE(cl$na)
  na_h <- max(m$key, m$text_h) + m$row_gap
  pad <- vellum::vl_unit(.bar_end_pad_mm(m), "mm")
  heights <- .c_units(
    if (m$show_title) vellum::vl_unit(th, "mm"),
    pad,
    vellum::vl_unit(1, "null"),
    pad,
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
  row_bar <- if (m$show_title) 3L else 2L
  if (m$show_title) {
    scene <- vellum::push(scene, vellum::vl_viewport(row = 1, col = 1))
    scene <- .draw_guide_title(scene, cl$name, rt)
    scene <- vellum::pop(scene)
  }
  scene <- vellum::push(scene, vellum::vl_viewport(row = row_bar, col = 1))
  bw <- cl$bar_width %||% m$bar_w
  ticks_on <- cl$bar_ticks %||% FALSE
  tick_col <- cl$bar_ticks_colour %||% "white"
  left_lab <- identical(cl$bar_label_pos, "left")
  lw <- if (left_lab) .mm_tw(blabs, m$text_fs) else 0
  x0 <- m$pad + if (left_lab) lw + m$lab_gap else 0
  lab_x <- if (left_lab) x0 - m$lab_gap else x0 + bw + m$lab_gap
  lab_just <- c(if (left_lab) "right" else "left", "centre")
  for (i in seq_len(n)) {
    scene <- vellum::draw(
      scene,
      vellum::rect_grob(
        x = vellum::vl_unit(x0 + bw / 2, "mm"),
        y = vellum::vl_unit((i - 0.5) / n, "npc"),
        width = vellum::vl_unit(bw, "mm"),
        height = vellum::vl_unit(1 / n, "npc"),
        gp = vellum::vl_gpar(fill = cols[i], col = "grey50", lwd = 0.5)
      )
    )
  }
  for (j in 0:n) {
    frac <- j / n
    if (ticks_on && j > 0L && j < n) {
      tx <- if (left_lab) {
        c(x0, x0 + .LEGEND_TICK_MM)
      } else {
        c(x0 + bw - .LEGEND_TICK_MM, x0 + bw)
      }
      scene <- vellum::draw(
        scene,
        vellum::segments_grob(
          vellum::vl_unit(tx[1], "mm"),
          vellum::vl_unit(frac, "npc"),
          vellum::vl_unit(tx[2], "mm"),
          vellum::vl_unit(frac, "npc"),
          gp = vellum::vl_gpar(col = tick_col, lwd = 0.8)
        )
      )
    }
    scene <- vellum::draw(
      scene,
      vellum::text_grob(
        blabs[j + 1L],
        x = vellum::vl_unit(lab_x, "mm"),
        y = vellum::vl_unit(frac, "npc"),
        just = lab_just,
        gp = txt
      )
    )
  }
  scene <- vellum::pop(scene)
  if (has_na) {
    # A grey NA swatch below the bar, mirroring the smooth colour bar.
    scene <- vellum::push(
      scene,
      vellum::vl_viewport(row = row_bar + 2L, col = 1)
    )
    scene <- vellum::draw(
      scene,
      vellum::rect_grob(
        x = vellum::vl_unit(x0 + bw / 2, "mm"),
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
        x = vellum::vl_unit(lab_x, "mm"),
        y = vellum::vl_unit(0.5, "npc"),
        just = lab_just,
        gp = txt
      )
    )
    scene <- vellum::pop(scene)
  }
  vellum::pop(scene)
}

# A proportional-symbol ("nested circle") size legend: the size keys drawn as
# concentric, bottom-aligned hollow circles with a leader from each circle's top
# to its label. Compact -- one big-circle's height rather than a stacked row per
# break -- and reads size directly. Drawn for a vertical legend; a horizontal one
# falls back to the stacked size keys.
.draw_guide_bubbles_v <- function(scene, g, m, rt, txt, th) {
  cl <- g$sc
  sizes <- cl$legend_sizes # radii, mm
  labs <- cl$legend_labels
  n <- length(sizes)
  bigr <- max(sizes)
  h <- 2 * bigr + m$text_h # bubble-area height (mm)
  xc <- m$pad + bigr # circle centre x (mm)
  label_x <- m$pad + 2 * bigr + m$lab_gap # label column x (mm)
  heights <- .c_units(
    if (m$show_title) vellum::vl_unit(th, "mm"),
    vellum::vl_unit(h, "mm")
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
  row_bub <- if (m$show_title) 2L else 1L
  if (m$show_title) {
    scene <- vellum::push(scene, vellum::vl_viewport(row = 1, col = 1))
    scene <- .draw_guide_title(scene, cl$name, rt)
    scene <- vellum::pop(scene)
  }
  scene <- vellum::push(scene, vellum::vl_viewport(row = row_bub, col = 1))
  # Circles bottom-aligned (centre at radius above the baseline), largest first so
  # the smaller outlines stay visible on top.
  for (i in order(sizes, decreasing = TRUE)) {
    scene <- vellum::draw(
      scene,
      vellum::points_grob(
        vellum::vl_unit(xc, "mm"),
        vellum::vl_unit(sizes[i] / h, "npc"),
        size = vellum::vl_unit(sizes[i], "mm"),
        shape = "circle",
        gp = vellum::vl_gpar(fill = NA, col = .geom_paint("grey35"), lwd = 0.7)
      )
    )
  }
  # A leader from each circle's top edge out to its label.
  for (i in seq_len(n)) {
    top <- 2 * sizes[i] / h
    scene <- vellum::draw(
      scene,
      vellum::segments_grob(
        vellum::vl_unit(xc, "mm"),
        vellum::vl_unit(top, "npc"),
        vellum::vl_unit(label_x - m$lab_gap / 2, "mm"),
        vellum::vl_unit(top, "npc"),
        gp = vellum::vl_gpar(col = "grey70", lwd = 0.4)
      )
    )
    scene <- vellum::draw(
      scene,
      vellum::text_grob(
        labs[i],
        x = vellum::vl_unit(label_x, "mm"),
        y = vellum::vl_unit(top, "npc"),
        just = c("left", "centre"),
        gp = txt
      )
    )
  }
  scene <- vellum::pop(scene)
  vellum::pop(scene)
}

# --- horizontal guide drawers (top/bottom legend) ---------------------------

# Draw a horizontal guide into the current viewport (its exact measured mm width).
.draw_guide_h <- function(scene, g, m, rt) {
  # Reserve the title band only when this guide has a name (see .guide_height_mm).
  m$show_title <- isTRUE(m$show_title) && !is.null(g$sc$name)
  txt <- .el_gpar_text(rt[["legend.text"]])
  th <- if (m$show_title) m$title_h + m$title_gap else 0
  if (isTRUE(g$sc$bins)) {
    return(.draw_guide_bins_h(scene, g, m, rt))
  }
  if (g$kind == "color_continuous") {
    return(.draw_guide_continuous_h(scene, g, m, rt, txt, th))
  }
  if (g$kind == "color_steps") {
    return(.draw_guide_steps_h(scene, g, m, rt, txt, th))
  }
  if (g$kind == "color_bivariate") {
    return(.draw_guide_bivariate(scene, g, m, rt))
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
    scene <- .draw_key(scene, g, i, m, rt, labs[i])
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
  # A horizontal bar's thickness is its vertical extent, so guide_colourbar()'s
  # `barheight` sets it (mirroring `barwidth` on the vertical bar). Ticks honour
  # the guide's on/off + colour. `barwidth` (the bar length) and top/bottom label
  # repositioning are honoured on the default vertical colourbar.
  bar_thick <- cl$bar_height %||% m$bar_w
  ticks_on <- cl$bar_ticks %||% TRUE
  tick_col <- cl$bar_ticks_colour %||% "white"
  heights <- .c_units(
    if (m$show_title) vellum::vl_unit(th, "mm"),
    vellum::vl_unit(bar_thick, "mm"),
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
  # Break positions along the bar (0..1), shared by the tick and label loops.
  fracs <- scales::rescale(cl$legend_breaks, from = cl$range)
  if (revb) {
    fracs <- 1 - fracs
  }
  # Break ticks reaching up from the bar's bottom (label-side) edge.
  for (i in seq_along(cl$legend_breaks)) {
    frac <- fracs[i]
    if (ticks_on) {
      scene <- vellum::draw(
        scene,
        vellum::segments_grob(
          vellum::vl_unit(frac, "npc"),
          vellum::vl_unit(0, "npc"),
          vellum::vl_unit(frac, "npc"),
          vellum::vl_unit(.LEGEND_TICK_MM, "mm"),
          gp = vellum::vl_gpar(col = tick_col, lwd = 0.8)
        )
      )
    }
  }
  scene <- vellum::pop(scene)
  scene <- vellum::push(scene, vellum::vl_viewport(row = off + 2L, col = 1))
  for (i in seq_along(cl$legend_breaks)) {
    frac <- fracs[i]
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

# A horizontal stepped colour bar (guide_coloursteps() under a top/bottom legend):
# N equal segments left-to-right, the N+1 boundaries labelled below.
.draw_guide_steps_h <- function(scene, g, m, rt, txt, th) {
  cl <- g$sc
  revb <- isTRUE(cl$reverse_bar)
  cols <- if (revb) rev(cl$colors) else cl$colors
  brk <- if (revb) rev(cl$breaks) else cl$breaks
  blabs <- .steps_break_labels(list(breaks = brk))
  n <- length(cols)
  bar_thick <- cl$bar_height %||% m$bar_w
  ticks_on <- cl$bar_ticks %||% FALSE
  tick_col <- cl$bar_ticks_colour %||% "white"
  has_na <- isTRUE(cl$na)
  na_w <- if (has_na) max(m$key, .mm_tw("NA", fs = m$text_fs)) else 0
  heights <- .c_units(
    if (m$show_title) vellum::vl_unit(th, "mm"),
    vellum::vl_unit(bar_thick, "mm"),
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
      layout = vellum::grid_layout(
        heights = heights,
        widths = widths
      )
    )
  )
  off <- if (m$show_title) 1L else 0L
  if (m$show_title) {
    scene <- vellum::push(scene, vellum::vl_viewport(row = 1, col = 1))
    scene <- .draw_guide_title(scene, cl$name, rt)
    scene <- vellum::pop(scene)
  }
  scene <- vellum::push(scene, vellum::vl_viewport(row = off + 1L, col = 1))
  for (i in seq_len(n)) {
    scene <- vellum::draw(
      scene,
      vellum::rect_grob(
        x = vellum::vl_unit((i - 0.5) / n, "npc"),
        y = vellum::vl_unit(0.5, "npc"),
        width = vellum::vl_unit(1 / n, "npc"),
        height = vellum::vl_unit(1, "npc"),
        gp = vellum::vl_gpar(fill = cols[i], col = "grey50", lwd = 0.5)
      )
    )
    if (ticks_on && i < n) {
      scene <- vellum::draw(
        scene,
        vellum::segments_grob(
          vellum::vl_unit(i / n, "npc"),
          vellum::vl_unit(0, "npc"),
          vellum::vl_unit(i / n, "npc"),
          vellum::vl_unit(.LEGEND_TICK_MM, "mm"),
          gp = vellum::vl_gpar(col = tick_col, lwd = 0.8)
        )
      )
    }
  }
  scene <- vellum::pop(scene)
  scene <- vellum::push(scene, vellum::vl_viewport(row = off + 2L, col = 1))
  for (j in 0:n) {
    frac <- j / n
    hjust <- if (j == 0L) {
      "left"
    } else if (j == n) {
      "right"
    } else {
      "centre"
    }
    scene <- vellum::draw(
      scene,
      vellum::text_grob(
        blabs[j + 1L],
        x = vellum::vl_unit(frac, "npc"),
        y = vellum::vl_unit(0.5, "npc"),
        just = c(hjust, "centre"),
        gp = txt
      )
    )
  }
  scene <- vellum::pop(scene)
  if (has_na) {
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

# Justify a legend block of `size_mm` along one axis ("x" / "y") of the current
# cell: `just` 0 is the left / bottom edge, 1 the right / top, 0.5 (the
# default, and what every legend area uses) centred -- which pushes nothing, so
# a centred legend draws exactly as before. Otherwise a three-track grid (the
# slack split `just : 1 - just` around the block) leaves the cursor in a cell
# exactly `size_mm` long. Undo with `.pop_justified()`.
.push_justified <- function(scene, size_mm, just, axis) {
  if (isTRUE(all.equal(just, 0.5))) {
    return(scene)
  }
  # rows run top to bottom, so on y the slack before the block is `1 - just`
  before <- if (axis == "x") just else 1 - just
  tracks <- c(
    vellum::vl_unit(before, "null"),
    vellum::vl_unit(size_mm, "mm"),
    vellum::vl_unit(1 - before, "null")
  )
  scene <- vellum::push(
    scene,
    vellum::vl_viewport(
      layout = if (axis == "x") {
        vellum::grid_layout(widths = tracks)
      } else {
        vellum::grid_layout(heights = tracks)
      }
    )
  )
  vellum::push(
    scene,
    vellum::vl_viewport(
      row = if (axis == "x") 1L else 2L,
      col = if (axis == "x") 2L else 1L
    )
  )
}
.pop_justified <- function(scene, just) {
  if (isTRUE(all.equal(just, 0.5))) {
    return(scene)
  }
  vellum::pop(vellum::pop(scene))
}
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
  avail_h = Inf,
  name = "legend",
  just = 0.5
) {
  scene <- vellum::push(
    scene,
    vellum::vl_viewport(
      row = cell$row,
      col = cell$col,
      rowspan = cell$rowspan %||% 1,
      colspan = cell$colspan %||% 1,
      name = name
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
    scene <- .push_justified(scene, total, just, "x")
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
    scene <- .pop_justified(scene, just)
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
      scene <- .push_justified(scene, total, just, "y")
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
      scene <- .pop_justified(scene, just)
      scene <- vellum::pop(scene) # this column's cell
    }
    scene <- vellum::pop(scene) # columns grid
  }
  scene <- vellum::pop(scene) # content (2, 2) cell
  scene <- vellum::pop(scene) # margin grid
  vellum::pop(scene) # legend cell
}
