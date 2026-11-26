#' @include classes.R
NULL

# Mark emitters: interval marks (errorbar, linerange, segment).

# The orientation of a range mark (errorbar / linerange / pointrange): `"x"`
# for vertical ranges at each `x` (`ymin`..`ymax`), `"y"` for horizontal ones at
# each `y` (`xmin`..`xmax`, ggplot2's `geom_errorbarh()`). An explicit
# `orientation` wins; otherwise, as in ggplot2, a layer with an `xmin`/`xmax`
# range and no `ymin`/`ymax` is horizontal.
.range_orient <- function(L) {
  o <- L$stat_params$orientation
  if (length(o) == 1L && !is.na(o)) {
    return(o)
  }
  v <- L$values
  has_x <- !is.null(v$xmin) || !is.null(v$xmax)
  has_y <- !is.null(v$ymin) || !is.null(v$ymax)
  if (has_x && !has_y) "y" else "x"
}

# Error bars from ymin to ymax at each x (with caps across the band), or from
# xmin to xmax at each y when horizontal (see `.range_orient()`).
.emit_errorbar <- function(scene, L, scales, caps = TRUE, keep = NULL) {
  n <- L$n
  orient <- .range_orient(L)
  ca <- if (identical(orient, "y")) "y" else "x"
  va <- if (identical(orient, "y")) "x" else "y"
  xn <- rep_len(scales[[ca]]$map(L$values[[ca]]), n)
  ymin <- rep_len(scales[[va]]$map(L$values[[paste0(va, "min")]]), n)
  ymax <- rep_len(scales[[va]]$map(L$values[[paste0(va, "max")]]), n)
  col <- rep_len(.aes_colour(L, scales, "black"), n)
  alpha <- rep_len(.aes_alpha(L, scales, NA_real_), n)
  lwd <- .aes_param(L, "linewidth", 1)
  lty <- .resolve_lty(L, scales, n)
  band <- scales[[ca]]$band_width %||% .resolution(xn)
  half <- .errorbar_width(L, band, n) / 2
  sk <- .mark_sketch(L, scales)

  gi <- 0L
  for (idx in .style_groups(n, list(col = col, alpha = alpha, lty = lty))) {
    # `keep`: the rows a caller draws (a point-range leaves out a missing size)
    if (!is.null(keep)) {
      idx <- idx[keep[idx]]
      if (!length(idx)) {
        next
      }
    }
    # (category position, value) endpoints, swapped below when horizontal
    x0 <- xn[idx]
    y0 <- ymin[idx]
    x1 <- xn[idx]
    y1 <- ymax[idx]
    if (caps) {
      x0 <- c(x0, xn[idx] - half[idx], xn[idx] - half[idx])
      x1 <- c(x1, xn[idx] + half[idx], xn[idx] + half[idx])
      y0 <- c(y0, ymin[idx], ymax[idx])
      y1 <- c(y1, ymin[idx], ymax[idx])
    }
    if (identical(orient, "y")) {
      sw <- list(x0 = y0, y0 = x0, x1 = y1, y1 = x1)
      x0 <- sw$x0
      y0 <- sw$y0
      x1 <- sw$x1
      y1 <- sw$y1
    }
    s <- .seg_units(
      scales,
      vellum::vl_unit(x0, "native"),
      vellum::vl_unit(y0, "native"),
      vellum::vl_unit(x1, "native"),
      vellum::vl_unit(y1, "native")
    )
    scene <- .draw(
      scene,
      vellum::segments_grob(
        s$x0,
        s$y0,
        s$x1,
        s$y1,
        sketch = .sketch_bump(sk, gi),
        gp = .gp_stroke(col, alpha, idx[1], lwd, lty)
      ),
      # PROVENANCE: each error bar is one datum (row-preserving). The segments are
      # emitted as [bars, lower caps, upper caps] (each `idx`-ordered), so a bar's
      # up-to-three segments all resolve to — and are keyed by — the same row.
      rows = if (caps) c(idx, idx, idx) else idx
    )
    gi <- gi + 1L
  }
  scene
}

# An error bar's cap width (or a cross-bar's box width) in data units along
# its category axis, as `mark_bar(width =)` is (VP-98, VP-104, VP-105): a
# mapped `width` (values, so a per-row width is honoured), else the constant,
# else 90% of the band (`band`: the discrete band or the resolution) --
# ggplot2's geom_errorbar() / geom_crossbar() default, the width of a default
# bar, so bars, error bars and cross-bars line up. Under dodge
# `.position_native()` has already narrowed it to the slot.
.errorbar_width <- function(L, band, n) {
  rep_len(L$values$width %||% L$params$width %||% (0.9 * band), n)
}

# Each error bar's (or cross-bar's) centre and width in native units along its
# category axis, after any dodge, as `.position_native()` places and narrows
# them: the error-bar twin of `.bar_geometry()`, for the axis extent training
# (`.mark_extent_data()`, VP-104, VP-105).
.errorbar_geometry <- function(L, xp, band, n) {
  w <- .errorbar_width(L, band, n)
  xc <- xp
  if (L$position %in% c("dodge", "dodge2")) {
    d <- .dodge_offsets(
      xp,
      .group_key(L),
      L$stat_params$dodge_width %||% (.DODGE_WIDTH * band),
      preserve = .dodge_preserve(L),
      n_single = L$dodge_n
    )
    xc <- xp + d$offset
    w <- w * d$share
  }
  list(x = xc, width = w)
}

.emit_linerange <- function(scene, L, scales) {
  .emit_errorbar(scene, L, scales, caps = FALSE)
}

# An identity point-range: the ymin..ymax vertical (reusing the capless errorbar)
# plus a point at (x, y). ggplot2's geom_pointrange.
.emit_pointrange <- function(scene, L, scales) {
  n <- L$n
  size <- rep_len(.aes_size(L, scales, 2.2), n)
  # A row with a missing size is left out whole, range and point, as in
  # ggplot2; `.warn_missing_positions()` reports it (VR-38).
  sized <- is.finite(size)
  scene <- .emit_errorbar(scene, L, scales, caps = FALSE, keep = sized)
  xn <- rep_len(scales$x$map(L$values$x), n)
  yn <- rep_len(scales$y$map(L$values$y), n)
  col <- rep_len(.aes_colour(L, scales, "black"), n)
  alpha <- rep_len(.aes_alpha(L, scales, NA_real_), n)
  sk <- .mark_sketch(L, scales)
  for (idx in .style_groups(n, list(col = col, alpha = alpha, size = size))) {
    idx <- idx[sized[idx]]
    if (!length(idx)) {
      next
    }
    xy <- .xy_units(scales, xn[idx], yn[idx])
    scene <- .draw(
      scene,
      vellum::points_grob(
        xy$x,
        xy$y,
        size = vellum::vl_unit(size[idx], "mm"),
        shape = "circle",
        sketch = sk,
        gp = vellum::vl_gpar(
          fill = col[idx[1]],
          col = col[idx[1]],
          alpha = gp_alpha(alpha[idx[1]])
        )
      ),
      rows = idx
    )
  }
  scene
}

# An identity cross-bar: a box spanning ymin..ymax (`width` wide, in data
# units, as an error bar's caps, VP-105) with a horizontal line at `y`.
# ggplot2's geom_crossbar.
.emit_crossbar <- function(scene, L, scales) {
  n <- L$n
  xn <- rep_len(scales$x$map(L$values$x), n)
  ymin <- rep_len(scales$y$map(L$values$ymin), n)
  ymax <- rep_len(scales$y$map(L$values$ymax), n)
  ymid <- rep_len(scales$y$map(L$values$y), n)
  # `fill` fills; a `color` given with it strokes the box and middle line
  # (default "grey20"); a lone `color` still fills (VP-46)
  fo <- .fill_and_outline(L, scales, "white", n)
  fill <- fo$fill
  col <- ifelse(is.na(fo$col), .geom_paint("grey20"), fo$col)
  alpha <- rep_len(.aes_alpha(L, scales, NA_real_), n)
  band <- scales$x$band_width %||% .resolution(xn)
  hw <- .errorbar_width(L, band, n) / 2
  sk <- .mark_sketch(L, scales)
  for (i in seq_len(n)) {
    r <- .rect_units(
      scales,
      xn[i],
      (ymin[i] + ymax[i]) / 2,
      2 * hw[i],
      abs(ymax[i] - ymin[i])
    )
    scene <- .draw(
      scene,
      vellum::rect_grob(
        x = r$x,
        y = r$y,
        width = r$width,
        height = r$height,
        sketch = .sketch_bump(sk, i),
        gp = vellum::vl_gpar(
          fill = fill[i],
          col = col[i],
          lwd = 1,
          alpha = gp_alpha(alpha[i])
        )
      ),
      rows = i
    )
    s <- .seg_units(
      scales,
      vellum::vl_unit(xn[i] - hw[i], "native"),
      vellum::vl_unit(ymid[i], "native"),
      vellum::vl_unit(xn[i] + hw[i], "native"),
      vellum::vl_unit(ymid[i], "native")
    )
    scene <- .draw(
      scene,
      vellum::segments_grob(
        s$x0,
        s$y0,
        s$x1,
        s$y1,
        sketch = .sketch_bump(sk, i),
        # the middle line is 2.5 times the box's width, as ggplot2 draws it
        # (geom_crossbar()'s `middle.linewidth`, formerly `fatten = 2.5`)
        gp = vellum::vl_gpar(col = col[i], lwd = 2.5)
      ),
      rows = i
    )
  }
  scene
}

# Straight segments from (x, y) to (xend, yend), batched per colour group.
.emit_segment <- function(scene, L, scales) {
  n <- L$n
  if (
    isTRUE(L$stat_params$auto) && n > .DATASHADE_AUTO && .can_datashade(scales)
  ) {
    return(.emit_segment_datashade(scene, L, scales))
  }
  x0 <- rep_len(scales$x$map(L$values$x), n)
  y0 <- rep_len(scales$y$map(L$values$y), n)
  x1 <- rep_len(scales$x$map(L$values$xend), n)
  y1 <- rep_len(scales$y$map(L$values$yend), n)
  col <- rep_len(.aes_colour(L, scales, "black"), n)
  alpha <- rep_len(.aes_alpha(L, scales, NA_real_), n)
  lwd <- .aes_param(L, "linewidth", 1)
  # A segment is already one element per row, so a mapped `linewidth` is just the
  # per-element `lwd` vector -- no re-segmenting, and no width steps within a
  # segment. A constant width keeps the shared `gp` (one combined stroke).
  lwv <- .mapped_linewidth(L, scales, n)
  lty <- .resolve_lty(L, scales, n)
  sk <- .mark_sketch(L, scales)
  curv <- L$stat_params$curvature %||% 0
  if (curv != 0) {
    return(.emit_curve(scene, L, scales, x0, y0, x1, y1, col, alpha, lwd, lty))
  }

  gi <- 0L
  for (idx in .style_groups(n, list(col = col, alpha = alpha, lty = lty))) {
    s <- .seg_units(
      scales,
      vellum::vl_unit(x0[idx], "native"),
      vellum::vl_unit(y0[idx], "native"),
      vellum::vl_unit(x1[idx], "native"),
      vellum::vl_unit(y1[idx], "native")
    )
    scene <- .draw(
      scene,
      vellum::segments_grob(
        s$x0,
        s$y0,
        s$x1,
        s$y1,
        sketch = .sketch_bump(sk, gi),
        gp = .gp_stroke(col, alpha, idx[1], lwd, lty),
        lwd = if (is.null(lwv)) NULL else lwv[idx]
      ),
      rows = idx
    )
    gi <- gi + 1L
  }
  scene
}

# Curved segments (`mark_curve()`): one polyline per row from (x0, y0) to
# (x1, y1), bent by `.curve_points()` in the panel's normalised frame. Native
# coordinates in, drawn through `.xy_path()` so polar / coord_trans bend them
# further.
.emit_curve <- function(
  scene,
  L,
  scales,
  x0,
  y0,
  x1,
  y1,
  col,
  alpha,
  lwd,
  lty
) {
  sp <- L$stat_params
  sk <- .mark_sketch(L, scales)
  lwv <- .mapped_linewidth(L, scales, L$n)
  span <- function(sc) {
    d <- suppressWarnings(as.numeric(sc$domain))
    w <- if (length(d) >= 2L) abs(diff(range(d, na.rm = TRUE))) else NA
    if (is.finite(w) && w > 0) w else 1
  }
  sx <- span(scales$x)
  sy <- span(scales$y)
  for (i in seq_len(L$n)) {
    if (!all(is.finite(c(x0[i], y0[i], x1[i], y1[i])))) {
      next
    }
    pts <- .curve_points(
      x0[i] / sx,
      y0[i] / sy,
      x1[i] / sx,
      y1[i] / sy,
      sp$curvature,
      sp$angle %||% 90,
      sp$ncp %||% 5L
    )
    xy <- .xy_path(scales, pts$x * sx, pts$y * sy)
    gp <- .gp_stroke(col, alpha, i, if (is.null(lwv)) lwd else lwv[i], lty)
    scene <- .draw(
      scene,
      vellum::lines_grob(xy$x, xy$y, sketch = .sketch_bump(sk, i), gp = gp),
      rows = i
    )
  }
  scene
}

# Vertices of a curve from (x0, y0) to (x1, y1) with ggplot2's `geom_curve()`
# parameters. With `angle = 90` the curve is the circular arc whose sagitta is
# `curvature` times the half-chord (grid's `curveGrob()` arc: `1` is a half
# circle); the sign picks the side, positive bending right of the direction of
# travel. Another `angle` slides the apex along the chord to where grid puts the
# corner of its control triangle (towards the start below 90, the end above).
# `ncp` sets the resolution.
.curve_points <- function(x0, y0, x1, y1, curvature, angle = 90, ncp = 5L) {
  dx <- x1 - x0
  dy <- y1 - y0
  len <- sqrt(dx^2 + dy^2)
  if (!is.finite(len) || len == 0 || curvature == 0) {
    return(list(x = c(x0, x1), y = c(y0, y1)))
  }
  ex <- dx / len
  ey <- dy / len
  # right-hand normal of the direction of travel
  nx <- ey
  ny <- -ex
  h <- len / 2
  theta <- 2 * atan(abs(curvature))
  r <- h / sin(theta)
  phi <- seq(-theta, theta, length.out = max(8L, 8L * (ncp + 1L)))
  f <- (1 + sin(phi) / sin(theta)) / 2 # fraction along the chord
  v <- r * (cos(phi) - cos(theta)) # height above it
  # slide the apex (f = 0.5) to the corner fraction, linearly on each side
  ac <- (1 - cos(angle * pi / 180)) / 2
  f <- ifelse(f <= 0.5, f * 2 * ac, ac + (f - 0.5) * 2 * (1 - ac))
  side <- sign(curvature)
  list(
    x = x0 + ex * f * len + side * nx * v,
    y = y0 + ey * f * len + side * ny * v
  )
}
