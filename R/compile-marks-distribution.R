#' @include classes.R
NULL

# Mark emitters: distribution marks (boxplot, violin, ridgeline, slabinterval).

# A box-and-whisker per x category: box (Q1-Q3), median line, Tukey whiskers
# (1.5*IQR), and outlier points. Summary is computed here from the raw y values.
# Row indices grouped by category level (kept in `levs` order), computed once
# instead of an O(n * levels) `which(xchar == levs[j])` rescan per level. Each
# level from `.cat_levels()` is present, so every entry is a (non-empty) vector.
.idx_by_level <- function(xchar, levs) {
  split(seq_along(xchar), factor(xchar, levels = levs))
}

.emit_boxplot <- function(scene, L, scales) {
  xv <- L$values$x
  yv <- as.numeric(L$values$y)
  colv <- rep_len(.aes_colour(L, scales, "white"), length(yv))
  alpha <- rep_len(.aes_alpha(L, scales, NA_real_), length(yv))
  sk <- .mark_sketch(L, scales)
  levs <- .cat_levels(xv)
  xc_all <- scales$x$map(levs)
  band <- scales$x$band_width %||% .resolution(scales$x$map(xv))
  hw <- 0.375 * band
  xchar <- as.character(xv)
  idx_by_level <- .idx_by_level(xchar, levs)
  my <- function(v) scales$y$map(v)

  for (j in seq_along(levs)) {
    sel <- idx_by_level[[j]]
    if (!length(sel)) {
      next
    }
    yy <- yv[sel]
    yy <- yy[is.finite(yy)]
    if (!length(yy)) {
      next
    }
    qs <- stats::quantile(yy, c(0.25, 0.5, 0.75), names = FALSE)
    q1 <- qs[1]
    med <- qs[2]
    q3 <- qs[3]
    iqr <- q3 - q1
    lo <- min(yy[yy >= q1 - 1.5 * iqr])
    hi <- max(yy[yy <= q3 + 1.5 * iqr])
    out <- yy[yy < lo | yy > hi]
    xc <- xc_all[j]
    fillc <- colv[sel[1]]
    a <- alpha[sel[1]]
    line_gp <- vellum::vl_gpar(col = "grey20", lwd = 1)
    skj <- .sketch_bump(sk, j)

    r <- .rect_units(
      scales,
      xc,
      (my(q1) + my(q3)) / 2,
      2 * hw,
      abs(my(q3) - my(q1))
    )
    scene <- .draw(
      scene,
      vellum::rect_grob(
        x = r$x,
        y = r$y,
        width = r$width,
        height = r$height,
        sketch = skj,
        gp = vellum::vl_gpar(
          fill = .pattern_at(L, scales, sel[1]) %||% .paint_or(L, fillc),
          col = "grey20",
          lwd = 1,
          alpha = gp_alpha(a)
        )
      ),
      # PROVENANCE: a box summarises all rows of its category.
      rows = sel
    )
    for (seg in list(
      c(xc - hw, my(med), xc + hw, my(med)), # median
      c(xc, my(q3), xc, my(hi)), # upper whisker
      c(xc, my(q1), xc, my(lo)) # lower whisker
    )) {
      s <- .seg_units(
        scales,
        vellum::vl_unit(seg[1], "native"),
        vellum::vl_unit(seg[2], "native"),
        vellum::vl_unit(seg[3], "native"),
        vellum::vl_unit(seg[4], "native")
      )
      scene <- .draw(
        scene,
        vellum::segments_grob(
          s$x0,
          s$y0,
          s$x1,
          s$y1,
          sketch = skj,
          gp = line_gp
        ),
        # PROVENANCE: median/whisker summarise the category's rows.
        rows = sel
      )
    }
    if (length(out)) {
      # The outlier points map 1:1 to their originating rows.
      sel_fin <- sel[is.finite(yv[sel])]
      out_rows <- sel_fin[yv[sel_fin] < lo | yv[sel_fin] > hi]
      xy <- .xy_units(scales, rep(xc, length(out)), my(out))
      scene <- .draw(
        scene,
        vellum::points_grob(
          xy$x,
          xy$y,
          size = vellum::vl_unit(1.2, "mm"),
          shape = "circle",
          sketch = skj,
          gp = vellum::vl_gpar(fill = "grey20", col = "grey20")
        ),
        rows = out_rows
      )
    }
  }
  scene
}

# Per-level `stats::density()` of `vals`, grouped by category `cats` over the
# ordered levels `levs`. Shared by the violin/ridgeline emitters and by scale
# training (`.expand_position_for_marks`) so the trained domain and the drawn
# geometry are computed from identical values. A level with fewer than two finite
# observations yields `NULL` (no density is drawn for it).
.density_by_cat <- function(vals, cats, levs, adjust) {
  ch <- as.character(cats)
  lapply(levs, function(l) {
    v <- vals[ch == l]
    v <- v[is.finite(v)]
    if (length(v) < 2) NULL else stats::density(v, adjust = adjust)
  })
}

# Default extents (fractions/multiples of the category band) of the density-shape
# marks. Shared by the emitters below and by `.mark_footprint()` (compile-train.R)
# so a layer's drawn footprint and the domain expansion that makes room for it are
# computed from the same numbers.
.VIOLIN_HALFWIDTH <- 0.4 # violin half-width as a fraction of the band
.RIDGE_HEIGHT <- 1.4 # default ridge height as a multiple of the row band
.SLAB_WIDTH <- 0.9 # default halfeye slab width as a fraction of the band

# Normalise a kernel density's heights to a drawable extent (peak maps to
# `extent`); the shared first step of every density-shape polygon.
.density_offset <- function(d, extent) (d$y / max(d$y)) * extent

# Native-coordinate vertices of a filled density polygon. `support` is the
# density support (`d$x`) already mapped to native on its own axis; `offset` is
# the normalised height (`.density_offset()`) laid out perpendicular from `base`.
# Orientation:
#   "violin" — mirrored about `base` (value axis), support runs along the other;
#   "slab"   — one-sided from `base` (flat edge at `base`, bulge to `base+offset`);
#   "ridge"  — one-sided, rising `offset` above a flat baseline at `base`.
# Returns `perp`/`supp`: the perpendicular (value) and support coordinates, which
# the caller assigns to x/y depending on which axis the value runs along.
.density_polygon <- function(support, base, offset, orient) {
  m <- length(support)
  supp <- c(support, rev(support))
  perp <- switch(
    orient,
    violin = c(base + offset, rev(base - offset)),
    slab = c(rep(base, m), base + rev(offset)),
    ridge = c(base + offset, rep(base, m))
  )
  list(perp = perp, supp = supp)
}

# A violin: a mirrored kernel-density of `y` per `x` category, drawn as a filled
# polygon whose half-width is the density scaled to the category band. Mirrors
# the boxplot layout (categorical x, value y).
.emit_violin <- function(scene, L, scales) {
  xv <- L$values$x
  yv <- as.numeric(L$values$y)
  colv <- rep_len(.aes_colour(L, scales, "grey70"), length(yv))
  alpha <- rep_len(.aes_alpha(L, scales, NA_real_), length(yv))
  adjust <- L$stat_params$adjust %||% 1
  levs <- .cat_levels(xv)
  xc_all <- scales$x$map(levs)
  band <- scales$x$band_width %||% .resolution(scales$x$map(xv))
  hw <- .VIOLIN_HALFWIDTH * band
  xchar <- as.character(xv)
  idx_by_level <- .idx_by_level(xchar, levs)
  dens <- .density_by_cat(yv, xv, levs, adjust)
  sk <- .mark_sketch(L, scales)
  for (j in seq_along(levs)) {
    d <- dens[[j]]
    if (is.null(d)) {
      next
    }
    sel <- idx_by_level[[j]]
    a <- alpha[sel[1]]
    # up the right side, then down the mirrored left side
    poly <- .density_polygon(
      scales$y$map(d$x),
      xc_all[j],
      .density_offset(d, hw),
      "violin"
    )
    xy <- .xy_units(scales, poly$perp, poly$supp)
    scene <- .draw(
      scene,
      vellum::polygon_grob(
        xy$x,
        xy$y,
        sketch = .sketch_bump(sk, j),
        gp = vellum::vl_gpar(
          fill = .pattern_at(L, scales, sel[1]) %||% .paint_or(L, colv[sel[1]]),
          col = "grey30",
          lwd = 1,
          alpha = gp_alpha(a)
        )
      ),
      # PROVENANCE: a violin summarises all rows of its category.
      rows = sel
    )
  }
  scene
}

# A ridgeline plot: a kernel-density of `x` per `y` category, each drawn as a
# filled ridge whose baseline sits at the category's position and whose height
# is the density scaled to (a multiple of) the band. Drawn back-to-front so
# nearer ridges overlap farther ones.
.emit_ridgeline <- function(scene, L, scales) {
  xv <- as.numeric(L$values$x)
  yv <- L$values$y
  colv <- rep_len(.aes_colour(L, scales, "grey70"), length(xv))
  adjust <- L$stat_params$adjust %||% 1
  levs <- .cat_levels(yv)
  ypos <- scales$y$map(levs)
  band <- scales$y$band_width %||% 1
  scale_h <- (L$stat_params$height %||% .RIDGE_HEIGHT) * band
  ychar <- as.character(yv)
  dens <- .density_by_cat(xv, yv, levs, adjust)
  alpha <- rep_len(.aes_alpha(L, scales, NA_real_), length(xv))
  sk <- .mark_sketch(L, scales)
  for (j in rev(seq_along(levs))) {
    d <- dens[[j]]
    if (is.null(d)) {
      next
    }
    sel <- which(ychar == levs[j])
    a <- alpha[sel[1]]
    poly <- .density_polygon(
      scales$x$map(d$x),
      ypos[j],
      .density_offset(d, scale_h),
      "ridge"
    )
    xy <- .xy_units(scales, poly$supp, poly$perp)
    scene <- .draw(
      scene,
      vellum::polygon_grob(
        xy$x,
        xy$y,
        sketch = .sketch_bump(sk, j),
        gp = vellum::vl_gpar(
          fill = .pattern_at(L, scales, sel[1]) %||% .paint_or(L, colv[sel[1]]),
          col = "grey30",
          lwd = 1,
          alpha = gp_alpha(a)
        )
      ),
      # PROVENANCE: a ridge summarises all rows of its category.
      rows = sel
    )
  }
  scene
}

# Equal-tailed quantile point-interval of a sample: the centre (median or mean)
# and the (lower, upper) bounds at each probability in `widths`.
.point_interval <- function(v, widths, point) {
  v <- v[is.finite(v)]
  ctr <- if (identical(point, "mean")) mean(v) else stats::median(v)
  ints <- lapply(widths, function(w) {
    a <- (1 - w) / 2
    unname(stats::quantile(v, c(a, 1 - a)))
  })
  list(point = ctr, ints = ints)
}

# ggdist-style slab + interval, per `x` category. Always draws a point-interval
# (median/mean point, thick inner + thin outer quantile bars); with `slab = TRUE`
# also draws a one-sided density "slab" (half violin) to the right of the tick.
# Aimed at sample/posterior input: many `y` rows per category.
.emit_slabinterval <- function(scene, L, scales, slab) {
  xv <- L$values$x
  yv <- as.numeric(L$values$y)
  levs <- .cat_levels(xv)
  xc_all <- scales$x$map(levs)
  band <- scales$x$band_width %||% .resolution(scales$x$map(xv))
  widths <- sort(L$stat_params$width %||% c(0.66, 0.95))
  point <- L$stat_params$point %||% "median"
  colv <- rep_len(.aes_colour(L, scales, "grey30"), length(yv))
  alpha <- rep_len(.aes_alpha(L, scales, NA_real_), length(yv))
  adjust <- L$stat_params$adjust %||% 1
  hw <- (L$stat_params$scale %||% .SLAB_WIDTH) * band
  xchar <- as.character(xv)
  idx_by_level <- .idx_by_level(xchar, levs)
  dens <- if (slab) .density_by_cat(yv, xv, levs, adjust) else NULL
  sk <- .mark_sketch(L, scales)
  mrk <- if (slab) "mark_halfeye" else "mark_interval"
  # inner intervals thick, outer thin; drawn widest-first so the thick bar is on top
  lwd_by_width <- seq(4, 1.5, length.out = length(widths))

  for (j in seq_along(levs)) {
    sel <- idx_by_level[[j]]
    # A point-interval summarises a distribution: skip a category with fewer than
    # 2 finite observations (a single/absent value has no interval and would
    # otherwise feed NA coordinates into the grobs), matching `.stat_density`.
    if (sum(is.finite(yv[sel])) < 2L) {
      cli::cli_warn(
        "Skipping {.field {levs[j]}}: {.fn {mrk}} needs at least 2 points."
      )
      next
    }
    xc <- xc_all[j]
    ccol <- colv[sel[1]]
    a <- alpha[sel[1]]

    if (slab && !is.null(dens[[j]])) {
      d <- dens[[j]]
      # up the flat left edge, down the bulge
      poly <- .density_polygon(
        scales$y$map(d$x),
        xc,
        .density_offset(d, hw),
        "slab"
      )
      xy <- .xy_units(scales, poly$perp, poly$supp)
      scene <- .draw(
        scene,
        vellum::polygon_grob(
          xy$x,
          xy$y,
          sketch = .sketch_bump(sk, j),
          gp = vellum::vl_gpar(
            fill = .pattern_at(L, scales, sel[1]) %||% .paint_or(L, ccol),
            col = NA,
            # honour a mapped/constant alpha; fall back to the slab's native 0.5
            alpha = gp_alpha(if (is.na(a)) 0.5 else a)
          )
        ),
        rows = sel
      )
    }

    pit <- .point_interval(yv[sel], widths, point)
    # widest first (widths is sorted ascending) so the thick inner bar draws last
    for (wi in rev(seq_along(widths))) {
      lo <- scales$y$map(pit$ints[[wi]][1])
      hi <- scales$y$map(pit$ints[[wi]][2])
      ab <- .xy_units(scales, c(xc, xc), c(lo, hi))
      scene <- .draw(
        scene,
        vellum::segments_grob(
          ab$x[1],
          ab$y[1],
          ab$x[2],
          ab$y[2],
          gp = vellum::vl_gpar(
            col = ccol,
            lwd = lwd_by_width[wi],
            alpha = gp_alpha(a)
          )
        ),
        # PROVENANCE: this category's rows -- the bar keys to its datum like the
        # slab and centre point do (a single segment, so the first key is used).
        rows = sel
      )
    }
    pt <- .xy_units(scales, xc, scales$y$map(pit$point))
    scene <- .draw(
      scene,
      vellum::points_grob(
        pt$x,
        pt$y,
        size = vellum::vl_unit(2, "mm"),
        shape = "circle",
        gp = vellum::vl_gpar(fill = ccol, col = ccol, alpha = gp_alpha(a))
      ),
      rows = sel
    )
  }
  scene
}

.emit_halfeye <- function(scene, L, scales) {
  .emit_slabinterval(scene, L, scales, slab = TRUE)
}

.emit_interval <- function(scene, L, scales) {
  .emit_slabinterval(scene, L, scales, slab = FALSE)
}
