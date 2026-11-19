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

# The drawing cells of a distribution mark (boxplot, violin, ridgeline,
# slab-interval): one per category level on `axis`, each split into one cell per
# `.group_key()` sub-group present there. Sub-groups are dodged across `span`
# (native units -- the mark's full width at one category) with `.dodge_offsets()`
# -- the groups present at each category share it (ggplot2's dodge2 for boxes),
# or with `position = "dodge"` every category keeps a slot for every group. A
# `dodge = FALSE` mark (ridgeline) overlays its sub-groups on the shared baseline
# instead. Each cell carries its rows (provenance keys), its native `centre`, and
# `share`, the fraction of the mark's usual width it may take.
.DIST_DODGE_PAD <- 0.1 # gap between dodged sub-groups, as a fraction of a slot

.by_category <- function(L, scales, axis = "x", span = 1, dodge = TRUE) {
  n <- L$n
  cv <- rep_len(L$values[[axis]], n)
  levs <- .cat_levels(cv)
  centres <- scales[[axis]]$map(levs)
  cchar <- as.character(cv)
  by_level <- .idx_by_level(cchar, levs)
  g <- .group_key(L)
  d <- NULL
  if (!is.null(g)) {
    gch <- as.character(g)
    glevs <- .cat_levels(g)
    if (dodge) {
      d <- .dodge_offsets(
        cchar,
        g,
        span,
        ragged = !identical(L$position, "dodge")
      )
    }
  }
  cells <- list()
  for (j in seq_along(levs)) {
    sel <- by_level[[j]]
    if (!length(sel)) {
      next
    }
    parts <- if (is.null(g)) {
      list(sel)
    } else {
      Filter(length, split(sel, factor(gch[sel], levels = glevs)))
    }
    for (rows in parts) {
      i0 <- rows[1]
      cells[[length(cells) + 1L]] <- list(
        level = j,
        label = levs[j],
        rows = rows,
        centre = centres[j] + if (is.null(d)) 0 else d$offset[i0],
        share = if (is.null(d)) 1 else d$share[i0] * (1 - .DIST_DODGE_PAD)
      )
    }
  }
  cells
}

# Fill and outline of a filled distribution mark, with ggplot2's semantics:
# `fill` fills, `color` strokes. A mapped `fill` goes through the colour scale,
# else a constant (non-paint) `fill`, else `fill_default`; the outline is a
# mapped or constant `color`, else `col_default`. Neither falls back to the other
# channel, so `color = g` outlines the shapes in g's colours over the default
# fill. Returns per-row vectors.
.dist_paint <- function(L, scales, fill_default, col_default = "grey20") {
  n <- L$n
  fill <- if (!is.null(scales$color) && !is.null(L$values$fill)) {
    scales$color$map(L$values$fill)
  } else {
    (if (.is_paint(L$params$fill)) NULL else L$params$fill) %||% fill_default
  }
  list(
    fill = rep_len(fill, n),
    col = rep_len(.aes_colour(L, scales, col_default, fill_fallback = FALSE), n)
  )
}

# Significance brackets (ggsignif/ggpubr style): for each precomputed bracket row
# draw a ┌──┐ over the two groups it spans (`x` .. `.x2` at height `y`, short
# down-ticks of `tip_length` of the y span) and its p-value label above.
.emit_signif <- function(scene, L, scales) {
  n <- L$n
  x0 <- scales$x$map(L$values$x)
  x1 <- scales$x$map(L$values$.x2)
  yv <- scales$y$map(L$values$y)
  labels <- L$values$.blabel
  tip <- (L$stat_params$tip_length %||% 0.03) * diff(scales$y$domain)
  col <- .aes_param(L, "color", "black")
  lwd <- .aes_param(L, "linewidth", 0.6)
  fs <- .aes_param(L, "size", 8)
  sk <- .mark_sketch(L, scales)
  for (i in seq_len(n)) {
    ln <- .xy_path(
      scales,
      c(x0[i], x0[i], x1[i], x1[i]),
      c(yv[i] - tip, yv[i], yv[i], yv[i] - tip)
    )
    scene <- .draw(
      scene,
      vellum::lines_grob(
        ln$x,
        ln$y,
        sketch = .sketch_bump(sk, i),
        gp = vellum::vl_gpar(col = col, lwd = lwd)
      )
    )
    pt <- .xy_units(scales, (x0[i] + x1[i]) / 2, yv[i] + tip * 0.5)
    scene <- .draw(
      scene,
      vellum::text_grob(
        labels[i],
        pt$x,
        pt$y,
        just = c("centre", "bottom"),
        gp = vellum::vl_gpar(fontsize = fs, col = col)
      )
    )
  }
  scene
}

.emit_boxplot <- function(scene, L, scales) {
  yv <- rep_len(as.numeric(L$values$y), L$n)
  pt <- .dist_paint(L, scales, "white")
  alpha <- rep_len(.aes_alpha(L, scales, NA_real_), L$n)
  sk <- .mark_sketch(L, scales)
  band <- scales$x$band_width %||% .resolution(scales$x$map(L$values$x))
  hw0 <- 0.375 * band
  my <- function(v) scales$y$map(v)
  cells <- .by_category(L, scales, "x", span = 2 * hw0)

  for (k in seq_along(cells)) {
    cl <- cells[[k]]
    sel <- cl$rows
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
    xc <- cl$centre
    hw <- hw0 * cl$share
    i0 <- sel[1]
    a <- alpha[i0]
    stroke <- pt$col[i0]
    line_gp <- vellum::vl_gpar(col = stroke, lwd = 1, alpha = gp_alpha(a))
    skj <- .sketch_bump(sk, k)

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
          fill = .pattern_at(L, scales, i0) %||% .paint_or(L, pt$fill[i0]),
          col = stroke,
          lwd = 1,
          alpha = gp_alpha(a)
        )
      ),
      # PROVENANCE: a box summarises all rows of its category (and sub-group).
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
        # PROVENANCE: median/whisker summarise the cell's rows.
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
          gp = vellum::vl_gpar(fill = stroke, col = stroke, alpha = gp_alpha(a))
        ),
        rows = out_rows
      )
    }
  }
  scene
}

# Per-level `stats::density()` of `vals`, grouped by category `cats` over the
# ordered levels `levs`. Shared by the violin/ridgeline emitters and by scale
# training (`.train_axis`) so the trained domain and the drawn
# geometry are computed from identical values. `vals` are *native* (already
# through the scale transform, see `.native_values()`): like ggplot2, the density
# is estimated after the scale transform, so its support is drawn as is. Mapping
# a data-space support instead would push the ~3-bandwidth padding below zero on
# a log/sqrt axis and emit NaN vertices. A level with fewer than two finite
# observations yields `NULL` (no density is drawn for it). With a `group` (the
# layer's `.group_key()`), the list is per (level, sub-group) cell instead.
.density_by_cat <- function(
  vals,
  cats,
  levs,
  adjust,
  group = NULL,
  trim = FALSE
) {
  ch <- as.character(cats)
  if (!is.null(group)) {
    # One density per (level, sub-group) cell, as the emitters draw them; the
    # footprint only needs their supports, so the flat list is enough.
    key <- paste(ch, as.character(group), sep = "\r")
    return(lapply(unique(key), function(k) {
      .density_of(vals[key == k], adjust, trim)
    }))
  }
  lapply(levs, function(l) .density_of(vals[ch == l], adjust, trim))
}

# The values of a density-shape mark's value axis in native units, as its
# density is estimated (see `.density_by_cat()`). Values the transform cannot
# represent (<= 0 on log10, < 0 on sqrt) become non-finite and are dropped by
# `.density_of()`, as training drops them.
.native_values <- function(sc, v) suppressWarnings(sc$map(as.numeric(v)))

# The kernel density of one cell's values, or NULL with fewer than two finite
# observations -- the per-cell form of `.density_by_cat()`. `trim = TRUE` ends
# the curve at the cell's smallest and largest value (ggplot2's violin default)
# instead of running on into the kernel's tails.
.density_of <- function(v, adjust, trim = FALSE) {
  v <- v[is.finite(v)]
  if (length(v) < 2) {
    return(NULL)
  }
  if (trim) {
    stats::density(v, adjust = adjust, from = min(v), to = max(v))
  } else {
    stats::density(v, adjust = adjust)
  }
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
  yv <- rep_len(as.numeric(L$values$y), L$n)
  pt <- .dist_paint(L, scales, "grey70")
  alpha <- rep_len(.aes_alpha(L, scales, NA_real_), L$n)
  adjust <- L$stat_params$adjust %||% 1
  trim <- isTRUE(L$stat_params$trim)
  band <- scales$x$band_width %||% .resolution(scales$x$map(L$values$x))
  hw0 <- .VIOLIN_HALFWIDTH * band
  sk <- .mark_sketch(L, scales)
  cells <- .by_category(L, scales, "x", span = 2 * hw0)
  for (k in seq_along(cells)) {
    cl <- cells[[k]]
    sel <- cl$rows
    d <- .density_of(.native_values(scales$y, yv[sel]), adjust, trim)
    if (is.null(d)) {
      next
    }
    i0 <- sel[1]
    # up the right side, then down the mirrored left side
    poly <- .density_polygon(
      d$x,
      cl$centre,
      .density_offset(d, hw0 * cl$share),
      "violin"
    )
    xy <- .xy_units(scales, poly$perp, poly$supp)
    scene <- .draw(
      scene,
      vellum::polygon_grob(
        xy$x,
        xy$y,
        sketch = .sketch_bump(sk, k),
        gp = vellum::vl_gpar(
          fill = .pattern_at(L, scales, i0) %||% .paint_or(L, pt$fill[i0]),
          col = pt$col[i0],
          lwd = 1,
          alpha = gp_alpha(alpha[i0])
        )
      ),
      # PROVENANCE: a violin summarises all rows of its category (and sub-group).
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
  xv <- rep_len(as.numeric(L$values$x), L$n)
  pt <- .dist_paint(L, scales, "grey70")
  adjust <- L$stat_params$adjust %||% 1
  band <- scales$y$band_width %||% 1
  scale_h <- (L$stat_params$height %||% .RIDGE_HEIGHT) * band
  alpha <- rep_len(.aes_alpha(L, scales, NA_real_), L$n)
  sk <- .mark_sketch(L, scales)
  # Sub-groups share their category's baseline and overlap (ridges are compared
  # by outline, not set side by side).
  cells <- .by_category(L, scales, "y", dodge = FALSE)
  # back-to-front, so nearer (lower) ridges overlap farther ones
  for (k in rev(seq_along(cells))) {
    cl <- cells[[k]]
    sel <- cl$rows
    d <- .density_of(.native_values(scales$x, xv[sel]), adjust)
    if (is.null(d)) {
      next
    }
    i0 <- sel[1]
    poly <- .density_polygon(
      d$x,
      cl$centre,
      .density_offset(d, scale_h),
      "ridge"
    )
    xy <- .xy_units(scales, poly$supp, poly$perp)
    scene <- .draw(
      scene,
      vellum::polygon_grob(
        xy$x,
        xy$y,
        sketch = .sketch_bump(sk, k),
        gp = vellum::vl_gpar(
          fill = .pattern_at(L, scales, i0) %||% .paint_or(L, pt$fill[i0]),
          col = pt$col[i0],
          lwd = 1,
          alpha = gp_alpha(alpha[i0])
        )
      ),
      # PROVENANCE: a ridge summarises all rows of its category (and sub-group).
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
  yv <- rep_len(as.numeric(L$values$y), L$n)
  band <- scales$x$band_width %||% .resolution(scales$x$map(L$values$x))
  widths <- sort(L$stat_params$width %||% c(0.66, 0.95))
  point <- L$stat_params$point %||% "median"
  colv <- rep_len(.aes_colour(L, scales, "grey30"), L$n)
  alpha <- rep_len(.aes_alpha(L, scales, NA_real_), L$n)
  adjust <- L$stat_params$adjust %||% 1
  hw0 <- (L$stat_params$scale %||% .SLAB_WIDTH) * band
  sk <- .mark_sketch(L, scales)
  mrk <- if (slab) "mark_halfeye" else "mark_interval"
  # inner intervals thick, outer thin; drawn widest-first so the thick bar is on top
  lwd_by_width <- seq(4, 1.5, length.out = length(widths))
  cells <- .by_category(L, scales, "x", span = .SLAB_WIDTH * band)

  for (k in seq_along(cells)) {
    cl <- cells[[k]]
    sel <- cl$rows
    # A point-interval summarises a distribution: skip a cell with fewer than 2
    # finite observations (a single/absent value has no interval and would
    # otherwise feed NA coordinates into the grobs), matching `.stat_density`.
    if (sum(is.finite(yv[sel])) < 2L) {
      cli::cli_warn(
        "Skipping {.field {cl$label}}: {.fn {mrk}} needs at least 2 points."
      )
      next
    }
    xc <- cl$centre
    ccol <- colv[sel[1]]
    a <- alpha[sel[1]]
    d <- if (slab) .density_of(.native_values(scales$y, yv[sel]), adjust)

    if (!is.null(d)) {
      # up the flat left edge, down the bulge
      poly <- .density_polygon(
        d$x,
        xc,
        .density_offset(d, hw0 * cl$share),
        "slab"
      )
      xy <- .xy_units(scales, poly$perp, poly$supp)
      scene <- .draw(
        scene,
        vellum::polygon_grob(
          xy$x,
          xy$y,
          sketch = .sketch_bump(sk, k),
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
