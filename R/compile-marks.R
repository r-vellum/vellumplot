#' @include classes.R
NULL

# Group row indices by the tuple of gpar-borne style fields, mirroring
# `vellum:::.gv_groups`: a batched grob carries a single gpar, so rows that must
# differ in fill/col/alpha/lwd have to be emitted as separate grobs. Geometry-
# borne aesthetics (size, shape, x, y) are vectorised and do NOT force a split.
# Continuous colour is already quantized upstream, so the group count is bounded.
.style_groups <- function(n, fields) {
  fields <- fields[!vapply(fields, is.null, logical(1))]
  # Fast path: with no styling fields, or when every field is constant (the
  # common large-n case of a single colour/alpha), there is exactly one group.
  # Skip the O(n) rep_len + paste + split entirely.
  if (
    !length(fields) ||
      all(vapply(fields, function(v) length(unique(v)) <= 1L, logical(1)))
  ) {
    return(list(seq_len(n)))
  }
  codes <- lapply(fields, function(v) {
    v <- rep_len(v, n)
    match(v, unique(v))
  })
  unname(split(seq_len(n), do.call(paste, c(codes, sep = "\036"))))
}

# Resolve a layer's mark colour: a mapped colour channel (via the trained colour
# scale), a constant colour param, or the supplied default. `fill_fallback` lets
# `fill` stand in for `color` (true for fill-based marks; false for text/label,
# where `fill` is the label background, not the ink colour).
.aes_colour <- function(L, scales, default, fill_fallback = TRUE) {
  if (!is.null(scales$color)) {
    if (!is.null(L$values$color)) {
      return(scales$color$map(L$values$color))
    }
    if (fill_fallback && !is.null(L$values$fill)) {
      return(scales$color$map(L$values$fill))
    }
  }
  # A paint-valued `fill` (gradient/pattern) is not a per-row colour: ignore it
  # here (the emitter substitutes it at the `vl_gpar(fill = )` site via
  # `.paint_or()`), so this returns a plain colour the row vector / grouping needs.
  fillp <- if (.is_paint(L$params$fill)) NULL else L$params$fill
  if (fill_fallback) {
    L$params$color %||% fillp %||% default
  } else {
    L$params$color %||% default
  }
}

# A fill value can be a constant *paint* (gradient or pattern), not just a colour.
.is_paint <- function(x) inherits(x, c("vellum_gradient", "vellum_pattern"))

.aes_param <- function(L, name, default) L$params[[name]] %||% default

# Resolve a per-group alpha for a `vl_gpar`: `NA` means "no alpha set" (the mark's
# native opacity), which vellum expects as `NULL` rather than `NA`.
gp_alpha <- function(a) if (is.na(a)) NULL else a

# The gpar for a filled mark's style group `i` (fill from the group's colour, no
# stroke, per-group alpha). Shared by the filled marks (band/bar/tile).
.gp_fill <- function(fill, alpha, i, paint = NULL) {
  vellum::vl_gpar(
    fill = paint %||% fill[i],
    col = NA,
    alpha = gp_alpha(alpha[i])
  )
}

# The gpar for a stroked mark's style group `i`: per-group colour + alpha, a
# shared line width, and an optional per-group line type (`lty = NULL` omits it,
# matching an unset linetype). Shared by the stroked marks
# (line/step/contour/segment/rug).
.gp_stroke <- function(col, alpha, i, lwd, lty = NULL) {
  vellum::vl_gpar(
    col = col[i],
    lwd = lwd,
    lty = if (is.null(lty)) NULL else lty[i],
    alpha = gp_alpha(alpha[i])
  )
}

# Clamp a mapped baseline into the finite trained span. On a log/sqrt y scale the
# zero baseline maps to -Inf/NaN (`map(0)`), which yields degenerate bars/areas;
# pin any non-finite (or out-of-range) baseline to the nearest edge of the
# domain. `min`/`max` (not `domain[1]`) so a reversed, decreasing domain works.
.clamp_baseline <- function(v, domain) {
  if (is.null(domain)) {
    return(v)
  }
  lo <- min(domain)
  hi <- max(domain)
  v[!is.finite(v)] <- lo
  pmin(pmax(v, lo), hi)
}

# Map a rectangle bound to native coords, resolving `-Inf`/`Inf` to the panel
# edge (mirrors ggplot2's `annotate("rect", xmin = -Inf, ...)` full-panel band).
# Infinities are substituted *before* mapping so a log/sqrt scale, where
# `map(-Inf)` would be `NaN`, still lands on the edge. Finite bounds keep their
# usual behaviour -- drawn full-size and clipped by the panel viewport, not
# clamped. `-Inf`/`Inf` map to the low/high native edge positionally; on a
# reversed axis this mirrors the band, an accepted edge case.
.bound_native <- function(v, scale) {
  lo <- min(scale$domain)
  hi <- max(scale$domain)
  out <- rep(NA_real_, length(v))
  neg <- v == -Inf
  pos <- v == Inf
  fin <- is.finite(v)
  out[neg] <- lo
  out[pos] <- hi
  if (any(fin)) {
    out[fin] <- scale$map(v[fin])
  }
  out
}

# A constant paint fill value on a layer (from `fill = linear_gradient(...)` or a
# `pattern_*()` builder), or NULL. When present, a filled mark paints its shapes
# with the paint rather than a per-row colour vector (a single value, not mapped).
.paint_fill <- function(L) {
  f <- L$params$fill
  if (.is_paint(f)) f else NULL
}

# Substitute a layer's constant paint fill for a resolved colour at a
# `vl_gpar(fill = )` site; returns the colour unchanged when there is no paint.
.paint_or <- function(L, col) .paint_fill(L) %||% col

# Per-row grouping key for a mapped `pattern` aesthetic (the trained level), or
# NULL when the layer has no pattern channel. Added to a fill mark's style
# grouping so each level draws with its own texture.
.aes_pattern_key <- function(L, scales, n) {
  if (!is.null(scales$pattern) && !is.null(L$values$pattern)) {
    rep_len(scales$pattern$map(L$values$pattern), n)
  }
}

# The `vellum_pattern` for the group whose representative row `i` has key
# `pkey[i]`, or NULL when there is no pattern mapping.
.pattern_obj <- function(scales, pkey, i) {
  if (is.null(pkey)) {
    return(NULL)
  }
  scales$pattern$objs[[pkey[i]]]
}

# The mapped `vellum_pattern` for layer row `i` (or NULL) -- the convenience used
# by the per-group emitters (boxplot / violin / ridgeline / halfeye / hull) whose
# fill is resolved one representative row at a time.
.pattern_at <- function(L, scales, i) {
  .pattern_obj(scales, .aes_pattern_key(L, scales, L$n), i)
}

# The marks whose emitters honour a constant paint (gradient/pattern) `fill`.
# Every other mark would pass the paint into an undefined per-row fill, so
# `.emit_layer` rejects it up front. (Inherently multi-category fills -- pie /
# donut / sunburst / chord -- take textures via the mapped `pattern` aesthetic,
# not a single constant paint.)
.PAINT_MARKS <- c(
  "bar",
  "area",
  "ribbon",
  "rect",
  "tile",
  "boxplot",
  "violin",
  "ridgeline",
  "halfeye",
  "hull",
  "ellipse",
  "sf"
)

# Resolve a layer's point size (mm): a mapped size channel (via the trained size
# scale), a constant size param, or the supplied default.
.aes_size <- function(L, scales, default) {
  if (!is.null(scales$size) && !is.null(L$values$size)) {
    return(scales$size$map(L$values$size))
  }
  L$params$size %||% default
}

# Resolve a layer's point shape: a mapped shape channel (via the trained shape
# scale), a constant shape param, or the supplied default.
.aes_shape <- function(L, scales, default) {
  if (!is.null(scales$shape) && !is.null(L$values$shape)) {
    return(scales$shape$map(L$values$shape))
  }
  L$params$shape %||% default
}

# Resolve a layer's opacity: a mapped alpha channel (via the trained alpha
# scale), a constant alpha param, or the supplied default. Vectorised when mapped.
.aes_alpha <- function(L, scales, default = NA_real_) {
  if (!is.null(scales$alpha) && !is.null(L$values$alpha)) {
    return(scales$alpha$map(L$values$alpha))
  }
  L$params$alpha %||% default
}

# Resolve a layer's line type: a mapped linetype channel (via the trained
# linetype scale), a constant linetype param, or the supplied default.
.aes_linetype <- function(L, scales, default = NULL) {
  if (!is.null(scales$linetype) && !is.null(L$values$linetype)) {
    return(scales$linetype$map(L$values$linetype))
  }
  L$params$linetype %||% default
}

# The layer's resolved line type recycled to the element count `n`, or NULL when
# no linetype is mapped/set (so the caller falls back to its own default). The
# per-mark idiom shared by the point/line/step/segment/area/... emitters.
.resolve_lty <- function(L, scales, n) {
  lty <- .aes_linetype(L, scales, NULL)
  if (is.null(lty)) NULL else rep_len(lty, n)
}

# Edge-scoped colour / alpha / linetype: identical to the `.aes_*` resolvers
# above but keyed on the edge channels + edge scales, so an edge's colour is
# trained and mapped independently of the node colour scale (N1a). A constant
# (e.g. `mark_edges(color = "grey60")`) lands in the matching edge param.
.aes_edge_colour <- function(L, scales, default) {
  if (!is.null(scales$edge_color) && !is.null(L$values$edge_color)) {
    return(scales$edge_color$map(L$values$edge_color))
  }
  L$params$edge_color %||% default
}
.aes_edge_alpha <- function(L, scales, default = NA_real_) {
  if (!is.null(scales$edge_alpha) && !is.null(L$values$edge_alpha)) {
    return(scales$edge_alpha$map(L$values$edge_alpha))
  }
  L$params$edge_alpha %||% default
}
.aes_edge_linetype <- function(L, scales, default = NULL) {
  if (!is.null(scales$edge_linetype) && !is.null(L$values$edge_linetype)) {
    return(scales$edge_linetype$map(L$values$edge_linetype))
  }
  L$params$edge_linetype %||% default
}

# Apply a layer's `nudge_x`/`nudge_y` (millimetres) to a resolved `.xy_units()`
# pair, as a device-exact compound offset (vellum's `native + mm` unit). A zero
# nudge is left untouched, so an un-nudged mark is byte-identical.
.nudge_xy <- function(xy, L, idx = NULL) {
  # A per-row nudge (mm) mapped as a `nudge_x`/`nudge_y` channel wins over the
  # scalar `nudge_x`/`nudge_y` param -- this is how `mark_node_text(dist=)` pushes
  # each label radially outward from the graph centroid. `idx` subsets it to the
  # rows drawn in the current style group.
  nx <- L$values$nudge_x %||% .aes_param(L, "nudge_x", 0)
  ny <- L$values$nudge_y %||% .aes_param(L, "nudge_y", 0)
  if (!is.null(idx)) {
    if (length(nx) > 1L) {
      nx <- nx[idx]
    }
    if (length(ny) > 1L) ny <- ny[idx]
  }
  if (!is.null(nx) && any(nx != 0)) {
    xy$x <- xy$x + vellum::vl_unit(nx, "mm")
  }
  if (!is.null(ny) && any(ny != 0)) {
    xy$y <- xy$y + vellum::vl_unit(ny, "mm")
  }
  xy
}

# Coordinate placement honouring coord_flip: under flip the horizontal axis
# carries the data y and the vertical axis the data x. Emitters compute native
# coordinates in data space, then place them through these helpers so only the
# final grob arguments swap.
.flipped <- function(scales) isTRUE(scales$flip)

# Whether the `auto` datashade fallback may fire. A datashade raster bins in
# uniform data space and fills its viewport linearly, so it misplaces under a
# warped coordinate system (polar / coord_trans) -- the same reason `seam.R`
# refuses raster/datashade marks under `coord_trans`. When warped, the `auto`
# switch is skipped and the mark's ordinary vector path draws instead.
.can_datashade <- function(scales) {
  is.null(scales$polar) && is.null(scales$trans)
}

# Map a pair of trained-scale native values to cartesian panel-native coordinates
# under polar projection: the theta aesthetic drives the angle, the other the
# radius, and the result lands in the [-1, 1] square panel. Vectorised.
.polar_xy <- function(scales, x, y) {
  ctx <- scales$polar
  if (identical(ctx$theta_aes, "x")) {
    ang <- ctx$theta_map(x)
    rad <- ctx$r_map(y)
  } else {
    ang <- ctx$theta_map(y)
    rad <- ctx$r_map(x)
  }
  list(
    x = vellum::vl_unit(rad * cos(ang), "native"),
    y = vellum::vl_unit(rad * sin(ang), "native")
  )
}

# Map trained-native (x, y) through a coord_trans display warp (separable per
# axis). No flip under coord_trans, so x stays horizontal.
.trans_xy <- function(scales, x, y) {
  ctx <- scales$trans
  list(
    x = vellum::vl_unit(ctx$x_map(x), "native"),
    y = vellum::vl_unit(ctx$y_map(y), "native")
  )
}

# Densify a polyline (trained-native x/y, draw order) so a straight data segment
# bends smoothly under a nonlinear coord_trans warp: split each segment into
# sub-steps proportional to how far it spans the warped range (short segments —
# already-dense data — get none). Linear axes (identity/reverse) return as is, so
# an identity coord_trans leaves output unchanged.
.trans_munch <- function(scales, x, y, k = 200L, cap = 500L) {
  ctx <- scales$trans
  m <- length(x)
  if (m < 2L || (isTRUE(ctx$x_lin) && isTRUE(ctx$y_lin))) {
    return(list(x = x, y = y))
  }
  xw <- ctx$x_map(x)
  yw <- ctx$y_map(y)
  xr <- diff(range(xw[is.finite(xw)]))
  yr <- diff(range(yw[is.finite(yw)]))
  xr <- if (isTRUE(xr > 0)) xr else 1
  yr <- if (isTRUE(yr > 0)) yr else 1
  ox <- vector("list", m - 1L)
  oy <- vector("list", m - 1L)
  for (i in seq_len(m - 1L)) {
    frac <- max(abs(xw[i + 1L] - xw[i]) / xr, abs(yw[i + 1L] - yw[i]) / yr)
    steps <- max(1L, min(cap, ceiling(frac * k)))
    t <- seq(0, 1, length.out = steps + 1L)[-(steps + 1L)]
    ox[[i]] <- x[i] + t * (x[i + 1L] - x[i])
    oy[[i]] <- y[i] + t * (y[i + 1L] - y[i])
  }
  list(x = c(unlist(ox), x[m]), y = c(unlist(oy), y[m]))
}

.xy_units <- function(scales, x, y) {
  if (!is.null(scales$polar)) {
    return(.polar_xy(scales, x, y))
  }
  if (!is.null(scales$trans)) {
    return(.trans_xy(scales, x, y))
  }
  if (.flipped(scales)) {
    list(x = vellum::vl_unit(y, "native"), y = vellum::vl_unit(x, "native"))
  } else {
    list(x = vellum::vl_unit(x, "native"), y = vellum::vl_unit(y, "native"))
  }
}

# Densify a polyline (trained-native x/y, in draw order) so that, under polar
# projection, straight data segments bend into smooth arcs: between consecutive
# vertices insert points until the angular step is <= `max_step`, interpolating
# x and y linearly. Returns the denser x/y; a < 2-point path is returned as is.
.polar_munch <- function(scales, x, y, max_step = 4 * pi / 180) {
  ctx <- scales$polar
  m <- length(x)
  if (m < 2L) {
    return(list(x = x, y = y))
  }
  tsrc <- if (identical(ctx$theta_aes, "x")) x else y
  ang <- ctx$theta_map(tsrc)
  ox <- vector("list", m - 1L)
  oy <- vector("list", m - 1L)
  for (i in seq_len(m - 1L)) {
    steps <- max(1L, ceiling(abs(ang[i + 1L] - ang[i]) / max_step))
    t <- seq(0, 1, length.out = steps + 1L)[-(steps + 1L)]
    ox[[i]] <- x[i] + t * (x[i + 1L] - x[i])
    oy[[i]] <- y[i] + t * (y[i + 1L] - y[i])
  }
  list(x = c(unlist(ox), x[m]), y = c(unlist(oy), y[m]))
}

# Map an ordered polyline to grob units, densifying first under polar/trans.
.xy_path <- function(scales, x, y) {
  if (!is.null(scales$polar)) {
    d <- .polar_munch(scales, x, y)
    return(.xy_units(scales, d$x, d$y))
  }
  if (!is.null(scales$trans)) {
    d <- .trans_munch(scales, x, y)
    return(.xy_units(scales, d$x, d$y))
  }
  .xy_units(scales, x, y)
}

# Map a filled outline (forward path a + reversed path b) to polygon units,
# densifying each side under polar so the band follows the arcs.
.xy_area <- function(scales, xa, ya, xb, yb) {
  if (!is.null(scales$polar)) {
    da <- .polar_munch(scales, xa, ya)
    db <- .polar_munch(scales, rev(xb), rev(yb))
    return(.xy_units(scales, c(da$x, db$x), c(da$y, db$y)))
  }
  if (!is.null(scales$trans)) {
    da <- .trans_munch(scales, xa, ya)
    db <- .trans_munch(scales, rev(xb), rev(yb))
    return(.xy_units(scales, c(da$x, db$x), c(da$y, db$y)))
  }
  .xy_units(scales, c(xa, rev(xb)), c(ya, rev(yb)))
}

.rect_units <- function(scales, xc, yc, w, h) {
  if (!is.null(scales$trans)) {
    # A separable warp keeps an axis-aligned rect axis-aligned, but its centre and
    # extent move nonlinearly, so map the corners and rebuild centre/size.
    ctx <- scales$trans
    x0 <- ctx$x_map(xc - w / 2)
    x1 <- ctx$x_map(xc + w / 2)
    y0 <- ctx$y_map(yc - h / 2)
    y1 <- ctx$y_map(yc + h / 2)
    return(list(
      x = vellum::vl_unit((x0 + x1) / 2, "native"),
      y = vellum::vl_unit((y0 + y1) / 2, "native"),
      width = vellum::vl_unit(abs(x1 - x0), "native"),
      height = vellum::vl_unit(abs(y1 - y0), "native")
    ))
  }
  if (.flipped(scales)) {
    list(
      x = vellum::vl_unit(yc, "native"),
      y = vellum::vl_unit(xc, "native"),
      width = vellum::vl_unit(h, "native"),
      height = vellum::vl_unit(w, "native")
    )
  } else {
    list(
      x = vellum::vl_unit(xc, "native"),
      y = vellum::vl_unit(yc, "native"),
      width = vellum::vl_unit(w, "native"),
      height = vellum::vl_unit(h, "native")
    )
  }
}

# Swap the two endpoints of a segment under flip (each arg is already a unit).
.seg_units <- function(scales, x0, y0, x1, y1) {
  if (.flipped(scales)) {
    list(x0 = y0, y0 = x0, x1 = y1, y1 = x1)
  } else {
    list(x0 = x0, y0 = y0, x1 = x1, y1 = y1)
  }
}

# An intercept may arrive as a mapped channel or a constant param.
.intercept <- function(L, name) L$values[[name]] %||% L$params[[name]]

# --- per-mark emitters (draw into the panel viewport, native units) ---------

# Above this row count, `mark_point(auto = TRUE)` switches to datashading.
.DATASHADE_AUTO <- 50000L

# Evaluate `expr` with the RNG temporarily seeded, restoring the caller's RNG
# stream afterwards (so a jitter seed gives reproducible output without
# disturbing the global random state). `seed = NULL` runs `expr` untouched.
.with_seed <- function(seed, expr) {
  if (is.null(seed)) {
    return(expr)
  }
  has_old <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  if (has_old) {
    old <- get(".Random.seed", envir = globalenv())
    on.exit(assign(".Random.seed", old, envir = globalenv()))
  } else {
    on.exit(rm(".Random.seed", envir = globalenv()))
  }
  set.seed(seed)
  expr
}

# --- sketch (hand-drawn) resolution -----------------------------------------

# The effective sketch spec for the layer currently being emitted: an explicit
# per-layer sketch wins; `NA`/`FALSE` forces crisp; `NULL` inherits the
# plot-wide theme_sketch() default carried on `scales$sketch`. A per-layer seed
# offset keeps different layers from sharing an identical wobble (the engine
# already varies the seed per element *within* a batched grob).
.mark_sketch <- function(L, scales) {
  s <- L$sketch
  if (is.null(s)) {
    s <- scales$sketch # inherit plot-wide default (NULL if none)
  } else if (length(s) == 1L && is.logical(s) && is.na(s)) {
    return(NULL) # NA / FALSE -> forced crisp
  }
  .sketch_bump(s, 100L * (.mark_ctx$layer %||% 0L))
}

# Return `s` with its seed shifted by `offset` (identity for NULL / non-sketch),
# so repeated grobs (style groups, categories) don't render the same wobble.
.sketch_bump <- function(s, offset) {
  if (is.null(s) || !inherits(s, "vellum_sketch") || !offset) {
    return(s)
  }
  s$seed <- s$seed + offset
  s
}

.emit_point <- function(scene, L, scales) {
  n <- L$n
  if (
    isTRUE(L$stat_params$auto) && n > .DATASHADE_AUTO && .can_datashade(scales)
  ) {
    return(.emit_datashade(scene, L, scales))
  }
  sk <- .mark_sketch(L, scales)
  xn <- rep_len(scales$x$map(L$values$x), n)
  yn <- rep_len(scales$y$map(L$values$y), n)
  band_x <- scales$x$band_width %||% .resolution(xn)
  # jitterdodge first offsets each element into its group's dodged slot, then
  # jitters within the (narrower) slot.
  slot <- band_x
  if (identical(L$position, "jitterdodge")) {
    grp <- L$values$color %||% L$values$fill
    dw <- L$stat_params$dodge_width %||% 0.75
    if (!is.null(grp)) {
      levs <- .cat_levels(grp)
      G <- length(levs)
      rank <- match(as.character(rep_len(grp, n)), levs)
      xn <- xn + (rank - (G + 1) / 2) / G * dw * band_x
      slot <- dw * band_x / G
    }
  }
  if (L$position %in% c("jitter", "jitterdodge")) {
    ax <- L$stat_params$jitter_width %||%
      (0.4 * if (identical(L$position, "jitterdodge")) slot else band_x)
    ay <- L$stat_params$jitter_height %||% (0.4 * .resolution(yn))
    jit <- .with_seed(
      L$stat_params$seed,
      list(
        x = stats::runif(n, -ax, ax),
        y = stats::runif(n, -ay, ay)
      )
    )
    xn <- xn + jit$x
    yn <- yn + jit$y
  }
  col <- rep_len(.aes_colour(L, scales, "black"), n)
  size <- rep_len(.aes_size(L, scales, 1), n)
  shape <- rep_len(.aes_shape(L, scales, "circle"), n)
  alpha <- rep_len(.aes_alpha(L, scales, NA_real_), n)

  for (idx in .style_groups(n, list(col = col, alpha = alpha))) {
    a <- alpha[idx[1]]
    xy <- .xy_units(scales, xn[idx], yn[idx])
    # PROVENANCE: `idx` are the layer rows in this style group -- the row-key
    # refinement (DESIGN §4). This is the canonical example; other grouped
    # emitters below should pass `rows = idx` the same way.
    scene <- .draw(
      scene,
      vellum::points_grob(
        xy$x,
        xy$y,
        size = vellum::vl_unit(size[idx], "mm"),
        shape = shape[idx],
        sketch = sk,
        gp = vellum::vl_gpar(
          fill = col[idx[1]],
          col = col[idx[1]],
          alpha = gp_alpha(a)
        )
      ),
      rows = idx
    )
  }
  scene
}

.emit_line <- function(scene, L, scales) {
  n <- L$n
  if (
    isTRUE(L$stat_params$auto) && n > .DATASHADE_AUTO && .can_datashade(scales)
  ) {
    return(.emit_line_datashade(scene, L, scales))
  }
  xn <- rep_len(scales$x$map(L$values$x), n)
  yn <- rep_len(scales$y$map(L$values$y), n)
  col <- rep_len(.aes_colour(L, scales, "black"), n)
  alpha <- rep_len(.aes_alpha(L, scales, NA_real_), n)
  lty <- .resolve_lty(L, scales, n)
  lwd <- .aes_param(L, "linewidth", 1.5)
  sk <- .mark_sketch(L, scales)

  gi <- 0L
  for (idx in .style_groups(n, list(col = col, alpha = alpha, lty = lty))) {
    o <- idx[order(xn[idx])] # a line is drawn in x order
    xy <- .xy_path(scales, xn[o], yn[o])
    scene <- .draw(
      scene,
      vellum::lines_grob(
        xy$x,
        xy$y,
        sketch = .sketch_bump(sk, gi),
        gp = .gp_stroke(col, alpha, idx[1], lwd, lty)
      ),
      # PROVENANCE: `o` are the layer rows this polyline draws (x-ordered).
      rows = o
    )
    gi <- gi + 1L
  }
  scene
}

# A sparkline (vsparkline()): a compact, axis-free chart of one series drawn to
# fill the panel box. `type` picks the shape -- a "line" trend (with optional dots
# on its extremes / last point), a "bar" column micro-chart (bars from the
# clamped baseline), or a "winloss" chart of equal up/down bars about the
# baseline. Read the options from `stat_params`; draws in the trained x/y space.
.emit_sparkline <- function(scene, L, scales) {
  n <- L$n
  sp <- L$stat_params
  type <- sp$type %||% "line"
  rawx <- rep_len(L$values$x, n)
  rawy <- rep_len(L$values$y, n)
  o <- order(rawx)
  rawx <- rawx[o]
  rawy <- rawy[o]
  x <- scales$x$map(rawx)
  y <- scales$y$map(rawy)
  col <- sp$color %||% "grey30"
  w <- 0.7 * .resolution(x)

  if (identical(type, "line")) {
    xy <- .xy_path(scales, x, y)
    scene <- .draw(
      scene,
      vellum::lines_grob(
        xy$x,
        xy$y,
        gp = vellum::vl_gpar(
          col = col,
          lwd = sp$linewidth %||% 1,
          lineend = "round",
          linejoin = "round"
        )
      ),
      rows = o
    )
    hi <- switch(
      sp$points %||% "extremes",
      extremes = unique(c(which.min(rawy), which.max(rawy))),
      last = n,
      integer(0)
    )
    if (length(hi)) {
      pc <- sp$point_color %||% "firebrick"
      d <- .xy_units(scales, x[hi], y[hi])
      scene <- .draw(
        scene,
        vellum::points_grob(
          d$x,
          d$y,
          size = vellum::vl_unit(sp$point_size %||% 1.5, "mm"),
          shape = "circle",
          gp = vellum::vl_gpar(fill = pc, col = pc)
        ),
        rows = o[hi]
      )
    }
    return(scene)
  }

  if (identical(type, "bar")) {
    base_n <- .clamp_baseline(scales$y$map(sp$baseline %||% 0), scales$y$domain)
    r <- .rect_units(
      scales,
      x,
      (base_n + y) / 2,
      rep_len(w, n),
      abs(y - base_n)
    )
    return(.draw(
      scene,
      vellum::rect_grob(
        r$x,
        r$y,
        width = r$width,
        height = r$height,
        gp = vellum::vl_gpar(fill = col, col = NA)
      ),
      rows = o
    ))
  }

  # winloss: equal-height up/down bars about the baseline threshold.
  baseval <- sp$baseline %||% 0
  up <- rawy >= baseval
  mid <- scales$y$map(baseval)
  dom <- scales$y$domain
  h <- 0.4 * (dom[2] - dom[1])
  ytop <- ifelse(up, mid + h, mid)
  ybot <- ifelse(up, mid, mid - h)
  for (side in list(
    list(sel = up, fill = sp$win_color %||% "#2c7fb8"),
    list(sel = !up, fill = sp$loss_color %||% "#d7301f")
  )) {
    sel <- which(side$sel)
    if (!length(sel)) {
      next
    }
    r <- .rect_units(
      scales,
      x[sel],
      (ytop[sel] + ybot[sel]) / 2,
      rep_len(w, length(sel)),
      abs(ytop[sel] - ybot[sel])
    )
    scene <- .draw(
      scene,
      vellum::rect_grob(
        r$x,
        r$y,
        width = r$width,
        height = r$height,
        gp = vellum::vl_gpar(fill = side$fill, col = NA)
      ),
      rows = o[sel]
    )
  }
  scene
}

# Contour lines: one polyline per traced piece, vertices kept in trace order (not
# x-sorted — a contour is not a function of x), coloured by the mapped level.
.emit_contour <- function(scene, L, scales) {
  n <- L$n
  if (!n) {
    return(scene)
  }
  xn <- rep_len(scales$x$map(L$values$x), n)
  yn <- rep_len(scales$y$map(L$values$y), n)
  col <- rep_len(.aes_colour(L, scales, "#3366bb"), n)
  alpha <- rep_len(.aes_alpha(L, scales, NA_real_), n)
  lwd <- .aes_param(L, "linewidth", 0.6)
  lty <- .resolve_lty(L, scales, n)
  piece <- L$values$.piece
  sk <- .mark_sketch(L, scales)
  gi <- 0L
  for (pid in unique(piece)) {
    idx <- which(piece == pid)
    xy <- .xy_path(scales, xn[idx], yn[idx])
    scene <- .draw(
      scene,
      vellum::lines_grob(
        xy$x,
        xy$y,
        sketch = .sketch_bump(sk, gi),
        gp = .gp_stroke(col, alpha, idx[1], lwd, lty)
      ),
      rows = idx
    )
    gi <- gi + 1L
  }
  scene
}

# Filled contour bands: one even-odd path per band (rings via `id`, so holes are
# cut out), filled by the band's level.
.emit_contour_filled <- function(scene, L, scales) {
  n <- L$n
  if (!n) {
    return(scene)
  }
  xn <- rep_len(scales$x$map(L$values$x), n)
  yn <- rep_len(scales$y$map(L$values$y), n)
  fill <- rep_len(.aes_colour(L, scales, "#3366bb"), n)
  alpha <- rep_len(.aes_alpha(L, scales, NA_real_), n)
  piece <- L$values$.piece
  ring <- L$values$.ring
  sk <- .mark_sketch(L, scales)
  gi <- 0L
  for (pid in unique(piece)) {
    idx <- which(piece == pid)
    a <- alpha[idx[1]]
    xy <- .xy_path(scales, xn[idx], yn[idx])
    scene <- .draw(
      scene,
      vellum::path_grob(
        xy$x,
        xy$y,
        id = ring[idx],
        rule = "evenodd",
        sketch = .sketch_bump(sk, gi),
        gp = vellum::vl_gpar(
          fill = fill[idx[1]],
          col = NA,
          alpha = gp_alpha(a)
        )
      ),
      rows = idx
    )
    gi <- gi + 1L
  }
  scene
}

.emit_rule <- function(scene, L, scales) {
  col <- .aes_colour(L, scales, "grey40")[1]
  lwd <- .aes_param(L, "linewidth", 1)
  lty <- .aes_linetype(L, scales, NULL)
  gp <- vellum::vl_gpar(col = col, lwd = lwd, lty = lty)
  sk <- .mark_sketch(L, scales)
  yi <- .intercept(L, "yintercept")
  xi <- .intercept(L, "xintercept")
  if (!is.null(yi)) {
    vy <- scales$y$map(yi)
    if (length(vy)) {
      k <- length(vy)
      s <- .seg_units(
        scales,
        vellum::vl_unit(rep(0, k), "npc"),
        vellum::vl_unit(vy, "native"),
        vellum::vl_unit(rep(1, k), "npc"),
        vellum::vl_unit(vy, "native")
      )
      scene <- .draw(
        scene,
        vellum::segments_grob(s$x0, s$y0, s$x1, s$y1, sketch = sk, gp = gp)
      )
    }
  }
  if (!is.null(xi)) {
    vx <- scales$x$map(xi)
    if (length(vx)) {
      k <- length(vx)
      s <- .seg_units(
        scales,
        vellum::vl_unit(vx, "native"),
        vellum::vl_unit(rep(0, k), "npc"),
        vellum::vl_unit(vx, "native"),
        vellum::vl_unit(rep(1, k), "npc")
      )
      scene <- .draw(
        scene,
        vellum::segments_grob(
          s$x0,
          s$y0,
          s$x1,
          s$y1,
          sketch = .sketch_bump(sk, 1L),
          gp = gp
        )
      )
    }
  }
  scene
}

# Smallest positive gap between sorted unique positions (the bar width unit on a
# continuous x); 1 if there is only one bar.
.resolution <- function(x) {
  u <- sort(unique(x[is.finite(x)]))
  if (length(u) < 2) {
    return(1)
  }
  min(diff(u))
}

# Bars in polar space become annular sectors (wedges). With theta = "x" (rose /
# coxcomb) the categorical x band sets the angular span and y the radius; with
# theta = "y" (pie / bullseye) the stacked [ymin, ymax] sets the angular span —
# normalized per x-group so a slice set always closes the full circle — and the
# x band sets the radial ring. Wedges tile the full circle (no inter-bar gap).
.emit_bar_polar <- function(scene, L, scales) {
  ctx <- scales$polar
  n <- L$n
  xp <- rep_len(scales$x$map(L$values$x), n)
  fill <- rep_len(.aes_colour(L, scales, "grey35"), n)
  alpha <- rep_len(.aes_alpha(L, scales, NA_real_), n)
  band <- scales$x$band_width %||% .resolution(xp)
  w <- rep_len(L$values$width %||% band, n)

  ymin_d <- if (!is.null(L$values$ymin)) L$values$ymin else rep(0, n)
  ymax_d <- if (!is.null(L$values$ymax)) L$values$ymax else L$values$y
  ymin <- rep_len(as.numeric(ymin_d), n)
  ymax <- rep_len(as.numeric(ymax_d), n)

  if (identical(ctx$theta_aes, "x")) {
    a0 <- ctx$theta_map(xp - w / 2)
    a1 <- ctx$theta_map(xp + w / 2)
    r0 <- ctx$r_map(ymin)
    r1 <- ctx$r_map(ymax)
  } else {
    # Normalize by the sum of the per-slice spans within an x-group. For the
    # stacked input this path expects (ymin/ymax cumulative from 0) this equals
    # max(ymax); summing the spans keeps the total correct if the slices are not
    # pre-stacked.
    total <- stats::ave(ymax - ymin, as.character(L$values$x), FUN = sum)
    total[total == 0] <- 1
    a0 <- ctx$ang_frac(ymin / total)
    a1 <- ctx$ang_frac(ymax / total)
    r0 <- ctx$r_map(xp - w / 2)
    r1 <- ctx$r_map(xp + w / 2)
  }
  theta0 <- pmin(a0, a1)
  theta1 <- pmax(a0, a1)
  sk <- .mark_sketch(L, scales)

  for (idx in .style_groups(n, list(fill = fill, alpha = alpha))) {
    a <- alpha[idx[1]]
    scene <- .draw(
      scene,
      vellum::sector_grob(
        x = vellum::vl_unit(rep(0, length(idx)), "native"),
        y = vellum::vl_unit(rep(0, length(idx)), "native"),
        r0 = vellum::vl_unit(r0[idx], "native"),
        r1 = vellum::vl_unit(r1[idx], "native"),
        theta0 = theta0[idx],
        theta1 = theta1[idx],
        fill = fill[idx],
        sketch = sk,
        gp = vellum::vl_gpar(col = NA, alpha = gp_alpha(a))
      ),
      rows = idx
    )
  }
  scene
}

# dodge2: at each x, place only the groups actually present, filling the total
# width `tw` with equal slots separated by `padding` (a fraction of the slot). So
# a category with fewer groups gets wider bars centred on the tick, rather than
# leaving gaps for absent groups (plain dodge). Returns per-element centre `xc`
# and width `w` (native px), aligned to the input row order.
.dodge2_bars <- function(xp, grp, n, tw, padding) {
  xc <- rep_len(xp, n)
  w <- rep_len(tw, n)
  if (is.null(grp)) {
    return(list(xc = xc, w = w))
  }
  levs <- .cat_levels(grp)
  g <- as.character(rep_len(grp, n))
  for (xi in unique(xc)) {
    rows <- which(xc == xi)
    rows <- rows[order(match(g[rows], levs))]
    k <- length(rows)
    slot <- tw[rows][1] / k
    for (j in seq_len(k)) {
      i <- rows[j]
      w[i] <- slot * (1 - padding)
      xc[i] <- xi + (j - (k + 1) / 2) * slot
    }
  }
  list(xc = xc, w = w)
}

.emit_bar <- function(scene, L, scales) {
  grad <- .paint_fill(L)
  if (!is.null(scales$polar)) {
    if (!is.null(grad)) {
      cli::cli_abort(
        "Paint fills (gradient/pattern) are not supported for polar bars / pies."
      )
    }
    return(.emit_bar_polar(scene, L, scales))
  }
  n <- L$n
  sk <- .mark_sketch(L, scales)
  xp <- rep_len(scales$x$map(L$values$x), n)
  fill <- if (is.null(grad)) rep_len(.aes_colour(L, scales, "grey35"), n)
  alpha <- rep_len(.aes_alpha(L, scales, NA_real_), n)
  band <- scales$x$band_width %||% .resolution(xp)

  # vertical span: a stacked [ymin, ymax], else 0 -> y. The baseline is clamped
  # into the finite domain so a log/sqrt y scale (where map(0) = -Inf) still
  # draws bars from the axis floor rather than a degenerate rect.
  ymin_d <- if (!is.null(L$values$ymin)) L$values$ymin else rep(0, n)
  ymax_d <- if (!is.null(L$values$ymax)) L$values$ymax else L$values$y
  y0 <- rep_len(.clamp_baseline(scales$y$map(ymin_d), scales$y$domain), n)
  y1 <- rep_len(scales$y$map(ymax_d), n)

  # Bar width: a stat-provided `width` (histogram bins) fills the bin so bars
  # touch; categorical / explicit bars leave a 10% gap around the band.
  w <- rep_len(L$values$width %||% (0.9 * band), n)
  xc <- xp
  if (identical(L$position, "dodge")) {
    grp <- L$values$color %||% L$values$fill
    if (!is.null(grp)) {
      levs <- .cat_levels(grp)
      G <- length(levs)
      rank <- match(as.character(rep_len(grp, n)), levs)
      # `dodge_width` (data units) overrides the band as the group's total span.
      span <- L$stat_params$dodge_width %||% band
      w <- w / G
      xc <- xp + (rank - (G + 1) / 2) / G * span
    }
  } else if (identical(L$position, "dodge2")) {
    d2 <- .dodge2_bars(
      xp,
      L$values$color %||% L$values$fill,
      n,
      L$values$width %||% (0.9 * band),
      L$stat_params$dodge2_padding %||% 0.1
    )
    xc <- d2$xc
    w <- d2$w
  }

  # A gradient fill paints every bar as one rect grob sharing the paint.
  if (!is.null(grad)) {
    r <- .rect_units(scales, xc, (y0 + y1) / 2, w, abs(y1 - y0))
    return(.draw(
      scene,
      vellum::rect_grob(
        x = r$x,
        y = r$y,
        width = r$width,
        height = r$height,
        sketch = sk,
        gp = vellum::vl_gpar(fill = grad, col = NA)
      ),
      rows = seq_len(n)
    ))
  }

  gi <- 0L
  pkey <- .aes_pattern_key(L, scales, n)
  for (idx in .style_groups(
    n,
    c(list(fill = fill, alpha = alpha), if (!is.null(pkey)) list(pat = pkey))
  )) {
    r <- .rect_units(
      scales,
      xc[idx],
      (y0[idx] + y1[idx]) / 2,
      w[idx],
      abs(y1[idx] - y0[idx])
    )
    scene <- .draw(
      scene,
      vellum::rect_grob(
        x = r$x,
        y = r$y,
        width = r$width,
        height = r$height,
        sketch = .sketch_bump(sk, gi),
        gp = .gp_fill(fill, alpha, idx[1], .pattern_obj(scales, pkey, idx[1]))
      ),
      rows = idx
    )
    gi <- gi + 1L
  }
  scene
}

# A fitted smooth: a confidence ribbon (polygon) under a fitted line, one per
# colour group.
.emit_smooth <- function(scene, L, scales) {
  n <- L$n
  xn <- rep_len(scales$x$map(L$values$x), n)
  yn <- rep_len(scales$y$map(L$values$y), n)
  col <- rep_len(.aes_colour(L, scales, "#3366CC"), n)
  sk <- .mark_sketch(L, scales)
  has_se <- !is.null(L$values$ymin)
  ymin <- if (has_se) scales$y$map(L$values$ymin)
  ymax <- if (has_se) scales$y$map(L$values$ymax)

  gi <- 0L
  for (idx in .style_groups(n, list(col = col))) {
    o <- idx[order(xn[idx])]
    cc <- col[idx[1]]
    if (has_se) {
      poly <- .xy_area(scales, xn[o], ymin[o], xn[o], ymax[o])
      scene <- .draw(
        scene,
        vellum::polygon_grob(
          poly$x,
          poly$y,
          sketch = .sketch_bump(sk, gi),
          gp = vellum::vl_gpar(fill = cc, col = NA, alpha = 0.25)
        )
      )
    }
    ln <- .xy_path(scales, xn[o], yn[o])
    scene <- .draw(
      scene,
      vellum::lines_grob(
        ln$x,
        ln$y,
        sketch = .sketch_bump(sk, gi + 50L),
        gp = vellum::vl_gpar(col = cc, lwd = 1.5)
      )
    )
    gi <- gi + 1L
  }
  scene
}

# A filled band between two y boundaries `ya`/`yb` (already mapped to native y),
# drawn as one x-ordered polygon per style group -- the shared body of the ribbon
# and area marks. A gradient fill paints the whole band as one polygon.
.emit_band <- function(scene, L, scales, ya, yb) {
  n <- L$n
  xn <- rep_len(scales$x$map(L$values$x), n)
  sk <- .mark_sketch(L, scales)

  grad <- .paint_fill(L)
  if (!is.null(grad)) {
    o <- order(xn)
    poly <- .xy_area(scales, xn[o], ya[o], xn[o], yb[o])
    return(.draw(
      scene,
      vellum::polygon_grob(
        poly$x,
        poly$y,
        sketch = sk,
        gp = vellum::vl_gpar(fill = grad, col = NA)
      ),
      # PROVENANCE: one polygon over the whole layer, in x order.
      rows = o
    ))
  }

  fill <- rep_len(.aes_colour(L, scales, "grey50"), n)
  alpha <- rep_len(.aes_alpha(L, scales, NA_real_), n)

  gi <- 0L
  for (idx in .style_groups(n, list(fill = fill, alpha = alpha))) {
    o <- idx[order(xn[idx])]
    poly <- .xy_area(scales, xn[o], ya[o], xn[o], yb[o])
    scene <- .draw(
      scene,
      vellum::polygon_grob(
        poly$x,
        poly$y,
        sketch = .sketch_bump(sk, gi),
        gp = .gp_fill(fill, alpha, idx[1])
      ),
      # PROVENANCE: `o` are the layer rows this polygon draws (x-ordered).
      rows = o
    )
    gi <- gi + 1L
  }
  scene
}

# A filled ribbon between ymin and ymax, one polygon per style group.
.emit_ribbon <- function(scene, L, scales) {
  n <- L$n
  ymin <- rep_len(scales$y$map(L$values$ymin), n)
  ymax <- rep_len(scales$y$map(L$values$ymax), n)
  .emit_band(scene, L, scales, ymin, ymax)
}

# An area mark: the region between `y` and the zero baseline, or between a
# stacked `[ymin, ymax]` span when `position = "stack"`/`"fill"` set one (see
# `.position_stack`). Baseline resolution mirrors `.emit_bar`.
.emit_area <- function(scene, L, scales) {
  n <- L$n
  # vertical span: a stacked [ymin, ymax], else 0 -> y. The baseline is clamped
  # into the finite domain so a log/sqrt y scale (map(0) = -Inf) still draws from
  # the axis floor. `.emit_band` takes (near, far) = (y1, y0) to match the
  # historical polygon winding.
  ymin_d <- if (!is.null(L$values$ymin)) L$values$ymin else rep(0, n)
  ymax_d <- if (!is.null(L$values$ymax)) L$values$ymax else L$values$y
  y0 <- rep_len(.clamp_baseline(scales$y$map(ymin_d), scales$y$domain), n)
  y1 <- rep_len(scales$y$map(ymax_d), n)
  .emit_band(scene, L, scales, y1, y0)
}

# A staircase line: each segment is expanded into a horizontal-then-vertical
# ("hv") or vertical-then-horizontal ("vh") pair before drawing.
.emit_step <- function(scene, L, scales) {
  n <- L$n
  if (
    isTRUE(L$stat_params$auto) && n > .DATASHADE_AUTO && .can_datashade(scales)
  ) {
    return(.emit_line_datashade(scene, L, scales, step = TRUE))
  }
  xn <- rep_len(scales$x$map(L$values$x), n)
  yn <- rep_len(scales$y$map(L$values$y), n)
  col <- rep_len(.aes_colour(L, scales, "black"), n)
  alpha <- rep_len(.aes_alpha(L, scales, NA_real_), n)
  lty <- .resolve_lty(L, scales, n)
  lwd <- .aes_param(L, "linewidth", 1.5)
  dir <- L$stat_params$direction %||% "hv"
  sk <- .mark_sketch(L, scales)

  gi <- 0L
  for (idx in .style_groups(n, list(col = col, alpha = alpha, lty = lty))) {
    o <- idx[order(xn[idx])]
    sx <- xn[o]
    sy <- yn[o]
    m <- length(sx)
    if (m >= 2) {
      if (identical(dir, "vh")) {
        ex <- c(rep(sx[-m], each = 2), sx[m])
        ey <- c(sy[1], rep(sy[-1], each = 2))
      } else {
        ex <- c(sx[1], rep(sx[-1], each = 2))
        ey <- c(rep(sy[-m], each = 2), sy[m])
      }
    } else {
      ex <- sx
      ey <- sy
    }
    ln <- .xy_path(scales, ex, ey)
    scene <- .draw(
      scene,
      vellum::lines_grob(
        ln$x,
        ln$y,
        sketch = .sketch_bump(sk, gi),
        gp = .gp_stroke(col, alpha, idx[1], lwd, lty)
      ),
      # PROVENANCE: `o` are the layer rows this staircase draws (x-ordered).
      rows = o
    )
    gi <- gi + 1L
  }
  scene
}

# Text colour for text/label marks: `.aes_colour` without the fill fallback,
# since `fill` is the label background here, not the ink colour.
.text_colour <- function(L, scales, default) {
  .aes_colour(L, scales, default, fill_fallback = FALSE)
}

# Per-element text angle: a mapped channel or a constant param (degrees).
.text_angle <- function(L, n) {
  if (!is.null(L$values$angle)) {
    rep_len(L$values$angle, n)
  } else {
    rep_len(.aes_param(L, "angle", 0), n)
  }
}

.emit_text <- function(scene, L, scales) {
  n <- L$n
  sol <- L$stat_params$repel$solution
  if (!is.null(sol)) {
    scene <- .emit_repel_leaders(scene, L, scales)
    xn <- rep_len(sol$x, n)
    yn <- rep_len(sol$y, n)
  } else {
    xn <- rep_len(scales$x$map(L$values$x), n)
    yn <- rep_len(scales$y$map(L$values$y), n)
  }
  # Labels may be plain character (multi-line "\n" supported by vellum), a single
  # rich md() label (drawn at every position), or a per-datum list of md() labels.
  # Only plain labels are flattened with as.character(); rich labels pass through.
  raw <- L$values$label
  rich_single <- inherits(raw, "vellum::vellum_label")
  rich_list <- !rich_single &&
    is.list(raw) &&
    length(raw) > 0L &&
    all(vapply(
      raw,
      function(x) inherits(x, "vellum::vellum_label"),
      logical(1)
    ))
  if (rich_list) {
    labs <- raw[rep_len(seq_along(raw), n)]
  } else if (!rich_single) {
    label <- rep_len(as.character(raw), n)
  }
  .label_of <- function(idx) {
    if (rich_single) {
      raw
    } else if (rich_list) {
      labs[idx]
    } else {
      label[idx]
    }
  }
  col <- rep_len(.text_colour(L, scales, "black"), n)
  alpha <- rep_len(.aes_alpha(L, scales, NA_real_), n)
  ang <- .text_angle(L, n)
  fs <- .aes_param(L, "size", 8)
  # `hjust`/`vjust` may arrive as a constant param or (when passed a variable) a
  # resolved value; check values first, like `.text_angle`. A single just applies
  # to the whole batch, so take the first if a vector came through.
  hj <- L$values$hjust %||% .aes_param(L, "hjust", "centre")
  vj <- L$values$vjust %||% .aes_param(L, "vjust", "centre")
  just <- c(hj[[1]], vj[[1]])

  for (idx in .style_groups(n, list(col = col, alpha = alpha))) {
    a <- alpha[idx[1]]
    xy <- .nudge_xy(.xy_units(scales, xn[idx], yn[idx]), L, idx)
    scene <- .draw(
      scene,
      vellum::text_grob(
        .label_of(idx),
        xy$x,
        xy$y,
        just = just,
        rot = ang[idx],
        gp = vellum::vl_gpar(
          fontsize = fs,
          col = col[idx[1]],
          fontfamily = .aes_param(L, "family", NULL),
          fontface = .aes_param(L, "fontface", NULL),
          alpha = gp_alpha(a)
        )
      ),
      # PROVENANCE: `idx` are the layer rows in this style group.
      rows = idx
    )
  }
  scene
}

# A text mark with a filled rounded background sized to each label.
.emit_label <- function(scene, L, scales) {
  n <- L$n
  sol <- L$stat_params$repel$solution
  if (!is.null(sol)) {
    scene <- .emit_repel_leaders(scene, L, scales)
    xn <- rep_len(sol$x, n)
    yn <- rep_len(sol$y, n)
  } else {
    xn <- rep_len(scales$x$map(L$values$x), n)
    yn <- rep_len(scales$y$map(L$values$y), n)
  }
  label <- rep_len(as.character(L$values$label), n)
  col <- rep_len(.text_colour(L, scales, "black"), n)
  # Label background: a mapped `fill` channel (through the colour scale), else a
  # constant `fill` param, else white. `.text_colour` deliberately keeps `fill`
  # out of the ink colour, so the background is resolved here on its own.
  bg <- if (!is.null(scales$color) && !is.null(L$values$fill)) {
    scales$color$map(L$values$fill)
  } else {
    L$params$fill %||% "white"
  }
  bg <- rep_len(bg, n)
  alpha <- rep_len(.aes_alpha(L, scales, NA_real_), n)
  sk <- .mark_sketch(L, scales)
  fs <- .aes_param(L, "size", 8)
  pad <- vellum::vl_unit(1.2, "mm")
  ws <- do.call(
    c,
    lapply(label, function(l) vellum::grobwidth(.txt(l, fs)) + pad)
  )
  hs <- do.call(
    c,
    lapply(label, function(l) vellum::grobheight(.txt(l, fs)) + pad)
  )

  for (idx in .style_groups(n, list(col = col, fill = bg, alpha = alpha))) {
    a <- alpha[idx[1]]
    xy <- .nudge_xy(.xy_units(scales, xn[idx], yn[idx]), L, idx)
    scene <- .draw(
      scene,
      vellum::roundrect_grob(
        x = xy$x,
        y = xy$y,
        width = ws[idx],
        height = hs[idx],
        r = vellum::vl_unit(0.8, "mm"),
        sketch = sk,
        gp = vellum::vl_gpar(
          fill = bg[idx[1]],
          col = NA,
          alpha = gp_alpha(a)
        )
      )
    )
    scene <- .draw(
      scene,
      vellum::text_grob(
        label[idx],
        xy$x,
        xy$y,
        gp = vellum::vl_gpar(
          fontsize = fs,
          col = col[idx[1]],
          alpha = gp_alpha(a)
        )
      )
    )
  }
  scene
}

# A heatmap of rectangular tiles at each (x, y), coloured by fill. Width/height
# default to the data resolution so tiles abut.
.emit_tile <- function(scene, L, scales) {
  n <- L$n
  paint <- .paint_fill(L)
  xp <- rep_len(scales$x$map(L$values$x), n)
  yp <- rep_len(scales$y$map(L$values$y), n)
  fill <- rep_len(.aes_colour(L, scales, "grey50"), n)
  alpha <- rep_len(.aes_alpha(L, scales, NA_real_), n)
  w <- rep_len(L$values$width %||% .resolution(xp), n)
  h <- rep_len(L$values$height %||% .resolution(yp), n)
  sk <- .mark_sketch(L, scales)

  gi <- 0L
  pkey <- .aes_pattern_key(L, scales, n)
  for (idx in .style_groups(
    n,
    c(list(fill = fill, alpha = alpha), if (!is.null(pkey)) list(pat = pkey))
  )) {
    r <- .rect_units(scales, xp[idx], yp[idx], w[idx], h[idx])
    scene <- .draw(
      scene,
      vellum::rect_grob(
        x = r$x,
        y = r$y,
        width = r$width,
        height = r$height,
        sketch = .sketch_bump(sk, gi),
        gp = .gp_fill(
          fill,
          alpha,
          idx[1],
          paint %||% .pattern_obj(scales, pkey, idx[1])
        )
      ),
      rows = idx
    )
    gi <- gi + 1L
  }
  scene
}

# A filled rectangle spanning [xmin, xmax] x [ymin, ymax] (from annotate("rect")).
# Unlike a tile, the corner bounds are kept so an infinite bound resolves to the
# panel edge via `.bound_native`. Bounds are mapped to native, then rebuilt into
# a centre + size for `.rect_units` (which handles flip / coord_trans).
.emit_rect <- function(scene, L, scales) {
  n <- L$n
  paint <- .paint_fill(L)
  x0 <- rep_len(.bound_native(L$values$xmin, scales$x), n)
  x1 <- rep_len(.bound_native(L$values$xmax, scales$x), n)
  y0 <- rep_len(.bound_native(L$values$ymin, scales$y), n)
  y1 <- rep_len(.bound_native(L$values$ymax, scales$y), n)
  xc <- (x0 + x1) / 2
  yc <- (y0 + y1) / 2
  w <- abs(x1 - x0)
  h <- abs(y1 - y0)
  fill <- rep_len(.aes_colour(L, scales, "grey50"), n)
  alpha <- rep_len(.aes_alpha(L, scales, NA_real_), n)
  sk <- .mark_sketch(L, scales)

  gi <- 0L
  pkey <- .aes_pattern_key(L, scales, n)
  for (idx in .style_groups(
    n,
    c(list(fill = fill, alpha = alpha), if (!is.null(pkey)) list(pat = pkey))
  )) {
    r <- .rect_units(scales, xc[idx], yc[idx], w[idx], h[idx])
    scene <- .draw(
      scene,
      vellum::rect_grob(
        x = r$x,
        y = r$y,
        width = r$width,
        height = r$height,
        sketch = .sketch_bump(sk, gi),
        gp = .gp_fill(
          fill,
          alpha,
          idx[1],
          paint %||% .pattern_obj(scales, pkey, idx[1])
        )
      ),
      rows = idx
    )
    gi <- gi + 1L
  }
  scene
}

# A heatmap drawn as one raster image (fast path for a complete regular grid).
.emit_raster <- function(scene, L, scales) {
  if (.flipped(scales)) {
    cli::cli_abort(
      "{.fn mark_raster} does not support {.fn coord_flip}; use {.fn mark_tile}."
    )
  }
  xv <- L$values$x
  yv <- L$values$y
  ux <- sort(unique(xv))
  uy <- sort(unique(yv))
  ci <- match(xv, ux)
  ri <- match(yv, uy)
  # A complete regular grid: the right row count AND one datum per cell. The
  # count alone would pass a duplicated cell paired with a missing one, leaving a
  # transparent hole where the missing cell should be.
  if (
    length(ux) * length(uy) != length(xv) ||
      anyDuplicated(cbind(ri, ci))
  ) {
    cli::cli_abort(
      "{.fn mark_raster} needs a complete regular grid; use {.fn mark_tile}."
    )
  }
  cols <- rep_len(.aes_colour(L, scales, "grey50"), length(xv))
  m <- matrix("#FFFFFF00", nrow = length(uy), ncol = length(ux))
  m[cbind(length(uy) - ri + 1L, ci)] <- cols # row 1 = top = max y
  xw <- if (length(ux) > 1) min(diff(ux)) else 1
  yw <- if (length(uy) > 1) min(diff(uy)) else 1
  xn <- scales$x$map(c(min(ux) - xw / 2, max(ux) + xw / 2))
  yn <- scales$y$map(c(min(uy) - yw / 2, max(uy) + yw / 2))
  .draw(
    scene,
    vellum::raster_grob(
      grDevices::as.raster(m),
      x = vellum::vl_unit(mean(xn), "native"),
      y = vellum::vl_unit(mean(yn), "native"),
      width = vellum::vl_unit(diff(range(xn)), "native"),
      height = vellum::vl_unit(diff(range(yn)), "native"),
      interpolate = FALSE
    )
  )
}

# One image drawn per row at (x, y): a raster_grob sized in mm, aspect preserved
# from the image's own pixel dimensions. `src` is a per-row column (values) or a
# constant path (params); images are decoded lazily and cached by path (logos
# repeat across rows). Positions warp through the value-based seam like text, so
# coord_flip/coord_trans place the fixed-mm image box correctly.
.emit_image <- function(scene, L, scales) {
  n <- L$n
  xn <- rep_len(scales$x$map(L$values$x), n)
  yn <- rep_len(scales$y$map(L$values$y), n)
  src <- L$values$src %||% L$params$src
  if (is.null(src) || !length(src)) {
    cli::cli_abort(
      "{.fn mark_image} needs an image source; map or set {.arg src}."
    )
  }
  src <- rep_len(as.character(src), n)
  size <- rep_len(
    if (!is.null(L$values$size)) L$values$size else .aes_param(L, "size", 5),
    n
  )
  interpolate <- isTRUE(L$params$interpolate %||% TRUE)
  cache <- new.env(parent = emptyenv())
  for (i in seq_len(n)) {
    img <- .read_image(src[i], cache)
    h_mm <- size[i]
    w_mm <- size[i] * (img$iw / img$ih)
    xy <- .nudge_xy(.xy_units(scales, xn[i], yn[i]), L, i)
    scene <- .draw(
      scene,
      vellum::raster_grob(
        img$raster,
        x = xy$x,
        y = xy$y,
        width = vellum::vl_unit(w_mm, "mm"),
        height = vellum::vl_unit(h_mm, "mm"),
        interpolate = interpolate
      ),
      # PROVENANCE + interactivity: this grob draws layer row i.
      rows = i
    )
  }
  scene
}

# Decode a local image file to an object `raster_grob` accepts, plus its pixel
# dims. magick reads most formats (PNG, JPEG, SVG, GIF, ...); memoised per path
# in `cache` so a repeated logo is decoded once.
.read_image <- function(path, cache = NULL) {
  if (!is.null(cache) && !is.null(cache[[path]])) {
    return(cache[[path]])
  }
  if (
    !is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)
  ) {
    cli::cli_abort("Each {.arg src} must be a non-empty file path.")
  }
  if (!file.exists(path)) {
    cli::cli_abort("Image file not found: {.path {path}}.")
  }
  .need_pkg("magick", "mark_image()")
  r <- grDevices::as.raster(magick::image_read(path))
  # dim(<raster>) is c(height, width) in pixels.
  d <- dim(r)
  out <- list(raster = r, ih = d[1], iw = d[2])
  if (!is.null(cache)) {
    cache[[path]] <- out
  }
  out
}

# A box-and-whisker per x category: box (Q1-Q3), median line, Tukey whiskers
# (1.5*IQR), and outlier points. Summary is computed here from the raw y values.
.emit_boxplot <- function(scene, L, scales) {
  xv <- L$values$x
  yv <- as.numeric(L$values$y)
  colv <- rep_len(.aes_colour(L, scales, "white"), length(yv))
  sk <- .mark_sketch(L, scales)
  levs <- .cat_levels(xv)
  xc_all <- scales$x$map(levs)
  band <- scales$x$band_width %||% .resolution(scales$x$map(xv))
  hw <- 0.375 * band
  xchar <- as.character(xv)
  my <- function(v) scales$y$map(v)

  for (j in seq_along(levs)) {
    sel <- which(xchar == levs[j])
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
          lwd = 1
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
  dens <- .density_by_cat(yv, xv, levs, adjust)
  sk <- .mark_sketch(L, scales)
  for (j in seq_along(levs)) {
    d <- dens[[j]]
    if (is.null(d)) {
      next
    }
    sel <- which(xchar == levs[j])
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
  dens <- if (slab) .density_by_cat(yv, xv, levs, adjust) else NULL
  sk <- .mark_sketch(L, scales)
  mrk <- if (slab) "mark_halfeye" else "mark_interval"
  # inner intervals thick, outer thin; drawn widest-first so the thick bar is on top
  lwd_by_width <- seq(4, 1.5, length.out = length(widths))

  for (j in seq_along(levs)) {
    sel <- which(xchar == levs[j])
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
            alpha = gp_alpha(0.5)
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
          gp = vellum::vl_gpar(col = ccol, lwd = lwd_by_width[wi])
        )
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
        gp = vellum::vl_gpar(fill = ccol, col = ccol)
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

# A sankey flow diagram. The layout (`L$sankey`, native [0, 1] coords) is computed
# at resolve; here we draw filled Bézier ribbons (under) then node rects + labels.
.emit_sankey <- function(scene, L, scales) {
  lay <- L$sankey
  nodes <- lay$nodes
  rib <- lay$ribbons
  sk <- .mark_sketch(L, scales)

  # ribbons: a filled band between two horizontal cubic-Bézier edges. With
  # `flow_color = "gradient"` each ribbon fades (horizontally, in native coords)
  # from its source colour to its target colour; otherwise a flat source/target
  # fill (`rib$colour`).
  gradient <- identical(L$params$flow_color, "gradient")
  for (i in seq_len(nrow(rib))) {
    top <- .sankey_bezier(rib$xl[i], rib$sy1[i], rib$xr[i], rib$ty1[i])
    bot <- .sankey_bezier(rib$xl[i], rib$sy0[i], rib$xr[i], rib$ty0[i])
    px <- scales$x$map(c(top$x, rev(bot$x)))
    py <- scales$y$map(c(top$y, rev(bot$y)))
    xy <- .xy_path(scales, px, py)
    fill <- if (gradient) {
      vellum::linear_gradient(
        c(rib$colour_src[i], rib$colour_tgt[i]),
        x1 = scales$x$map(rib$xl[i]),
        x2 = scales$x$map(rib$xr[i]),
        units = "native",
        interpolation = "oklab"
      )
    } else {
      rib$colour[i]
    }
    scene <- .draw(
      scene,
      vellum::polygon_grob(
        xy$x,
        xy$y,
        gp = vellum::vl_gpar(
          fill = fill,
          col = NA,
          alpha = .SANKEY_RIBBON_ALPHA
        )
      )
    )
  }

  # nodes: a thin filled rect per node
  for (i in seq_len(nrow(nodes))) {
    xc <- scales$x$map((nodes$x0[i] + nodes$x1[i]) / 2)
    yc <- scales$y$map((nodes$y0[i] + nodes$y1[i]) / 2)
    w <- scales$x$map(nodes$x1[i]) - scales$x$map(nodes$x0[i])
    h <- scales$y$map(nodes$y1[i]) - scales$y$map(nodes$y0[i])
    r <- .rect_units(scales, xc, yc, w, h)
    scene <- .draw(
      scene,
      vellum::rect_grob(
        x = r$x,
        y = r$y,
        width = r$width,
        height = r$height,
        sketch = sk,
        gp = vellum::vl_gpar(fill = nodes$colour[i], col = NA)
      )
    )
  }

  # labels: source column (x0 ~ 0) to the left, everything else to the right;
  # with `show_values` the node's value is appended, e.g. "Grid (60)".
  if (isTRUE(L$params$label)) {
    show_values <- isTRUE(L$params$show_values)
    for (i in seq_len(nrow(nodes))) {
      left <- nodes$x0[i] < 1e-6
      xlab <- if (left) nodes$x0[i] else nodes$x1[i]
      just <- c(if (left) "right" else "left", "centre")
      txt <- nodes$name[i]
      if (show_values) {
        txt <- paste0(txt, " (", .label_number_default(nodes$value[i]), ")")
      }
      xy <- .xy_units(
        scales,
        scales$x$map(xlab),
        scales$y$map((nodes$y0[i] + nodes$y1[i]) / 2)
      )
      pad <- vellum::vl_unit(if (left) -1 else 1, "mm")
      scene <- .draw(
        scene,
        vellum::text_grob(
          txt,
          x = xy$x + pad,
          y = xy$y,
          just = just,
          gp = vellum::vl_gpar(fontsize = .SANKEY_LABEL_FONTSIZE)
        )
      )
    }
  }
  scene
}

# A space-filling hierarchy (sunburst / icicle / treemap / circlepack). The
# layout (`L$hierarchy`, native geometry centred at the origin) is computed at
# resolve; drawn as one batched grob per type in the aspect-locked square panel
# (domain [-1, 1], so native 0 is the centre). Nodes are painted shallow-first,
# the order the layout returns (BFS-ish), so nested children overlay their parent.
.HIER_LABEL_FS <- 8 # label font size (pt)

.emit_hierarchy <- function(scene, L, scales) {
  lay <- L$hierarchy
  if (!nrow(lay)) {
    return(scene)
  }
  type <- L$params$type %||% "sunburst"
  # Node fill comes from the (branch or mapped) fill scale; in branch mode fade
  # each node toward white by depth so levels within a branch stay distinct.
  base <- rep_len(.aes_colour(L, scales, "#7f7f7f"), nrow(lay))
  if (identical(L$hier_fill_mode, "branch")) {
    D <- max(lay$depth)
    amt <- (lay$depth - 1L) / max(1L, D - 1L) * (L$params$lighten %||% 0.6)
    lay$colour <- .lighten(base, amt)
  } else {
    lay$colour <- base
  }
  scene <- switch(
    type,
    sunburst = .emit_sunburst_regions(scene, lay),
    icicle = ,
    treemap = .emit_rect_regions(scene, lay),
    circlepack = .emit_circle_regions(scene, lay)
  )
  if (isTRUE(L$params$label)) {
    scene <- switch(
      type,
      sunburst = .emit_sunburst_labels(scene, L, lay),
      icicle = .emit_rect_labels(scene, L, lay, leaf_only = FALSE),
      treemap = .emit_rect_labels(scene, L, lay, leaf_only = TRUE),
      circlepack = .emit_circle_labels(scene, L, lay)
    )
  }
  scene
}

.emit_sunburst_regions <- function(scene, lay) {
  n <- nrow(lay)
  .draw(
    scene,
    vellum::sector_grob(
      x = vellum::vl_unit(rep(0, n), "native"),
      y = vellum::vl_unit(rep(0, n), "native"),
      r0 = vellum::vl_unit(lay$r0, "native"),
      r1 = vellum::vl_unit(lay$r1, "native"),
      theta0 = lay$theta0,
      theta1 = lay$theta1,
      fill = lay$colour,
      gp = vellum::vl_gpar(col = "white", lwd = 0.5)
    )
  )
}

# Treemap / icicle rectangles: `rect_grob`/`circle_grob` take a single gpar fill,
# so batch one grob per distinct colour (siblings share a branch+depth hue). The
# layout is shallow-first, so children still overlay parents within a colour.
.emit_rect_regions <- function(scene, lay) {
  for (col in unique(lay$colour)) {
    s <- lay[lay$colour == col, , drop = FALSE]
    scene <- .draw(
      scene,
      vellum::rect_grob(
        x = vellum::vl_unit((s$x0 + s$x1) / 2, "native"),
        y = vellum::vl_unit((s$y0 + s$y1) / 2, "native"),
        width = vellum::vl_unit(s$x1 - s$x0, "native"),
        height = vellum::vl_unit(s$y1 - s$y0, "native"),
        gp = vellum::vl_gpar(fill = col, col = "white", lwd = 0.5)
      )
    )
  }
  scene
}

# Circle-pack circles: one grob per distinct colour (drop zero-radius nodes).
.emit_circle_regions <- function(scene, lay) {
  lay <- lay[lay$cr > 0, , drop = FALSE]
  for (col in unique(lay$colour)) {
    s <- lay[lay$colour == col, , drop = FALSE]
    scene <- .draw(
      scene,
      vellum::circle_grob(
        x = vellum::vl_unit(s$cx, "native"),
        y = vellum::vl_unit(s$cy, "native"),
        r = vellum::vl_unit(s$cr, "native"),
        gp = vellum::vl_gpar(fill = col, col = "white", lwd = 0.5)
      )
    )
  }
  scene
}

# Draw the segment labels (and the optional centre/root label). Each label is
# placed at its segment's centroid, oriented to fit the wedge, kept upright, and
# inked for contrast; a label that fits in no allowed orientation is dropped.
.emit_sunburst_labels <- function(scene, L, lay) {
  fs <- .HIER_LABEL_FS
  # native -> mm: the aspect-locked panel spans the [-1, 1] square, so radius 1
  # (native) is ~half the shorter page side. Approximate (ignores gutters), which
  # is fine for a fit/hide heuristic.
  pg <- .mark_ctx$page
  page_mm <- if (!is.null(pg)) min(pg) * 25.4 else 6 * 25.4
  nat2mm <- page_mm / 2

  labels <- lay$id
  if (isTRUE(L$params$show_values)) {
    labels <- paste0(labels, " (", .label_number_default(lay$value), ")")
  }
  lw <- vapply(labels, function(s) .mm_tw(s, fs), numeric(1)) # label width, mm
  lh <- fs * 25.4 / 72 # a line's height, mm

  thetamid <- (lay$theta0 + lay$theta1) / 2
  rmid <- (lay$r0 + lay$r1) / 2
  arc_mm <- rmid * abs(lay$theta1 - lay$theta0) * nat2mm # tangential capacity
  rad_mm <- (lay$r1 - lay$r0) * nat2mm # radial capacity

  want <- L$params$orientation %||% "auto"
  # Per segment: choose an orientation whose capacity holds the label, else NA
  # (dropped). "auto" tries tangential -> radial -> horizontal.
  fit_orient <- function(i) {
    tang <- arc_mm[i] >= lw[i] && rad_mm[i] >= lh
    radial <- rad_mm[i] >= lw[i] && arc_mm[i] >= lh
    horiz <- arc_mm[i] >= lw[i] && rad_mm[i] >= lh # wedge box near rmid holds it
    if (want == "tangential") {
      return(if (tang) "tangential" else NA_character_)
    }
    if (want == "radial") {
      return(if (radial) "radial" else NA_character_)
    }
    if (want == "horizontal") {
      return(if (horiz) "horizontal" else NA_character_)
    }
    if (tang) {
      "tangential"
    } else if (radial) {
      "radial"
    } else {
      NA_character_
    }
  }
  orient <- vapply(seq_len(nrow(lay)), fit_orient, character(1))
  ink <- .contrast_ink(lay$colour)

  keep <- which(!is.na(orient))
  for (i in keep) {
    rot <- switch(
      orient[i],
      tangential = .upright_rot(thetamid[i], "tangent"),
      radial = .upright_rot(thetamid[i], "radius"),
      0
    )
    scene <- .draw(
      scene,
      vellum::text_grob(
        labels[i],
        vellum::vl_unit(rmid[i] * cos(thetamid[i]), "native"),
        vellum::vl_unit(rmid[i] * sin(thetamid[i]), "native"),
        just = c("centre", "centre"),
        rot = rot,
        gp = vellum::vl_gpar(fontsize = fs, col = ink[i])
      )
    )
  }

  # Optional centre / root label.
  root <- attr(lay, "root")
  if (isTRUE(L$params$root_label) && !is.null(root)) {
    txt <- root$id
    if (isTRUE(L$params$show_values)) {
      txt <- paste0(txt, "\n", .label_number_default(root$value))
    }
    scene <- .draw(
      scene,
      vellum::text_grob(
        txt,
        vellum::vl_unit(0, "native"),
        vellum::vl_unit(0, "native"),
        just = c("centre", "centre"),
        gp = vellum::vl_gpar(fontsize = fs + 1, col = "black")
      )
    )
  }
  scene
}

# native -> mm for the aspect-locked [-1, 1] square panel: radius 1 is ~half the
# shorter page side. Approximate (ignores gutters), fine for a fit/hide heuristic.
.hier_nat2mm <- function() {
  pg <- .mark_ctx$page
  page_mm <- if (!is.null(pg)) min(pg) * 25.4 else 6 * 25.4
  page_mm / 2
}

# Treemap / icicle labels: centred horizontally in each node's rectangle, drawn
# where the text fits (width and a line's height both inside the rect). Treemap
# labels only leaves (parents sit under their children); icicle labels every
# node (bands do not overlap).
.emit_rect_labels <- function(scene, L, lay, leaf_only) {
  fs <- .HIER_LABEL_FS
  nat2mm <- .hier_nat2mm()
  labels <- lay$id
  if (isTRUE(L$params$show_values)) {
    labels <- paste0(labels, " (", .label_number_default(lay$value), ")")
  }
  lh <- fs * 25.4 / 72
  cand <- if (leaf_only) which(lay$leaf) else seq_len(nrow(lay))
  ink <- .contrast_ink(lay$colour)
  for (i in cand) {
    w_mm <- (lay$x1[i] - lay$x0[i]) * nat2mm
    h_mm <- (lay$y1[i] - lay$y0[i]) * nat2mm
    if (.mm_tw(labels[i], fs) > w_mm || lh > h_mm) {
      next
    }
    scene <- .draw(
      scene,
      vellum::text_grob(
        labels[i],
        vellum::vl_unit((lay$x0[i] + lay$x1[i]) / 2, "native"),
        vellum::vl_unit((lay$y0[i] + lay$y1[i]) / 2, "native"),
        just = c("centre", "centre"),
        gp = vellum::vl_gpar(fontsize = fs, col = ink[i])
      )
    )
  }
  scene
}

# Circle-pack labels: centred in each leaf's circle, drawn where the text chord
# fits (internal nodes are covered by their children, so only leaves are named).
.emit_circle_labels <- function(scene, L, lay) {
  fs <- .HIER_LABEL_FS
  nat2mm <- .hier_nat2mm()
  labels <- lay$id
  if (isTRUE(L$params$show_values)) {
    labels <- paste0(labels, " (", .label_number_default(lay$value), ")")
  }
  lh <- fs * 25.4 / 72
  ink <- .contrast_ink(lay$colour)
  for (i in which(lay$leaf & lay$cr > 0)) {
    r_mm <- lay$cr[i] * nat2mm
    half <- (r_mm * r_mm) - (lh / 2)^2 # widest chord at the text's height
    if (half <= 0) {
      next
    }
    if (.mm_tw(labels[i], fs) > 2 * sqrt(half)) {
      next
    }
    scene <- .draw(
      scene,
      vellum::text_grob(
        labels[i],
        vellum::vl_unit(lay$cx[i], "native"),
        vellum::vl_unit(lay$cy[i], "native"),
        just = c("centre", "centre"),
        gp = vellum::vl_gpar(fontsize = fs, col = ink[i])
      )
    )
  }
  scene
}

# A chord diagram (`L$chord`, sectors + ribbons in native circle coords centred
# at the origin). Ribbons are drawn first (filled Bezier polygons, under), then
# the sector band (one batched sector grob), then the node labels. `sector_grob`
# and the ribbon points share vellum's y-up frame, so arcs and ribbons align.
.CHORD_RIBBON_ALPHA <- 0.6
.CHORD_LABEL_FS <- 8
.CHORD_GAP <- 0.06 # radial inset of a ribbon's target end for `direction = "gap"`

# Append an alpha byte to a `#RRGGBB` colour, for per-stop gradient opacity.
.chord_rgba <- function(hex, a) {
  paste0(hex, sprintf("%02X", pmin(255L, pmax(0L, as.integer(round(a * 255))))))
}

.emit_chord <- function(scene, L, scales) {
  ch <- L$chord
  sec <- ch$sectors
  rib <- ch$ribbons
  if (!nrow(sec)) {
    return(scene)
  }
  r_in <- .CHORD_R_IN
  dir <- L$params$direction %||% "gradient"
  grad <- dir %in% c("gradient", "both")
  # "gap" pulls the target end short of the ring, leaving a directional gap.
  r_tgt <- if (dir %in% c("gap", "both")) r_in - .CHORD_GAP else r_in

  # Ribbons: a closed polygon bounded by the source sub-arc, a Bezier through the
  # centre to the target sub-arc, the target sub-arc, and a Bezier back. The
  # source end sits on the ring; the target end may be inset (gap) and/or faded
  # (gradient), so direction reads as solid-source -> faint/short-target.
  for (i in seq_len(nrow(rib))) {
    s <- .chord_arc(r_in, rib$sa0[i], rib$sa1[i])
    t <- .chord_arc(r_tgt, rib$ta0[i], rib$ta1[i])
    ns <- length(s$x)
    nt <- length(t$x)
    b1 <- .chord_bezier(s$x[ns], s$y[ns], t$x[1], t$y[1])
    b2 <- .chord_bezier(t$x[nt], t$y[nt], s$x[1], s$y[1])
    px <- c(s$x, b1$x, t$x, b2$x)
    py <- c(s$y, b1$y, t$y, b2$y)
    xy <- .xy_path(scales, px, py)
    if (grad) {
      # Alpha fade along the source->target axis (opaque source, faint target).
      sm <- (rib$sa0[i] + rib$sa1[i]) / 2
      tm <- (rib$ta0[i] + rib$ta1[i]) / 2
      fill <- vellum::linear_gradient(
        c(
          .chord_rgba(rib$colour[i], 0.75),
          .chord_rgba(rib$colour[i], 0.08)
        ),
        x1 = scales$x$map(r_in * cos(sm)),
        y1 = scales$y$map(r_in * sin(sm)),
        x2 = scales$x$map(r_tgt * cos(tm)),
        y2 = scales$y$map(r_tgt * sin(tm)),
        units = "native"
      )
      alpha <- 1
    } else {
      fill <- rib$colour[i]
      alpha <- .CHORD_RIBBON_ALPHA
    }
    scene <- .draw(
      scene,
      vellum::polygon_grob(
        xy$x,
        xy$y,
        gp = vellum::vl_gpar(fill = fill, col = NA, alpha = alpha)
      )
    )
  }

  # Sector band: one batched sector grob (centre at native origin).
  n <- nrow(sec)
  scene <- .draw(
    scene,
    vellum::sector_grob(
      x = vellum::vl_unit(rep(0, n), "native"),
      y = vellum::vl_unit(rep(0, n), "native"),
      r0 = vellum::vl_unit(rep(r_in, n), "native"),
      r1 = vellum::vl_unit(rep(.CHORD_R_OUT, n), "native"),
      theta0 = sec$theta0,
      theta1 = sec$theta1,
      fill = sec$colour,
      gp = vellum::vl_gpar(col = "white", lwd = 0.5)
    )
  )

  # Node labels radial, kept upright, and anchored at their *inner* end just
  # outside the ring so the text runs outward and never overlaps the sectors.
  # Upright-flipping reverses the reading direction on the left half, so the
  # justification that means "outward" flips with it.
  if (isTRUE(L$params$label)) {
    fs <- .CHORD_LABEL_FS
    rl <- .CHORD_R_OUT + 0.04
    mid <- (sec$theta0 + sec$theta1) / 2
    for (i in seq_len(n)) {
      deg <- ((mid[i] * 180 / pi + 180) %% 360) - 180
      flip <- deg > 90 || deg <= -90 # left half: label is flipped upright
      scene <- .draw(
        scene,
        vellum::text_grob(
          sec$node[i],
          vellum::vl_unit(rl * cos(mid[i]), "native"),
          vellum::vl_unit(rl * sin(mid[i]), "native"),
          just = c(if (flip) "right" else "left", "centre"),
          rot = .upright_rot(mid[i], "radius"),
          gp = vellum::vl_gpar(fontsize = fs, col = "black")
        )
      )
    }
  }
  scene
}

# A group summary region (ellipse / convex hull): one closed polygon per group
# `.piece`. Stroked by the `color` aesthetic; filled only when a `fill` is
# mapped or set (so the ggplot2 default -- an unfilled boundary -- holds). The
# `.piece` grouping comes from the stat; each polygon draws its own vertices.
#
# PROVENANCE: `rows = idx` records the polygon's own vertices; when the layer
# maps a discrete colour/fill, each vertex also carries its series key
# ("<aes>:<value>") in `meta$legend` (via `.legend_membership`), which is the
# cross-layer hook a host uses to link a region to its group's points
# (GAP-ANALYSIS.md 12.1).
.emit_region <- function(scene, L, scales) {
  n <- L$n
  if (!n) {
    return(scene)
  }
  xn <- rep_len(scales$x$map(L$values$x), n)
  yn <- rep_len(scales$y$map(L$values$y), n)
  stroke <- rep_len(
    .aes_colour(L, scales, "#3366bb", fill_fallback = FALSE),
    n
  )
  # Fill only when the layer supplies one; otherwise draw an unfilled boundary.
  # A constant paint (gradient/pattern) fill is substituted at the gp below, not
  # carried in the per-row colour vector (which `rep_len` would corrupt).
  paint <- .paint_fill(L)
  fillv <- if (!is.null(L$values$fill)) {
    scales$color$map(L$values$fill)
  } else if (!is.null(L$params$fill) && is.null(paint)) {
    L$params$fill
  } else {
    NA
  }
  fillv <- rep_len(fillv, n)
  alpha <- rep_len(.aes_alpha(L, scales, NA_real_), n)
  lwd <- .aes_param(L, "linewidth", 1)
  piece <- L$values$.piece
  sk <- .mark_sketch(L, scales)
  gi <- 0L
  for (pid in unique(piece)) {
    idx <- which(piece == pid)
    i0 <- idx[1]
    xy <- .xy_path(scales, xn[idx], yn[idx])
    scene <- .draw(
      scene,
      vellum::polygon_grob(
        xy$x,
        xy$y,
        sketch = .sketch_bump(sk, gi),
        gp = vellum::vl_gpar(
          fill = .pattern_at(L, scales, i0) %||% .paint_or(L, fillv[i0]),
          col = stroke[i0],
          lwd = lwd,
          alpha = gp_alpha(alpha[i0])
        )
      ),
      rows = idx
    )
    gi <- gi + 1L
  }
  scene
}

# Vertical error bars from ymin to ymax (with optional horizontal caps).
.emit_errorbar <- function(scene, L, scales, caps = TRUE) {
  n <- L$n
  xn <- rep_len(scales$x$map(L$values$x), n)
  ymin <- rep_len(scales$y$map(L$values$ymin), n)
  ymax <- rep_len(scales$y$map(L$values$ymax), n)
  col <- rep_len(.aes_colour(L, scales, "black"), n)
  lwd <- .aes_param(L, "linewidth", 1)
  lty <- .resolve_lty(L, scales, n)
  band <- scales$x$band_width %||% .resolution(xn)
  half <- (.aes_param(L, "width", 0.5) * band) / 2
  sk <- .mark_sketch(L, scales)

  gi <- 0L
  for (idx in .style_groups(n, list(col = col, lty = lty))) {
    x0 <- xn[idx]
    y0 <- ymin[idx]
    x1 <- xn[idx]
    y1 <- ymax[idx]
    if (caps) {
      x0 <- c(x0, xn[idx] - half, xn[idx] - half)
      x1 <- c(x1, xn[idx] + half, xn[idx] + half)
      y0 <- c(y0, ymin[idx], ymax[idx])
      y1 <- c(y1, ymin[idx], ymax[idx])
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
        gp = vellum::vl_gpar(
          col = col[idx[1]],
          lwd = lwd,
          lty = if (is.null(lty)) NULL else lty[idx[1]]
        )
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

.emit_linerange <- function(scene, L, scales) {
  .emit_errorbar(scene, L, scales, caps = FALSE)
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
  lty <- .resolve_lty(L, scales, n)
  sk <- .mark_sketch(L, scales)

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
        gp = .gp_stroke(col, alpha, idx[1], lwd, lty)
      ),
      rows = idx
    )
    gi <- gi + 1L
  }
  scene
}

# Network edges: straight lines from (x, y) to (xend, yend), batched by style
# (colour / alpha / width) into one segments_grob per group -- so a graph with a
# handful of edge styles emits a handful of grobs regardless of edge count.
# Parallel / reciprocal offsets are baked into the coordinates by vgraph(); a
# self-loop is an edge whose endpoints coincide, drawn here as a small loop
# (nested outward when a vertex has several). Edge width maps through the
# edge-width scale when present. Optional arrowheads mark directed edges.
.emit_edges <- function(scene, L, scales) {
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
  col <- rep_len(.aes_edge_colour(L, scales, "grey40"), n)
  alpha <- rep_len(.aes_edge_alpha(L, scales, NA_real_), n)
  lwd <- rep_len(.edge_width(L, scales, 0.5), n)
  lty <- .aes_edge_linetype(L, scales, NULL)
  lty <- if (is.null(lty)) NULL else rep_len(lty, n)
  # `arrow` is resolved to a vl_arrow spec (or NULL) at mark-build time, so a
  # bare `arrow = TRUE` and an explicit `arrow = vl_arrow(...)` share one path.
  arr <- L$stat_params$arrow
  # Straight edges can be sketched; self-loops (loop_grob) are always crisp.
  sk <- .mark_sketch(L, scales)

  # Per-edge endpoint node radii (mm), for exact node-boundary capping. vellum
  # resolves the mm caps in device space at render, so this is correct at any
  # size/dpi and for arbitrary per-vertex sizes -- no native-per-mm estimate.
  gh <- scales$graph
  # A small gap (mm) past the node radius so the edge/arrowhead clears the marker.
  gap <- 0.4

  loop <- x0 == x1 & y0 == y1
  si <- which(!loop)
  # Bundle the resolved per-edge arrays for the routing helpers below. Non-loop
  # edges are drawn straight (default), as an orthogonal elbow (hierarchies), or
  # as a source->target gradient -- each still made of straight segments (no
  # curvature, a design non-goal). Self-loops always take the loop_grob path.
  E <- list(
    x0 = x0,
    y0 = y0,
    x1 = x1,
    y1 = y1,
    col = col,
    alpha = alpha,
    lwd = lwd,
    lty = lty,
    arr = arr,
    sk = sk,
    gh = gh,
    gap = gap
  )
  routing <- L$stat_params$routing %||% "straight"
  E$elbow_at <- L$stat_params$elbow_at %||% "mid"
  E$elbow_axis <- L$stat_params$elbow_axis %||% "auto"
  if (length(si)) {
    scene <- if (identical(routing, "elbow")) {
      .emit_edges_elbow(scene, si, E, scales)
    } else if (isTRUE(L$stat_params$gradient)) {
      .emit_edges_gradient(scene, si, E, scales)
    } else {
      .emit_edges_straight(scene, si, E, scales)
    }
  }

  # Self-loops: vellum's teardrop loop_grob (igraph shape) anchored at the vertex,
  # sized in mm so it tracks the node marker, feet on the node boundary (`foot`).
  # Each loop points into the largest gap between the vertex's incident edges
  # (the igraph "flower-petal" placement, precomputed in .loop_geometry), so loops
  # land in empty space; several on a vertex spread across the gap. A directed
  # loop carries an arrowhead tangent at the returning foot.
  li <- which(loop)
  if (length(li)) {
    cx <- mean(c(x0, x1))
    cy <- mean(c(y0, y1))
    for (j in li) {
      node_r_mm <- if (!is.null(gh)) gh$end_cap[j] else 2
      size_mm <- node_r_mm * 5 # size/0.3 = device bulge; clears the marker
      ang <- gh$loop_angle[j] %||% NA_real_
      if (!is.finite(ang)) {
        ang <- atan2(y0[j] - cy, x0[j] - cx) # fallback: away from centre
        if (!is.finite(ang)) {
          ang <- pi / 2
        }
      }
      # narrow the petal when the incident-edge gap is tight (rigraph factor)
      narrow <- gh$loop_narrow[j] %||% 1
      xy <- .xy_units(scales, x0[j], y0[j])
      a <- alpha[j]
      scene <- .draw(
        scene,
        vellum::loop_grob(
          xy$x,
          xy$y,
          size = vellum::vl_unit(size_mm, "mm"),
          foot = vellum::vl_unit(node_r_mm, "mm"),
          angle = ang,
          width = narrow,
          arrow = arr,
          gp = vellum::vl_gpar(
            col = col[j],
            lwd = lwd[j],
            lty = if (is.null(lty)) NULL else lty[j],
            alpha = gp_alpha(a)
          )
        )
      )
    }
  }
  scene
}

# Straight edges: one batched segments_grob per (colour, alpha, width, linetype)
# style group, each end capped at its node boundary (mm) and parallel/reciprocal
# edges spread by the device-space offset. The default routing.
.emit_edges_straight <- function(scene, si, E, scales) {
  grp_lwd <- round(E$lwd, 2)
  for (idx in .style_groups(
    length(si),
    list(
      col = E$col[si],
      alpha = E$alpha[si],
      lwd = grp_lwd[si],
      lty = if (is.null(E$lty)) NULL else E$lty[si]
    )
  )) {
    g <- si[idx]
    a <- E$alpha[g[1]]
    s <- .seg_units(
      scales,
      vellum::vl_unit(E$x0[g], "native"),
      vellum::vl_unit(E$y0[g], "native"),
      vellum::vl_unit(E$x1[g], "native"),
      vellum::vl_unit(E$y1[g], "native")
    )
    start_cap <- if (!is.null(E$gh)) {
      vellum::vl_unit(E$gh$start_cap[g] + E$gap, "mm")
    }
    end_cap <- if (!is.null(E$gh)) {
      vellum::vl_unit(E$gh$end_cap[g] + E$gap, "mm")
    }
    offset <- if (!is.null(E$gh)) vellum::vl_unit(E$gh$offset[g], "mm")
    scene <- .draw(
      scene,
      vellum::segments_grob(
        s$x0,
        s$y0,
        s$x1,
        s$y1,
        arrow = E$arr,
        start_cap = start_cap,
        end_cap = end_cap,
        offset = offset,
        sketch = E$sk,
        gp = vellum::vl_gpar(
          col = E$col[g[1]],
          lwd = E$lwd[g[1]],
          lty = if (is.null(E$lty)) NULL else E$lty[g[1]],
          alpha = gp_alpha(a)
        )
      ),
      rows = g
    )
  }
  scene
}

# The orthogonal step for one edge: a right-angle polyline (no curvature). The
# step runs along whichever axis the endpoints are farther apart on, so a
# top-down tree bends vertically and a left-right one horizontally, keeping
# sibling edges consistent.
# `at` places the corner along the primary axis: `"mid"` (default; the S-bend
# used by tree/DAG layouts), or `"start"`/`"end"` for the corner at the source or
# target coordinate -- `"start"` gives the dendrogram *bracket* (siblings share a
# bar at the parent's level). `axis` forces the primary axis (`"v"` = corner
# moves in y, `"h"` = in x); `"auto"` picks the longer side (the historical
# behaviour). Defaults reproduce the original midpoint elbow exactly.
.elbow_points <- function(x0, y0, x1, y1, at = "mid", axis = "auto") {
  vertical <- switch(
    axis,
    v = TRUE,
    h = FALSE,
    abs(y1 - y0) >= abs(x1 - x0)
  )
  frac <- switch(at, start = 0, end = 1, 0.5)
  if (vertical) {
    ym <- y0 + (y1 - y0) * frac
    list(x = c(x0, x0, x1, x1), y = c(y0, ym, ym, y1))
  } else {
    xm <- x0 + (x1 - x0) * frac
    list(x = c(x0, xm, xm, x1), y = c(y0, y0, y1, y1))
  }
}

# Elbow (orthogonal) edges for tree / DAG / dendrogram layouts. Each edge is a
# right-angle polyline drawn as its own lines_grob -- `lines_grob` caps trim the
# whole path, so per-edge node-boundary caps (mm) and the target arrowhead need
# one grob per edge (fine: elbows are for hierarchies, not hairballs). No
# curvature, no parallel offset.
.emit_edges_elbow <- function(scene, si, E, scales) {
  for (e in si) {
    p <- .elbow_points(
      E$x0[e],
      E$y0[e],
      E$x1[e],
      E$y1[e],
      at = E$elbow_at,
      axis = E$elbow_axis
    )
    xy <- .xy_units(scales, p$x, p$y)
    start_cap <- if (!is.null(E$gh)) {
      vellum::vl_unit(E$gh$start_cap[e] + E$gap, "mm")
    }
    end_cap <- if (!is.null(E$gh)) {
      vellum::vl_unit(E$gh$end_cap[e] + E$gap, "mm")
    }
    scene <- .draw(
      scene,
      vellum::lines_grob(
        xy$x,
        xy$y,
        arrow = E$arr,
        start_cap = start_cap,
        end_cap = end_cap,
        sketch = E$sk,
        gp = vellum::vl_gpar(
          col = E$col[e],
          lwd = E$lwd[e],
          lty = if (is.null(E$lty)) NULL else E$lty[e],
          alpha = gp_alpha(E$alpha[e])
        )
      ),
      rows = e
    )
  }
  scene
}

# Number of straight sub-segments a gradient edge is split into.
.GRADIENT_SEGMENTS <- 20L

# Source->target gradient edges: each straight edge is cut into `.GRADIENT_SEGMENTS`
# pieces whose opacity ramps from faint (source) to full (target), so direction
# reads without an arrowhead (igraph `edge.gradient`). All edges' piece `j` share
# one opacity, so the whole layer emits `K x styles` batched segments_grob calls.
# Nodes overdraw the endpoints (z-order), so no caps/arrows are needed here.
.emit_edges_gradient <- function(scene, si, E, scales) {
  k <- .GRADIENT_SEGMENTS
  grp_lwd <- round(E$lwd, 2)
  groups <- .style_groups(
    length(si),
    list(
      col = E$col[si],
      alpha = E$alpha[si],
      lwd = grp_lwd[si],
      lty = if (is.null(E$lty)) NULL else E$lty[si]
    )
  )
  for (j in seq_len(k)) {
    t0 <- (j - 1L) / k
    t1 <- j / k
    ramp <- 0.12 + 0.88 * ((j - 0.5) / k) # faint at source -> opaque at target
    for (idx in groups) {
      g <- si[idx]
      base_a <- E$alpha[g[1]]
      a <- (if (is.na(base_a)) 1 else base_a) * ramp
      ax <- E$x0[g] + t0 * (E$x1[g] - E$x0[g])
      ay <- E$y0[g] + t0 * (E$y1[g] - E$y0[g])
      bx <- E$x0[g] + t1 * (E$x1[g] - E$x0[g])
      by <- E$y0[g] + t1 * (E$y1[g] - E$y0[g])
      s <- .seg_units(
        scales,
        vellum::vl_unit(ax, "native"),
        vellum::vl_unit(ay, "native"),
        vellum::vl_unit(bx, "native"),
        vellum::vl_unit(by, "native")
      )
      scene <- .draw(
        scene,
        vellum::segments_grob(
          s$x0,
          s$y0,
          s$x1,
          s$y1,
          sketch = E$sk,
          gp = vellum::vl_gpar(
            col = E$col[g[1]],
            lwd = E$lwd[g[1]],
            lty = if (is.null(E$lty)) NULL else E$lty[g[1]],
            alpha = gp_alpha(a)
          )
        ),
        rows = g
      )
    }
  }
  scene
}

# Bundled edges: draw each edge as a curved trunk instead of a straight line.
# The geometry is delegated to edgebundle (the "delegate the algorithm, own the
# drawing" pattern shared with graphlayouts): reconstruct an igraph + node
# coordinates from the edge endpoints, hand them to edgebundle::edge_bundle() for
# the chosen algorithm, and draw the returned polylines with this layer's edge
# aesthetics. Bundling runs in the layout's own (native) coordinate space, so the
# returned paths come back in native coords and map through .xy_path once, exactly
# like a straight edge. Self-loops carry no geometry to bundle and are skipped.
.emit_edge_bundle <- function(scene, L, scales) {
  .need_pkg("edgebundle", "mark_edge_bundle()")
  .need_pkg("igraph", "mark_edge_bundle()")
  n <- L$n
  x0 <- rep_len(scales$x$map(L$values$x), n)
  y0 <- rep_len(scales$y$map(L$values$y), n)
  x1 <- rep_len(scales$x$map(L$values$xend), n)
  y1 <- rep_len(scales$y$map(L$values$yend), n)
  col <- rep_len(.aes_edge_colour(L, scales, "grey40"), n)
  # Bundled trunks overlap heavily, so a faint default alpha lets density read.
  alpha <- rep_len(.aes_edge_alpha(L, scales, 0.3), n)
  lwd <- rep_len(.edge_width(L, scales, 0.5), n)
  lty <- .aes_edge_linetype(L, scales, NULL)
  lty <- if (is.null(lty)) NULL else rep_len(lty, n)
  sk <- .mark_sketch(L, scales)

  # Reconstruct nodes (unique endpoints) and a directed igraph over them. `keep`
  # drops self-loops (degenerate for bundling); the group->edge index below maps
  # back through it to recover each trunk's original-edge aesthetics.
  loop <- x0 == x1 & y0 == y1
  keep <- which(!loop)
  if (!length(keep)) {
    return(scene)
  }
  src <- cbind(x0[keep], y0[keep])
  tgt <- cbind(x1[keep], y1[keep])
  pts <- unique(rbind(src, tgt))
  key <- function(m) paste(m[, 1L], m[, 2L], sep = "\r")
  pk <- key(pts)
  g <- igraph::graph_from_edgelist(
    cbind(match(key(src), pk), match(key(tgt), pk)),
    directed = TRUE
  )

  type <- L$stat_params$type %||% "force"
  args <- c(
    list(object = g, xy = pts, type = type),
    L$stat_params$params %||% list()
  )
  b <- do.call(edgebundle::edge_bundle, args)

  # `group` labels each returned path; it is the edge index for most algorithms
  # and "<edge>.<stub>" for type = "stub" (two half-paths per edge). Stripping the
  # decimal recovers the position within `keep`, hence the original edge row.
  edge_of <- keep[as.integer(sub("\\..*$", "", as.character(b$group)))]
  paths <- split(seq_len(nrow(b)), b$group)
  for (idx in paths) {
    e <- edge_of[idx[1L]]
    xy <- .xy_path(scales, b$x[idx], b$y[idx])
    scene <- .draw(
      scene,
      vellum::lines_grob(
        xy$x,
        xy$y,
        sketch = sk,
        gp = vellum::vl_gpar(
          col = col[e],
          lwd = lwd[e],
          lty = if (is.null(lty)) NULL else lty[e],
          alpha = gp_alpha(alpha[e])
        )
      ),
      rows = e
    )
  }
  scene
}

# Flow map: a one-to-many tree from `root` fanning out to every destination
# along smooth merging branches whose width tracks the flow volume (the Minard
# idiom). Like `.emit_edge_bundle`, the graph + node xy are reconstructed from the
# edge endpoints (native coords) and the algorithm is delegated to edgebundle; the
# root is located as the reconstructed point nearest the coordinate resolved at
# mark-build time. `flow_tree` (spiral) and the `tnss_*` pipeline (Steiner) both
# return per-branch paths with a `flow` column; branches are drawn as constant-flow
# runs, each a `lines_grob` whose width comes from `flow` (round caps join them).
.emit_flow_map <- function(scene, L, scales) {
  .need_pkg("edgebundle", "mark_flow_map()")
  .need_pkg("igraph", "mark_flow_map()")
  sp <- L$stat_params
  type <- sp$type %||% "spiral"
  n <- L$n
  x0 <- rep_len(scales$x$map(L$values$x), n)
  y0 <- rep_len(scales$y$map(L$values$y), n)
  x1 <- rep_len(scales$x$map(L$values$xend), n)
  y1 <- rep_len(scales$y$map(L$values$yend), n)

  loop <- x0 == x1 & y0 == y1
  keep <- which(!loop)
  if (!length(keep)) {
    return(scene)
  }
  src <- cbind(x0[keep], y0[keep])
  tgt <- cbind(x1[keep], y1[keep])
  pts <- unique(rbind(src, tgt))
  key <- function(m) paste(m[, 1L], m[, 2L], sep = "\r")
  pk <- key(pts)
  g <- igraph::graph_from_edgelist(
    cbind(match(key(src), pk), match(key(tgt), pk)),
    directed = FALSE
  )
  igraph::E(g)$weight <- rep_len(sp$weight %||% 1, n)[keep]
  # Root: the reconstructed point nearest the build-time root coordinate, mapped
  # into the same native space as the endpoints.
  rxy <- c(scales$x$map(sp$root_xy[1]), scales$y$map(sp$root_xy[2]))
  root_i <- which.min((pts[, 1] - rxy[1])^2 + (pts[, 2] - rxy[2])^2)

  fm <- .flow_paths(type, g, pts, root_i, sp$params %||% list())
  if (is.null(fm) || !nrow(fm)) {
    return(scene)
  }

  # Map the computed flow onto the drawn width range (linewidth units), bounded so
  # a 1000x flow range stays legible rather than swamping the panel.
  wr <- sp$width_range %||% c(0.3, 3)
  fr <- range(fm$flow)
  wmap <- if (diff(fr) < .Machine$double.eps) {
    function(f) mean(wr)
  } else {
    function(f) wr[1L] + (f - fr[1L]) / (fr[2L] - fr[1L]) * (wr[2L] - wr[1L])
  }

  col <- sp$color %||% "#B2182B"
  a <- sp$alpha %||% 1
  sk <- .mark_sketch(L, scales)

  for (idx in split(seq_len(nrow(fm)), fm$grp)) {
    fv <- fm$flow[idx]
    for (r in .const_runs(fv)) {
      sub <- idx[r]
      xy <- .xy_path(scales, fm$x[sub], fm$y[sub])
      scene <- .draw(
        scene,
        vellum::lines_grob(
          xy$x,
          xy$y,
          sketch = sk,
          gp = vellum::vl_gpar(
            col = col,
            lwd = wmap(fv[r[1L]]),
            alpha = gp_alpha(a),
            lineend = "round",
            linejoin = "round"
          )
        )
      )
    }
  }
  scene
}

# Dispatch the flow-tree builder for `.emit_flow_map`, normalising both backends
# to a data frame of x / y / flow / grp (one group per drawable branch).
.flow_paths <- function(type, g, xy, root, params) {
  if (identical(type, "steiner")) {
    .need_pkg("interp", "mark_flow_map(type = \"steiner\")")
    dummies <- edgebundle::tnss_dummies(xy, root)
    gt <- do.call(
      edgebundle::tnss_tree,
      c(list(g = g, xy = xy, xydummy = dummies, root = root), params)
    )
    sm <- edgebundle::tnss_smooth(gt)
    data.frame(x = sm$x, y = sm$y, flow = sm$flow, grp = sm$destination)
  } else {
    fm <- do.call(
      edgebundle::flow_tree,
      c(list(object = g, xy = xy, root = root), params)
    )
    data.frame(x = fm$x, y = fm$y, flow = fm$flow, grp = fm$edge)
  }
}

# Split an ordered flow vector into maximal runs of equal flow, each run extended
# to include the first point of the next so consecutive `lines_grob`s meet with no
# gap (round caps hide the width step). Returns a list of index ranges into `f`.
.const_runs <- function(f) {
  n <- length(f)
  if (n <= 1L) {
    return(list(seq_len(n)))
  }
  brk <- which(f[-1L] != f[-n]) # positions where flow changes
  starts <- c(1L, brk + 1L)
  ends <- c(brk, n)
  Map(function(s, e) s:min(e + 1L, n), starts, ends)
}

# Pie / donut node glyphs: each vertex is drawn as a pie whose wedges are sized by
# the row's `cols` values (a composition), in absolute mm at the native vertex
# anchor (a proper circle under the aspect-locked graph panel). Wedges for
# category `j` across all nodes batch into one sector_grob, so the layer emits one
# grob per category. Categories take the qualitative palette (or `fill`) in order.
.emit_node_pie <- function(scene, L, scales) {
  sp <- L$stat_params
  weights <- sp$weights
  n <- nrow(weights)
  k <- ncol(weights)
  if (!n || !k) {
    return(scene)
  }
  xn <- rep_len(scales$x$map(L$values$x), n)
  yn <- rep_len(scales$y$map(L$values$y), n)
  cols_fill <- sp$fill %||% .qual_palette(k)
  # per-node fractions -> cumulative angles (a zero-weight node draws nothing)
  rs <- rowSums(weights, na.rm = TRUE)
  rs[!is.finite(rs) | rs == 0] <- 1
  frac <- weights / rs
  frac[!is.finite(frac)] <- 0
  cum <- t(apply(frac, 1L, function(z) cumsum(c(0, z)))) # n x (k + 1)
  r0 <- vellum::vl_unit(sp$inner * sp$size, "mm")
  r1 <- vellum::vl_unit(sp$size, "mm")
  for (j in seq_len(k)) {
    th0 <- cum[, j] * 2 * pi
    th1 <- cum[, j + 1L] * 2 * pi
    keep <- (th1 - th0) > 1e-9
    if (!any(keep)) {
      next
    }
    xy <- .xy_units(scales, xn[keep], yn[keep])
    scene <- .draw(
      scene,
      vellum::sector_grob(
        x = xy$x,
        y = xy$y,
        r0 = r0,
        r1 = r1,
        theta0 = th0[keep],
        theta1 = th1[keep],
        fill = cols_fill[j],
        gp = vellum::vl_gpar(
          col = sp$color,
          lwd = sp$linewidth,
          alpha = gp_alpha(sp$alpha)
        )
      ),
      rows = which(keep)
    )
  }
  scene
}

# Resolve an edge's width: a mapped linewidth channel via the trained edge-width
# scale, a constant linewidth param, or the default.
.edge_width <- function(L, scales, default) {
  if (!is.null(scales$edge_width) && !is.null(L$values$linewidth)) {
    return(scales$edge_width$map(L$values$linewidth))
  }
  L$params$linewidth %||% default
}

# A hexbin heatmap: one flat-top hexagon per occupied bin, filled by count. The
# hexes are sized in data units (full x extent `2 * width`, full y extent
# `height`, from stat_hexbin) so they tile the panel cleanly at any aspect; a
# bare `size` would be device-regular and only tile under coord_fixed. Drawn in
# one batched, per-hex-filled call via vellum's non-regular hexagon_grob (vellum
# >= 0.0.0.9001). Under coord_flip the data axes swap, so the extents swap too.
.emit_hex <- function(scene, L, scales) {
  n <- L$n
  xn <- rep_len(scales$x$map(L$values$x), n)
  yn <- rep_len(scales$y$map(L$values$y), n)
  fill <- rep_len(.aes_colour(L, scales, "grey50"), n)
  a <- .aes_param(L, "alpha", NA_real_)
  r <- (L$values$width %||% 1)[1]
  w_full <- 2 * r # full x extent (width is the x circumradius)
  h_full <- (L$values$height %||% (r * sqrt(3)))[1] # full y extent
  xy <- .xy_units(scales, xn, yn)
  flip <- .flipped(scales)
  .draw(
    scene,
    vellum::hexagon_grob(
      xy$x,
      xy$y,
      width = vellum::vl_unit(if (flip) h_full else w_full, "native"),
      height = vellum::vl_unit(if (flip) w_full else h_full, "native"),
      fill = fill,
      orientation = "flat",
      gp = vellum::vl_gpar(alpha = gp_alpha(a))
    ),
    rows = seq_len(n)
  )
}

# Datashade: aggregate the points into a density raster filling the panel. The
# raster is binned over the panel's native domain so it aligns with the axes.
# The named arguments common to vellum's datashade*() family, resolved from a
# datashade layer's stat params: the flip-aware raster dimensions + limits and
# the shading controls. `spread_default` differs by mark (points: none;
# lines/segments: "auto" dynspread); `colors` lets the categorical point path
# pass per-level hues in place of the default density ramp.
.datashade_args <- function(
  sp,
  scales,
  flip,
  spread_default = NULL,
  colors = NULL
) {
  w <- as.integer(sp$width %||% 400L)
  h <- as.integer(sp$height %||% 300L)
  list(
    width = if (flip) h else w,
    height = if (flip) w else h,
    xlim = if (flip) scales$y$domain else scales$x$domain,
    ylim = if (flip) scales$x$domain else scales$y$domain,
    colors = colors %||% sp$colors %||% c("#deebf7", "#08306b"),
    how = sp$how %||% "eq_hist",
    span = sp$span,
    clip = sp$clip,
    spread = sp$spread %||% spread_default,
    interpolate = FALSE
  )
}

.emit_datashade <- function(scene, L, scales) {
  # Full coordinate vectors live in `L$ds` (training only saw their range, see
  # .resolve_layer); `mark_point(auto=)` falls through here without an `ds` slot,
  # so fall back to the resolved values in that case.
  ds <- L$ds %||% list(x = L$values$x, y = L$values$y)
  xn <- scales$x$map(ds$x)
  yn <- scales$y$map(ds$y)
  sp <- L$stat_params

  # Categorical (count_cat): a mapped colour/fill trained the discrete colour
  # scale (see .resolve_layer). Feed the full-length category vector plus the
  # per-level hues from that scale's map, so a legend comes for free. A
  # non-discrete colour mapping (or none) falls back to plain density shading.
  category <- NULL
  colors <- NULL
  if (
    !is.null(ds$cat) &&
      !is.null(scales$color) &&
      identical(scales$color$kind, "discrete")
  ) {
    category <- as.character(ds$cat)
    levels <- unique(category)
    colors <- stats::setNames(scales$color$map(levels), levels)
  }

  # Under flip the raster axes swap with the data (the per-point category does not).
  flip <- .flipped(scales)
  g <- do.call(
    vellum::datashade,
    c(
      list(
        if (flip) yn else xn,
        if (flip) xn else yn,
        category = category
      ),
      .datashade_args(sp, scales, flip, colors = colors)
    )
  )
  .draw(scene, g)
}

# Datashade fallback for a dense line/step layer (`mark_line(auto=)` /
# `mark_step(auto=)`): rasterise the vertices into a line-density grid via
# vellum::datashade_lines() instead of one lines_grob per style group. Series are
# split exactly as the vector path splits style groups (colour / alpha / linetype),
# so datashade_lines() breaks the polyline between series and never draws a
# spurious segment joining two series; shading is by line density, so those style
# fields only define breaks here, not hue. `step = TRUE` expands each series into
# its hv/vh staircase (as `.emit_step`) before aggregating.
.emit_line_datashade <- function(scene, L, scales, step = FALSE) {
  n <- L$n
  xn <- rep_len(scales$x$map(L$values$x), n)
  yn <- rep_len(scales$y$map(L$values$y), n)
  col <- rep_len(.aes_colour(L, scales, "black"), n)
  alpha <- rep_len(.aes_alpha(L, scales, NA_real_), n)
  lty <- .resolve_lty(L, scales, n)
  dir <- L$stat_params$direction %||% "hv"

  gx <- gy <- grp <- list()
  gi <- 0L
  for (idx in .style_groups(n, list(col = col, alpha = alpha, lty = lty))) {
    gi <- gi + 1L
    o <- idx[order(xn[idx])] # each series drawn in x order, as the vector path
    sx <- xn[o]
    sy <- yn[o]
    if (step && length(sx) >= 2L) {
      m <- length(sx)
      if (identical(dir, "vh")) {
        sx <- c(rep(sx[-m], each = 2), sx[m])
        sy <- c(sy[1], rep(sy[-1], each = 2))
      } else {
        sx <- c(sx[1], rep(sx[-1], each = 2))
        sy <- c(rep(sy[-m], each = 2), sy[m])
      }
    }
    gx[[gi]] <- sx
    gy[[gi]] <- sy
    grp[[gi]] <- rep.int(gi, length(sx))
  }
  x <- unlist(gx, use.names = FALSE)
  y <- unlist(gy, use.names = FALSE)
  group <- unlist(grp, use.names = FALSE)

  sp <- L$stat_params
  flip <- .flipped(scales)
  # Thin single-pixel lines vanish, so datashaded lines default to dynspread.
  g <- do.call(
    vellum::datashade_lines,
    c(
      list(if (flip) y else x, if (flip) x else y, group = group),
      .datashade_args(sp, scales, flip, spread_default = "auto")
    )
  )
  .draw(scene, g)
}

# Datashade fallback for a dense segment / edge layer (`mark_segment(auto=)` /
# `mark_edges(auto=)`): rasterise the segments into an edge-density grid via
# vellum::datashade_segments() instead of a vector segment each. This is the
# hairball path -- it rasterises in data space, so the device-space refinements of
# `.emit_edges` (parallel-edge offsets, node-boundary caps, arrowheads, and the
# per-loop teardrop self-loops) do not apply; coincident-endpoint self-loops
# collapse to a point, negligible against a dense edge field. Use the vector path
# for graphs small enough to want that detail.
.emit_segment_datashade <- function(scene, L, scales) {
  n <- L$n
  x0 <- rep_len(scales$x$map(L$values$x), n)
  y0 <- rep_len(scales$y$map(L$values$y), n)
  x1 <- rep_len(scales$x$map(L$values$xend), n)
  y1 <- rep_len(scales$y$map(L$values$yend), n)

  sp <- L$stat_params
  flip <- .flipped(scales)
  g <- do.call(
    vellum::datashade_segments,
    c(
      list(
        if (flip) y0 else x0,
        if (flip) x0 else y0,
        if (flip) y1 else x1,
        if (flip) x1 else y1
      ),
      .datashade_args(sp, scales, flip, spread_default = "auto")
    )
  )
  .draw(scene, g)
}

# Draw an sf layer. Coordinates come from the decomposed geometry (`L$sf`, one
# entry per feature, each a list of point/line/poly primitives), not from x/y
# encodings; feature attributes (fill/colour/alpha) recycle over features. Within
# each geometry kind, features sharing a resolved (colour, alpha) batch into a
# single grob -- polygons into one `path_grob` (every ring an `evenodd` sub-path,
# so holes cut and islands stay solid regardless of winding), lines into one
# NA-separated `lines_grob`, points into one `points_grob`. This batching is the
# fast path for huge maps (a whole choropleth of one fill becomes a single grob).
#
# EXCEPTION -- interactivity: a `path_grob`/`lines_grob` is a single-shape mark
# carrying one data key for the whole grob (there is no per-sub-path key), so when
# the layer declares interactivity (`.mark_ctx$data_id` is set) poly/line features
# are emitted ONE grob PER FEATURE, keyed by that feature's `data_id`. `.draw()`
# gates key attachment on the same `.mark_ctx$data_id`, so the batched path loses
# no identity: it only runs when there are no keys to attach.
#
# CAVEAT -- the batched polygon path fills the whole style group with one
# `evenodd` rule. For disjoint features (every real choropleth / coverage map)
# this is identical to per-feature drawing; two same-fill features that *overlap*
# would XOR-cancel in the overlap (the same property a self-overlapping
# MULTIPOLYGON already has today). Interactive maps are unaffected (per feature).
# The interactive-vs-batched fan-out shared by the sf polygon and line branches.
# When interactive, emit one keyed grob per feature so `.draw(rows = i)` can
# attach its data_id/tooltip; otherwise batch the whole style group into a single
# grob. `gather(rows)` assembles a feature set's drawing coords (NULL to skip);
# `emit(scene, g, rows)` draws them.
.sf_fan_out <- function(scene, idx, interactive, gather, emit) {
  if (interactive) {
    for (i in idx) {
      scene <- emit(scene, gather(i), i)
    }
    scene
  } else {
    emit(scene, gather(idx), idx)
  }
}

.emit_sf <- function(scene, L, scales) {
  feats <- L$sf
  n <- L$n
  # Interactivity gate: batch only when no per-feature key will be attached (see
  # the EXCEPTION note above). Mirrors the predicate `.draw()` uses at emit time.
  interactive <- !is.null(.mark_ctx$data_id)
  # primary colour = mapped fill/colour (choropleth) or a constant param; NA when
  # nothing is set (then filled per-kind below). Border/alpha/lwd/size are params.
  primary <- rep_len(.aes_colour(L, scales, NA_character_), n)
  alpha <- rep_len(.aes_alpha(L, scales, NA_real_), n)
  border <- .aes_param(L, "color", "grey40")
  lwd <- .aes_param(L, "linewidth", 0.5)
  size <- rep_len(.aes_size(L, scales, 1.5), n)
  sk <- .mark_sketch(L, scales)

  # per-kind colour vector, coalescing the primary colour with a kind default.
  kind_col <- function(default) {
    v <- primary
    v[is.na(v)] <- default
    v
  }
  has_kind <- function(k) {
    vapply(
      feats,
      function(prims) any(vapply(prims, function(p) p$kind == k, logical(1))),
      logical(1)
    )
  }
  prims_of <- function(i, k) Filter(function(p) p$kind == k, feats[[i]])
  mapnat <- function(m) list(x = scales$x$map(m[, 1]), y = scales$y$map(m[, 2]))

  # Gather the polygon rings of features `fi` into one path payload: raw x/y and a
  # ring id that is unique across the whole call (so several features batch into
  # one `evenodd` path -- holes cut, islands solid, disjoint features independent).
  # Ids are contiguous and in draw order, so `path_grob`'s internal id reordering
  # is a no-op. Raw coordinates are collected here and mapped once by the caller
  # (one `scales$*$map` call per grob instead of one per ring). NULL if no rings.
  gather_poly <- function(fi) {
    px <- py <- pid <- list()
    k <- 0L
    rid <- 0L
    for (i in fi) {
      for (p in prims_of(i, "poly")) {
        for (ring in p$parts) {
          nr <- nrow(ring)
          if (!nr) {
            next
          }
          rid <- rid + 1L
          k <- k + 1L
          px[[k]] <- ring[, 1L]
          py[[k]] <- ring[, 2L]
          pid[[k]] <- rep.int(rid, nr)
        }
      }
    }
    if (!k) {
      return(NULL)
    }
    list(
      x = unlist(px, use.names = FALSE),
      y = unlist(py, use.names = FALSE),
      id = unlist(pid, use.names = FALSE)
    )
  }

  # Gather the line parts of features `fi` into one payload, NA-separated across
  # both parts and features (so one `lines_grob` draws them all as broken
  # polylines). Raw coordinates; mapped once by the caller. NULL if no segments.
  gather_line <- function(fi) {
    px <- py <- list()
    k <- 0L
    for (i in fi) {
      for (p in prims_of(i, "line")) {
        for (seg in p$parts) {
          nr <- nrow(seg)
          if (!nr) {
            next
          }
          k <- k + 1L
          px[[k]] <- c(seg[, 1L], NA_real_)
          py[[k]] <- c(seg[, 2L], NA_real_)
        }
      }
    }
    if (!k) {
      return(NULL)
    }
    list(x = unlist(px, use.names = FALSE), y = unlist(py, use.names = FALSE))
  }

  # Iterate style groups of the features that carry kind `k`, calling `draw`.
  by_style <- function(scene, k, colvec, draw) {
    sub <- which(has_kind(k))
    if (!length(sub)) {
      return(scene)
    }
    for (g in .style_groups(
      length(sub),
      list(col = colvec[sub], alpha = alpha[sub])
    )) {
      idx <- sub[g]
      scene <- draw(scene, idx, colvec[idx[1]], alpha[idx[1]])
    }
    scene
  }

  # polygons: batched into one path_grob per style group (its features' rings are
  # `evenodd` sub-paths) -- unless the layer is interactive, when each feature is
  # its own grob so `.draw()` can key it (`rows = i`) with the feature's data_id /
  # tooltip. See the EXCEPTION / CAVEAT notes on `.emit_sf`.
  scene <- by_style(
    scene,
    "poly",
    kind_col("grey80"),
    function(scene, idx, fill, a) {
      gpp <- vellum::vl_gpar(
        fill = .paint_or(L, fill),
        col = border,
        lwd = lwd,
        alpha = gp_alpha(a)
      )
      emit <- function(scene, g, rows) {
        if (is.null(g)) {
          return(scene)
        }
        xy <- .xy_units(scales, scales$x$map(g$x), scales$y$map(g$y))
        .draw(
          scene,
          vellum::path_grob(
            xy$x,
            xy$y,
            id = as.integer(g$id),
            rule = "evenodd",
            sketch = sk,
            gp = gpp
          ),
          rows = rows
        )
      }
      .sf_fan_out(scene, idx, interactive, gather_poly, emit)
    }
  )

  # lines: batched into one NA-separated lines_grob per style group -- unless the
  # layer is interactive, when each feature is its own grob (`rows = i`) so it
  # stays addressable. See the EXCEPTION note on `.emit_sf`.
  scene <- by_style(
    scene,
    "line",
    kind_col("grey20"),
    function(scene, idx, col, a) {
      gpp <- vellum::vl_gpar(col = col, lwd = lwd, alpha = gp_alpha(a))
      emit <- function(scene, g, rows) {
        if (is.null(g)) {
          return(scene)
        }
        xy <- .xy_units(scales, scales$x$map(g$x), scales$y$map(g$y))
        .draw(
          scene,
          vellum::lines_grob(xy$x, xy$y, sketch = sk, gp = gpp),
          rows = rows
        )
      }
      .sf_fan_out(scene, idx, interactive, gather_line, emit)
    }
  )

  # points: one points_grob per style group.
  scene <- by_style(
    scene,
    "point",
    kind_col("black"),
    function(scene, idx, col, a) {
      xs <- ys <- szs <- numeric(0)
      rows <- integer(0) # feature index per emitted point (for per-point keys)
      for (i in idx) {
        for (p in prims_of(i, "point")) {
          for (m in p$parts) {
            if (!nrow(m)) {
              next
            }
            nat <- mapnat(m)
            xs <- c(xs, nat$x)
            ys <- c(ys, nat$y)
            szs <- c(szs, rep(size[i], nrow(m)))
            rows <- c(rows, rep(i, nrow(m)))
          }
        }
      }
      if (!length(xs)) {
        return(scene)
      }
      xy <- .xy_units(scales, xs, ys)
      .draw(
        scene,
        vellum::points_grob(
          xy$x,
          xy$y,
          size = vellum::vl_unit(szs, "mm"),
          sketch = sk,
          gp = vellum::vl_gpar(fill = col, col = col, alpha = gp_alpha(a))
        ),
        rows = rows
      )
    }
  )

  scene
}

.emit_layer <- function(scene, L, scales) {
  # A constant paint `fill` (gradient/pattern) is only honoured by the filled
  # marks; anywhere else it would leak into a per-row `vl_gpar(fill = ...)`
  # undefined, so reject it with a clear message (cf. the polar-bar abort in
  # `.emit_bar`).
  if (!is.null(.paint_fill(L)) && !L$mark %in% .PAINT_MARKS) {
    cli::cli_abort(c(
      "A paint {.arg fill} (gradient or pattern) is not supported for the {.val {L$mark}} mark.",
      i = "It works on the filled marks: {.val {(.PAINT_MARKS)}}."
    ))
  }
  switch(
    L$mark,
    point = .emit_point(scene, L, scales),
    sf = .emit_sf(scene, L, scales),
    line = .emit_line(scene, L, scales),
    rule = .emit_rule(scene, L, scales),
    bar = .emit_bar(scene, L, scales),
    smooth = .emit_smooth(scene, L, scales),
    area = .emit_area(scene, L, scales),
    ribbon = .emit_ribbon(scene, L, scales),
    step = .emit_step(scene, L, scales),
    text = .emit_text(scene, L, scales),
    label = .emit_label(scene, L, scales),
    tile = .emit_tile(scene, L, scales),
    rect = .emit_rect(scene, L, scales),
    raster = .emit_raster(scene, L, scales),
    image = .emit_image(scene, L, scales),
    boxplot = .emit_boxplot(scene, L, scales),
    errorbar = .emit_errorbar(scene, L, scales),
    linerange = .emit_linerange(scene, L, scales),
    segment = .emit_segment(scene, L, scales),
    edges = .emit_edges(scene, L, scales),
    edge_bundle = .emit_edge_bundle(scene, L, scales),
    flow_map = .emit_flow_map(scene, L, scales),
    nodes = .emit_point(scene, L, scales),
    node_pie = .emit_node_pie(scene, L, scales),
    node_text = .emit_text(scene, L, scales),
    edge_text = .emit_text(scene, L, scales),
    hex = .emit_hex(scene, L, scales),
    datashade = .emit_datashade(scene, L, scales),
    rug = .emit_rug(scene, L, scales),
    violin = .emit_violin(scene, L, scales),
    ridgeline = .emit_ridgeline(scene, L, scales),
    halfeye = .emit_halfeye(scene, L, scales),
    interval = .emit_interval(scene, L, scales),
    contour = .emit_contour(scene, L, scales),
    contour_filled = .emit_contour_filled(scene, L, scales),
    ellipse = .emit_region(scene, L, scales),
    hull = .emit_region(scene, L, scales),
    sankey = .emit_sankey(scene, L, scales),
    sparkline = .emit_sparkline(scene, L, scales),
    hierarchy = .emit_hierarchy(scene, L, scales),
    chord = .emit_chord(scene, L, scales),
    cli::cli_abort("Unknown mark {.val {L$mark}}.")
  )
}

# A rug: short marginal ticks at each datum's x (bottom/top) and/or y (left/
# right) position. Ticks are drawn at the panel edge in npc units, so `sides`
# selects the edges. Cartesian only (no flip/polar). `length` is the tick length
# as a fraction of the panel (npc).
.emit_rug <- function(scene, L, scales) {
  n <- L$n
  sides <- L$stat_params$sides %||% "bl"
  len <- L$stat_params$length %||% 0.03
  col <- rep_len(.aes_colour(L, scales, "black"), n)
  alpha <- rep_len(.aes_alpha(L, scales, NA_real_), n)
  lwd <- .aes_param(L, "linewidth", 0.5)
  lty <- .resolve_lty(L, scales, n)
  # One segments grob per distinct (colour, alpha, linetype) so a mapped
  # colour/alpha/linetype is honoured per tick rather than collapsed to the
  # first row's style.
  groups <- .style_groups(n, list(col = col, alpha = alpha, lty = lty))
  # `pos` are native positions over all rows; draw each style group's subset.
  tick <- function(scene, pos, y0, y1, vertical) {
    for (idx in groups) {
      gp <- .gp_stroke(col, alpha, idx[1], lwd, lty)
      u <- vellum::vl_unit(pos[idx], "native")
      if (vertical) {
        grob <- vellum::segments_grob(
          u,
          vellum::vl_unit(y0, "npc"),
          u,
          vellum::vl_unit(y1, "npc"),
          gp = gp
        )
      } else {
        grob <- vellum::segments_grob(
          vellum::vl_unit(y0, "npc"),
          u,
          vellum::vl_unit(y1, "npc"),
          u,
          gp = gp
        )
      }
      scene <- .draw(scene, grob, rows = idx)
    }
    scene
  }
  if (!is.null(L$values$x) && (grepl("b", sides) || grepl("t", sides))) {
    nx <- rep_len(scales$x$map(L$values$x), n)
    if (grepl("b", sides)) {
      scene <- tick(scene, nx, 0, len, TRUE)
    }
    if (grepl("t", sides)) scene <- tick(scene, nx, 1, 1 - len, TRUE)
  }
  if (!is.null(L$values$y) && (grepl("l", sides) || grepl("r", sides))) {
    ny <- rep_len(scales$y$map(L$values$y), n)
    if (grepl("l", sides)) {
      scene <- tick(scene, ny, 0, len, FALSE)
    }
    if (grepl("r", sides)) scene <- tick(scene, ny, 1, 1 - len, FALSE)
  }
  scene
}

# Compile every layer's marks into the (already panel-positioned) scene. A layer
# with a non-normal blend mode is wrapped in its own vl_viewport(blend=) so its
# whole content composites as one isolated group against the backdrop (the panel
# and earlier layers); the wrapper carries the panel's scales so native
# coordinates still resolve.
# Per-layer SVG identity. `.compile_marks` records the layer currently being
# emitted here; `.draw()` stamps each grob with that `id` before handing it to
# vellum, so SVG output carries a `data-vellum-id` per layer (e.g. "layer-1-point")
# -- a stable selector for snapshot tests / accessibility / future interactivity.
# It is purely additive metadata: raster/PDF output is unchanged. (A small env is
# used so emitters need not thread the id through every grob call; the `id` is set
# per layer and is single-threaded with the rest of compilation.)
.mark_ctx <- new.env(parent = emptyenv())

# Set the per-plot (or per composition-cell) interaction context the mark emitter
# reads: `plot_interactive` (a plot declaring any selection/filter/bind keys its
# marks so a host can address them) and `plot_filters` (the selection names whose
# filter_by() targets this view -- emitted as per-element `filt` tags so a host
# scopes the hide to this view, never the cross-view source). Called by both the
# single-plot path (`.draw_plot`) and the aligned-composition path.
.set_interaction_ctx <- function(spec) {
  # Page size (inches), for emitters that must compare device text extent to a
  # native span -- e.g. sunburst label fitting. NULL when unknown.
  .mark_ctx$page <- NULL
  if (!S7::S7_inherits(spec, PlotSpec)) {
    .mark_ctx$plot_interactive <- FALSE
    .mark_ctx$plot_filters <- NULL
    return(invisible())
  }
  .mark_ctx$page <- c(w = spec@width, h = spec@height)
  filt_names <- vapply(spec@filters, function(f) f@selection, character(1))
  .mark_ctx$plot_interactive <- length(spec@selections) > 0L ||
    length(filt_names) > 0L ||
    length(spec@binds) > 0L
  .mark_ctx$plot_filters <- if (length(filt_names)) unique(filt_names) else NULL
  invisible()
}

# Stamp every emitted grob with its stable, globally-unique node id (surfaced as
# `data-vellum-id` in SVG) and record its provenance (DESIGN §4, see
# `R/provenance.R`). `rows` is the row-key refinement: pass the original
# input-data row indices this grob draws when the emitter groups rows by style;
# it defaults to the whole layer (`.mark_ctx$rows`) otherwise. Purely additive
# metadata -- raster/PDF output is unchanged.
.draw <- function(scene, grob, rows = NULL) {
  id <- .provenance_record(rows = rows)
  if (!is.null(id)) {
    grob@id <- id
  }
  # Per-element interactivity (DESIGN-INTERACTIVITY.md Phase 2). Attach the data
  # key + tooltip/hover metadata only when the emitter refined `rows` to *this*
  # grob's own elements (so `data_id[rows]` aligns 1:1 with what is drawn) and
  # this is a real mark, not an effect halo. Gated on a declared `data_id` in the
  # layer context, so a non-interactive plot sets nothing. `keys`/`meta` flow into
  # vellum's SVG `data-key` and `scene_model()`; a static PNG/SVG render ignores
  # them.
  if (
    !is.null(rows) &&
      identical(.mark_ctx$kind, "mark") &&
      !is.null(.mark_ctx$data_id)
  ) {
    grob@keys <- .mark_ctx$data_id[rows]
    m <- .elem_meta(rows)
    if (!is.null(m)) {
      grob@meta <- m
    }
  }
  vellum::draw(scene, grob)
}

# Build the per-element `meta` list (one record per drawn element) from the layer
# context's resolved tooltip / hover-group, indexed by this grob's `rows`. Returns
# NULL when neither is declared (so grobs carry no `meta`).
.elem_meta <- function(rows) {
  tt <- .mark_ctx$tooltip
  hg <- .mark_ctx$hover_group
  hc <- .mark_ctx$hover_color
  sc <- .mark_ctx$selected_color
  lg <- .mark_ctx$legend
  fv <- .mark_ctx$filter_value
  cond <- .mark_ctx$conditions
  # Graph edge endpoints: the two node keys (`name`s) this edge joins, so a host
  # can relate edges to nodes for neighbour highlighting. NULL for non-edge marks.
  esrc <- .mark_ctx$edge_source
  etgt <- .mark_ctx$edge_target
  # The "<sel>:<aes>" tags this layer's elements participate in (same for every
  # row of the layer, since a condition applies to the whole layer). A per-row
  # `if_false` column is carried per element as `cond_value`; a constant one lives
  # in the plot-level block.
  cond_tags <- if (length(cond)) {
    paste0(
      vapply(cond, function(cnd) cnd$selection, character(1)),
      ":",
      names(cond)
    )
  } else {
    NULL
  }
  cond_rowvals <- if (length(cond)) {
    cv <- lapply(cond, function(cnd) {
      f <- cnd$if_false
      if (!is.null(f) && length(f) > 1L) f else NULL # per-row only
    })
    if (any(lengths(cv) > 0L)) stats::setNames(cv, cond_tags) else NULL
  } else {
    NULL
  }
  # `filt` tags: the selection names whose filter_by() targets this view (plot /
  # composition cell), the same for every element. A host hides a tagged element
  # when it is not in that selection's members — scoped to this view, so a
  # cross-view filter never touches the source cell.
  filt_tags <- .mark_ctx$plot_filters
  # `join`: the cross-view identity (the original data id before per-cell key
  # prefixing), so a host can match a selection in one cell to rows in another.
  # Present only in a composition; NULL in a single plot.
  join <- .mark_ctx$join
  if (
    is.null(tt) &&
      is.null(hg) &&
      is.null(hc) &&
      is.null(sc) &&
      is.null(lg) &&
      is.null(fv) &&
      is.null(cond_tags) &&
      is.null(filt_tags) &&
      is.null(join) &&
      is.null(esrc) &&
      is.null(etgt)
  ) {
    return(NULL)
  }
  lapply(rows, function(i) {
    rec <- list()
    if (!is.null(tt)) {
      rec$tooltip <- as.character(tt[[i]])
    }
    if (!is.null(hg)) {
      rec$hover_group <- as.character(hg[[i]])
    }
    if (!is.null(hc)) {
      rec$hover_color <- as.character(hc[[i]])
    }
    if (!is.null(sc)) {
      rec$selected_color <- as.character(sc[[i]])
    }
    if (!is.null(esrc)) {
      rec$source <- as.character(esrc[[i]])
    }
    if (!is.null(etgt)) {
      rec$target <- as.character(etgt[[i]])
    }
    # `legend`: the discrete series this element belongs to ("<aes>:<value>"), so a
    # legend swatch (tagged with `legend_for`) can highlight the whole series.
    if (!is.null(lg)) {
      rec$legend <- lg[[i]]
    }
    # `filter_value`: this element's value on the continuous colour scale, for the
    # interactive colorbar filter.
    if (!is.null(fv)) {
      rec$filter_value <- fv[[i]]
    }
    # `cond`: the conditional-encoding tags this element participates in.
    if (!is.null(cond_tags)) {
      rec$cond <- cond_tags
      if (!is.null(cond_rowvals)) {
        rec$cond_value <- lapply(cond_rowvals, function(v) v[[i]])
      }
    }
    # `filt`: the filter selections this view is filtered by.
    if (!is.null(filt_tags)) {
      rec$filt <- filt_tags
    }
    # `join`: cross-view identity (per-cell key prefixing keeps it separate).
    if (!is.null(join)) {
      rec$join <- as.character(join[[i]])
    }
    rec
  })
}

# Per-row legend membership: for each discrete colour/fill/shape scale the layer
# maps, the element's series key(s) "<aes>:<value>". Returns a list (one entry per
# row, each a character vector), or NULL when the layer maps no discrete legend
# aesthetic. Matches the `legend_for` a discrete legend swatch carries.
.legend_membership <- function(L, scales) {
  n <- L$n
  cols <- list()
  if (!is.null(scales$color) && identical(scales$color$kind, "discrete")) {
    cv <- L$values$color %||% L$values$fill
    if (!is.null(cv)) {
      cols[["color"]] <- paste0("color:", as.character(rep_len(cv, n)))
    }
  }
  if (!is.null(scales$shape) && !is.null(L$values$shape)) {
    cols[["shape"]] <- paste0(
      "shape:",
      as.character(rep_len(L$values$shape, n))
    )
  }
  if (!length(cols)) {
    return(NULL)
  }
  lapply(seq_len(n), function(i) {
    unname(vapply(cols, function(v) v[[i]], character(1)))
  })
}

# The [xscale, yscale] a blend/effect wrapper viewport carries so native
# coordinates still resolve inside it (a polar panel uses the fixed [-1, 1] square).
.panel_scale_range <- function(scales) {
  if (is.null(scales$polar)) {
    list(x = scales$x$domain, y = scales$y$domain)
  } else {
    list(x = c(-1, 1), y = c(-1, 1))
  }
}

# grid `lwd` per millimetre of stroke width (1 lwd = 1/96 inch, as in grid).
.MM_TO_LWD <- 96 / 25.4

# The glow halo's base width: a stroke's linewidth (lwd) or a point's diameter
# (mm), read from the same param + default the emitter would use.
.glow_base <- function(L) {
  if (L$mark %in% c("point", "nodes")) {
    L$params$size %||% 1
  } else {
    L$params$linewidth %||%
      switch(
        L$mark,
        line = 1.5,
        step = 1.5,
        edges = 0.5,
        edge_bundle = 0.5,
        1
      )
  }
}

# Generalized underlay copy-emitter for glow / outline / shadow: draw one copy
# of the mark per entry of `deltas` (width in mm added to the base stroke width /
# point diameter), at `alpha` and `colour`, composited under `blend`, offset by
# (`xoff`, `yoff`) in **millimetres** (device-exact, via vellum's compound
# `npc + mm` unit). Reuses the mark's own emitter so coords / flip / polar all
# stay correct. Widest first, so opacity accumulates toward centre.
.emit_copies <- function(
  scene,
  L,
  scales,
  deltas,
  alpha,
  colour,
  blend,
  xoff = 0,
  yoff = 0
) {
  is_point <- L$mark %in% c("point", "nodes")
  base <- .glow_base(L)
  rng <- .panel_scale_range(scales)
  scene <- vellum::push(
    scene,
    vellum::vl_viewport(
      x = vellum::vl_unit(0.5, "npc") + vellum::vl_unit(xoff, "mm"),
      y = vellum::vl_unit(0.5, "npc") + vellum::vl_unit(yoff, "mm"),
      xscale = rng$x,
      yscale = rng$y,
      blend = if (identical(blend, "normal")) NULL else blend
    )
  )
  for (d in deltas) {
    L2 <- L
    L2$effects <- list() # copies are plain (their halo followed the same path)
    L2$params$alpha <- alpha
    if (!is.null(colour)) {
      L2$values$color <- NULL
      L2$values$fill <- NULL
      L2$params$color <- colour
      L2$params$fill <- colour
    }
    if (is_point) {
      L2$values$size <- NULL
      L2$params$size <- base + d
    } else {
      L2$params$linewidth <- base + d * .MM_TO_LWD
    }
    scene <- .emit_layer(scene, L2, scales)
  }
  vellum::pop(scene)
}

.emit_glow <- function(scene, L, scales, g) {
  frac <- seq.int(g@layers, 1L) / g@layers
  .emit_copies(scene, L, scales, frac * g@size, g@alpha, g@color, g@blend)
}

.emit_outline <- function(scene, L, scales, o) {
  # one opaque copy, wider by `size` per side, in a contrasting colour
  .emit_copies(scene, L, scales, 2 * o@size, o@alpha, o@color, "normal")
}

.emit_shadow <- function(scene, L, scales, s) {
  frac <- seq.int(s@layers, 1L) / s@layers
  .emit_copies(
    scene,
    L,
    scales,
    frac * s@spread,
    s@alpha,
    s@color,
    "multiply",
    xoff = s@x,
    yoff = s@y
  )
}

.emit_motion <- function(scene, L, scales, m) {
  # A fading trail: copy `i` sits at fraction `f` of the (x, y) offset and widens
  # by `spread * f`; opacity ramps from `alpha` at the nearest copy toward the
  # tail (shaped by `decay`). Draw furthest/faintest first so nearer, stronger
  # ghosts overpaint, all beneath the crisp original.
  for (i in seq.int(m@n, 1L)) {
    f <- i / m@n
    a <- m@alpha * ((m@n - i + 1L) / m@n)^m@decay
    scene <- .emit_copies(
      scene,
      L,
      scales,
      m@spread * f,
      a,
      m@color,
      m@blend,
      xoff = m@x * f,
      yoff = m@y * f
    )
  }
  scene
}

.emit_underlay <- function(scene, L, scales, e) {
  # Effect copies are decorative underlays, not the core layer: tag their
  # provenance entries so a consumer can tell a halo apart from the data mark.
  old_kind <- .mark_ctx$kind
  .mark_ctx$kind <- "effect"
  on.exit(.mark_ctx$kind <- old_kind, add = TRUE)
  if (S7::S7_inherits(e, GlowSpec)) {
    .emit_glow(scene, L, scales, e)
  } else if (S7::S7_inherits(e, OutlineSpec)) {
    .emit_outline(scene, L, scales, e)
  } else if (S7::S7_inherits(e, ShadowSpec)) {
    .emit_shadow(scene, L, scales, e)
  } else if (S7::S7_inherits(e, MotionSpec)) {
    .emit_motion(scene, L, scales, e)
  } else {
    scene
  }
}

.compile_marks <- function(scene, resolved, scales, panel = NA_character_) {
  on.exit(.mark_ctx$id <- NULL, add = TRUE)
  .mark_ctx$panel <- panel
  for (i in seq_along(resolved)) {
    L <- resolved[[i]]
    if (!L$n) {
      next
    } # empty facet panel
    .mark_ctx$id <- sprintf("layer-%d-%s", i, L$mark)
    # Provenance context for every grob this layer emits (DESIGN §4). Set once
    # per layer: `.draw()` reads it. `rows` defaults to the whole layer -- an
    # emitter that groups rows by style refines it per group (see `PROVENANCE:`).
    .mark_ctx$layer <- i
    .mark_ctx$mark <- L$mark
    .mark_ctx$channels <- .layer_channels(L, scales)
    .mark_ctx$rows <- seq_len(L$n)
    .mark_ctx$kind <- "mark"
    # Per-row interactivity for this layer (NULL when none declared). Used only
    # when it aligns to the drawn rows: a row-preserving mark keeps `length == n`;
    # an aggregating stat (bin/count) changes `n`, so we drop it rather than
    # mis-key (a future phase can re-derive keys from computed columns).
    ok <- !is.null(L$meta) &&
      !is.null(L$meta$data_id) &&
      length(L$meta$data_id) == L$n
    .mark_ctx$data_id <- if (ok) L$meta$data_id else NULL
    .mark_ctx$tooltip <- if (ok) L$meta$tooltip else NULL
    .mark_ctx$hover_group <- if (ok) L$meta$hover_group else NULL
    .mark_ctx$hover_color <- if (ok) L$meta$hover_color else NULL
    .mark_ctx$selected_color <- if (ok) L$meta$selected_color else NULL
    # Legend membership (for legend-driven series highlight/select) — only when the
    # layer is interactive (its elements are keyed and thus addressable).
    .mark_ctx$legend <- if (ok) .legend_membership(L, scales) else NULL
    # Per-mark value on a continuous colour scale, for the interactive colorbar
    # filter (a host hides marks whose value is outside the dragged range). Only
    # for keyed marks (addressable) with a continuous colour scale; the raw data
    # value is `L$values$color` (fallback `fill`), recycled to the row count.
    .mark_ctx$filter_value <- local({
      cc <- scales$color
      if (!ok || is.null(cc) || !identical(cc$kind, "continuous")) {
        return(NULL)
      }
      v <- L$values$color %||% L$values$fill
      if (is.null(v)) NULL else as.numeric(rep_len(v, L$n))
    })
    # Conditional encodings (from `condition()`): a layer bearing one is
    # interactive, so its elements need addressable keys even if no `data_id` was
    # declared -- default to the row index. `cond` carries, per row, the "<sel>:<aes>"
    # tags this element participates in (like `legend`), so a host can style
    # non-members; the `if_false` constant + `empty` live in the plot-level block.
    .mark_ctx$conditions <- if (length(L$conditions)) L$conditions else NULL
    if (
      is.null(.mark_ctx$data_id) &&
        (length(L$conditions) || isTRUE(.mark_ctx$plot_interactive)) &&
        L$n >= 1L
    ) {
      .mark_ctx$data_id <- as.character(seq_len(L$n))
    }
    # A `group_by`/`fields` point selection groups elements sharing those column
    # values -- carried as the element `hover_group`, so a host links the whole
    # group on hover/click (the existing hover-group machinery; the condition then
    # spotlights the group). Only when the layer doesn't already declare a
    # hover_group, and the values align 1:1 with the drawn rows.
    if (
      is.null(.mark_ctx$hover_group) &&
        !is.null(L$selgroup) &&
        length(L$selgroup) == L$n
    ) {
      .mark_ctx$hover_group <- as.character(L$selgroup)
    }
    # Graph marks (nodes/edges): when the plot is interactive, key nodes by vertex
    # `name` and carry each edge's two endpoint node names as source/target, so a
    # host can relate nodes to edges for neighbour highlighting. Overrides the
    # row-index default above with the stable graph key. A node with no declared
    # tooltip defaults to its name. Inert on a static render (gated on
    # `plot_interactive`). Straight / gradient / elbow edges all carry per-edge
    # keys (they pass `rows=` to `.draw`); self-loops draw without `rows=` so they
    # stay unkeyed -- harmless, a loop's only neighbour is its own node.
    .mark_ctx$edge_source <- NULL
    .mark_ctx$edge_target <- NULL
    gi <- L$graph_identity
    if (isTRUE(.mark_ctx$plot_interactive) && !is.null(gi)) {
      .mark_ctx$data_id <- gi$key
      if (identical(gi$kind, "edges")) {
        .mark_ctx$edge_source <- gi$source
        .mark_ctx$edge_target <- gi$target
      } else if (is.null(.mark_ctx$tooltip)) {
        .mark_ctx$tooltip <- gi$key
      }
    }
    # In a composition, each cell is a separate plot compiled independently, so
    # cells share row-index keys that would collide in one host runtime (hiding one
    # cell's key-3 would hit every cell's key-3). So a composition cell's DOM key is
    # made unique (prefixed with its `subplot-N` panel), while the original value is
    # kept as `join` -- the cross-view identity a host matches on (brush cell A ->
    # filter cell B's rows with the same join). A single plot or facet panel
    # (`panel-r-c`, whose keys are already original row ids) is unchanged: no prefix,
    # no `join`.
    .mark_ctx$join <- NULL
    if (
      !is.null(.mark_ctx$data_id) &&
        !is.na(.mark_ctx$panel) &&
        startsWith(.mark_ctx$panel, "subplot-")
    ) {
      .mark_ctx$join <- .mark_ctx$data_id
      .mark_ctx$data_id <- paste0(.mark_ctx$panel, ":", .mark_ctx$data_id)
    }
    # Layer effects (glow / outline / shadow) draw beneath the core, in order.
    for (e in .underlay_effects(L)) {
      scene <- .emit_underlay(scene, L, scales, e)
    }
    # The core layer, optionally isolated in its own group for a blend and/or a
    # per-layer clip (clip_layer()). Both are viewport properties, so a layer that
    # has both takes one group carrying the mask + blend together.
    blend <- L$blend %||% "normal"
    # Per-layer clip (cartesian only, like the panel-level clip); the native-coord
    # mask would not align under polar / nonlinear trans.
    layer_mask <- if (
      !is.null(L$clip) && is.null(scales$polar) && is.null(scales$trans)
    ) {
      rng <- .panel_scale_range(scales)
      .clip_mask(L$clip, rng$x, rng$y)
    }
    if (!identical(blend, "normal") || !is.null(layer_mask)) {
      rng <- .panel_scale_range(scales)
      scene <- vellum::push(
        scene,
        vellum::vl_viewport(
          xscale = rng$x,
          yscale = rng$y,
          blend = if (identical(blend, "normal")) NULL else blend,
          mask = layer_mask
        )
      )
      scene <- .emit_layer(scene, L, scales)
      scene <- vellum::pop(scene)
    } else {
      scene <- .emit_layer(scene, L, scales)
    }
  }
  scene
}
