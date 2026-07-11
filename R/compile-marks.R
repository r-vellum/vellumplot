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
  if (fill_fallback) {
    L$params$color %||% L$params$fill %||% default
  } else {
    L$params$color %||% default
  }
}

.aes_param <- function(L, name, default) L$params[[name]] %||% default

# A gradient/paint fill value on a layer (from `fill = linear_gradient(...)`), or
# NULL. When present, a filled mark paints its whole region with one grob rather
# than a per-row colour vector (the paint is a single value, not data-mapped).
.grad_fill <- function(L) {
  f <- L$params$fill
  if (inherits(f, "vellum_gradient")) f else NULL
}

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

# Apply a layer's `nudge_x`/`nudge_y` (millimetres) to a resolved `.xy_units()`
# pair, as a device-exact compound offset (vellum's `native + mm` unit). A zero
# nudge is left untouched, so an un-nudged mark is byte-identical.
.nudge_xy <- function(xy, L) {
  nx <- .aes_param(L, "nudge_x", 0)
  ny <- .aes_param(L, "nudge_y", 0)
  if (!is.null(nx) && nx != 0) {
    xy$x <- xy$x + vellum::vl_unit(nx, "mm")
  }
  if (!is.null(ny) && ny != 0) {
    xy$y <- xy$y + vellum::vl_unit(ny, "mm")
  }
  xy
}

# Coordinate placement honouring coord_flip: under flip the horizontal axis
# carries the data y and the vertical axis the data x. Emitters compute native
# coordinates in data space, then place them through these helpers so only the
# final grob arguments swap.
.flipped <- function(scales) isTRUE(scales$flip)

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
  if (isTRUE(L$stat_params$auto) && n > .DATASHADE_AUTO) {
    return(.emit_datashade(scene, L, scales))
  }
  sk <- .mark_sketch(L, scales)
  xn <- rep_len(scales$x$map(L$values$x), n)
  yn <- rep_len(scales$y$map(L$values$y), n)
  if (identical(L$position, "jitter")) {
    ax <- 0.4 * (scales$x$band_width %||% .resolution(xn))
    ay <- 0.4 * .resolution(yn)
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
          alpha = if (is.na(a)) NULL else a
        )
      ),
      rows = idx
    )
  }
  scene
}

.emit_line <- function(scene, L, scales) {
  n <- L$n
  xn <- rep_len(scales$x$map(L$values$x), n)
  yn <- rep_len(scales$y$map(L$values$y), n)
  col <- rep_len(.aes_colour(L, scales, "black"), n)
  alpha <- rep_len(.aes_alpha(L, scales, NA_real_), n)
  lty <- .aes_linetype(L, scales, NULL)
  lty <- if (is.null(lty)) NULL else rep_len(lty, n)
  lwd <- .aes_param(L, "linewidth", 1.5)
  sk <- .mark_sketch(L, scales)

  gi <- 0L
  for (idx in .style_groups(n, list(col = col, alpha = alpha, lty = lty))) {
    o <- idx[order(xn[idx])] # a line is drawn in x order
    a <- alpha[idx[1]]
    xy <- .xy_path(scales, xn[o], yn[o])
    scene <- .draw(
      scene,
      vellum::lines_grob(
        xy$x,
        xy$y,
        sketch = .sketch_bump(sk, gi),
        gp = vellum::vl_gpar(
          col = col[idx[1]],
          lwd = lwd,
          lty = lty[idx[1]],
          alpha = if (is.na(a)) NULL else a
        )
      ),
      # PROVENANCE: `o` are the layer rows this polyline draws (x-ordered).
      rows = o
    )
    gi <- gi + 1L
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
  piece <- L$values$.piece
  sk <- .mark_sketch(L, scales)
  gi <- 0L
  for (pid in unique(piece)) {
    idx <- which(piece == pid)
    a <- alpha[idx[1]]
    xy <- .xy_path(scales, xn[idx], yn[idx])
    scene <- .draw(
      scene,
      vellum::lines_grob(
        xy$x,
        xy$y,
        sketch = .sketch_bump(sk, gi),
        gp = vellum::vl_gpar(
          col = col[idx[1]],
          lwd = lwd,
          alpha = if (is.na(a)) NULL else a
        )
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
          alpha = if (is.na(a)) NULL else a
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
  gp <- vellum::vl_gpar(col = col, lwd = lwd)
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
        gp = vellum::vl_gpar(col = NA, alpha = if (is.na(a)) NULL else a)
      ),
      rows = idx
    )
  }
  scene
}

.emit_bar <- function(scene, L, scales) {
  grad <- .grad_fill(L)
  if (!is.null(scales$polar)) {
    if (!is.null(grad)) {
      cli::cli_abort("Gradient fills are not supported for polar bars / pies.")
    }
    return(.emit_bar_polar(scene, L, scales))
  }
  n <- L$n
  sk <- .mark_sketch(L, scales)
  xp <- rep_len(scales$x$map(L$values$x), n)
  fill <- if (is.null(grad)) rep_len(.aes_colour(L, scales, "grey35"), n)
  alpha <- rep_len(.aes_alpha(L, scales, NA_real_), n)
  band <- scales$x$band_width %||% .resolution(xp)

  # vertical span: a stacked [ymin, ymax], else 0 -> y
  ymin_d <- if (!is.null(L$values$ymin)) L$values$ymin else rep(0, n)
  ymax_d <- if (!is.null(L$values$ymax)) L$values$ymax else L$values$y
  y0 <- rep_len(scales$y$map(ymin_d), n)
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
      w <- w / G
      xc <- xp + (rank - (G + 1) / 2) / G * band
    }
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
  for (idx in .style_groups(n, list(fill = fill, alpha = alpha))) {
    a <- alpha[idx[1]]
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
        gp = vellum::vl_gpar(
          fill = fill[idx[1]],
          col = NA,
          alpha = if (is.na(a)) NULL else a
        )
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

# A filled ribbon between ymin and ymax, one polygon per style group.
.emit_ribbon <- function(scene, L, scales) {
  n <- L$n
  xn <- rep_len(scales$x$map(L$values$x), n)
  ymin <- rep_len(scales$y$map(L$values$ymin), n)
  ymax <- rep_len(scales$y$map(L$values$ymax), n)
  sk <- .mark_sketch(L, scales)

  grad <- .grad_fill(L)
  if (!is.null(grad)) {
    o <- order(xn)
    poly <- .xy_area(scales, xn[o], ymin[o], xn[o], ymax[o])
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
    a <- alpha[idx[1]]
    poly <- .xy_area(scales, xn[o], ymin[o], xn[o], ymax[o])
    scene <- .draw(
      scene,
      vellum::polygon_grob(
        poly$x,
        poly$y,
        sketch = .sketch_bump(sk, gi),
        gp = vellum::vl_gpar(
          fill = fill[idx[1]],
          col = NA,
          alpha = if (is.na(a)) NULL else a
        )
      ),
      # PROVENANCE: `o` are the layer rows this polygon draws (x-ordered).
      rows = o
    )
    gi <- gi + 1L
  }
  scene
}

# An area mark: the region between `y` and the zero baseline (a ribbon with
# ymin = 0).
.emit_area <- function(scene, L, scales) {
  n <- L$n
  xn <- rep_len(scales$x$map(L$values$x), n)
  y0 <- rep_len(scales$y$map(0), n)
  y1 <- rep_len(scales$y$map(L$values$y), n)
  sk <- .mark_sketch(L, scales)

  # A gradient fill paints the whole area as one polygon (in x order).
  grad <- .grad_fill(L)
  if (!is.null(grad)) {
    o <- order(xn)
    poly <- .xy_area(scales, xn[o], y1[o], xn[o], y0[o])
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
    a <- alpha[idx[1]]
    poly <- .xy_area(scales, xn[o], y1[o], xn[o], y0[o])
    scene <- .draw(
      scene,
      vellum::polygon_grob(
        poly$x,
        poly$y,
        sketch = .sketch_bump(sk, gi),
        gp = vellum::vl_gpar(
          fill = fill[idx[1]],
          col = NA,
          alpha = if (is.na(a)) NULL else a
        )
      ),
      # PROVENANCE: `o` are the layer rows this polygon draws (x-ordered).
      rows = o
    )
    gi <- gi + 1L
  }
  scene
}

# A staircase line: each segment is expanded into a horizontal-then-vertical
# ("hv") or vertical-then-horizontal ("vh") pair before drawing.
.emit_step <- function(scene, L, scales) {
  n <- L$n
  xn <- rep_len(scales$x$map(L$values$x), n)
  yn <- rep_len(scales$y$map(L$values$y), n)
  col <- rep_len(.aes_colour(L, scales, "black"), n)
  alpha <- rep_len(.aes_alpha(L, scales, NA_real_), n)
  lty <- .aes_linetype(L, scales, NULL)
  lty <- if (is.null(lty)) NULL else rep_len(lty, n)
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
    a <- alpha[idx[1]]
    ln <- .xy_path(scales, ex, ey)
    scene <- .draw(
      scene,
      vellum::lines_grob(
        ln$x,
        ln$y,
        sketch = .sketch_bump(sk, gi),
        gp = vellum::vl_gpar(
          col = col[idx[1]],
          lwd = lwd,
          lty = lty[idx[1]],
          alpha = if (is.na(a)) NULL else a
        )
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
  xn <- rep_len(scales$x$map(L$values$x), n)
  yn <- rep_len(scales$y$map(L$values$y), n)
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
  just <- c(.aes_param(L, "hjust", "centre"), .aes_param(L, "vjust", "centre"))

  for (idx in .style_groups(n, list(col = col, alpha = alpha))) {
    a <- alpha[idx[1]]
    xy <- .nudge_xy(.xy_units(scales, xn[idx], yn[idx]), L)
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
          alpha = if (is.na(a)) NULL else a
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
  xn <- rep_len(scales$x$map(L$values$x), n)
  yn <- rep_len(scales$y$map(L$values$y), n)
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
    xy <- .nudge_xy(.xy_units(scales, xn[idx], yn[idx]), L)
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
          alpha = if (is.na(a)) NULL else a
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
          alpha = if (is.na(a)) NULL else a
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
  xp <- rep_len(scales$x$map(L$values$x), n)
  yp <- rep_len(scales$y$map(L$values$y), n)
  fill <- rep_len(.aes_colour(L, scales, "grey50"), n)
  alpha <- rep_len(.aes_alpha(L, scales, NA_real_), n)
  w <- rep_len(L$values$width %||% .resolution(xp), n)
  h <- rep_len(L$values$height %||% .resolution(yp), n)
  sk <- .mark_sketch(L, scales)

  gi <- 0L
  for (idx in .style_groups(n, list(fill = fill, alpha = alpha))) {
    a <- alpha[idx[1]]
    r <- .rect_units(scales, xp[idx], yp[idx], w[idx], h[idx])
    scene <- .draw(
      scene,
      vellum::rect_grob(
        x = r$x,
        y = r$y,
        width = r$width,
        height = r$height,
        sketch = .sketch_bump(sk, gi),
        gp = vellum::vl_gpar(
          fill = fill[idx[1]],
          col = NA,
          alpha = if (is.na(a)) NULL else a
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
  if (length(ux) * length(uy) != length(xv)) {
    cli::cli_abort(
      "{.fn mark_raster} needs a complete regular grid; use {.fn mark_tile}."
    )
  }
  cols <- rep_len(.aes_colour(L, scales, "grey50"), length(xv))
  ci <- match(xv, ux)
  ri <- match(yv, uy)
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
        gp = vellum::vl_gpar(fill = fillc, col = "grey20", lwd = 1)
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
  hw <- 0.4 * band
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
    dn <- (d$y / max(d$y)) * hw
    xc <- xc_all[j]
    # up the right side, then down the mirrored left side
    px <- c(xc + dn, rev(xc - dn))
    py <- scales$y$map(c(d$x, rev(d$x)))
    xy <- .xy_units(scales, px, py)
    scene <- .draw(
      scene,
      vellum::polygon_grob(
        xy$x,
        xy$y,
        sketch = .sketch_bump(sk, j),
        gp = vellum::vl_gpar(
          fill = colv[sel[1]],
          col = "grey30",
          lwd = 1,
          alpha = if (is.na(a)) NULL else a
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
  scale_h <- (L$stat_params$scale %||% 1.4) * band
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
    h <- (d$y / max(d$y)) * scale_h
    base <- ypos[j]
    px <- scales$x$map(c(d$x, rev(d$x)))
    py <- c(base + h, rep(base, length(h)))
    xy <- .xy_units(scales, px, py)
    scene <- .draw(
      scene,
      vellum::polygon_grob(
        xy$x,
        xy$y,
        sketch = .sketch_bump(sk, j),
        gp = vellum::vl_gpar(
          fill = colv[sel[1]],
          col = "grey30",
          lwd = 1,
          alpha = if (is.na(a)) NULL else a
        )
      ),
      # PROVENANCE: a ridge summarises all rows of its category.
      rows = sel
    )
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
  band <- scales$x$band_width %||% .resolution(xn)
  half <- (.aes_param(L, "width", 0.5) * band) / 2
  sk <- .mark_sketch(L, scales)

  gi <- 0L
  for (idx in .style_groups(n, list(col = col))) {
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
        gp = vellum::vl_gpar(col = col[idx[1]], lwd = lwd)
      )
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
  x0 <- rep_len(scales$x$map(L$values$x), n)
  y0 <- rep_len(scales$y$map(L$values$y), n)
  x1 <- rep_len(scales$x$map(L$values$xend), n)
  y1 <- rep_len(scales$y$map(L$values$yend), n)
  col <- rep_len(.aes_colour(L, scales, "black"), n)
  alpha <- rep_len(.aes_alpha(L, scales, NA_real_), n)
  lwd <- .aes_param(L, "linewidth", 1)
  sk <- .mark_sketch(L, scales)

  gi <- 0L
  for (idx in .style_groups(n, list(col = col, alpha = alpha))) {
    a <- alpha[idx[1]]
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
        gp = vellum::vl_gpar(
          col = col[idx[1]],
          lwd = lwd,
          alpha = if (is.na(a)) NULL else a
        )
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
  x0 <- rep_len(scales$x$map(L$values$x), n)
  y0 <- rep_len(scales$y$map(L$values$y), n)
  x1 <- rep_len(scales$x$map(L$values$xend), n)
  y1 <- rep_len(scales$y$map(L$values$yend), n)
  col <- rep_len(.aes_colour(L, scales, "grey40"), n)
  alpha <- rep_len(.aes_alpha(L, scales, NA_real_), n)
  lwd <- rep_len(.edge_width(L, scales, 0.5), n)
  arr <- if (isTRUE(L$stat_params$arrow)) {
    vellum::vl_arrow(type = "closed", length = vellum::vl_unit(2, "mm"))
  } else {
    NULL
  }
  # Straight edges can be sketched; self-loops (loop_grob) are always crisp.
  sk <- .mark_sketch(L, scales)

  # Per-edge endpoint node radii (mm), for exact node-boundary capping. vellum
  # resolves the mm caps in device space at render, so this is correct at any
  # size/dpi and for arbitrary per-vertex sizes -- no native-per-mm estimate.
  gh <- scales$graph
  # A small gap (mm) past the node radius so the edge/arrowhead clears the marker.
  gap <- 0.4

  loop <- x0 == x1 & y0 == y1
  # Straight edges, batched by (col, alpha, rounded lwd). Each end is capped at
  # its node's boundary via vellum's start_cap/end_cap (absolute mm).
  si <- which(!loop)
  if (length(si)) {
    grp_lwd <- round(lwd, 2)
    for (idx in .style_groups(
      length(si),
      list(col = col[si], alpha = alpha[si], lwd = grp_lwd[si])
    )) {
      g <- si[idx]
      a <- alpha[g[1]]
      s <- .seg_units(
        scales,
        vellum::vl_unit(x0[g], "native"),
        vellum::vl_unit(y0[g], "native"),
        vellum::vl_unit(x1[g], "native"),
        vellum::vl_unit(y1[g], "native")
      )
      # Node-boundary caps and parallel-edge spacing are both absolute (mm),
      # resolved by vellum in device space -> they track the mm node markers.
      start_cap <- if (!is.null(gh)) {
        vellum::vl_unit(gh$start_cap[g] + gap, "mm")
      }
      end_cap <- if (!is.null(gh)) vellum::vl_unit(gh$end_cap[g] + gap, "mm")
      offset <- if (!is.null(gh)) vellum::vl_unit(gh$offset[g], "mm")
      scene <- .draw(
        scene,
        vellum::segments_grob(
          s$x0,
          s$y0,
          s$x1,
          s$y1,
          arrow = arr,
          start_cap = start_cap,
          end_cap = end_cap,
          offset = offset,
          sketch = sk,
          gp = vellum::vl_gpar(
            col = col[g[1]],
            lwd = lwd[g[1]],
            alpha = if (is.na(a)) NULL else a
          )
        ),
        rows = g
      )
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
            alpha = if (is.na(a)) NULL else a
          )
        )
      )
    }
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
      gp = vellum::vl_gpar(alpha = if (is.na(a)) NULL else a)
    ),
    rows = seq_len(n)
  )
}

# Datashade: aggregate the points into a density raster filling the panel. The
# raster is binned over the panel's native domain so it aligns with the axes.
.emit_datashade <- function(scene, L, scales) {
  # Full coordinate vectors live in `L$ds` (training only saw their range, see
  # .resolve_layer); `mark_point(auto=)` falls through here without an `ds` slot,
  # so fall back to the resolved values in that case.
  ds <- L$ds %||% list(x = L$values$x, y = L$values$y)
  xn <- scales$x$map(ds$x)
  yn <- scales$y$map(ds$y)
  sp <- L$stat_params
  w <- as.integer(sp$width %||% 400L)
  h <- as.integer(sp$height %||% 300L)
  # Under flip the raster axes swap with the data.
  flip <- .flipped(scales)
  g <- vellum::datashade(
    if (flip) yn else xn,
    if (flip) xn else yn,
    width = if (flip) h else w,
    height = if (flip) w else h,
    xlim = if (flip) scales$y$domain else scales$x$domain,
    ylim = if (flip) scales$x$domain else scales$y$domain,
    colors = sp$colors %||% c("#deebf7", "#08306b"),
    how = sp$how %||% "eq_hist",
    interpolate = FALSE
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
  gp_alpha <- function(a) if (is.na(a)) NULL else a

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
        fill = fill,
        col = border,
        lwd = lwd,
        alpha = gp_alpha(a)
      )
      emit1 <- function(scene, g, rows) {
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
      if (interactive) {
        for (i in idx) {
          scene <- emit1(scene, gather_poly(i), rows = i)
        }
        scene
      } else {
        emit1(scene, gather_poly(idx), rows = idx)
      }
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
      emit1 <- function(scene, g, rows) {
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
      if (interactive) {
        for (i in idx) {
          scene <- emit1(scene, gather_line(i), rows = i)
        }
        scene
      } else {
        emit1(scene, gather_line(idx), rows = idx)
      }
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
    raster = .emit_raster(scene, L, scales),
    boxplot = .emit_boxplot(scene, L, scales),
    errorbar = .emit_errorbar(scene, L, scales),
    linerange = .emit_linerange(scene, L, scales),
    segment = .emit_segment(scene, L, scales),
    edges = .emit_edges(scene, L, scales),
    nodes = .emit_point(scene, L, scales),
    node_text = .emit_text(scene, L, scales),
    hex = .emit_hex(scene, L, scales),
    datashade = .emit_datashade(scene, L, scales),
    rug = .emit_rug(scene, L, scales),
    violin = .emit_violin(scene, L, scales),
    ridgeline = .emit_ridgeline(scene, L, scales),
    contour = .emit_contour(scene, L, scales),
    contour_filled = .emit_contour_filled(scene, L, scales),
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
  # One segments grob per distinct (colour, alpha) so a mapped colour/alpha is
  # honoured per tick rather than collapsed to the first row's style.
  groups <- .style_groups(n, list(col = col, alpha = alpha))
  # `pos` are native positions over all rows; draw each style group's subset.
  tick <- function(scene, pos, y0, y1, vertical) {
    for (idx in groups) {
      a <- alpha[idx[1]]
      gp <- vellum::vl_gpar(
        col = col[idx[1]],
        lwd = lwd,
        alpha = if (is.na(a)) NULL else a
      )
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
  if (is.null(tt) && is.null(hg) && is.null(hc) && is.null(sc) && is.null(lg)) {
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
    # `legend`: the discrete series this element belongs to ("<aes>:<value>"), so a
    # legend swatch (tagged with `legend_for`) can highlight the whole series.
    if (!is.null(lg)) {
      rec$legend <- lg[[i]]
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
      switch(L$mark, line = 1.5, step = 1.5, edges = 0.5, 1)
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
    # Layer effects (glow / outline / shadow) draw beneath the core, in order.
    for (e in .underlay_effects(L)) {
      scene <- .emit_underlay(scene, L, scales, e)
    }
    # The core layer (isolated in its own blend group if it carries a blend).
    blend <- L$blend %||% "normal"
    if (!identical(blend, "normal")) {
      rng <- .panel_scale_range(scales)
      scene <- vellum::push(
        scene,
        vellum::vl_viewport(xscale = rng$x, yscale = rng$y, blend = blend)
      )
      scene <- .emit_layer(scene, L, scales)
      scene <- vellum::pop(scene)
    } else {
      scene <- .emit_layer(scene, L, scales)
    }
  }
  scene
}
