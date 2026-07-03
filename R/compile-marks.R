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
    x = vellum::unit(rad * cos(ang), "native"),
    y = vellum::unit(rad * sin(ang), "native")
  )
}

.xy_units <- function(scales, x, y) {
  if (!is.null(scales$polar)) {
    return(.polar_xy(scales, x, y))
  }
  if (.flipped(scales)) {
    list(x = vellum::unit(y, "native"), y = vellum::unit(x, "native"))
  } else {
    list(x = vellum::unit(x, "native"), y = vellum::unit(y, "native"))
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

# Map an ordered polyline to grob units, densifying first under polar.
.xy_path <- function(scales, x, y) {
  if (!is.null(scales$polar)) {
    d <- .polar_munch(scales, x, y)
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
  .xy_units(scales, c(xa, rev(xb)), c(ya, rev(yb)))
}

.rect_units <- function(scales, xc, yc, w, h) {
  if (.flipped(scales)) {
    list(
      x = vellum::unit(yc, "native"),
      y = vellum::unit(xc, "native"),
      width = vellum::unit(h, "native"),
      height = vellum::unit(w, "native")
    )
  } else {
    list(
      x = vellum::unit(xc, "native"),
      y = vellum::unit(yc, "native"),
      width = vellum::unit(w, "native"),
      height = vellum::unit(h, "native")
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

.emit_point <- function(scene, L, scales) {
  n <- L$n
  if (isTRUE(L$stat_params$auto) && n > .DATASHADE_AUTO) {
    return(.emit_datashade(scene, L, scales))
  }
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
  alpha <- rep_len(.aes_param(L, "alpha", NA_real_), n)

  for (idx in .style_groups(n, list(col = col, alpha = alpha))) {
    a <- alpha[idx[1]]
    xy <- .xy_units(scales, xn[idx], yn[idx])
    scene <- .draw(
      scene,
      vellum::points_grob(
        xy$x,
        xy$y,
        size = vellum::unit(size[idx], "mm"),
        shape = shape[idx],
        gp = vellum::gpar(
          fill = col[idx[1]],
          col = col[idx[1]],
          alpha = if (is.na(a)) NULL else a
        )
      )
    )
  }
  scene
}

.emit_line <- function(scene, L, scales) {
  n <- L$n
  xn <- rep_len(scales$x$map(L$values$x), n)
  yn <- rep_len(scales$y$map(L$values$y), n)
  col <- rep_len(.aes_colour(L, scales, "black"), n)
  alpha <- rep_len(.aes_param(L, "alpha", NA_real_), n)
  lwd <- .aes_param(L, "linewidth", 1.5)

  for (idx in .style_groups(n, list(col = col, alpha = alpha))) {
    o <- idx[order(xn[idx])] # a line is drawn in x order
    a <- alpha[idx[1]]
    xy <- .xy_path(scales, xn[o], yn[o])
    scene <- .draw(
      scene,
      vellum::lines_grob(
        xy$x,
        xy$y,
        gp = vellum::gpar(
          col = col[idx[1]],
          lwd = lwd,
          alpha = if (is.na(a)) NULL else a
        )
      )
    )
  }
  scene
}

.emit_rule <- function(scene, L, scales) {
  col <- .aes_colour(L, scales, "grey40")[1]
  lwd <- .aes_param(L, "linewidth", 1)
  gp <- vellum::gpar(col = col, lwd = lwd)
  yi <- .intercept(L, "yintercept")
  xi <- .intercept(L, "xintercept")
  if (!is.null(yi)) {
    vy <- scales$y$map(yi)
    if (length(vy)) {
      k <- length(vy)
      s <- .seg_units(
        scales,
        vellum::unit(rep(0, k), "npc"),
        vellum::unit(vy, "native"),
        vellum::unit(rep(1, k), "npc"),
        vellum::unit(vy, "native")
      )
      scene <- .draw(
        scene,
        vellum::segments_grob(s$x0, s$y0, s$x1, s$y1, gp = gp)
      )
    }
  }
  if (!is.null(xi)) {
    vx <- scales$x$map(xi)
    if (length(vx)) {
      k <- length(vx)
      s <- .seg_units(
        scales,
        vellum::unit(vx, "native"),
        vellum::unit(rep(0, k), "npc"),
        vellum::unit(vx, "native"),
        vellum::unit(rep(1, k), "npc")
      )
      scene <- .draw(
        scene,
        vellum::segments_grob(s$x0, s$y0, s$x1, s$y1, gp = gp)
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
  alpha <- rep_len(.aes_param(L, "alpha", NA_real_), n)
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

  for (idx in .style_groups(n, list(fill = fill, alpha = alpha))) {
    a <- alpha[idx[1]]
    scene <- .draw(
      scene,
      vellum::sector_grob(
        x = vellum::unit(rep(0, length(idx)), "native"),
        y = vellum::unit(rep(0, length(idx)), "native"),
        r0 = vellum::unit(r0[idx], "native"),
        r1 = vellum::unit(r1[idx], "native"),
        theta0 = theta0[idx],
        theta1 = theta1[idx],
        fill = fill[idx],
        gp = vellum::gpar(col = NA, alpha = if (is.na(a)) NULL else a)
      )
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
  xp <- rep_len(scales$x$map(L$values$x), n)
  fill <- if (is.null(grad)) rep_len(.aes_colour(L, scales, "grey35"), n)
  alpha <- rep_len(.aes_param(L, "alpha", NA_real_), n)
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
        gp = vellum::gpar(fill = grad, col = NA)
      )
    ))
  }

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
        gp = vellum::gpar(
          fill = fill[idx[1]],
          col = NA,
          alpha = if (is.na(a)) NULL else a
        )
      )
    )
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
  has_se <- !is.null(L$values$ymin)
  ymin <- if (has_se) scales$y$map(L$values$ymin)
  ymax <- if (has_se) scales$y$map(L$values$ymax)

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
          gp = vellum::gpar(fill = cc, col = NA, alpha = 0.25)
        )
      )
    }
    ln <- .xy_path(scales, xn[o], yn[o])
    scene <- .draw(
      scene,
      vellum::lines_grob(
        ln$x,
        ln$y,
        gp = vellum::gpar(col = cc, lwd = 1.5)
      )
    )
  }
  scene
}

# A filled ribbon between ymin and ymax, one polygon per style group.
.emit_ribbon <- function(scene, L, scales) {
  n <- L$n
  xn <- rep_len(scales$x$map(L$values$x), n)
  ymin <- rep_len(scales$y$map(L$values$ymin), n)
  ymax <- rep_len(scales$y$map(L$values$ymax), n)

  grad <- .grad_fill(L)
  if (!is.null(grad)) {
    o <- order(xn)
    poly <- .xy_area(scales, xn[o], ymin[o], xn[o], ymax[o])
    return(.draw(
      scene,
      vellum::polygon_grob(
        poly$x,
        poly$y,
        gp = vellum::gpar(fill = grad, col = NA)
      )
    ))
  }

  fill <- rep_len(.aes_colour(L, scales, "grey50"), n)
  alpha <- rep_len(.aes_param(L, "alpha", NA_real_), n)

  for (idx in .style_groups(n, list(fill = fill, alpha = alpha))) {
    o <- idx[order(xn[idx])]
    a <- alpha[idx[1]]
    poly <- .xy_area(scales, xn[o], ymin[o], xn[o], ymax[o])
    scene <- .draw(
      scene,
      vellum::polygon_grob(
        poly$x,
        poly$y,
        gp = vellum::gpar(
          fill = fill[idx[1]],
          col = NA,
          alpha = if (is.na(a)) NULL else a
        )
      )
    )
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
        gp = vellum::gpar(fill = grad, col = NA)
      )
    ))
  }

  fill <- rep_len(.aes_colour(L, scales, "grey50"), n)
  alpha <- rep_len(.aes_param(L, "alpha", NA_real_), n)

  for (idx in .style_groups(n, list(fill = fill, alpha = alpha))) {
    o <- idx[order(xn[idx])]
    a <- alpha[idx[1]]
    poly <- .xy_area(scales, xn[o], y1[o], xn[o], y0[o])
    scene <- .draw(
      scene,
      vellum::polygon_grob(
        poly$x,
        poly$y,
        gp = vellum::gpar(
          fill = fill[idx[1]],
          col = NA,
          alpha = if (is.na(a)) NULL else a
        )
      )
    )
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
  alpha <- rep_len(.aes_param(L, "alpha", NA_real_), n)
  lwd <- .aes_param(L, "linewidth", 1.5)
  dir <- L$stat_params$direction %||% "hv"

  for (idx in .style_groups(n, list(col = col, alpha = alpha))) {
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
        gp = vellum::gpar(
          col = col[idx[1]],
          lwd = lwd,
          alpha = if (is.na(a)) NULL else a
        )
      )
    )
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
  label <- rep_len(as.character(L$values$label), n)
  col <- rep_len(.text_colour(L, scales, "black"), n)
  alpha <- rep_len(.aes_param(L, "alpha", NA_real_), n)
  ang <- .text_angle(L, n)
  fs <- .aes_param(L, "size", 8)
  just <- c(.aes_param(L, "hjust", "centre"), .aes_param(L, "vjust", "centre"))

  for (idx in .style_groups(n, list(col = col, alpha = alpha))) {
    a <- alpha[idx[1]]
    xy <- .xy_units(scales, xn[idx], yn[idx])
    scene <- .draw(
      scene,
      vellum::text_grob(
        label[idx],
        xy$x,
        xy$y,
        just = just,
        rot = ang[idx],
        gp = vellum::gpar(
          fontsize = fs,
          col = col[idx[1]],
          fontfamily = .aes_param(L, "family", NULL),
          fontface = .aes_param(L, "fontface", NULL),
          alpha = if (is.na(a)) NULL else a
        )
      )
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
  bg <- L$params$fill %||% "white"
  fs <- .aes_param(L, "size", 8)
  pad <- vellum::unit(1.2, "mm")
  ws <- do.call(
    c,
    lapply(label, function(l) vellum::grobwidth(.txt(l, fs)) + pad)
  )
  hs <- do.call(
    c,
    lapply(label, function(l) vellum::grobheight(.txt(l, fs)) + pad)
  )

  for (idx in .style_groups(n, list(col = col))) {
    xy <- .xy_units(scales, xn[idx], yn[idx])
    scene <- .draw(
      scene,
      vellum::roundrect_grob(
        x = xy$x,
        y = xy$y,
        width = ws[idx],
        height = hs[idx],
        r = vellum::unit(0.8, "mm"),
        gp = vellum::gpar(fill = bg, col = NA)
      )
    )
    scene <- .draw(
      scene,
      vellum::text_grob(
        label[idx],
        xy$x,
        xy$y,
        gp = vellum::gpar(fontsize = fs, col = col[idx[1]])
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
  alpha <- rep_len(.aes_param(L, "alpha", NA_real_), n)
  w <- rep_len(L$values$width %||% .resolution(xp), n)
  h <- rep_len(L$values$height %||% .resolution(yp), n)

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
        gp = vellum::gpar(
          fill = fill[idx[1]],
          col = NA,
          alpha = if (is.na(a)) NULL else a
        )
      )
    )
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
      x = vellum::unit(mean(xn), "native"),
      y = vellum::unit(mean(yn), "native"),
      width = vellum::unit(diff(range(xn)), "native"),
      height = vellum::unit(diff(range(yn)), "native"),
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
    line_gp <- vellum::gpar(col = "grey20", lwd = 1)

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
        gp = vellum::gpar(fill = fillc, col = "grey20", lwd = 1)
      )
    )
    for (seg in list(
      c(xc - hw, my(med), xc + hw, my(med)), # median
      c(xc, my(q3), xc, my(hi)), # upper whisker
      c(xc, my(q1), xc, my(lo)) # lower whisker
    )) {
      s <- .seg_units(
        scales,
        vellum::unit(seg[1], "native"),
        vellum::unit(seg[2], "native"),
        vellum::unit(seg[3], "native"),
        vellum::unit(seg[4], "native")
      )
      scene <- .draw(
        scene,
        vellum::segments_grob(s$x0, s$y0, s$x1, s$y1, gp = line_gp)
      )
    }
    if (length(out)) {
      xy <- .xy_units(scales, rep(xc, length(out)), my(out))
      scene <- .draw(
        scene,
        vellum::points_grob(
          xy$x,
          xy$y,
          size = vellum::unit(1.2, "mm"),
          shape = "circle",
          gp = vellum::gpar(fill = "grey20", col = "grey20")
        )
      )
    }
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
      vellum::unit(x0, "native"),
      vellum::unit(y0, "native"),
      vellum::unit(x1, "native"),
      vellum::unit(y1, "native")
    )
    scene <- .draw(
      scene,
      vellum::segments_grob(
        s$x0,
        s$y0,
        s$x1,
        s$y1,
        gp = vellum::gpar(col = col[idx[1]], lwd = lwd)
      )
    )
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
  alpha <- rep_len(.aes_param(L, "alpha", NA_real_), n)
  lwd <- .aes_param(L, "linewidth", 1)

  for (idx in .style_groups(n, list(col = col, alpha = alpha))) {
    a <- alpha[idx[1]]
    s <- .seg_units(
      scales,
      vellum::unit(x0[idx], "native"),
      vellum::unit(y0[idx], "native"),
      vellum::unit(x1[idx], "native"),
      vellum::unit(y1[idx], "native")
    )
    scene <- .draw(
      scene,
      vellum::segments_grob(
        s$x0,
        s$y0,
        s$x1,
        s$y1,
        gp = vellum::gpar(
          col = col[idx[1]],
          lwd = lwd,
          alpha = if (is.na(a)) NULL else a
        )
      )
    )
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
  alpha <- rep_len(.aes_param(L, "alpha", NA_real_), n)
  lwd <- rep_len(.edge_width(L, scales, 0.5), n)
  arr <- if (isTRUE(L$stat_params$arrow)) {
    vellum::arrow(type = "closed", length = vellum::unit(2, "mm"))
  } else {
    NULL
  }

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
        vellum::unit(x0[g], "native"),
        vellum::unit(y0[g], "native"),
        vellum::unit(x1[g], "native"),
        vellum::unit(y1[g], "native")
      )
      # Node-boundary caps and parallel-edge spacing are both absolute (mm),
      # resolved by vellum in device space -> they track the mm node markers.
      start_cap <- if (!is.null(gh)) vellum::unit(gh$start_cap[g] + gap, "mm")
      end_cap <- if (!is.null(gh)) vellum::unit(gh$end_cap[g] + gap, "mm")
      offset <- if (!is.null(gh)) vellum::unit(gh$offset[g], "mm")
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
          gp = vellum::gpar(
            col = col[g[1]],
            lwd = lwd[g[1]],
            alpha = if (is.na(a)) NULL else a
          )
        )
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
          size = vellum::unit(size_mm, "mm"),
          foot = vellum::unit(node_r_mm, "mm"),
          angle = ang,
          width = narrow,
          arrow = arr,
          gp = vellum::gpar(
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
      width = vellum::unit(if (flip) h_full else w_full, "native"),
      height = vellum::unit(if (flip) w_full else h_full, "native"),
      fill = fill,
      orientation = "flat",
      gp = vellum::gpar(alpha = if (is.na(a)) NULL else a)
    )
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
# single grob: polygons -> one `path_grob` (rings as `evenodd` sub-paths, so
# holes cut and islands stay solid regardless of winding); lines -> one
# NA-separated `lines_grob`; points -> one `points_grob`.
.emit_sf <- function(scene, L, scales) {
  feats <- L$sf
  n <- L$n
  # primary colour = mapped fill/colour (choropleth) or a constant param; NA when
  # nothing is set (then filled per-kind below). Border/alpha/lwd/size are params.
  primary <- rep_len(.aes_colour(L, scales, NA_character_), n)
  alpha <- rep_len(.aes_param(L, "alpha", NA_real_), n)
  border <- .aes_param(L, "color", "grey40")
  lwd <- .aes_param(L, "linewidth", 0.5)
  size <- rep_len(.aes_size(L, scales, 1.5), n)

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

  # polygons: rings across the group become one path_grob (each ring a sub-path).
  scene <- by_style(
    scene,
    "poly",
    kind_col("grey80"),
    function(scene, idx, fill, a) {
      xs <- ys <- ids <- numeric(0)
      rid <- 0L
      for (i in idx) {
        for (p in prims_of(i, "poly")) {
          for (ring in p$parts) {
            if (!nrow(ring)) {
              next
            }
            rid <- rid + 1L
            nat <- mapnat(ring)
            xs <- c(xs, nat$x)
            ys <- c(ys, nat$y)
            ids <- c(ids, rep(rid, nrow(ring)))
          }
        }
      }
      if (!length(xs)) {
        return(scene)
      }
      xy <- .xy_units(scales, xs, ys)
      .draw(
        scene,
        vellum::path_grob(
          xy$x,
          xy$y,
          id = as.integer(ids),
          rule = "evenodd",
          gp = vellum::gpar(
            fill = fill,
            col = border,
            lwd = lwd,
            alpha = gp_alpha(a)
          )
        )
      )
    }
  )

  # lines: NA-separated sub-paths in one lines_grob.
  scene <- by_style(
    scene,
    "line",
    kind_col("grey20"),
    function(scene, idx, col, a) {
      xs <- ys <- numeric(0)
      for (i in idx) {
        for (p in prims_of(i, "line")) {
          for (seg in p$parts) {
            if (!nrow(seg)) {
              next
            }
            nat <- mapnat(seg)
            xs <- c(xs, nat$x, NA_real_)
            ys <- c(ys, nat$y, NA_real_)
          }
        }
      }
      if (!length(xs)) {
        return(scene)
      }
      xy <- .xy_units(scales, xs, ys)
      .draw(
        scene,
        vellum::lines_grob(
          xy$x,
          xy$y,
          gp = vellum::gpar(col = col, lwd = lwd, alpha = gp_alpha(a))
        )
      )
    }
  )

  # points: one points_grob per style group.
  scene <- by_style(
    scene,
    "point",
    kind_col("black"),
    function(scene, idx, col, a) {
      xs <- ys <- szs <- numeric(0)
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
          size = vellum::unit(szs, "mm"),
          gp = vellum::gpar(fill = col, col = col, alpha = gp_alpha(a))
        )
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
    cli::cli_abort("Unknown mark {.val {L$mark}}.")
  )
}

# Compile every layer's marks into the (already panel-positioned) scene. A layer
# with a non-normal blend mode is wrapped in its own viewport(blend=) so its
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

.draw <- function(scene, grob) {
  id <- .mark_ctx$id
  if (!is.null(id)) {
    grob@id <- id
  }
  vellum::draw(scene, grob)
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
# (`xoff`, `yoff`) in npc. Reuses the mark's own emitter so coords / flip / polar
# all stay correct. Widest first, so opacity accumulates toward centre.
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
    vellum::viewport(
      x = 0.5 + xoff,
      y = 0.5 + yoff,
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

.compile_marks <- function(scene, resolved, scales) {
  on.exit(.mark_ctx$id <- NULL, add = TRUE)
  for (i in seq_along(resolved)) {
    L <- resolved[[i]]
    if (!L$n) {
      next
    } # empty facet panel
    .mark_ctx$id <- sprintf("layer-%d-%s", i, L$mark)
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
        vellum::viewport(xscale = rng$x, yscale = rng$y, blend = blend)
      )
      scene <- .emit_layer(scene, L, scales)
      scene <- vellum::pop(scene)
    } else {
      scene <- .emit_layer(scene, L, scales)
    }
  }
  scene
}
