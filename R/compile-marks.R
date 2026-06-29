#' @include classes.R
NULL

# Group row indices by the tuple of gpar-borne style fields, mirroring
# `vellum:::.gv_groups`: a batched grob carries a single gpar, so rows that must
# differ in fill/col/alpha/lwd have to be emitted as separate grobs. Geometry-
# borne aesthetics (size, shape, x, y) are vectorised and do NOT force a split.
# Continuous colour is already quantized upstream, so the group count is bounded.
.style_groups <- function(n, fields) {
  fields <- fields[!vapply(fields, is.null, logical(1))]
  if (!length(fields)) {
    return(list(seq_len(n)))
  }
  codes <- lapply(fields, function(v) {
    v <- rep_len(v, n)
    match(v, unique(v))
  })
  unname(split(seq_len(n), do.call(paste, c(codes, sep = "\036"))))
}

# Resolve a layer's mark colour: a mapped colour channel (via the trained colour
# scale), a constant colour param, or the supplied default.
.aes_colour <- function(L, scales, default) {
  if (!is.null(scales$color)) {
    if (!is.null(L$values$color)) {
      return(scales$color$map(L$values$color))
    }
    if (!is.null(L$values$fill)) return(scales$color$map(L$values$fill))
  }
  L$params$color %||% L$params$fill %||% default
}

.aes_param <- function(L, name, default) L$params[[name]] %||% default

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

.xy_units <- function(scales, x, y) {
  if (.flipped(scales)) {
    list(x = vellum::unit(y, "native"), y = vellum::unit(x, "native"))
  } else {
    list(x = vellum::unit(x, "native"), y = vellum::unit(y, "native"))
  }
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
    scene <- vellum::draw(
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
    xy <- .xy_units(scales, xn[o], yn[o])
    scene <- vellum::draw(
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
    for (v in scales$y$map(yi)) {
      s <- .seg_units(
        scales,
        vellum::unit(0, "npc"),
        vellum::unit(v, "native"),
        vellum::unit(1, "npc"),
        vellum::unit(v, "native")
      )
      scene <- vellum::draw(
        scene,
        vellum::segments_grob(s$x0, s$y0, s$x1, s$y1, gp = gp)
      )
    }
  }
  if (!is.null(xi)) {
    for (v in scales$x$map(xi)) {
      s <- .seg_units(
        scales,
        vellum::unit(v, "native"),
        vellum::unit(0, "npc"),
        vellum::unit(v, "native"),
        vellum::unit(1, "npc")
      )
      scene <- vellum::draw(
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

.emit_bar <- function(scene, L, scales) {
  n <- L$n
  xp <- rep_len(scales$x$map(L$values$x), n)
  fill <- rep_len(.aes_colour(L, scales, "grey35"), n)
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

  for (idx in .style_groups(n, list(fill = fill, alpha = alpha))) {
    a <- alpha[idx[1]]
    r <- .rect_units(
      scales,
      xc[idx],
      (y0[idx] + y1[idx]) / 2,
      w[idx],
      abs(y1[idx] - y0[idx])
    )
    scene <- vellum::draw(
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
      px <- c(xn[o], rev(xn[o]))
      py <- c(ymin[o], rev(ymax[o]))
      poly <- .xy_units(scales, px, py)
      scene <- vellum::draw(
        scene,
        vellum::polygon_grob(
          poly$x,
          poly$y,
          gp = vellum::gpar(fill = cc, col = NA, alpha = 0.25)
        )
      )
    }
    ln <- .xy_units(scales, xn[o], yn[o])
    scene <- vellum::draw(
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
  fill <- rep_len(.aes_colour(L, scales, "grey50"), n)
  alpha <- rep_len(.aes_param(L, "alpha", NA_real_), n)

  for (idx in .style_groups(n, list(fill = fill, alpha = alpha))) {
    o <- idx[order(xn[idx])]
    a <- alpha[idx[1]]
    poly <- .xy_units(scales, c(xn[o], rev(xn[o])), c(ymin[o], rev(ymax[o])))
    scene <- vellum::draw(
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
  fill <- rep_len(.aes_colour(L, scales, "grey50"), n)
  alpha <- rep_len(.aes_param(L, "alpha", NA_real_), n)

  for (idx in .style_groups(n, list(fill = fill, alpha = alpha))) {
    o <- idx[order(xn[idx])]
    a <- alpha[idx[1]]
    poly <- .xy_units(scales, c(xn[o], rev(xn[o])), c(y1[o], rev(y0[o])))
    scene <- vellum::draw(
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
    ln <- .xy_units(scales, ex, ey)
    scene <- vellum::draw(
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

# Text colour for text/label marks: a mapped colour channel, a constant param,
# or the default (without the fill fallback `.aes_colour` uses, since `fill` is
# the label background here).
.text_colour <- function(L, scales, default) {
  if (!is.null(scales$color) && !is.null(L$values$color)) {
    return(scales$color$map(L$values$color))
  }
  L$params$color %||% default
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
    scene <- vellum::draw(
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
    scene <- vellum::draw(
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
    scene <- vellum::draw(
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

# Datashade: aggregate the points into a density raster filling the panel. The
# raster is binned over the panel's native domain so it aligns with the axes.
.emit_datashade <- function(scene, L, scales) {
  xn <- scales$x$map(L$values$x)
  yn <- scales$y$map(L$values$y)
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
  vellum::draw(scene, g)
}

.emit_layer <- function(scene, L, scales) {
  switch(
    L$mark,
    point = .emit_point(scene, L, scales),
    line = .emit_line(scene, L, scales),
    rule = .emit_rule(scene, L, scales),
    bar = .emit_bar(scene, L, scales),
    smooth = .emit_smooth(scene, L, scales),
    area = .emit_area(scene, L, scales),
    ribbon = .emit_ribbon(scene, L, scales),
    step = .emit_step(scene, L, scales),
    text = .emit_text(scene, L, scales),
    label = .emit_label(scene, L, scales),
    datashade = .emit_datashade(scene, L, scales),
    cli::cli_abort("Unknown mark {.val {L$mark}}.")
  )
}

# Compile every layer's marks into the (already panel-positioned) scene. A layer
# with a non-normal blend mode is wrapped in its own viewport(blend=) so its
# whole content composites as one isolated group against the backdrop (the panel
# and earlier layers); the wrapper carries the panel's scales so native
# coordinates still resolve.
.compile_marks <- function(scene, resolved, scales) {
  for (L in resolved) {
    if (!L$n) {
      next
    } # empty facet panel
    blend <- L$blend %||% "normal"
    if (!identical(blend, "normal")) {
      scene <- vellum::push(
        scene,
        vellum::viewport(
          xscale = scales$x$domain,
          yscale = scales$y$domain,
          blend = blend
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
