#' @include classes.R
NULL

# Group row indices by the tuple of gpar-borne style fields, mirroring
# `vellum:::.gv_groups`: a batched grob carries a single gpar, so rows that must
# differ in fill/col/alpha/lwd have to be emitted as separate grobs. Geometry-
# borne aesthetics (size, shape, x, y) are vectorised and do NOT force a split.
# Continuous colour is already quantized upstream, so the group count is bounded.
.style_groups <- function(n, fields) {
  fields <- fields[!vapply(fields, is.null, logical(1))]
  if (!length(fields)) return(list(seq_len(n)))
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
    if (!is.null(L$values$color)) return(scales$color$map(L$values$color))
    if (!is.null(L$values$fill)) return(scales$color$map(L$values$fill))
  }
  L$params$color %||% L$params$fill %||% default
}

.aes_param <- function(L, name, default) L$params[[name]] %||% default

# Resolve a layer's point size (mm): a mapped size channel (via the trained size
# scale), a constant size param, or the supplied default.
.aes_size <- function(L, scales, default) {
  if (!is.null(scales$size) && !is.null(L$values$size)) return(scales$size$map(L$values$size))
  L$params$size %||% default
}

# An intercept may arrive as a mapped channel or a constant param.
.intercept <- function(L, name) L$values[[name]] %||% L$params[[name]]

# --- per-mark emitters (draw into the panel viewport, native units) ---------

# Above this row count, `mark_point(auto = TRUE)` switches to datashading.
.DATASHADE_AUTO <- 50000L

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
    xn <- xn + stats::runif(n, -ax, ax)
    yn <- yn + stats::runif(n, -ay, ay)
  }
  col <- rep_len(.aes_colour(L, scales, "black"), n)
  size <- rep_len(.aes_size(L, scales, 2), n)
  shape <- rep_len(.aes_param(L, "shape", "circle"), n)
  alpha <- rep_len(.aes_param(L, "alpha", NA_real_), n)

  for (idx in .style_groups(n, list(col = col, alpha = alpha))) {
    a <- alpha[idx[1]]
    scene <- vellum::draw(scene, vellum::points_grob(
      vellum::unit(xn[idx], "native"), vellum::unit(yn[idx], "native"),
      size = vellum::unit(size[idx], "mm"), shape = shape[idx],
      gp = vellum::gpar(fill = col[idx[1]], col = col[idx[1]],
                        alpha = if (is.na(a)) NULL else a)
    ))
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
    scene <- vellum::draw(scene, vellum::lines_grob(
      vellum::unit(xn[o], "native"), vellum::unit(yn[o], "native"),
      gp = vellum::gpar(col = col[idx[1]], lwd = lwd,
                        alpha = if (is.na(a)) NULL else a)
    ))
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
      scene <- vellum::draw(scene, vellum::segments_grob(
        vellum::unit(0, "npc"), vellum::unit(v, "native"),
        vellum::unit(1, "npc"), vellum::unit(v, "native"), gp = gp))
    }
  }
  if (!is.null(xi)) {
    for (v in scales$x$map(xi)) {
      scene <- vellum::draw(scene, vellum::segments_grob(
        vellum::unit(v, "native"), vellum::unit(0, "npc"),
        vellum::unit(v, "native"), vellum::unit(1, "npc"), gp = gp))
    }
  }
  scene
}

# Smallest positive gap between sorted unique positions (the bar width unit on a
# continuous x); 1 if there is only one bar.
.resolution <- function(x) {
  u <- sort(unique(x[is.finite(x)]))
  if (length(u) < 2) return(1)
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

  w <- 0.9 * band
  xc <- xp
  if (identical(L$position, "dodge")) {
    grp <- L$values$color %||% L$values$fill
    if (!is.null(grp)) {
      levs <- sort(unique(as.character(grp)))
      G <- length(levs)
      rank <- match(as.character(rep_len(grp, n)), levs)
      w <- 0.9 * band / G
      xc <- xp + (rank - (G + 1) / 2) / G * band
    }
  }

  for (idx in .style_groups(n, list(fill = fill, alpha = alpha))) {
    a <- alpha[idx[1]]
    scene <- vellum::draw(scene, vellum::rect_grob(
      x = vellum::unit(xc[idx], "native"), y = vellum::unit((y0[idx] + y1[idx]) / 2, "native"),
      width = vellum::unit(rep(w, length(idx)), "native"),
      height = vellum::unit(abs(y1[idx] - y0[idx]), "native"),
      gp = vellum::gpar(fill = fill[idx[1]], col = NA,
                        alpha = if (is.na(a)) NULL else a)
    ))
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
      scene <- vellum::draw(scene, vellum::polygon_grob(
        vellum::unit(px, "native"), vellum::unit(py, "native"),
        gp = vellum::gpar(fill = cc, col = NA, alpha = 0.25)))
    }
    scene <- vellum::draw(scene, vellum::lines_grob(
      vellum::unit(xn[o], "native"), vellum::unit(yn[o], "native"),
      gp = vellum::gpar(col = cc, lwd = 1.5)))
  }
  scene
}

# Datashade: aggregate the points into a density raster filling the panel. The
# raster is binned over the panel's native domain so it aligns with the axes.
.emit_datashade <- function(scene, L, scales) {
  xn <- scales$x$map(L$values$x)
  yn <- scales$y$map(L$values$y)
  sp <- L$stat_params
  g <- vellum::datashade(
    xn, yn,
    width = as.integer(sp$width %||% 400L), height = as.integer(sp$height %||% 300L),
    xlim = scales$x$domain, ylim = scales$y$domain,
    colors = sp$colors %||% c("#deebf7", "#08306b"),
    how = sp$how %||% "eq_hist", interpolate = FALSE)
  vellum::draw(scene, g)
}

.emit_layer <- function(scene, L, scales) {
  switch(L$mark,
    point = .emit_point(scene, L, scales),
    line = .emit_line(scene, L, scales),
    rule = .emit_rule(scene, L, scales),
    bar = .emit_bar(scene, L, scales),
    smooth = .emit_smooth(scene, L, scales),
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
    if (!L$n) next # empty facet panel
    blend <- L$blend %||% "normal"
    if (!identical(blend, "normal")) {
      scene <- vellum::push(scene, vellum::viewport(
        xscale = scales$x$domain, yscale = scales$y$domain, blend = blend))
      scene <- .emit_layer(scene, L, scales)
      scene <- vellum::pop(scene)
    } else {
      scene <- .emit_layer(scene, L, scales)
    }
  }
  scene
}
