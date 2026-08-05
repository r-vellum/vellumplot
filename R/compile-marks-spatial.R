#' @include classes.R
NULL

# Mark emitters: spatial marks (tile, raster, image, hex, datashade, sf).

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

  .emit_rect_groups(
    scene,
    L,
    scales,
    xp,
    yp,
    w,
    h,
    fill,
    alpha,
    sk,
    paint = paint
  )
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

  .emit_rect_groups(
    scene,
    L,
    scales,
    xc,
    yc,
    w,
    h,
    fill,
    alpha,
    sk,
    paint = paint
  )
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
  # One batched hexagon_grob carries a single scalar alpha, so a mapped alpha can
  # only apply as a representative (its first value), but honour the trained alpha
  # scale rather than reading the constant param alone (which ignored it entirely).
  a <- .aes_alpha(L, scales, NA_real_)[1]
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
  # Line/point colour = mapped fill/colour (choropleth) or a constant param; NA
  # when nothing is set (then filled per-kind below). A polygon's fill is a
  # *distinct* channel resolved separately (`poly_fill`) so a constant `color`
  # (which sets the polygon border) never leaks into the fill. Border/alpha/lwd/
  # size are params.
  primary <- rep_len(.aes_colour(L, scales, NA_character_), n)
  poly_fill <- rep_len(.aes_fill_colour(L, scales, NA_character_), n)
  alpha <- rep_len(.aes_alpha(L, scales, NA_real_), n)
  border <- .aes_param(L, "color", "grey40")
  lwd <- .aes_param(L, "linewidth", 0.5)
  size <- rep_len(.aes_size(L, scales, 1.5), n)
  sk <- .mark_sketch(L, scales)

  # per-kind colour vector, coalescing a resolved colour with a kind default.
  kind_col <- function(vec, default) {
    vec[is.na(vec)] <- default
    vec
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

  # Dissolve a style group's polygon features into ONE region: union each
  # feature's rings (interpreted even-odd, so holes and multipart features stay
  # correct) with `vl_path_op`, so the shared borders between same-valued
  # features vanish and the fill is a single crisp path (exact in PDF). Raw
  # coordinates are unioned, then the result is mapped once -- the same map-once
  # discipline as `gather_poly`. Returns a mapped `path_grob`, or NULL when there
  # is nothing to merge (< 2 non-empty features) so the caller falls back to the
  # ordinary batched emit.
  merge_poly <- function(idx, gpp) {
    operand <- function(i) {
      fp <- gather_poly(i)
      if (is.null(fp)) {
        return(NULL)
      }
      list(x = fp$x, y = fp$y, nper = rle(fp$id)$lengths)
    }
    ops <- Filter(Negate(is.null), lapply(idx, operand))
    if (length(ops) < 2L) {
      return(NULL)
    }
    acc <- ops[[1L]]
    for (j in seq_along(ops)[-1L]) {
      acc <- vellum::vl_path_op(acc, ops[[j]], op = "union", rule = "evenodd")
    }
    rx <- as.numeric(vctrs::field(acc@x, "value"))
    ry <- as.numeric(vctrs::field(acc@y, "value"))
    if (!length(rx)) {
      return(NULL)
    }
    nper <- acc@nper %||% length(rx)
    xy <- .xy_units(scales, scales$x$map(rx), scales$y$map(ry))
    vellum::path_grob(
      xy$x,
      xy$y,
      id = rep(seq_along(nper), nper),
      rule = "winding",
      sketch = sk,
      gp = gpp
    )
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
  # tooltip. See the EXCEPTION / CAVEAT notes on `.emit_sf`. With `merge = TRUE`
  # (and non-interactive) the group's features are instead dissolved into one
  # unioned region so their shared borders disappear.
  do_merge <- isTRUE(L$stat_params$merge) && !interactive
  scene <- by_style(
    scene,
    "poly",
    kind_col(poly_fill, "grey80"),
    function(scene, idx, fill, a) {
      gpp <- vellum::vl_gpar(
        fill = .paint_or(L, fill),
        col = border,
        lwd = lwd,
        alpha = gp_alpha(a)
      )
      merged <- if (do_merge) merge_poly(idx, gpp) else NULL
      if (!is.null(merged)) {
        return(.draw(scene, merged, rows = NULL))
      }
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
    kind_col(primary, "grey20"),
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
    kind_col(primary, "black"),
    function(scene, idx, col, a) {
      # Accumulate into lists and combine once (a nested-loop `c()` grow is
      # O(k^2)); mirrors gather_poly/gather_line. `rows` is the feature index per
      # emitted point (for per-point keys).
      xa <- ya <- sa <- ra <- list()
      k <- 0L
      for (i in idx) {
        for (p in prims_of(i, "point")) {
          for (m in p$parts) {
            if (!nrow(m)) {
              next
            }
            nat <- mapnat(m)
            k <- k + 1L
            xa[[k]] <- nat$x
            ya[[k]] <- nat$y
            sa[[k]] <- rep.int(size[i], nrow(m))
            ra[[k]] <- rep.int(i, nrow(m))
          }
        }
      }
      if (!k) {
        return(scene)
      }
      xs <- unlist(xa, use.names = FALSE)
      ys <- unlist(ya, use.names = FALSE)
      szs <- unlist(sa, use.names = FALSE)
      rows <- unlist(ra, use.names = FALSE)
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
