#' @include classes.R
NULL

# Mark emitters: shared aesthetic helpers and coordinate transforms.

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

# Resolve a per-row *fill* colour, preferring the `fill` channel over `color`
# (the mirror of `.aes_colour`, which prefers `color`). For a polygon the fill
# and stroke are distinct channels: a constant `color` sets the border and must
# never leak into the fill. Mapped `fill` wins, then mapped `color`; as a
# constant, `fill` (a plain colour, not a paint) then `default` -- never `color`.
.aes_fill_colour <- function(L, scales, default) {
  if (!is.null(scales$color)) {
    if (!is.null(L$values$fill)) {
      return(scales$color$map(L$values$fill))
    }
    if (!is.null(L$values$color)) {
      return(scales$color$map(L$values$color))
    }
  }
  fillp <- if (.is_paint(L$params$fill)) NULL else L$params$fill
  fillp %||% default
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
  neg <- !is.na(v) & v == -Inf
  pos <- !is.na(v) & v == Inf
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

# vellum's built-in point-marker shapes; anything else a `shape` resolves to is
# taken to be an SVG icon (a path `d` string, or a `.svg` file).
.BUILTIN_SHAPES <- c(
  "circle",
  "square",
  "triangle",
  "diamond",
  "plus",
  "cross",
  "triangle_down",
  "star"
)

# For each resolved shape, the SVG path `d` to draw it as a vector icon, or NA
# for a built-in marker. A `.svg` file is read and its `<path d=...>` attributes
# concatenated (icon sets ship exactly these paths); any other non-builtin
# string is taken to be a `d` already. `svg_grob()` (vellum) parses the `d`;
# vellumplot only does the four lines of xml2 for the file case.
.shape_svg_d <- function(shape) {
  vapply(
    as.character(shape),
    function(s) {
      if (is.na(s) || s %in% .BUILTIN_SHAPES) {
        return(NA_character_)
      }
      if (grepl("\\.svg$", s, ignore.case = TRUE) && file.exists(s)) {
        .need_pkg("xml2", "SVG-file markers (shape = a .svg path)")
        doc <- xml2::read_xml(s)
        ds <- xml2::xml_attr(
          xml2::xml_find_all(doc, ".//*[local-name()='path']"),
          "d"
        )
        ds <- ds[!is.na(ds)]
        if (!length(ds)) {
          cli::cli_abort(
            "SVG file {.file {s}} has no {.field <path d=...>} to draw."
          )
        }
        return(paste(ds, collapse = " "))
      }
      s # already a path `d`
    },
    character(1),
    USE.NAMES = FALSE
  )
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
    return(list(x = x, y = y, at = seq_along(x)))
  }
  xw <- ctx$x_map(x)
  yw <- ctx$y_map(y)
  xr <- diff(range(xw[is.finite(xw)]))
  yr <- diff(range(yw[is.finite(yw)]))
  xr <- if (isTRUE(xr > 0)) xr else 1
  yr <- if (isTRUE(yr > 0)) yr else 1
  ox <- vector("list", m - 1L)
  oy <- vector("list", m - 1L)
  oa <- vector("list", m - 1L)
  for (i in seq_len(m - 1L)) {
    frac <- max(abs(xw[i + 1L] - xw[i]) / xr, abs(yw[i + 1L] - yw[i]) / yr)
    steps <- max(1L, min(cap, ceiling(frac * k)))
    t <- seq(0, 1, length.out = steps + 1L)[-(steps + 1L)]
    ox[[i]] <- x[i] + t * (x[i + 1L] - x[i])
    oy[[i]] <- y[i] + t * (y[i + 1L] - y[i])
    oa[[i]] <- i + t # see `.polar_munch`
  }
  list(x = c(unlist(ox), x[m]), y = c(unlist(oy), y[m]), at = c(unlist(oa), m))
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
    return(list(x = x, y = y, at = seq_along(x)))
  }
  tsrc <- if (identical(ctx$theta_aes, "x")) x else y
  ang <- ctx$theta_map(tsrc)
  ox <- vector("list", m - 1L)
  oy <- vector("list", m - 1L)
  oa <- vector("list", m - 1L)
  for (i in seq_len(m - 1L)) {
    steps <- max(1L, ceiling(abs(ang[i + 1L] - ang[i]) / max_step))
    t <- seq(0, 1, length.out = steps + 1L)[-(steps + 1L)]
    ox[[i]] <- x[i] + t * (x[i + 1L] - x[i])
    oy[[i]] <- y[i] + t * (y[i + 1L] - y[i])
    # `at` is each output point's position in ORIGINAL vertex-index space, so a
    # caller carrying a per-vertex quantity (a width profile) can resample it on
    # the identical parameterisation rather than guessing the expansion.
    oa[[i]] <- i + t
  }
  list(x = c(unlist(ox), x[m]), y = c(unlist(oy), y[m]), at = c(unlist(oa), m))
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

# `.xy_path()`, but carrying a per-vertex weight through the same densification.
#
# Under polar/`coord_trans` a polyline is munched into MORE vertices than it went
# in with, so a per-vertex width profile would desync from the coordinates. Both
# munchers report `at`, each output vertex's position in original vertex-index
# space, so the weight is resampled on exactly the parameterisation the geometry
# used -- linear, matching the munchers' own linear interpolation.
.xy_path_w <- function(scales, x, y, w) {
  if (length(x) < 2L || (is.null(scales$polar) && is.null(scales$trans))) {
    xy <- .xy_path(scales, x, y)
    return(list(x = xy$x, y = xy$y, w = w))
  }
  d <- if (!is.null(scales$polar)) {
    .polar_munch(scales, x, y)
  } else {
    .trans_munch(scales, x, y)
  }
  wm <- stats::approx(seq_along(w), w, xout = d$at, rule = 2)$y
  xy <- .xy_units(scales, d$x, d$y)
  list(x = xy$x, y = xy$y, w = wm)
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

# Resolve a per-row stroke width from a mapped `linewidth` channel, or NULL when
# the layer sets none (the caller then uses its constant `linewidth` param, and
# the cheaper single-`lwd` grob that goes with it). `.lwd_add` is the widening an
# effect copy asked for (`.emit_copies()`), which for a constant width lands in
# `params$linewidth` instead -- a halo must widen the mapped widths too.
# `.lwd_add` is internal: a dot-prefixed `params` entry is never a user-facing
# mark argument, only a channel between an effect copy and this resolver.
.mapped_linewidth <- function(L, scales, n) {
  if (is.null(scales$linewidth) || is.null(L$values$linewidth)) {
    return(NULL)
  }
  w <- as.numeric(scales$linewidth$map(L$values$linewidth))
  rep_len(w, n) + (L$params$.lwd_add %||% 0)
}

# Split an ordered polyline into the segments a per-segment width draws it as:
# each segment carries one width, the mean of its two endpoint values, so the
# width steps at every vertex (see `scale_linewidth()`). Under polar/trans the
# segment is munched exactly as `.xy_path()` would munch the whole polyline, and
# every piece of it inherits that segment's width, so a warped panel bends the
# same way it does for a constant-width line. Returns grob units plus `row`, the
# index (into `x`/`y`) of each drawn piece's originating vertex, for provenance.
.seg_widths_path <- function(scales, x, y, w) {
  m <- length(x)
  if (m < 2L) {
    return(NULL)
  }
  # The mean of the two endpoint widths -- but a missing width must not poison
  # the mean, or one missing value would erase BOTH segments meeting at that
  # vertex (a non-finite `lwd` reaches the backend as width 0, i.e. invisible).
  # Fall back to whichever endpoint is known, so a missing value costs that one
  # datum's contribution instead and the line stays continuous; only a segment
  # missing a width at *both* ends is dropped. See `scale_linewidth()`.
  lwd <- (w[-m] + w[-1L]) / 2
  if (anyNA(lwd)) {
    lwd <- rowMeans(cbind(w[-m], w[-1L]), na.rm = TRUE)
    lwd[!is.finite(lwd)] <- NA_real_
  }
  row <- seq_len(m - 1L)
  if (is.null(scales$polar) && is.null(scales$trans)) {
    xa <- x[-m]
    ya <- y[-m]
    xb <- x[-1L]
    yb <- y[-1L]
  } else {
    parts <- lapply(row, function(i) {
      j <- c(i, i + 1L)
      d <- if (!is.null(scales$polar)) {
        .polar_munch(scales, x[j], y[j])
      } else {
        .trans_munch(scales, x[j], y[j])
      }
      k <- length(d$x)
      list(
        xa = d$x[-k],
        ya = d$y[-k],
        xb = d$x[-1L],
        yb = d$y[-1L],
        lwd = rep(lwd[i], k - 1L),
        row = rep(i, k - 1L)
      )
    })
    fld <- function(nm) unlist(lapply(parts, `[[`, nm), use.names = FALSE)
    xa <- fld("xa")
    ya <- fld("ya")
    xb <- fld("xb")
    yb <- fld("yb")
    lwd <- fld("lwd")
    row <- fld("row")
  }
  # A segment with no width at either end carries no information: drop it rather
  # than hand the backend a width-0 element that silently renders as nothing.
  if (anyNA(lwd)) {
    keep <- !is.na(lwd)
    if (!any(keep)) {
      return(NULL)
    }
    xa <- xa[keep]
    ya <- ya[keep]
    xb <- xb[keep]
    yb <- yb[keep]
    lwd <- lwd[keep]
    row <- row[keep]
  }
  a <- .xy_units(scales, xa, ya)
  b <- .xy_units(scales, xb, yb)
  list(x0 = a$x, y0 = a$y, x1 = b$x, y1 = b$y, lwd = lwd, row = row)
}

# Resolve an edge's width: a mapped linewidth channel via the trained edge-width
# scale, a constant linewidth param, or the default.
.edge_width <- function(L, scales, default) {
  if (!is.null(scales$edge_width) && !is.null(L$values$linewidth)) {
    return(scales$edge_width$map(L$values$linewidth))
  }
  L$params$linewidth %||% default
}
