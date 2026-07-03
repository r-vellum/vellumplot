#' @include classes.R
NULL

# Infer a channel's variable type from its resolved values.
.infer_type <- function(v) {
  if (is.numeric(v)) "quantitative" else "nominal"
}

# Evaluate one layer's stage-1 (data) channel expressions against the data.
# `after_stat()` channels are deferred (kept as quosures in `after`) and
# evaluated later against the stat output. Returns the resolved values, types,
# the deferred after-channels, and the layer's stat/position config.
.resolve_layer <- function(layer, data) {
  values <- list()
  types <- list()
  after <- list()
  for (nm in names(layer@encoding)) {
    ch <- layer@encoding[[nm]]
    if (isTRUE(ch@after)) {
      after[[nm]] <- ch@expr
      next
    }
    v <- rlang::eval_tidy(ch@expr, data = data)
    values[[nm]] <- v
    types[[nm]] <- if (nzchar(ch@type)) ch@type else .infer_type(v)
  }
  n <- if (length(values)) max(lengths(values)) else nrow(data)

  # An sf layer draws from its geometry column, not from x/y encodings. Decompose
  # each feature's geometry into drawing primitives, and synthesise x/y values
  # from the bounding box so the existing position-training path derives the
  # panel domain (unioned across layers) from the projected extent. `n` stays the
  # feature count -- style vectors recycle over features, geometry reads `sf`.
  sf <- NULL
  if (identical(layer@mark, "sf")) {
    if (!.is_sf(data)) {
      cli::cli_abort("{.fn mark_sf} requires an {.cls sf} data frame.")
    }
    sfc <- data[[.sf_geom_col(data)]]
    sf <- lapply(sfc, .sf_decompose)
    bb <- .sf_bbox(sf)
    values$x <- c(bb[1], bb[2])
    values$y <- c(bb[3], bb[4])
    types$x <- types$y <- "quantitative"
    n <- length(sf)
  }

  # A datashade layer aggregates its (potentially hundreds of millions of) points
  # into a raster in one Rust pass. Keep the full coordinate vectors in `ds` for
  # the emitter, but hand scale training only their 2-value range via `values`:
  # this way the position trainer never scans -- let alone copies or concatenates
  # across layers -- the full point cloud (see `.train_position_continuous`).
  ds <- NULL
  if (identical(layer@mark, "datashade")) {
    ds <- list(x = values$x, y = values$y)
    if (!is.null(values$x)) {
      values$x <- suppressWarnings(range(as.numeric(values$x), finite = TRUE))
    }
    if (!is.null(values$y)) {
      values$y <- suppressWarnings(range(as.numeric(values$y), finite = TRUE))
    }
  }

  list(
    mark = layer@mark,
    values = values,
    types = types,
    after = after,
    params = layer@params,
    n = n,
    sf = sf,
    ds = ds,
    stat = layer@stat,
    stat_params = layer@stat_params,
    position = layer@position,
    blend = layer@blend,
    effects = layer@effects
  )
}

# Resolve every layer of a spec against a given data frame (a facet panel's
# subset, or the whole data): evaluate channels, apply the stat, then apply the
# pre-train part of the position adjustment (stack/fill). A layer with its own
# `data` resolves against that instead (per-panel subsetting is handled upstream
# in `.build_panels`; this path is the non-faceted / fallback case).
.resolve_on <- function(spec, data) {
  lapply(spec@layers, function(layer) {
    ld <- layer@data %||% data
    .apply_position(.apply_stat(.resolve_layer(layer, ld)))
  })
}

# Resolve every layer of a spec against its full data.
.resolve_layers <- function(spec) .resolve_on(spec, spec@data)

# The aesthetic name(s) that share one scale: `color` and `fill` are aliases for
# a single colour scale.
.aes_aliases <- function(aesthetic) {
  if (aesthetic %in% c("color", "fill")) c("color", "fill") else aesthetic
}

# The legend key glyph a mark draws with: a filled point, a short line, or a
# filled square swatch (the fallback for area/bar/tile/polygon-like marks).
.MARK_KEY_GLYPH <- c(
  point = "point",
  nodes = "point",
  line = "line",
  smooth = "line",
  step = "line",
  rule = "line",
  segment = "line",
  density = "line",
  linerange = "line",
  errorbar = "line",
  edges = "line"
)

.key_glyph_for_mark <- function(mark) {
  if (mark %in% names(.MARK_KEY_GLYPH)) .MARK_KEY_GLYPH[[mark]] else "square"
}

# The key glyph for a scale, from the marks of the layers that map `aesthetic`
# (colour aliases to fill). A point-drawing layer wins (the common scatter case),
# then a line-drawing layer, else the square swatch -- so a legend key matches
# what the plot actually draws (e.g. circles for a point layer, not squares).
.key_glyph_for_aes <- function(resolved, aesthetic) {
  aliases <- .aes_aliases(aesthetic)
  glyphs <- character(0)
  for (L in resolved) {
    if (any(aliases %in% names(L$values))) {
      glyphs <- c(glyphs, .key_glyph_for_mark(L$mark))
    }
  }
  if ("point" %in% glyphs) {
    "point"
  } else if ("line" %in% glyphs) {
    "line"
  } else {
    "square"
  }
}

# The user-declared scale for an aesthetic, or NULL. `color` and `fill` share a
# colour scale; either declaration applies.
.scale_for <- function(spec, aesthetic) {
  match_aes <- .aes_aliases(aesthetic)
  for (s in rev(spec@scales)) {
    if (s@aesthetic %in% match_aes) return(s)
  }
  NULL
}

# Category levels for a discrete aesthetic, preserving factor level order. `x`
# may be a single vector or a list of vectors pooled across layers: the first
# factor's levels win; otherwise sorted unique character values.
.cat_levels <- function(x) {
  vals <- if (is.list(x)) x else list(x)
  for (v in vals) {
    if (is.factor(v)) return(levels(v))
  }
  sort(unique(as.character(unlist(
    lapply(vals, as.character),
    use.names = FALSE
  ))))
}

# Does any resolved layer draw bars (forcing the y axis through zero, and a
# "count" default title when no y is mapped)?
.has_bar <- function(resolved) {
  any(vapply(resolved, function(L) identical(L$mark, "bar"), logical(1)))
}

# Marks that sit on a zero baseline, forcing the y axis through 0.
.needs_zero <- function(resolved) {
  any(vapply(
    resolved,
    function(L) L$mark %in% c("bar", "area"),
    logical(1)
  ))
}

# The default y-axis title: "count" when bars count rows (no y encoding on any
# layer), otherwise the first y encoding's label.
.y_axis_title <- function(spec, resolved) {
  if (!is.null(spec@labels[["y"]])) {
    return(spec@labels[["y"]])
  }
  y_mapped <- any(vapply(
    spec@layers,
    function(L) "y" %in% names(L@encoding),
    logical(1)
  ))
  if (.has_bar(resolved) && !y_mapped) "count" else .default_title(spec, "y")
}

# Pool the resolved values of an aesthetic across all layers that map it. For
# position channels this is x or y; returns NULL if no layer maps it.
.pool_values <- function(resolved, aesthetic) {
  vs <- lapply(resolved, function(L) L$values[[aesthetic]])
  vs <- vs[!vapply(vs, is.null, logical(1))]
  if (!length(vs)) {
    return(NULL)
  }
  vs
}

# Derive a default axis/legend title for an aesthetic from the first layer that
# maps it (its channel expression as text).
.default_title <- function(spec, aesthetic) {
  match_aes <- .aes_aliases(aesthetic)
  for (a in match_aes) {
    lab <- spec@labels[[a]]
    if (!is.null(lab)) {
      return(lab)
    }
  }
  for (layer in spec@layers) {
    for (a in match_aes) {
      ch <- layer@encoding[[a]]
      if (!is.null(ch)) return(rlang::as_label(rlang::quo_get_expr(ch@expr)))
    }
  }
  aesthetic
}
