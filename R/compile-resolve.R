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
  conditions <- list()
  for (nm in names(layer@encoding)) {
    ch <- layer@encoding[[nm]]
    if (isTRUE(ch@after)) {
      after[[nm]] <- ch@expr
      next
    }
    # An ordinary channel evaluates its `expr`; a `condition()` channel's `expr`
    # already *is* the `if_true` branch (set at split), so this resolves + trains
    # transparently. The condition's selection + `if_false` (a constant, or a
    # per-row column) are then recorded for the interactive host.
    v <- rlang::eval_tidy(ch@expr, data = data)
    values[[nm]] <- v
    types[[nm]] <- if (nzchar(ch@type)) ch@type else .infer_type(v)
    if (!is.null(ch@condition)) {
      cnd <- ch@condition
      if_false <- if (!is.null(cnd$if_false)) {
        rlang::eval_tidy(cnd$if_false, data = data)
      } else {
        NULL
      }
      conditions[[nm]] <- list(
        selection = cnd$selection,
        if_false = if_false,
        empty = cnd$empty
      )
    }
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

  # A sankey layer draws a flow diagram, not x/y points: compute the layout from
  # the `from`/`to`/`value` channels into native [0, 1] coordinates (see
  # `.sankey_layout`), then synthesise the panel extent so position training gets
  # a domain (mirrors the sf path). Geometry is read from `sankey` by the emitter.
  sankey <- NULL
  if (identical(layer@mark, "sankey")) {
    if (is.null(values$from) || is.null(values$to) || is.null(values$value)) {
      cli::cli_abort(
        "{.fn vsankey} needs {.arg from}, {.arg to}, and {.arg value}."
      )
    }
    sankey <- .sankey_layout(
      values$from,
      values$to,
      values$value,
      node_width = layer@params$node_width %||% .SANKEY_NODE_WIDTH,
      node_gap = layer@params$node_gap %||% .SANKEY_NODE_GAP,
      flow_color = layer@params$flow_color %||% "source"
    )
    # Nodes/ribbons live in [0, 1]; when node labels are drawn, widen the x
    # domain so the outer-column labels (left of the source column, right of the
    # terminal column) fall inside the panel instead of clipping at its edge.
    xmargin <- if (isTRUE(layer@params$label)) .SANKEY_LABEL_MARGIN else 0
    values$x <- c(-xmargin, 1 + xmargin)
    values$y <- c(0, 1)
    types$x <- types$y <- "quantitative"
    n <- nrow(sankey$nodes)
  }

  # A hierarchy layer (sunburst / icicle / treemap / circlepack) draws a
  # space-filling tree: compute the layout from the `id`/`parent`/`value`
  # channels (see `.hierarchy_layout`) and synthesise the centred [-1, 1] extent
  # so the aspect-locked square panel holds it.
  hierarchy <- NULL
  hier_fill_mode <- NULL
  if (identical(layer@mark, "hierarchy")) {
    if (is.null(values$id) || is.null(values$parent) || is.null(values$value)) {
      cli::cli_abort(
        "{.fn vhierarchy} needs {.arg id}, {.arg parent}, and {.arg value}."
      )
    }
    hierarchy <- .hierarchy_layout(
      values$id,
      values$parent,
      values$value,
      type = layer@params$type %||% "sunburst",
      inner_radius = layer@params$inner_radius %||% 0,
      flow = layer@params$flow %||% "down"
    )
    # Fill is a normal discrete/continuous scale. Unmapped, colour by the depth-1
    # branch (a factor whose levels are the branches in input order, so the
    # default palette matches the historical look) and let the emitter lighten by
    # depth. Mapped, realign the node column to the layout's (depth-sorted) rows
    # and train it as given; the emitter uses it verbatim (no depth fade).
    if (is.null(values$fill)) {
      values$fill <- factor(
        hierarchy$branch,
        levels = attr(hierarchy, "branch_levels")
      )
      types$fill <- "nominal"
      hier_fill_mode <- "branch"
    } else {
      values$fill <- values$fill[hierarchy$.node]
      hier_fill_mode <- "mapped"
    }
    values$x <- c(-1, 1)
    values$y <- c(-1, 1)
    types$x <- types$y <- "quantitative"
    n <- nrow(hierarchy)
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
    # Categorical (count_cat) shading: a mapped colour/fill is the aggregation
    # category. Keep the full-length vector in `ds$cat` for the emitter, but hand
    # colour training only its unique levels -- the discrete colour scale (and its
    # legend) trains from the levels, never the full cloud.
    cat <- values$color %||% values$fill
    if (!is.null(cat)) {
      ds$cat <- cat
      values$color <- unique(cat)
      values$fill <- NULL
    }
  }

  # Graph identity (inert): lets an interactive host relate nodes to edges for
  # neighbour highlighting. Node marks key by vertex `name`; edge marks carry
  # their two endpoint node `name`s as source/target plus a stable per-edge key
  # (endpoint pair + per-edge row index, joined with a unit separator so
  # parallel/reciprocal edges stay distinct and adjacent fields cannot run
  # together). Pure data -- NOT keyed here (untouched by `.resolve_interactivity`);
  # the emitter attaches it only when the plot is interactive, so the static
  # render stays byte-identical.
  graph_identity <- NULL
  if (
    layer@mark %in% c("nodes", "node_text", "node_pie") && !is.null(data$name)
  ) {
    graph_identity <- list(kind = "nodes", key = as.character(data$name))
  } else if (identical(layer@mark, "edges") && !is.null(data$from)) {
    frm <- as.character(data$from)
    to <- as.character(data$to)
    graph_identity <- list(
      kind = "edges",
      key = paste(frm, to, seq_along(frm), sep = ""),
      source = frm,
      target = to
    )
  }

  list(
    mark = layer@mark,
    values = values,
    types = types,
    after = after,
    params = layer@params,
    n = n,
    sf = sf,
    sankey = sankey,
    hierarchy = hierarchy,
    hier_fill_mode = hier_fill_mode,
    ds = ds,
    graph_identity = graph_identity,
    stat = layer@stat,
    stat_params = layer@stat_params,
    position = layer@position,
    blend = layer@blend,
    effects = layer@effects,
    sketch = layer@sketch,
    # Per-row interactivity (NULL when none declared): a data key + optional
    # tooltip / hover-group, evaluated against the raw layer data. Kept out of
    # `values` so it is never scale-trained. Aligns to the drawn elements when the
    # mark is `stat = "identity"` (row-preserving); a length guard at emit time
    # drops it for aggregating stats where rows no longer map 1:1.
    meta = .resolve_interactivity(layer@interactivity, data),
    # Per-aesthetic conditional encodings (from `condition()`): selection name +
    # `if_false` + `empty`, keyed by aesthetic. Empty when the layer declares
    # none. Drives the per-element `cond` membership tags at emit and the
    # plot-level interaction block; inert on a static render.
    conditions = conditions
  )
}

# Evaluate a layer's interactivity quosures (`tooltip`/`data_id`/`hover_group`)
# per data row. Returns NULL when none are declared (so a non-interactive layer
# stays exactly as before). Any interactive element needs an addressable key, so
# `data_id` defaults to the row index when a tooltip/hover-group is given without
# one. Scalars recycle to the row count.
.resolve_interactivity <- function(interactivity, data) {
  if (!length(interactivity)) {
    return(NULL)
  }
  n0 <- nrow(data)
  ev <- function(nm) {
    q <- interactivity[[nm]]
    if (is.null(q)) {
      return(NULL)
    }
    v <- rlang::eval_tidy(q, data = data)
    if (length(v) == 1L && n0 != 1L) {
      v <- rep(v, n0)
    }
    # A per-row interactive value must align 1:1 with the data; a wrong-length
    # vector would silently misalign the element keys/metadata.
    if (length(v) != n0) {
      cli::cli_abort(c(
        "Interactive {.arg {nm}} must be length 1 or {n0} (the row count).",
        i = "Got length {length(v)}."
      ))
    }
    v
  }
  tooltip <- ev("tooltip")
  data_id <- ev("data_id")
  hover_group <- ev("hover_group")
  hover_color <- ev("hover_color")
  selected_color <- ev("selected_color")
  # Any declaration makes the element interactive; an interactive element needs
  # an addressable key, so default data_id to the row index when it is absent.
  present <- !is.null(tooltip) ||
    !is.null(hover_group) ||
    !is.null(hover_color) ||
    !is.null(selected_color)
  if (is.null(data_id) && present) {
    data_id <- as.character(seq_len(n0))
  }
  if (is.null(data_id) && !present) {
    return(NULL)
  }
  list(
    data_id = if (!is.null(data_id)) as.character(data_id) else NULL,
    tooltip = tooltip,
    hover_group = hover_group,
    hover_color = hover_color,
    selected_color = selected_color,
    n = n0
  )
}

# Resolve every layer of a spec against a given data frame (a facet panel's
# subset, or the whole data): evaluate channels, apply the stat, then apply the
# pre-train part of the position adjustment (stack/fill). A layer with its own
# `data` resolves against that instead (per-panel subsetting is handled upstream
# in `.build_panels`; this path is the non-faceted / fallback case).
.resolve_on <- function(spec, data) {
  # A point selection with `fields`/`group_by` (from select_point()) groups
  # elements sharing those column values — which is exactly what `hover_group`
  # already does. Compute the per-row group value here (spec + data both in
  # scope) so a host links the whole group on hover/click via the existing
  # hover-group machinery; the emitter (`.compile_marks`) sets it as the element
  # hover_group when the layer doesn't declare one. v1 uses the first such
  # selection whose columns are all present.
  fsel <- Filter(
    function(s) {
      S7::S7_inherits(s, SelectionSpec) && length(s@fields) > 0L
    },
    spec@selections
  )
  lapply(spec@layers, function(layer) {
    ld <- layer@data %||% data
    res <- .apply_position(.apply_stat(.resolve_layer(layer, ld)))
    res$selgroup <- .selection_group_values(fsel, ld)
    res
  })
}

# The per-row group key for the first fields-selection whose columns are all in
# `data` (the fields joined with a unit separator). NULL when none applies.
.selection_group_values <- function(fsel, data) {
  for (s in fsel) {
    if (all(s@fields %in% names(data))) {
      parts <- lapply(s@fields, function(col) as.character(data[[col]]))
      return(do.call(paste, c(parts, sep = "")))
    }
  }
  NULL
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

# The resolved title for a scale: the user's `scale_*(name = )` if set, else
# `default`. `default` is a lazily-evaluated fallback (only computed when no name
# is given), so passing e.g. `.default_title(spec, "color")` costs nothing when a
# name is present. The single choke point every per-kind trainer routes through.
.scale_title <- function(scalespec, default) {
  if (!is.null(scalespec) && !is.null(scalespec@name)) {
    scalespec@name
  } else {
    default
  }
}
