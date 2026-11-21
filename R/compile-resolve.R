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

  # An sf-label layer draws one text label at each feature's interior point. The
  # geometry has already been reprojected to the target CRS (`.project_sf_data`),
  # so the interior points come out in the same projected space as `mark_sf`'s
  # rings -- position training then unions them into the same panel extent, and
  # the text emitter maps them through the shared scales. `st_point_on_surface()`
  # is guaranteed to fall inside each (multi)polygon (unlike a centroid), which is
  # what a label wants. The `label`/`color`/`size` encodings resolved above.
  if (identical(layer@mark, "sf_label")) {
    .need_pkg("sf", "mark_sf_label()")
    if (!.is_sf(data)) {
      cli::cli_abort("{.fn mark_sf_label} requires an {.cls sf} data frame.")
    }
    geom <- sf::st_geometry(data)
    # An empty geometry has no interior point (st_point_on_surface -> POINT EMPTY
    # -> NA coordinates); drop those features so a label is never placed at NA.
    # Subset the resolved encodings (label/colour/size, length = feature count) to
    # match, keeping every label aligned with its point.
    keep <- !sf::st_is_empty(geom)
    xy <- sf::st_coordinates(suppressWarnings(sf::st_point_on_surface(geom[
      keep
    ])))
    if (!all(keep)) {
      nf <- length(geom)
      values <- lapply(values, function(v) {
        if (length(v) == nf) v[keep] else v
      })
    }
    values$x <- as.numeric(xy[, "X"])
    values$y <- as.numeric(xy[, "Y"])
    types$x <- types$y <- "quantitative"
    n <- sum(keep)
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

  # A venn layer draws an abstract set diagram in npc; its geometry is
  # precomputed on the layer (`params$venn`). Just synthesise the unit [0, 1]
  # panel extent (drawn square by the fixed coord) so position training has a
  # domain -- like the other layout marks above.
  if (identical(layer@mark, "venn")) {
    values$x <- c(0, 1)
    values$y <- c(0, 1)
    types$x <- types$y <- "quantitative"
    n <- 1L
  }

  # A waffle layer draws a grid of cells in npc, one row per cell, coloured by the
  # mapped `fill` (so a discrete legend trains). Synthesise the unit [0, 1] extent
  # for position training but keep `n` (the cell count) and the per-cell fill.
  if (identical(layer@mark, "waffle")) {
    values$x <- c(0, 1)
    values$y <- c(0, 1)
    types$x <- types$y <- "quantitative"
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

  # A chord layer wraps a flow list onto a circle: sectors + ribbons computed
  # from the `from`/`to`/`value` channels (see `.chord_layout`), drawn in the
  # aspect-locked square panel. The ring is at native radius 1; widen the extent
  # so the sector labels (which radiate outward past the ring) fit inside the
  # panel instead of clipping at its edge.
  chord <- NULL
  if (identical(layer@mark, "chord")) {
    if (is.null(values$from) || is.null(values$to) || is.null(values$value)) {
      cli::cli_abort(
        "{.fn vchord} needs {.arg from}, {.arg to}, and {.arg value}."
      )
    }
    chord <- .chord_layout(
      values$from,
      values$to,
      values$value,
      gap = layer@params$gap %||% 0.02,
      sort = layer@params$sort %||% "input",
      link_color = layer@params$link_color %||% "source"
    )
    ext <- .chord_extent(
      chord$sectors$node,
      isTRUE(layer@params$label %||% TRUE)
    )
    values$x <- c(-ext, ext)
    values$y <- c(-ext, ext)
    types$x <- types$y <- "quantitative"
    n <- nrow(chord$sectors)
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
    # colour training only its unique levels -- the discrete colour (or fill)
    # scale (and its legend) trains from the levels, never the full cloud.
    ch <- if (!is.null(values$color)) "color" else "fill"
    cat <- values[[ch]]
    if (!is.null(cat)) {
      ds$cat <- cat
      ds$cat_channel <- ch
      values$color <- NULL
      values$fill <- NULL
      values[[ch]] <- unique(cat)
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
    chord = chord,
    ds = ds,
    graph_identity = graph_identity,
    stat = layer@stat,
    stat_params = layer@stat_params,
    position = layer@position,
    blend = layer@blend,
    effects = layer@effects,
    sketch = layer@sketch,
    clip = layer@clip, # per-layer clip (clip_layer()) | NULL
    annotation = layer@annotation, # from annotate(): no plot-level default keys

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
  # The row positions are a placeholder: `.compile_marks()` replaces a default
  # key with the panel/layer/row key from `.default_keys()`.
  default_id <- is.null(data_id) && present
  if (default_id) {
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
    n = n0,
    # no `data_id` declared: the key is a default (see `.default_keys()`)
    default_id = default_id
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
    res$selgroup <- .realign_row_value(
      .selection_group_values(fsel, ld),
      res$stat_map
    )
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

# Filled marks whose `color` strokes the outline rather than filling (ggplot2's
# boxplot / violin semantics).
.OUTLINE_MARKS <- c("boxplot", "violin", "ridgeline")

# Filled marks where `fill` fills and a `color` given alongside it outlines,
# while a lone `color` still fills (`.fill_and_outline()`, VP-46).
.FILL_OUTLINE_MARKS <- c(
  "bar",
  "tile",
  "rect",
  "polygon",
  "area",
  "ribbon",
  "hex",
  "crossbar"
)

# Does layer `L` draw with `.fill_and_outline()` semantics? A dot plot is a
# point layer whose dots are filled shapes.
.fills_and_outlines <- function(L) {
  L$mark %in%
    .FILL_OUTLINE_MARKS ||
    (identical(L$mark, "point") && identical(L$stat, "dotplot"))
}

# A constant plain-colour `fill` param of `L` (not a gradient/pattern paint), or
# NULL.
.plain_fill_param <- function(L) {
  f <- L$params$fill
  if (.is_string(f) && !.is_paint(f)) f
}

# The constant half of a filled mark's legend key when the other half is mapped
# (ggplot2 draws both into the key): `list(col = )` -- a constant outline around
# swatches coloured by a mapped `fill` -- or `list(fill = )` -- a constant fill
# inside outlines coloured by a mapped `color`. NULL otherwise (the swatch is
# the mapped colour alone). Read from the first filled layer mapping the scale.
.key_style_for_aes <- function(resolved, aesthetic) {
  for (L in resolved) {
    if (!aesthetic %in% names(L$values)) {
      next
    }
    if (!.fills_and_outlines(L) && !L$mark %in% .OUTLINE_MARKS) {
      next
    }
    has_fill <- !is.null(L$values$fill)
    has_col <- !is.null(L$values$color)
    if (has_fill && !has_col && .is_string(L$params$color)) {
      return(list(col = L$params$color))
    }
    if (has_col && !has_fill && !is.null(.plain_fill_param(L))) {
      return(list(fill = .plain_fill_param(L)))
    }
    return(NULL)
  }
  NULL
}

# The opacity a legend key takes from its layer, as ggplot2's key drawers
# (`draw_key_rect()`, `draw_key_point()`, `draw_key_path()`, ...) take the
# layer alpha (VP-67, VP-70). Read from the first layer mapping `aesthetic`
# that draws the scale's key glyph (`glyph`: a point layer for a point key, a
# line layer for a line key, a filled layer for a square or an outlined box):
# `list(alpha = )` for a constant `alpha` (`edge_alpha` on an edge layer),
# which the mark applies to its fill and outline alike, or `list(fill_alpha = )`
# for `mark_density()`'s default fill alpha, which leaves the outline opaque.
# NULL when that layer sets no alpha or maps one (a mapped alpha has its own
# legend), and for `mark_smooth()`, whose alpha is the band's: the fitted line,
# all its key draws, stays opaque.
.key_alpha_for_aes <- function(
  resolved,
  aesthetic,
  glyph = .key_glyph_for_aes(resolved, aesthetic)
) {
  want <- if (glyph %in% c("point", "line")) glyph else "square"
  for (L in resolved) {
    if (!aesthetic %in% names(L$values)) {
      next
    }
    if (!identical(.key_glyph_for_mark(L$mark), want)) {
      next
    }
    ch <- if (L$mark %in% .EDGE_ALPHA_MARKS) "edge_alpha" else "alpha"
    if (!is.null(L$values[[ch]]) || identical(L$mark, "smooth")) {
      return(NULL)
    }
    if (.density_default_alpha(L)) {
      return(list(fill_alpha = .DENSITY_FILL_ALPHA))
    }
    a <- L$params[[ch]]
    if (is.numeric(a) && length(a) == 1L && !is.na(a)) {
      return(list(alpha = a))
    }
    return(NULL)
  }
  NULL
}

# Marks whose opacity is the `edge_alpha` channel (`.rename_edge_aes()`).
.EDGE_ALPHA_MARKS <- c("edges", "edge_bundle")

.key_glyph_for_mark <- function(mark) {
  if (mark %in% names(.MARK_KEY_GLYPH)) .MARK_KEY_GLYPH[[mark]] else "square"
}

# The key glyph for a scale, from the marks of the layers that map `aesthetic`.
# A point-drawing layer wins (the common scatter case), then a line-drawing
# layer, else the square swatch -- so a legend key matches what the plot
# actually draws (e.g. circles for a point layer, not squares).
.key_glyph_for_aes <- function(resolved, aesthetic) {
  glyphs <- character(0)
  for (L in resolved) {
    if (aesthetic %in% names(L$values)) {
      glyph <- .key_glyph_for_mark(L$mark)
      # A distribution mark whose `color` is mapped draws coloured outlines, so
      # the colour key is an outlined box. So does a filled mark (bar, tile,
      # area, ...) whose mapped `color` outlines a fill -- a constant one
      # (VP-46) or a mapped one, which the fill scale keys (VP-48).
      outlines <- identical(aesthetic, "color") &&
        (L$mark %in%
          .OUTLINE_MARKS ||
          (L$mark %in%
            .FILL_OUTLINE_MARKS &&
            (!is.null(L$values$fill) || !is.null(.plain_fill_param(L)))))
      if (outlines) {
        glyph <- "outline"
      }
      glyphs <- c(glyphs, glyph)
    }
  }
  if ("point" %in% glyphs) {
    "point"
  } else if ("line" %in% glyphs) {
    "line"
  } else if (length(glyphs) && all(glyphs == "outline")) {
    "outline"
  } else {
    "square"
  }
}

# The user-declared scale for an aesthetic (the last one declared), or NULL.
.scale_for <- function(spec, aesthetic) {
  for (s in rev(spec@scales)) {
    if (identical(s@aesthetic, aesthetic)) return(s)
  }
  NULL
}

# Category levels for a discrete aesthetic, preserving factor level order. `x`
# may be a single vector or a list of vectors pooled across layers (or, under
# faceting, the same variable evaluated per panel). When any pooled value is a
# factor, the union of all factor levels wins, taken in first-appearance order so
# a declared factor ordering survives -- pooling one factor per panel (each
# carrying only its panel's level) must still recover every level, not just the
# first panel's. Otherwise: sorted unique character values.
#
# NA is never a level here, whichever branch runs. A scale that shows missing
# values adds them as its own trailing level (ggplot2's `na.translate`): the
# discrete position trainer as an "NA" band, the colour / shape / ... trainers
# as an "NA" legend key drawn in the scale's `na_value`.
.cat_levels <- function(x) {
  vals <- if (is.list(x)) x else list(x)
  facs <- Filter(is.factor, vals)
  if (length(facs)) {
    # Union the factor levels (preserving their order), then append any categories
    # contributed only by non-factor layers. Combining with `c()` here would coerce
    # a factor to its integer codes when a sibling layer maps the same axis as a
    # character; taking levels + the extra strings keeps every real category.
    fac_levs <- unique(unlist(lapply(facs, levels), use.names = FALSE))
    others <- unique(as.character(unlist(
      lapply(Filter(Negate(is.factor), vals), as.character),
      use.names = FALSE
    )))
    others <- others[!is.na(others)]
    return(unique(c(fac_levs, others[!others %in% fac_levs])))
  }
  chr <- unique(as.character(unlist(
    lapply(vals, as.character),
    use.names = FALSE
  )))
  chr <- chr[!is.na(chr)]
  # Categories that are all numeric-looking (e.g. a numeric column drawn discretely
  # by mark_bar) order numerically, so bars run 1, 2, 10 rather than lexicographic
  # 1, 10, 2. Otherwise fall back to alphabetical order.
  num <- suppressWarnings(as.numeric(chr))
  if (length(chr) && !anyNA(num)) {
    chr[order(num)]
  } else {
    sort(chr)
  }
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

