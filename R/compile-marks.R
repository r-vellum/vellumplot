#' @include classes.R
NULL

# Mark dispatch (.emit_layer), the scene-drawing machinery, per-mark effects,
# and the top-level compile entry point. The per-mark emitters live in the
# sibling compile-marks-*.R files.

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
  # The generic draw-time position step (dodge / jitter for marks that do not
  # lay them out themselves); a no-op for every other layer.
  pos <- .position_native(L, scales)
  L <- pos$L
  scales <- pos$scales
  switch(
    L$mark,
    point = .emit_point(scene, L, scales),
    sf = .emit_sf(scene, L, scales),
    scalebar = .emit_scalebar(scene, L, scales),
    compass = .emit_compass(scene, L, scales),
    line = .emit_line(scene, L, scales),
    rule = .emit_rule(scene, L, scales),
    abline = .emit_abline(scene, L, scales),
    fun = .emit_function(scene, L, scales),
    bar = .emit_bar(scene, L, scales),
    smooth = .emit_smooth(scene, L, scales),
    area = .emit_area(scene, L, scales),
    ribbon = .emit_ribbon(scene, L, scales),
    step = .emit_step(scene, L, scales),
    text = .emit_text(scene, L, scales),
    sf_label = .emit_text(scene, L, scales),
    text_path = .emit_text_path(scene, L, scales),
    label = .emit_label(scene, L, scales),
    tile = .emit_tile(scene, L, scales),
    rect = .emit_rect(scene, L, scales),
    raster = .emit_raster(scene, L, scales),
    image = .emit_image(scene, L, scales),
    boxplot = .emit_boxplot(scene, L, scales),
    signif = .emit_signif(scene, L, scales),
    errorbar = .emit_errorbar(scene, L, scales),
    linerange = .emit_linerange(scene, L, scales),
    pointrange = .emit_pointrange(scene, L, scales),
    crossbar = .emit_crossbar(scene, L, scales),
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
    polygon = .emit_polygon(scene, L, scales),
    sankey = .emit_sankey(scene, L, scales),
    venn = .emit_venn(scene, L, scales),
    waffle = .emit_waffle(scene, L, scales),
    sparkline = .emit_sparkline(scene, L, scales),
    grob = .emit_grob(scene, L, scales),
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

# Fields of `.mark_ctx` that accumulate over a whole compile, so a nested draw
# must not roll them back: the provenance table and the grob counter.
.MARK_CTX_SHARED <- c("provenance", "seq")

# Snapshot / restore the per-plot context around a nested `.draw_plot()` (an
# inset sub-plot drawn from inside an outer layer). Restoring removes fields the
# nested draw added and puts back the caller's values, leaving the shared
# accumulators as the nested draw left them.
.mark_ctx_save <- function() {
  keep <- setdiff(ls(.mark_ctx, all.names = TRUE), .MARK_CTX_SHARED)
  mget(keep, envir = .mark_ctx)
}

.mark_ctx_restore <- function(saved) {
  now <- setdiff(ls(.mark_ctx, all.names = TRUE), .MARK_CTX_SHARED)
  rm(list = setdiff(now, names(saved)), envir = .mark_ctx)
  for (nm in names(saved)) {
    assign(nm, saved[[nm]], envir = .mark_ctx)
  }
  invisible()
}

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
    .mark_ctx$plot_interval_aes <- NULL
    return(invisible())
  }
  .mark_ctx$page <- c(w = spec@width, h = spec@height)
  filt_names <- vapply(spec@filters, function(f) f@selection, character(1))
  .mark_ctx$plot_interactive <- length(spec@selections) > 0L ||
    length(filt_names) > 0L ||
    length(spec@binds) > 0L
  .mark_ctx$plot_filters <- if (length(filt_names)) unique(filt_names) else NULL
  .mark_ctx$plot_interval_aes <- .interval_encodings(spec@selections)
  invisible()
}

# The position channels ("x" / "y") any interval selection on this plot brushes,
# or NULL when it declares none. Their per-element data values are emitted so a
# host can test interval membership by data value (see `.interval_values`).
.interval_encodings <- function(selections) {
  enc <- unlist(lapply(selections, function(s) {
    if (!identical(s@kind, "interval")) {
      return(NULL)
    }
    s@encodings %||% .on_encodings(s@on)
  }))
  if (length(enc)) intersect(c("x", "y"), enc) else NULL
}

# Per-row data values of a layer on the brushed position channels, for interval
# membership. A named list of vectors aligned to the layer's rows: `x` / `y`
# hold the value -- numeric for a continuous / date / datetime / binned scale (a
# date as its epoch number, the same units the host inverts a pixel to), the
# category label for a discrete scale (the host matches it against the brushed
# levels). A mark that spans a numeric range on the channel (`xmin`/`xmax` of a
# rect, `x`/`xend` of a segment) also gets `xh` / `yh`, the upper end, so the
# host tests overlap. A channel the layer does not map is omitted. NULL when
# there is nothing to carry.
.interval_values <- function(L, scales, aes) {
  val <- function(nm) {
    v <- L$values[[nm]]
    if (is.null(v) || !length(v)) {
      return(NULL)
    }
    v <- rep_len(v, L$n)
    if (is.numeric(v) || inherits(v, c("Date", "POSIXt"))) as.numeric(v)
  }
  out <- list()
  for (a in aes) {
    v <- L$values[[a]]
    if (isTRUE(scales[[a]]$discrete)) {
      if (!is.null(v) && length(v)) {
        out[[a]] <- as.character(rep_len(v, L$n))
      }
      next
    }
    lo <- val(a)
    hi <- NULL
    if (is.null(lo)) {
      lo <- val(paste0(a, "min"))
      hi <- val(paste0(a, "max"))
    } else {
      hi <- val(paste0(a, "end"))
    }
    if (is.null(lo)) {
      next
    }
    if (!is.null(hi)) {
      # order each row's pair so `a` <= `ah` (a segment may run backwards)
      out[[a]] <- pmin(lo, hi)
      out[[paste0(a, "h")]] <- pmax(lo, hi)
    } else {
      out[[a]] <- lo
    }
  }
  if (length(out)) out else NULL
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
  # Accessibility (#145): a chart is ONE figure with one text alternative -- the
  # plot-level `plot_alt()` description handed to the scene title/desc. The
  # individual marks are the visual content of that figure, not sub-figures, so
  # they are decorative for tagging. Without a role, the tagged-PDF backend makes
  # every mark its own `Figure` and fills its `/Alt` from the mark's provenance
  # `id` (a `mark_text(repel=)` label from its `vl_place` handle name), so a
  # screen reader announces internal identifiers ("layer-1-line-g1",
  # "repel:panel-1-1:2:0"). `role = "presentation"` marks them as artifacts (the
  # mechanism that already silences gridlines) -- vellum then keeps the single
  # whole-page Figure carrying `plot_alt()`. `id`/`keys` are untouched, so
  # provenance and interactivity are unchanged; an emitter that set its own role
  # (e.g. "grid") keeps it.
  if (is.null(grob@role)) {
    grob@role <- "presentation"
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
    if (!is.null(m) && length(rows) > 1L && .is_single_shape(grob)) {
      m[[1L]] <- .interval_series_span(m[[1L]], rows)
    }
    if (!is.null(m)) {
      grob@meta <- m
    }
  }
  vellum::draw(scene, grob)
}

# Default element keys for an interactive layer that declares no `data_id`
# (VP-55). Keys must be unique across the whole scene: a host addresses
# elements by key, so a repeated key makes a hover/brush/selection in one place
# light up every element sharing it. So every default key names its panel, its
# layer and its row -- "p<r>-<c>:l<layer>:<row>", e.g. "p1-1:l2:7" for row 7 of
# layer 2 in the top-left (or only) panel -- whatever produced it: plain
# keyed layers, tooltip-only layers (`meta$default_id`), stat output and
# line/polygon series (which take their first row's key). <row> is the
# source-table row the element draws when the layer maps one-to-one onto its
# input rows (provenance's `.layer_source()`), and its position in the layer
# otherwise. Two layers never share a default key; layers meant to link share
# an explicit `data_id`. Composition cells add their `subplot-N` / `cell-k`
# prefix in front in `.compile_marks()`.
#
# Besides `key`, returns `join` -- the cross-view identity a host matches
# filters on: the source row in a facet panel (so the same datum drawn in two
# panels still links, VP-51), NULL otherwise -- and `cross`, the identity a
# composition cell falls back to when there is no `join` (the row, or
# "<panel>:<row>" in a facet panel whose rows are not source rows).
.default_keys <- function(L, panel, layer) {
  rows <- seq_len(L$n)
  src <- L$source
  per_row <- !is.null(src) &&
    isTRUE(src$per_row) &&
    is.null(src$members) &&
    length(src$rows) == L$n &&
    !anyDuplicated(src$rows)
  if (per_row) {
    rows <- src$rows
  }
  rc <- if (!is.na(panel) && startsWith(panel, "panel-")) {
    sub("^panel-", "", panel)
  } else {
    "1-1" # an aligned composition cell draws one panel
  }
  key <- paste0("p", rc, ":l", layer, ":", rows)
  if (
    !isTRUE(.mark_ctx$faceted) || is.na(panel) || !startsWith(panel, "panel-")
  ) {
    return(list(key = key, join = NULL, cross = as.character(rows)))
  }
  list(
    key = key,
    join = if (per_row) as.character(rows),
    cross = if (per_row) as.character(rows) else paste0(panel, ":", rows)
  )
}

# A grob vellum treats as ONE element however many rows it draws (a line,
# path, or polygon series): `scene_model()` keeps only its first meta record.
.is_single_shape <- function(grob) {
  inherits(
    grob,
    c("vellum::grob_lines", "vellum::grob_path", "vellum::grob_polygon")
  )
}

# For a single-shape element drawn from several rows, widen its numeric interval
# values to the span of all those rows (`ix` = min, `ixh` = max; likewise y), so
# a host counts the series a member when any part of it lies in the brushed
# range rather than only its first vertex. A discrete value stays the first row's.
.interval_series_span <- function(rec, rows) {
  iv <- .mark_ctx$interval_values
  for (a in intersect(c("x", "y"), names(iv))) {
    v <- iv[[a]]
    if (!is.numeric(v)) {
      next
    }
    r <- suppressWarnings(range(
      v[rows],
      iv[[paste0(a, "h")]][rows],
      na.rm = TRUE
    ))
    if (all(is.finite(r))) {
      rec[[paste0("i", a)]] <- r[[1L]]
      rec[[paste0("i", a, "h")]] <- r[[2L]]
    }
  }
  rec
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
  # Data values on the brushed channels (interval selections), per row.
  iv <- .mark_ctx$interval_values
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
      is.null(etgt) &&
      is.null(iv)
  ) {
    return(NULL)
  }
  ctx <- list(
    tt = tt,
    hg = hg,
    hc = hc,
    sc = sc,
    esrc = esrc,
    etgt = etgt,
    lg = lg,
    fv = fv,
    cond_tags = cond_tags,
    cond_rowvals = cond_rowvals,
    filt_tags = filt_tags,
    join = join,
    iv = iv
  )
  if (is.null(iv) && is.null(cond_rowvals)) {
    .elem_meta_cols(rows, ctx)
  } else {
    .elem_meta_rows(rows, ctx)
  }
}

# `.elem_meta()` built column-wise (VR-16): each field's per-row values at
# once, then one record per row. A big scatter has a record per point, and the
# per-row closure below was a large share of compiling it. Same records as
# `.elem_meta_rows()` (fields in the same order, a NULL value leaves its field
# out); used when no field varies in shape per row (no interval values, no
# per-row condition values).
.elem_meta_cols <- function(rows, ctx) {
  n <- length(rows)
  chr <- function(v) {
    if (is.atomic(v)) as.list(as.character(v[rows])) else lapply(v[rows], as.character)
  }
  raw <- function(v) {
    if (is.list(v) && !is.object(v)) {
      v[rows]
    } else if (is.atomic(v) && !is.object(v)) {
      as.list(v[rows])
    } else {
      lapply(rows, function(i) v[[i]])
    }
  }
  cols <- list()
  if (!is.null(ctx$tt)) cols$tooltip <- chr(ctx$tt)
  if (!is.null(ctx$hg)) cols$hover_group <- chr(ctx$hg)
  if (!is.null(ctx$hc)) cols$hover_color <- chr(ctx$hc)
  if (!is.null(ctx$sc)) cols$selected_color <- chr(ctx$sc)
  if (!is.null(ctx$esrc)) cols$source <- chr(ctx$esrc)
  if (!is.null(ctx$etgt)) cols$target <- chr(ctx$etgt)
  if (!is.null(ctx$lg)) cols$legend <- raw(ctx$lg)
  if (!is.null(ctx$fv)) cols$filter_value <- raw(ctx$fv)
  if (!is.null(ctx$cond_tags)) cols$cond <- rep(list(ctx$cond_tags), n)
  if (!is.null(ctx$filt_tags)) cols$filt <- rep(list(ctx$filt_tags), n)
  if (!is.null(ctx$join)) cols$join <- chr(ctx$join)
  recs <- .mapply(list, cols, NULL)
  # A NULL value (`rec$legend <- NULL`) leaves the field out of that record.
  nul <- Reduce(
    `|`,
    lapply(cols[c("legend", "filter_value")], function(l) {
      if (is.null(l)) logical(n) else vapply(l, is.null, logical(1))
    })
  )
  if (any(nul)) {
    recs[nul] <- lapply(recs[nul], function(r) {
      r <- r[!vapply(r, is.null, logical(1))]
      if (length(r)) r else list()
    })
  }
  recs
}

# The per-row build of `.elem_meta()`, for the fields whose shape varies by row.
.elem_meta_rows <- function(rows, ctx) {
  tt <- ctx$tt
  hg <- ctx$hg
  hc <- ctx$hc
  sc <- ctx$sc
  esrc <- ctx$esrc
  etgt <- ctx$etgt
  lg <- ctx$lg
  fv <- ctx$fv
  cond_tags <- ctx$cond_tags
  cond_rowvals <- ctx$cond_rowvals
  filt_tags <- ctx$filt_tags
  join <- ctx$join
  iv <- ctx$iv
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
    # `ix` / `iy`: the element's data value on a brushed position channel
    # (numeric), or `ixd` / `iyd` for a discrete channel (the category label).
    # (`ixh` / `iyh` is the upper end when the element spans a range.)
    for (a in names(iv)) {
      v <- iv[[a]][[i]]
      if (is.character(v)) {
        rec[[paste0("i", a, "d")]] <- v
      } else {
        rec[[paste0("i", a)]] <- v
      }
    }
    rec
  })
}

# Per-row legend membership: for each discrete colour/fill/shape scale the layer
# maps, the element's series key(s) "<aes>:<value>". Returns a list (one entry per
# row, each a character vector), or NULL when the layer maps no discrete legend
# aesthetic. Matches the `legend_for` a discrete legend swatch carries. A fill
# key reads "color:<value>" unless the fill legend stands beside a separate
# colour legend (`.fill_series_aes()`).
.legend_membership <- function(L, scales) {
  n <- L$n
  cols <- list()
  for (ch in c("color", "fill")) {
    sc <- scales[[ch]]
    v <- L$values[[ch]]
    if (!is.null(sc) && identical(sc$type, "discrete") && !is.null(v)) {
      ns <- if (ch == "fill") sc$series_aes %||% "color" else "color"
      key <- paste0(ns, ":", as.character(rep_len(v, n)))
      # a layer mapping both to one variable keys each element once
      if (!identical(cols[["color"]], key)) {
        cols[[ch]] <- key
      }
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
  # One series key per aesthetic per element, without a closure per element.
  cols <- unname(cols)
  if (length(cols) == 1L) {
    return(as.list(cols[[1L]]))
  }
  .mapply(c, cols, NULL)
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
      # A mapped `linewidth` ignores `params$linewidth`, so the halo would come
      # out the width of the mark it is meant to sit behind. Carry the widening
      # separately for `.mapped_linewidth()` to add to each mapped width.
      L2$params$.lwd_add <- d * .MM_TO_LWD
    }
    scene <- .emit_layer(scene, L2, scales)
  }
  vellum::pop(scene)
}

# One soft copy of the layer inside a blurred group -- the real-blur replacement
# for the old stack of N widened low-opacity copies. `blur` is the Gaussian
# radius (mm); the engine blurs the whole group once (cheaper, smoother, and it
# works on text, unlike stroking N glyph copies). `widen` optionally fattens the
# stroke/point first so the halo has body before the blur spreads it.
.emit_soft <- function(
  scene,
  L,
  scales,
  blur,
  alpha,
  colour,
  blend = "normal",
  xoff = 0,
  yoff = 0,
  widen = 0
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
      blur = blur,
      blend = if (identical(blend, "normal")) NULL else blend
    )
  )
  L2 <- L
  L2$effects <- list()
  L2$params$alpha <- alpha
  if (!is.null(colour)) {
    L2$values$color <- NULL
    L2$values$fill <- NULL
    L2$params$color <- colour
    L2$params$fill <- colour
  }
  # Widening fattens a stroke/point before the blur; a text glyph cannot be
  # fattened, so a text halo comes from the blur alone.
  if (widen > 0 && L$mark %in% .STROKE_POINT_MARKS) {
    if (is_point) {
      L2$values$size <- NULL
      L2$params$size <- base + widen
    } else {
      L2$params$linewidth <- base + widen * .MM_TO_LWD
      L2$params$.lwd_add <- widen * .MM_TO_LWD
    }
  }
  scene <- .emit_layer(scene, L2, scales)
  vellum::pop(scene)
}

.emit_glow <- function(scene, L, scales, g) {
  # A real Gaussian blur of one wider copy -- a soft neon halo -- instead of the
  # old stack of `layers` widened copies. `alpha` accumulates the old per-copy
  # opacity so brightness is comparable; `blend` (screen by default) keeps the
  # additive neon look over a dark backdrop.
  .emit_soft(
    scene,
    L,
    scales,
    blur = g@size * 0.6,
    alpha = min(1, g@alpha * g@layers),
    colour = g@color,
    blend = g@blend,
    widen = g@size * 0.35
  )
}

.emit_outline <- function(scene, L, scales, o) {
  # one opaque copy, wider by `size` per side, in a contrasting colour (sharp --
  # a sticker halo, not blurred)
  col <- o@color %||% .geom_paint("white", "neutral")
  .emit_copies(scene, L, scales, 2 * o@size, o@alpha, col, "normal")
}

.emit_shadow <- function(scene, L, scales, s) {
  # A real drop shadow: one blurred, offset copy in the shadow colour, instead of
  # the old stack of widened copies.
  .emit_soft(
    scene,
    L,
    scales,
    blur = s@spread,
    alpha = min(1, s@alpha * s@layers),
    colour = s@color,
    blend = "normal",
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

.compile_marks <- function(
  scene,
  resolved,
  scales,
  panel = NA_character_,
  layer_index = seq_along(resolved)
) {
  on.exit(.mark_ctx$id <- NULL, add = TRUE)
  .mark_ctx$panel <- panel
  for (i in seq_along(resolved)) {
    L <- resolved[[i]]
    if (!L$n) {
      next
    } # empty facet panel
    # The layer's stable index in the *original* layer list, so a partitioned
    # draw (clipped vs unclipped overlay, see `.dp_draw_panels`) keeps provenance
    # ids/layer numbers consistent with the un-partitioned order.
    li <- layer_index[i]
    .mark_ctx$id <- sprintf("layer-%d-%s", li, L$mark)
    # Provenance context for every grob this layer emits (DESIGN §4). Set once
    # per layer: `.draw()` reads it. `rows` defaults to the whole layer -- an
    # emitter that groups rows by style refines it per group (see `PROVENANCE:`).
    # Those are indices into the resolved layer; `source` maps them back to the
    # rows of the table the layer was drawn from (see `.provenance_record`).
    .mark_ctx$layer <- li
    .mark_ctx$mark <- L$mark
    .mark_ctx$channels <- .layer_channels(L, scales)
    .mark_ctx$rows <- seq_len(L$n)
    .mark_ctx$source <- L$source
    .mark_ctx$kind <- "mark"
    # Per-row interactivity for this layer (NULL when none declared). A stat has
    # already realigned it to its output rows (`.realign_meta()`): a
    # row-preserving stat follows `.rows`, a summarising one keeps only values
    # constant per group or drops them. The length check is a last guard that
    # it aligns to the drawn rows.
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
    # for keyed marks (addressable) with a continuous colour (else fill) scale;
    # the raw data value is the layer's `color` (else `fill`), recycled to the
    # row count.
    .mark_ctx$filter_value <- local({
      v <- NULL
      for (ch in c("color", "fill")) {
        cc <- scales[[ch]]
        if (!is.null(cc) && identical(cc$type, "continuous")) {
          v <- L$values[[ch]]
          if (!is.null(v)) break
        }
      }
      if (!ok || is.null(v)) NULL else as.numeric(rep_len(v, L$n))
    })
    # Conditional encodings (from `condition()`): a layer bearing one is
    # interactive, so its elements need addressable keys even if no `data_id` was
    # declared -- a default key (see `.default_keys()`). `cond` carries, per row,
    # the "<sel>:<aes>" tags this element participates in (like `legend`), so a
    # host can style non-members; the `if_false` constant + `empty` live in the plot-level block.
    .mark_ctx$conditions <- if (length(L$conditions)) L$conditions else NULL
    # Every default key -- a tooltip-only layer's (`meta$default_id`), or a
    # keyless layer made interactive by a condition or a plot-level selection
    # (not an annotation, VW-24) -- comes from `.default_keys()`.
    default_join <- NULL
    default_cross <- NULL
    if (
      (ok && isTRUE(L$meta$default_id)) ||
        (is.null(.mark_ctx$data_id) &&
          (length(L$conditions) ||
            (isTRUE(.mark_ctx$plot_interactive) && !isTRUE(L$annotation))) &&
          L$n >= 1L)
    ) {
      dk <- .default_keys(L, .mark_ctx$panel, li)
      .mark_ctx$data_id <- dk$key
      default_join <- dk$join
      default_cross <- dk$cross
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
    # default key above with the stable graph key. A node with no declared
    # tooltip defaults to its name. Inert on a static render (gated on
    # `plot_interactive`). Straight / gradient / elbow edges all carry per-edge
    # keys (they pass `rows=` to `.draw`); self-loops draw without `rows=` so they
    # stay unkeyed -- harmless, a loop's only neighbour is its own node.
    .mark_ctx$edge_source <- NULL
    .mark_ctx$edge_target <- NULL
    gi <- L$graph_identity
    if (isTRUE(.mark_ctx$plot_interactive) && !is.null(gi)) {
      .mark_ctx$data_id <- gi$key
      default_join <- NULL
      default_cross <- NULL
      if (identical(gi$kind, "edges")) {
        .mark_ctx$edge_source <- gi$source
        .mark_ctx$edge_target <- gi$target
      } else if (is.null(.mark_ctx$tooltip)) {
        .mark_ctx$tooltip <- gi$key
      }
    }
    # Interval selections test membership by data value: carry each keyed
    # element's value on the brushed channel(s).
    .mark_ctx$interval_values <- if (
      !is.null(.mark_ctx$data_id) && length(.mark_ctx$plot_interval_aes)
    ) {
      .interval_values(L, scales, .mark_ctx$plot_interval_aes)
    }
    # In a composition, each cell is a separate plot compiled independently, so
    # cells share keys that would collide in one host runtime (hiding one cell's
    # "p1-1:l1:3" would hit every cell's). So a composition cell's DOM key is
    # made unique (prefixed with its `subplot-N` panel), while a cross-view
    # identity is kept as `join` -- what a host matches on (brush cell A ->
    # filter cell B's rows with the same join): a declared `data_id` as given, a
    # default key's row (`.default_keys()`). A single plot gets no prefix, and a
    # `join` only for facet-panel default keys (the source row).
    .mark_ctx$join <- NULL
    if (
      !is.null(default_join) &&
        length(default_join) == length(.mark_ctx$data_id)
    ) {
      .mark_ctx$join <- default_join
    }
    if (
      !is.null(.mark_ctx$data_id) &&
        !is.na(.mark_ctx$panel) &&
        startsWith(.mark_ctx$panel, "subplot-")
    ) {
      .mark_ctx$join <- .mark_ctx$join %||%
        default_cross %||%
        .mark_ctx$data_id
      .mark_ctx$data_id <- paste0(.mark_ctx$panel, ":", .mark_ctx$data_id)
    }
    # A composition cell drawn as a whole plot (faceted, or otherwise not
    # aligned) names its panels `panel-r-c` like a lone plot, so its keys
    # repeat from cell to cell. Prefix them with the cell id
    # ("cell-2:p1-1:l1:3"), keeping a cross-view identity as `join` (the facet
    # default's source row, a default key's row, or a declared value), as for
    # an aligned cell (VP-51).
    cell <- .mark_ctx$panel_cell
    if (
      !is.null(.mark_ctx$data_id) &&
        !is.null(cell) &&
        !is.na(.mark_ctx$panel) &&
        startsWith(.mark_ctx$panel, "panel-")
    ) {
      .mark_ctx$join <- .mark_ctx$join %||%
        default_cross %||%
        .mark_ctx$data_id
      .mark_ctx$data_id <- paste0(cell, ":", .mark_ctx$data_id)
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
