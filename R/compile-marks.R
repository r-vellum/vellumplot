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
    return(invisible())
  }
  .mark_ctx$page <- c(w = spec@width, h = spec@height)
  filt_names <- vapply(spec@filters, function(f) f@selection, character(1))
  .mark_ctx$plot_interactive <- length(spec@selections) > 0L ||
    length(filt_names) > 0L ||
    length(spec@binds) > 0L
  .mark_ctx$plot_filters <- if (length(filt_names)) unique(filt_names) else NULL
  invisible()
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
  fv <- .mark_ctx$filter_value
  cond <- .mark_ctx$conditions
  # Graph edge endpoints: the two node keys (`name`s) this edge joins, so a host
  # can relate edges to nodes for neighbour highlighting. NULL for non-edge marks.
  esrc <- .mark_ctx$edge_source
  etgt <- .mark_ctx$edge_target
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
      is.null(etgt)
  ) {
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
  .emit_copies(scene, L, scales, 2 * o@size, o@alpha, o@color, "normal")
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
    .mark_ctx$layer <- li
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
    # Per-mark value on a continuous colour scale, for the interactive colorbar
    # filter (a host hides marks whose value is outside the dragged range). Only
    # for keyed marks (addressable) with a continuous colour scale; the raw data
    # value is `L$values$color` (fallback `fill`), recycled to the row count.
    .mark_ctx$filter_value <- local({
      cc <- scales$color
      if (!ok || is.null(cc) || !identical(cc$kind, "continuous")) {
        return(NULL)
      }
      v <- L$values$color %||% L$values$fill
      if (is.null(v)) NULL else as.numeric(rep_len(v, L$n))
    })
    # Conditional encodings (from `condition()`): a layer bearing one is
    # interactive, so its elements need addressable keys even if no `data_id` was
    # declared -- default to the row index. `cond` carries, per row, the "<sel>:<aes>"
    # tags this element participates in (like `legend`), so a host can style
    # non-members; the `if_false` constant + `empty` live in the plot-level block.
    .mark_ctx$conditions <- if (length(L$conditions)) L$conditions else NULL
    if (
      is.null(.mark_ctx$data_id) &&
        (length(L$conditions) || isTRUE(.mark_ctx$plot_interactive)) &&
        L$n >= 1L
    ) {
      .mark_ctx$data_id <- as.character(seq_len(L$n))
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
    # row-index default above with the stable graph key. A node with no declared
    # tooltip defaults to its name. Inert on a static render (gated on
    # `plot_interactive`). Straight / gradient / elbow edges all carry per-edge
    # keys (they pass `rows=` to `.draw`); self-loops draw without `rows=` so they
    # stay unkeyed -- harmless, a loop's only neighbour is its own node.
    .mark_ctx$edge_source <- NULL
    .mark_ctx$edge_target <- NULL
    gi <- L$graph_identity
    if (isTRUE(.mark_ctx$plot_interactive) && !is.null(gi)) {
      .mark_ctx$data_id <- gi$key
      if (identical(gi$kind, "edges")) {
        .mark_ctx$edge_source <- gi$source
        .mark_ctx$edge_target <- gi$target
      } else if (is.null(.mark_ctx$tooltip)) {
        .mark_ctx$tooltip <- gi$key
      }
    }
    # In a composition, each cell is a separate plot compiled independently, so
    # cells share row-index keys that would collide in one host runtime (hiding one
    # cell's key-3 would hit every cell's key-3). So a composition cell's DOM key is
    # made unique (prefixed with its `subplot-N` panel), while the original value is
    # kept as `join` -- the cross-view identity a host matches on (brush cell A ->
    # filter cell B's rows with the same join). A single plot or facet panel
    # (`panel-r-c`, whose keys are already original row ids) is unchanged: no prefix,
    # no `join`.
    .mark_ctx$join <- NULL
    if (
      !is.null(.mark_ctx$data_id) &&
        !is.na(.mark_ctx$panel) &&
        startsWith(.mark_ctx$panel, "subplot-")
    ) {
      .mark_ctx$join <- .mark_ctx$data_id
      .mark_ctx$data_id <- paste0(.mark_ctx$panel, ":", .mark_ctx$data_id)
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
