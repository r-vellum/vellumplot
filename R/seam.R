#' @include classes.R coord.R compile-resolve.R compile-train.R
#' @include compile-facet.R compile-layout.R compile-guides.R compile-marks.R
#' @include marginal.R
NULL

# --- panel-level scale metadata (the `scales` meta convention) --------------
# A per-axis descriptor of a trained position scale, attached to a cartesian
# panel viewport's `meta` (surfaced by vellum as `scene_model()$panels$meta`) and
# read by a host (vellumwidget) to map device px <-> native <-> data. All values
# are in data/native space; the host pairs them with the panel's device-px rect
# (which vellum resolves) to build the affine. The carrier (panel `meta`) is
# vellum's `vignette("scene-contract")`; the fields below are this package's convention.
#
# Per-axis fields: `type` ("continuous" | "log10" | "discrete" | "binned" | "date"
# | "datetime"), `transform` (identity | log10 | sqrt | reverse — how native maps
# back to data), `data_lo`/`data_hi` (data-space extent), `native_lo`/`native_hi`
# (the expanded native domain == the panel viewport's xscale/yscale), `breaks` +
# `labels` (native tick positions and their formatted strings), `discrete`,
# `band_width`, and `time_unit` (NULL | "day" | "second" — the epoch unit for a
# date/datetime axis, so a host round-trips `as.numeric()` values back to R dates).
.axis_scale_desc <- function(sc) {
  if (is.null(sc)) {
    return(NULL)
  }
  # Report date/datetime as the axis type (vellumplot keeps the internal `type`
  # as "continuous"); `time_unit` disambiguates and gives the numeric epoch unit.
  rtype <- if (identical(sc$time_unit, "second")) {
    "datetime"
  } else if (identical(sc$time_unit, "day")) {
    "date"
  } else {
    sc$type
  }
  list(
    type = rtype,
    transform = sc$transform %||% "identity",
    data_lo = sc$data_range[1],
    data_hi = sc$data_range[2],
    native_lo = sc$domain[1],
    native_hi = sc$domain[2],
    breaks = as.numeric(sc$breaks),
    labels = as.character(sc$labels),
    discrete = isTRUE(sc$discrete),
    band_width = sc$band_width,
    time_unit = sc$time_unit
  )
}

# The `meta` list for a cartesian panel viewport: per-axis scale descriptors plus
# a `cartesian = TRUE` marker. NULL when neither axis has a descriptor (so the
# viewport carries no meta). Non-cartesian panels (polar / coord_trans / sf) get
# no `scales` meta at all, which the host reads as "decline data-space mapping".
.panel_scales_meta <- function(hsc, vsc) {
  x <- .axis_scale_desc(hsc)
  y <- .axis_scale_desc(vsc)
  if (is.null(x) && is.null(y)) {
    return(NULL)
  }
  list(scales = list(cartesian = TRUE, x = x, y = y))
}

# --- .draw_plot() compatibility guards --------------------------------------
# Each aborts on an unsupported combination up front (before drawing), with a
# clear message rather than a low-level failure deeper in the pipeline. Split out
# of .draw_plot() so the orchestrator reads as a sequence of named checks.

# Layout marks (sankey/hierarchy/chord) compute one global layout in an axis-free
# panel; they cannot be faceted or share a panel with other marks. Guard *before*
# building panels: faceting would resolve the flow/hierarchy per panel (splitting
# it) and fail obscurely inside the layout instead.
.check_layout_mark_solo <- function(spec) {
  if (
    !any(vapply(spec@layers, function(l) l@mark %in% .LAYOUT_MARKS, logical(1)))
  ) {
    return(invisible())
  }
  if (length(spec@layers) != 1L) {
    cli::cli_abort(
      "A layout plot (sankey/hierarchy/chord) takes no other layers."
    )
  }
  if (!is.null(spec@facet)) {
    cli::cli_abort(
      "A layout plot (sankey/hierarchy/chord) cannot be faceted."
    )
  }
  invisible()
}

# Marginal plots (add_marginal()) reserve tracks around a single panel and reuse
# its scales; they are incompatible with facets, non-Cartesian coords, and a
# locked aspect (which would distort the panel under `respect = TRUE`).
.check_marginal_compat <- function(spec, built, co, rt) {
  if (is.null(spec@marginal)) {
    return(invisible())
  }
  if (built$fa$R != 1L || built$fa$C != 1L) {
    cli::cli_abort(
      "{.fn add_marginal} is not supported with facets (single panel only)."
    )
  }
  if (!identical(co@kind, "cartesian")) {
    cli::cli_abort(
      "{.fn add_marginal} requires the default Cartesian coordinate system (no flip/polar/fixed/sf)."
    )
  }
  if (!is.null(rt[["aspect.ratio"]])) {
    cli::cli_abort(
      "{.fn add_marginal} is incompatible with a locked aspect ratio."
    )
  }
  invisible()
}

# coord_trans warps only the marks that route through the value-based position
# seam (.xy_units / .xy_path / .xy_area / .rect_units). Marks built from pre-made
# unit segments (rule/segment/interval/edges/boxplot) or rasters
# (datashade/raster) would misplace in the warped viewport, so refuse them rather
# than silently clip. bar/area also draw from a zero baseline, which a nonlinear
# value-axis transform cannot place (0 -> -Inf under log).
.check_coord_trans_marks <- function(spec, co) {
  if (!identical(co@kind, "trans")) {
    return(invisible())
  }
  ok_marks <- c(
    "point",
    "nodes",
    "line",
    "smooth",
    "ribbon",
    "area",
    "step",
    "text",
    "label",
    "image",
    "node_text",
    "tile",
    "rect",
    "bar",
    "violin",
    "ridgeline",
    "hex",
    "sf",
    "contour",
    "contour_filled"
  )
  marks <- unique(vapply(spec@layers, function(L) L@mark, character(1)))
  bad <- setdiff(marks, ok_marks)
  if (length(bad)) {
    cli::cli_abort(c(
      "{.fn coord_trans} does not yet support the {.val {bad}} mark{?s}.",
      i = "Supported: points, lines, areas/ribbons, bars, tiles, smooths, text, and similar.",
      i = "Segment/rule/interval, boxplot, edges, and raster/datashade marks are not warped yet."
    ))
  }
  # (An x-only warp leaves the y baseline linear, so allow it.)
  zero_marks <- intersect(marks, c("bar", "area"))
  if (length(zero_marks) && !.is_linear_trans(co@ytrans)) {
    cli::cli_abort(c(
      "{.fn coord_trans} cannot place the zero baseline of the {.val {zero_marks}} mark{?s} on a nonlinear {.field y} axis.",
      i = "Transform the scale instead: {.code scale_y_continuous(trans = ...)}."
    ))
  }
  invisible()
}

# Secondary axes (sec_axis()/dup_axis()) are v1-scoped to the default Cartesian
# system with shared position scales; reject the unsupported combinations up
# front with a clear message rather than mis-drawing.
.check_sec_axis_compat <- function(spec, built, flip, polar, trans) {
  sec_aes <- unique(vapply(
    Filter(function(s) !is.null(s@sec_axis), spec@scales),
    function(s) s@aesthetic,
    character(1)
  ))
  if (!length(sec_aes)) {
    return(invisible())
  }
  if (flip || polar || trans) {
    cli::cli_abort(c(
      "A secondary axis is only supported with the default Cartesian coordinate system.",
      i = "Remove {.fn coord_flip} / {.fn coord_polar} / {.fn coord_trans}, or drop the {.arg sec.axis}."
    ))
  }
  if (!is.null(spec@marginal)) {
    cli::cli_abort(
      "A secondary axis is not supported together with {.fn add_marginal}."
    )
  }
  if (built$free_x && "x" %in% sec_aes) {
    cli::cli_abort(
      "A secondary {.field x} axis requires a shared {.field x} scale (not {.code scales = \"free_x\"})."
    )
  }
  if (built$free_y && "y" %in% sec_aes) {
    cli::cli_abort(
      "A secondary {.field y} axis requires a shared {.field y} scale (not {.code scales = \"free_y\"})."
    )
  }
  invisible()
}

# sf layers: an sf mark implies a map coordinate system. If the user did not ask
# for coord_sf() explicitly, adopt it (matching geom_sf's auto-add) so the map is
# aspect-locked. Reproject every sf layer to the target CRS before training, and
# record whether that CRS is geographic (for the aspect ratio). Returns the
# (possibly coord-adopted, reprojected) spec plus the resolved coord and sf facts.
.dp_setup_coord <- function(spec) {
  co <- .coord_of(spec)
  sf_geographic <- FALSE
  sf_crs <- NULL
  if (.has_sf_layer(spec) && identical(co@kind, "cartesian")) {
    spec@coord <- CoordSpec(kind = "sf", xlim = co@xlim, ylim = co@ylim)
    co <- spec@coord
  }
  if (identical(co@kind, "sf")) {
    proj <- .project_sf_data(spec)
    spec <- proj$spec
    sf_geographic <- proj$geographic
    sf_crs <- proj$crs
  }
  list(spec = spec, co = co, sf_geographic = sf_geographic, sf_crs = sf_crs)
}

# Push the plot's outer scaffold and leave the cursor inside `panel-area`: an
# outer mm-margin grid around a single `null` cell (a grid, not an inset viewport,
# because vellum disallows the mixed npc/mm unit arithmetic an inset would need),
# the plot background behind the margins, the margin-inset centre cell, and the
# panel-area layout grid. Structural viewports carry stable `name`s so vellum's
# layout-debug tools work on a compiled plot: `render(scene, debug = TRUE)`
# outlines them and `why_size(scene, "<name>")` explains a region's extent (the
# "plot" / "plot-background" / "panel-area" / "panel-<r>-<c>" / "axis-*" /
# "strip-*" / "legend" / title-band names are the query keys). Leaves three
# viewports pushed; the caller pops them after drawing.
.dp_push_frame <- function(scene, lay, rt) {
  m <- rep_len(rt[["plot.margin"]] %||% 0, 4L) # (t, r, b, l) mm
  scene <- vellum::push(
    scene,
    vellum::vl_viewport(
      name = "plot",
      layout = vellum::grid_layout(
        c(
          vellum::vl_unit(m[4], "mm"),
          vellum::vl_unit(1, "null"),
          vellum::vl_unit(m[2], "mm")
        ),
        c(
          vellum::vl_unit(m[1], "mm"),
          vellum::vl_unit(1, "null"),
          vellum::vl_unit(m[3], "mm")
        )
      )
    )
  )

  # plot background fills the whole page region (behind the margins too)
  pbg <- rt[["plot.background"]]
  if (!.is_blank(pbg)) {
    scene <- vellum::push(
      scene,
      vellum::vl_viewport(
        row = 1,
        col = 1,
        rowspan = 3,
        colspan = 3,
        name = "plot-background"
      )
    )
    # decorative backdrop -> a PDF artifact, not a read-aloud figure
    scene <- vellum::draw(
      scene,
      vellum::rect_grob(gp = .el_gpar_rect(pbg), role = "presentation")
    )
    scene <- vellum::pop(scene)
  }

  # the real layout lives in the centre cell, inset by the margins
  scene <- vellum::push(scene, vellum::vl_viewport(row = 2, col = 2))
  vellum::push(
    scene,
    vellum::vl_viewport(
      name = "panel-area",
      layout = vellum::grid_layout(
        lay$widths,
        lay$heights,
        respect = lay$respect
      )
    )
  )
}

# Draw every panel: background + gridlines + marks, each in its own native
# scales, into the pushed `panel-area` grid. A polar panel uses a fixed symmetric
# [-1, 1] square scale (the polar context maps data to cartesian within it) and
# its own circular gridlines/labels; a coord_trans panel spans the warped domain;
# the cartesian branch (incl. coord_flip) carries the pan group, clip mask, and
# scale meta. See `dp` (assembled in `.draw_plot`) for the threaded state.
# Marks whose geometry legitimately extends past the panel bbox and so must not
# be clipped: graph decorations (self-loops, node markers) and physically-sized
# annotations (grob / sparkline insets pinned at a data point near the edge). A
# plot made *entirely* of these leaves the whole panel unclipped; a mixed plot
# clips the panel for its ordinary marks and draws only these in an unclipped
# sibling overlay (see `.dp_draw_panels`).
.BOUNDARY_MARKS <- c(
  "edges",
  "edge_bundle",
  "flow_map",
  "edge_text",
  "nodes",
  "node_pie",
  "node_text",
  "sparkline",
  "grob"
)

.dp_draw_panels <- function(scene, dp) {
  spec <- dp$spec
  co <- dp$co
  built <- dp$built
  lay <- dp$lay
  rt <- dp$rt
  flip <- dp$flip
  polar <- dp$polar
  trans <- dp$trans
  plot_sketch <- dp$plot_sketch
  panel_clip <- dp$panel_clip
  hscale <- dp$hscale
  vscale <- dp$vscale
  for (p in built$panels) {
    hsc <- hscale(p)
    vsc <- vscale(p)
    psc <- list(
      x = p$x_sc,
      y = p$y_sc,
      color = built$scales$color,
      size = built$scales$size,
      shape = built$scales$shape,
      pattern = built$scales$pattern,
      edge_width = built$scales$edge_width,
      linewidth = built$scales$linewidth,
      alpha = built$scales$alpha,
      linetype = built$scales$linetype,
      edge_color = built$scales$edge_color,
      edge_alpha = built$scales$edge_alpha,
      edge_linetype = built$scales$edge_linetype,
      flip = flip,
      polar = NULL,
      trans = NULL,
      sketch = plot_sketch,
      sf_geographic = isTRUE(built$sf_geographic),
      sf_crs = built$sf_crs
    )
    psc$graph <- .graph_caps(
      p$resolved,
      spec@edge_data,
      nrow(spec@data),
      psc
    )
    pname <- sprintf("panel-%d-%d", p$r, p$c)
    # Overlay descriptor (scales + clip flag) for the unclipped boundary-mark
    # sibling, filled per branch below and used by the shared tail.
    this_clip <- panel_clip
    ov_xscale <- hsc$domain
    ov_yscale <- vsc$domain
    if (polar) {
      ctx <- .polar_ctx(co, p$x_sc, p$y_sc)
      psc$polar <- ctx
      this_clip <- TRUE
      ov_xscale <- c(-1, 1)
      ov_yscale <- c(-1, 1)
      scene <- vellum::push(
        scene,
        vellum::vl_viewport(
          row = lay$panel_row[p$r],
          col = lay$panel_col[p$c],
          xscale = c(-1, 1),
          yscale = c(-1, 1),
          clip = TRUE,
          name = pname
        )
      )
      scene <- .draw_panel_polar(scene, ctx, rt)
    } else if (trans) {
      # Nonlinear display remap: the panel viewport spans the warped domain; marks
      # warp via `psc$trans`, gridlines/ticks via break/domain-warped scale copies.
      ctx <- .trans_ctx(co, p$x_sc, p$y_sc)
      psc$trans <- ctx
      ov_xscale <- ctx$x_dom
      ov_yscale <- ctx$y_dom
      scene <- vellum::push(
        scene,
        vellum::vl_viewport(
          row = lay$panel_row[p$r],
          col = lay$panel_col[p$c],
          xscale = ctx$x_dom,
          yscale = ctx$y_dom,
          clip = panel_clip,
          name = pname,
          # A linear (e.g. identity) coord_trans is effectively cartesian, so it
          # stays pannable and byte-identical to the plain plot; a nonlinear warp is
          # not (a linear pan/zoom wouldn't be axis-aware there).
          pannable = .is_linear_trans(co@xtrans) && .is_linear_trans(co@ytrans)
        )
      )
      scene <- .draw_panel_bg(
        scene,
        .warp_scale(hsc, ctx$x_map),
        .warp_scale(vsc, ctx$y_map),
        rt
      )
    } else {
      scene <- vellum::push(
        scene,
        vellum::vl_viewport(
          row = lay$panel_row[p$r],
          col = lay$panel_col[p$c],
          xscale = hsc$domain,
          yscale = vsc$domain,
          clip = panel_clip,
          # A clip_to()/set_mask() geometry mask, built in this panel's native
          # coords (NULL when none is set, leaving the common path untouched).
          mask = .clip_mask(spec@clip, hsc$domain, vsc$domain),
          name = pname,
          meta = .panel_scales_meta(hsc, vsc),
          # Clip-stable pan group so a host (vellumwidget) can pan/zoom this panel's
          # marks while its clip + the axes stay fixed (axis-aware zoom). Inert for
          # static rendering. This cartesian branch (incl. coord_flip) is pannable;
          # a linear coord_trans matches it (above); polar / nonlinear trans are not.
          pannable = TRUE
        )
      )
      scene <- .draw_panel_bg(scene, hsc, vsc, rt)
      if (identical(co@kind, "sf") && !is.null(co@graticule)) {
        scene <- .draw_graticule(
          scene,
          hsc,
          vsc,
          rt,
          built$sf_crs,
          isTRUE(built$sf_geographic),
          co@graticule
        )
      }
    }
    boundary <- vapply(
      p$resolved,
      function(L) L$mark %in% .BOUNDARY_MARKS,
      logical(1)
    )
    if (this_clip && any(boundary) && !all(boundary)) {
      # Mixed plot: draw the ordinary marks clipped to the panel, then the
      # boundary marks (grob/sparkline annotations, graph decorations) in an
      # unclipped sibling overlay at the same cell + scales, so a single inset
      # near the edge no longer disables clipping for the whole panel. Boundary
      # marks land on top of the ordinary ones (they are annotations/decorations).
      scene <- .compile_marks(
        scene,
        p$resolved[!boundary],
        psc,
        panel = pname,
        layer_index = which(!boundary)
      )
      scene <- vellum::pop(scene) # leave the clipped panel viewport
      scene <- vellum::push(
        scene,
        vellum::vl_viewport(
          row = lay$panel_row[p$r],
          col = lay$panel_col[p$c],
          xscale = ov_xscale,
          yscale = ov_yscale,
          clip = FALSE,
          name = paste0(pname, "-overlay")
        )
      )
      scene <- .compile_marks(
        scene,
        p$resolved[boundary],
        psc,
        panel = pname,
        layer_index = which(boundary)
      )
      scene <- vellum::pop(scene) # leave the overlay
    } else {
      scene <- .compile_marks(scene, p$resolved, psc, panel = pname)
      scene <- vellum::pop(scene)
    }
  }
  scene
}

# Cartesian axes down the left / along the bottom: per panel when the scale is
# free, otherwise once per panel row / column for alignment (the left gutter shows
# the vertical scale, the bottom gutter the horizontal; they swap under
# coord_flip). Secondary axes go on the opposite edge (shared-scale path only, so
# h == x and v == y). Polar panels draw their angular/radial labels inside the
# panel, so this whole block is skipped for them.
.dp_draw_axes <- function(scene, dp) {
  if (dp$polar) {
    return(scene)
  }
  built <- dp$built
  lay <- dp$lay
  rt <- dp$rt
  warp_h <- dp$warp_h
  warp_v <- dp$warp_v
  hscale <- dp$hscale
  vscale <- dp$vscale
  hshared <- dp$hshared
  vshared <- dp$vshared
  if (built$free_y) {
    for (p in built$panels) {
      scene <- .draw_y_axis(
        scene,
        lay$panel_row[p$r],
        lay$ylabels_col[p$c],
        warp_v(vscale(p)),
        rt
      )
    }
  } else {
    for (r in seq_len(lay$R)) {
      scene <- .draw_y_axis(
        scene,
        lay$panel_row[r],
        lay$ylabels_col[1],
        warp_v(vshared),
        rt
      )
    }
  }
  if (built$free_x) {
    for (p in built$panels) {
      scene <- .draw_x_axis(
        scene,
        lay$xlabels_row[p$r],
        lay$panel_col[p$c],
        warp_h(hscale(p)),
        rt
      )
    }
  } else {
    for (cc in seq_len(lay$C)) {
      scene <- .draw_x_axis(
        scene,
        lay$xlabels_row[1],
        lay$panel_col[cc],
        warp_h(hshared),
        rt,
        lay$xwrap_mm
      )
    }
  }
  # Secondary axes on the opposite edge. Only the shared-scale path can reach here
  # (free + sec is rejected above), so flip/polar/trans are already ruled out.
  if (!is.null(vshared$sec)) {
    for (r in seq_len(lay$R)) {
      scene <- .draw_y_axis_sec(
        scene,
        lay$panel_row[r],
        lay$y2labels_col,
        vshared$sec,
        rt
      )
    }
  }
  if (!is.null(hshared$sec)) {
    for (cc in seq_len(lay$C)) {
      scene <- .draw_x_axis_sec(
        scene,
        lay$x2labels_row,
        lay$panel_col[cc],
        hshared$sec,
        rt
      )
    }
  }
  scene
}

# Axis titles, the legend, and the plot title/subtitle/caption/tag bands. Axis
# titles span the panel block (suppressed under polar, which has no title
# gutters); a left/right legend takes its whole column spanning every row, a
# top/bottom legend its row spanning the panel columns; the plot bands span the
# full figure width.
.dp_draw_labels_legends <- function(scene, dp) {
  lay <- dp$lay
  rt <- dp$rt
  guides <- dp$guides
  vshared <- dp$vshared
  hshared <- dp$hshared
  labels <- dp$spec@labels
  if (!dp$polar) {
    scene <- .draw_y_title(
      scene,
      lay$panel_row[1],
      lay$ytitle_col,
      vshared$name,
      rt,
      rowspan = lay$panel_row[lay$R] - lay$panel_row[1] + 1
    )
    scene <- .draw_x_title(
      scene,
      lay$xtitle_row,
      lay$panel_col[1],
      hshared$name,
      rt,
      colspan = lay$panel_col[lay$C] - lay$panel_col[1] + 1
    )
    if (!is.null(vshared$sec) && !is.na(lay$y2title_col)) {
      scene <- .draw_y_title_sec(
        scene,
        lay$panel_row[1],
        lay$y2title_col,
        vshared$sec$name,
        rt,
        rowspan = lay$panel_row[lay$R] - lay$panel_row[1] + 1
      )
    }
    if (!is.null(hshared$sec) && !is.na(lay$x2title_row)) {
      scene <- .draw_x_title_sec(
        scene,
        lay$x2title_row,
        lay$panel_col[1],
        hshared$sec$name,
        rt,
        colspan = lay$panel_col[lay$C] - lay$panel_col[1] + 1
      )
    }
  }
  # A left/right legend takes its column spanning every row; a top/bottom legend
  # takes its row spanning the panel columns (centred under/over the panels).
  if (!is.na(lay$legend_col)) {
    # A vertical legend takes its whole column, spanning every row, and centres its
    # content block within — so a tall multi-guide legend has the full figure
    # height to work with instead of only the panel-row span.
    scene <- .draw_legends(
      scene,
      list(
        row = 1,
        col = lay$legend_col,
        rowspan = lay$nrow_total
      ),
      guides,
      rt,
      orient = "vertical",
      avail_h = lay$legend_avail_h
    )
  } else if (!is.na(lay$legend_row)) {
    scene <- .draw_legends(
      scene,
      list(
        row = lay$legend_row,
        col = lay$panel_col[1],
        colspan = lay$panel_col[lay$C] - lay$panel_col[1] + 1
      ),
      guides,
      rt,
      orient = "horizontal"
    )
  }

  # plot title / subtitle / caption bands + tag overlay (full width)
  if (!is.na(lay$title_row)) {
    scene <- .draw_title(
      scene,
      lay$title_row,
      lay$ncol_total,
      labels$title,
      rt,
      lay$band_w
    )
  }
  if (!is.na(lay$subtitle_row)) {
    scene <- .draw_subtitle(
      scene,
      lay$subtitle_row,
      lay$ncol_total,
      labels$subtitle,
      rt,
      lay$band_w
    )
  }
  if (!is.na(lay$caption_row)) {
    scene <- .draw_caption(
      scene,
      lay$caption_row,
      lay$ncol_total,
      labels$caption,
      rt,
      lay$band_w
    )
  }
  if (!is.na(lay$tag_row)) {
    scene <- .draw_tag(scene, lay$tag_row, lay$ncol_total, labels$tag, rt)
  }
  scene
}

# Compile one plot: spec -> build panels (facet split + resolve + train) ->
# layout -> guides + strips + per-panel marks. The single-panel case is a 1x1
# grid. `.draw_plot()` renders into the *current* viewport of `scene` (pushing
# and popping its own grid_layout), so a composition can place plots in cells.
.draw_plot <- function(scene, spec, frozen_scales = NULL) {
  if (!length(spec@layers)) {
    cli::cli_abort(
      "Nothing to draw: add a layer with {.fn mark_point} / {.fn mark_line}."
    )
  }
  # Interaction context for the mark emitter (single-plot path; the aligned
  # composition path sets it per cell). See `.set_interaction_ctx`.
  .set_interaction_ctx(spec)
  # Draw-order bands: layers sort by `z` (stable — insertion order within a band).
  # Ordinary marks all sit at z = 0 (unchanged); graph marks fix edges (1) under
  # edge labels (2) under nodes (3) under node labels (4) regardless of pipe order.
  zz <- vapply(spec@layers, function(l) l@z, integer(1))
  if (any(zz != 0L)) {
    spec@layers <- spec@layers[order(zz, seq_along(zz))]
  }
  # Clip the panel unless *every* layer is a boundary mark (a pure graph plot,
  # whose decorations legitimately extend past the bbox). A mixed plot still
  # clips: `.dp_draw_panels` draws its boundary marks in an unclipped overlay, so
  # one inset near the edge no longer un-clips the ordinary marks. (A plain plot
  # has no boundary marks, so this stays TRUE -- unchanged.)
  panel_clip <- !all(vapply(
    spec@layers,
    function(l) l@mark %in% .BOUNDARY_MARKS,
    logical(1)
  ))
  rt <- .resolve_theme(.theme_of(spec))

  sfc <- .dp_setup_coord(spec)
  spec <- sfc$spec
  co <- sfc$co
  sf_geographic <- sfc$sf_geographic
  sf_crs <- sfc$sf_crs

  .check_layout_mark_solo(spec)

  built <- .build_panels(spec, frozen_scales = frozen_scales)
  built$sf_geographic <- sf_geographic
  built$sf_crs <- sf_crs

  .check_marginal_compat(spec, built, co, rt)
  # Reserved resolved-theme keys: `.sketch` / `.interactive` are injected into the
  # resolved theme `rt` as a per-plot transport, but they are NOT theme members --
  # they come from the spec, not from theme(), so they bypass the theme tree
  # (absent from .DRAWN_LEAVES / .SETTINGS_DEFAULTS). The contract: reserved keys
  # carry a leading dot and are documented here, so a
  # drawer reading one is never confused with a real theme leaf. Read by the guide
  # drawers in compile-guides.R.
  #
  # Plot-wide hand-drawn default (from theme_sketch()); mark emitters fall back
  # to it when a layer sets no sketch of its own, and legend keys read it from
  # `rt` so they match a hand-drawn plot.
  plot_sketch <- .theme_sketch_default(spec)
  rt[[".sketch"]] <- plot_sketch
  # Interactivity: when any layer declares it, discrete legend swatches are tagged
  # (a keyed, hoverable element per series) so a host can highlight/select a whole
  # series from the legend. Read by the guide drawers (see `.tag_legend_swatch`).
  rt[[".interactive"]] <- .spec_interactive(spec)
  guides <- .legend_guides(built$scales)
  # coord_flip swaps which trained scale drives the horizontal vs vertical axis.
  flip <- identical(co@kind, "flip")
  polar <- identical(co@kind, "polar")
  trans <- identical(co@kind, "trans")
  # A geometry clip/mask is applied only in the cartesian panel branch; warn if
  # one was set under a coordinate system that cannot carry it.
  if (!is.null(spec@clip) && (polar || trans)) {
    cli::cli_warn(
      "{.fn clip_to} / {.fn set_mask} are only applied under cartesian coordinates; ignoring here."
    )
  }
  hscale <- function(p) .hv_roles(p$x_sc, p$y_sc, flip)$h # horizontal (bottom)
  vscale <- function(p) .hv_roles(p$x_sc, p$y_sc, flip)$v # vertical (left)
  shared_hv <- .hv_roles(built$scales$x, built$scales$y, flip)
  hshared <- shared_hv$h
  vshared <- shared_hv$v
  # coord_trans warps the display of the horizontal/vertical axes (no flip under
  # trans, so h = x, v = y). The axis drawers get break/domain-warped scale copies
  # (labels kept); marks warp via the per-panel context built in the panel loop.
  tfx <- if (trans) .resolve_coord_trans(co@xtrans, "x") else NULL
  tfy <- if (trans) .resolve_coord_trans(co@ytrans, "y") else NULL
  warp_h <- function(sc) if (trans) .warp_scale(sc, tfx) else sc
  warp_v <- function(sc) if (trans) .warp_scale(sc, tfy) else sc
  .check_coord_trans_marks(spec, co)
  .check_sec_axis_compat(spec, built, flip, polar, trans)
  lay <- .build_layout(
    built,
    guides,
    spec@labels,
    rt,
    flip,
    co,
    spec@marginal,
    page_height = spec@height,
    page_width = spec@width
  )

  # The draw context threaded to the panel/axis/legend drawers below, so each is
  # a `(scene, dp)` helper rather than a call with a dozen positional locals.
  # `hscale`/`vscale` (per-panel h/v role picks) and `warp_h`/`warp_v` (coord_trans
  # axis warps) are closures captured here; everything else is a plain value.
  dp <- list(
    spec = spec,
    co = co,
    built = built,
    lay = lay,
    rt = rt,
    guides = guides,
    flip = flip,
    polar = polar,
    trans = trans,
    plot_sketch = plot_sketch,
    panel_clip = panel_clip,
    hshared = hshared,
    vshared = vshared,
    hscale = hscale,
    vscale = vscale,
    warp_h = warp_h,
    warp_v = warp_v
  )

  scene <- .dp_push_frame(scene, lay, rt)

  scene <- .dp_draw_panels(scene, dp)

  # marginal distributions (add_marginal()): drawn into the reserved top/right
  # tracks, sharing the single panel's scales. Single panel, Cartesian only
  # (guarded above), so `built$panels[[1]]` is the panel.
  if (!is.null(spec@marginal)) {
    p1 <- built$panels[[1]]
    src <- .marginal_source(p1$resolved)
    scene <- .draw_marginals(
      scene,
      spec@marginal,
      src,
      lay,
      hscale(p1),
      vscale(p1),
      built$scales$color,
      rt,
      plot_sketch
    )
  }

  scene <- .dp_draw_axes(scene, dp)

  scene <- .draw_strips(scene, built, lay, rt)

  scene <- .dp_draw_labels_legends(scene, dp)

  scene <- vellum::pop(scene) # inner layout grid
  scene <- vellum::pop(scene) # centre cell
  scene <- vellum::pop(scene) # outer margin grid
  # What the grammar lint rules need from the trained scales, left on the scene
  # for them to read. Set here because this is where `built` already exists --
  # recomputing it would put a panel build on every render, including every frame
  # of an animation, to serve a check almost no render asks for. See R/lint.R.
  attr(scene, "vellumplot_lint_scales") <- .lint_scales_summary(built$scales)
  scene
}

# The scene (page) background, derived from the resolved theme's plot.background:
# a blank / NA-fill background yields a transparent canvas (so the plot sits on
# whatever surrounds it -- e.g. an embedded sparkline). The default theme keeps a
# white plot.background, so ordinary plots are unchanged.
.scene_bg <- function(rt) {
  pbg <- rt[["plot.background"]]
  if (.is_blank(pbg)) {
    return("transparent")
  }
  fill <- pbg@fill
  if (is.null(fill) || (length(fill) == 1L && is.na(fill))) {
    "transparent"
  } else {
    fill
  }
}

# The body of the as_vellum_scene() method for a single plot.
.compile_plot <- function(spec, frozen_scales = NULL) {
  .provenance_reset()
  scene <- vellum::vl_scene(
    width = spec@width,
    height = spec@height,
    dpi = spec@dpi,
    bg = .scene_bg(.resolve_theme(.theme_of(spec))),
    # Accessibility (WCAG 1.1.1): the plot title is the accessible name and the
    # alt text is the description. vellum emits these as an accessible SVG /
    # tagged PDF. Additive — geometry is unchanged. See R/alt.R.
    title = .alt_name(spec@labels),
    desc = .alt_desc_safe(spec)
  )
  scene <- .draw_plot(scene, spec, frozen_scales = frozen_scales)
  # Carry the row-key / scale-ref schema on the returned scene (DESIGN §4). Set
  # last so it survives to the caller; the `id` of each record matches a grob's
  # `data-vellum-id`. Additive only -- render() ignores it.
  attr(scene, "vellumplot_provenance") <- .provenance_snapshot()
  scene
}

# Facet strips: wrap draws one above each panel; grid draws column strips along
# the top and rotated row strips down the right.
.draw_strips <- function(scene, built, lay, rt) {
  fa <- built$fa
  if (fa$type == "wrap") {
    labs <- fa$wrap_labels
    for (i in seq_along(built$panels)) {
      p <- built$panels[[i]]
      scene <- .draw_strip(
        scene,
        lay$wrapstrip_row[p$r],
        lay$panel_col[p$c],
        labs[i],
        rt,
        name = sprintf("strip-%d-%d", p$r, p$c)
      )
    }
  } else if (fa$type == "grid") {
    if (!is.null(fa$col_labels)) {
      for (cc in seq_len(lay$C)) {
        scene <- .draw_strip(
          scene,
          lay$colstrip_row,
          lay$panel_col[cc],
          fa$col_labels[cc],
          rt,
          name = sprintf("strip-col-%d", cc)
        )
      }
    }
    if (!is.null(fa$row_labels)) {
      for (r in seq_len(lay$R)) {
        scene <- .draw_strip(
          scene,
          lay$panel_row[r],
          lay$rowstrip_col,
          fa$row_labels[r],
          rt,
          rot = 90,
          name = sprintf("strip-row-%d", r)
        )
      }
    }
  }
  scene
}

# Register the compiler on vellum's seam generic. Bind the generic to a local
# name first: the `method(g, cls) <- fn` replacement form would otherwise try to
# assign back into the `vellum` namespace. Mutating via the local binding still
# registers on the shared generic, so `vellum::render(plot, path)` dispatches here.
.as_vellum_scene <- vellum::as_vellum_scene
S7::method(.as_vellum_scene, PlotSpec) <- function(x, ...) {
  scene <- .compile_plot(x)
  # Label repulsion is a single post-compile pass over the built scene: the
  # engine solver moves each named label (and its background) by an absolute mm
  # offset and we draw the leaders. Coordinate-agnostic, so it spans facets /
  # polar / warped panels together. See R/repel.R.
  if (.any_repel(x)) {
    scene <- .repel_scene(scene, x)
  }
  scene
}

#' Render a plot to a file
#'
#' Compiles a [PlotSpec] into a [vellum::vl_scene()] and writes it. The output
#' format is taken from the file extension (`.png`, `.svg`, `.pdf`).
#' [vellum::render()] also works on a plot directly, dispatching through the
#' `as_vellum_scene()` seam.
#'
#' @param plot A [PlotSpec].
#' @param path Output file path.
#' @param text For SVG output, how text is written (see [vellum::render()]).
#' @param dpi Output resolution in dots per inch. `NULL` (default) uses the
#'   plot's authored resolution (set on [vplot()]); a number overrides it for
#'   this render, so the PNG's pixel dimensions become `width * dpi` by
#'   `height * dpi`. Ignored for `.svg`/`.pdf`, which are resolution-independent.
#' @param cvd Colour-vision-deficiency simulation for a `.png` render: one of
#'   `"none"` (default), `"protanopia"`, `"deuteranopia"`, `"tritanopia"`, or
#'   `"achromatopsia"`. Use it to preview how a palette reads for a
#'   colour-blind viewer (pair with [plot_lint()], which flags low contrast, and
#'   [scale_pattern()] / [pattern_hatch()], which add a redundant non-colour
#'   encoding). Simulation is raster-only; it is ignored for `.svg`/`.pdf`.
#' @return `path`, invisibly.
#' @examples
#' p <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
#' f <- tempfile(fileext = ".png")
#' render_plot(p, f)
#' render_plot(p, f, dpi = 300) # denser raster, same physical size
#' render_plot(p, f, cvd = "deuteranopia") # preview for red-green colour blindness
#' @export
render_plot <- function(plot, path, text = "native", dpi = NULL, cvd = "none") {
  if (
    !S7::S7_inherits(plot, PlotSpec) &&
      !S7::S7_inherits(plot, PlotComposition) &&
      !S7::S7_inherits(plot, VTable)
  ) {
    cli::cli_abort(
      "{.arg plot} must be a {.cls PlotSpec}, {.cls PlotComposition}, or {.cls VTable}."
    )
  }
  scene <- vellum::as_vellum_scene(plot)
  if (!is.null(dpi)) {
    .check_dpi(dpi)
    scene <- S7::set_props(scene, dpi = as.double(dpi))
  }
  vellum::render(scene, path, text = text, cvd = cvd)
}
