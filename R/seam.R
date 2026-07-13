#' @include classes.R coord.R compile-resolve.R compile-train.R
#' @include compile-facet.R compile-layout.R compile-guides.R compile-marks.R
#' @include marginal.R
NULL

# Compile one plot: spec -> build panels (facet split + resolve + train) ->
# layout -> guides + strips + per-panel marks. The single-panel case is a 1x1
# grid. `.draw_plot()` renders into the *current* viewport of `scene` (pushing
# and popping its own grid_layout), so a composition can place plots in cells.
.draw_plot <- function(scene, spec) {
  if (!length(spec@layers)) {
    cli::cli_abort(
      "Nothing to draw: add a layer with {.fn mark_point} / {.fn mark_line}."
    )
  }
  # Draw-order bands: layers sort by `z` (stable — insertion order within a band).
  # Ordinary marks all sit at z = 0 (unchanged); graph marks fix edges (1) under
  # nodes (2) under labels (3) regardless of pipe order.
  zz <- vapply(spec@layers, function(l) l@z, integer(1))
  if (any(zz != 0L)) {
    spec@layers <- spec@layers[order(zz, seq_along(zz))]
  }
  # A graph has no meaningful domain edges, and its mark decorations (self-loops,
  # mm node markers) legitimately extend past the layout bbox -- so don't clip the
  # panel for graph plots (ordinary plots still clip to their axes).
  panel_clip <- !any(vapply(
    spec@layers,
    function(l) l@mark %in% c("edges", "nodes", "node_text"),
    logical(1)
  ))
  rt <- .resolve_theme(.theme_of(spec))

  # sf layers: an sf mark implies a map coordinate system. If the user did not
  # ask for coord_sf() explicitly, adopt it (matching geom_sf's auto-add) so the
  # map is aspect-locked. Reproject every sf layer to the target CRS before
  # training, and record whether that CRS is geographic (for the aspect ratio).
  co <- .coord_of(spec)
  sf_geographic <- FALSE
  if (.has_sf_layer(spec) && identical(co@kind, "cartesian")) {
    spec@coord <- CoordSpec(kind = "sf", xlim = co@xlim, ylim = co@ylim)
    co <- spec@coord
  }
  if (identical(co@kind, "sf")) {
    proj <- .project_sf_data(spec)
    spec <- proj$spec
    sf_geographic <- proj$geographic
  }

  built <- .build_panels(spec)
  built$sf_geographic <- sf_geographic

  # Marginal plots (add_marginal()) reserve tracks around a single panel and reuse
  # its scales; they are incompatible with facets, non-Cartesian coords, and a
  # locked aspect (which would distort the panel under `respect = TRUE`).
  if (!is.null(spec@marginal)) {
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
  }
  # Reserved resolved-theme keys: `.sketch` / `.interactive` are injected into the
  # resolved theme `rt` as a per-plot transport, but they are NOT theme members --
  # they come from the spec, not from theme(), so they bypass the theme tree
  # (absent from .DRAWN_LEAVES / .SETTINGS_DEFAULTS). The contract: reserved keys
  # carry a leading dot and are documented here + in _docs/DESIGN.md 3.10, so a
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
  # coord_trans warps only the marks that route through the value-based position
  # seam (.xy_units / .xy_path / .xy_area / .rect_units). Marks built from
  # pre-made unit segments (rule/segment/interval/edges/boxplot) or rasters
  # (datashade/raster) would misplace in the warped viewport, so refuse them
  # rather than silently clip.
  if (trans) {
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
    # bar/area draw from a zero baseline, which a nonlinear value-axis transform
    # cannot place (0 -> -Inf under log). Refuse rather than draw a meaningless
    # log-baseline bar. (An x-only warp leaves the y baseline linear, so allow it.)
    zero_marks <- intersect(marks, c("bar", "area"))
    if (length(zero_marks) && !.is_linear_trans(co@ytrans)) {
      cli::cli_abort(c(
        "{.fn coord_trans} cannot place the zero baseline of the {.val {zero_marks}} mark{?s} on a nonlinear {.field y} axis.",
        i = "Transform the scale instead: {.code scale_y_continuous(trans = ...)}."
      ))
    }
  }
  lay <- .build_layout(built, guides, spec@labels, rt, flip, co, spec@marginal)

  # Outer margin grid: absolute mm tracks around a single `null` cell holding the
  # real layout. (Done as a grid rather than an inset viewport because vellum
  # disallows the mixed npc/mm unit arithmetic an inset would need.)
  # Structural viewports carry stable `name`s so vellum's layout-debug tools work
  # on a compiled plot: `render(scene, debug = TRUE)` outlines and labels them,
  # and `why_size(scene, "<name>")` explains a region's resolved extent. The
  # names below ("plot", "panel-area", "panel-<r>-<c>", "axis-*", "strip-*",
  # "legend", title bands) are the query keys.
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
    scene <- vellum::draw(scene, vellum::rect_grob(gp = .el_gpar_rect(pbg)))
    scene <- vellum::pop(scene)
  }

  # the real layout lives in the centre cell, inset by the margins
  scene <- vellum::push(scene, vellum::vl_viewport(row = 2, col = 2))
  scene <- vellum::push(
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

  # panels: background + gridlines + marks, each in its own native scales. A
  # polar panel uses a fixed symmetric [-1, 1] square scale (the polar context
  # maps data to cartesian within it) and its own circular gridlines/labels.
  for (p in built$panels) {
    hsc <- hscale(p)
    vsc <- vscale(p)
    psc <- list(
      x = p$x_sc,
      y = p$y_sc,
      color = built$scales$color,
      size = built$scales$size,
      shape = built$scales$shape,
      edge_width = built$scales$edge_width,
      alpha = built$scales$alpha,
      linetype = built$scales$linetype,
      flip = flip,
      polar = NULL,
      trans = NULL,
      sketch = plot_sketch
    )
    psc$graph <- .graph_caps(
      p$resolved,
      spec@edge_data,
      nrow(spec@data),
      psc
    )
    pname <- sprintf("panel-%d-%d", p$r, p$c)
    if (polar) {
      ctx <- .polar_ctx(co, p$x_sc, p$y_sc)
      psc$polar <- ctx
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
      scene <- vellum::push(
        scene,
        vellum::vl_viewport(
          row = lay$panel_row[p$r],
          col = lay$panel_col[p$c],
          xscale = ctx$x_dom,
          yscale = ctx$y_dom,
          clip = panel_clip,
          name = pname
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
          name = pname
        )
      )
      scene <- .draw_panel_bg(scene, hsc, vsc, rt)
    }
    scene <- .compile_marks(scene, p$resolved, psc, panel = pname)
    scene <- vellum::pop(scene)
  }

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

  # axes: per panel when scales are free, otherwise once down the left / along
  # the bottom (drawn per panel row / column for alignment).
  # Left gutter shows the vertical scale; bottom gutter the horizontal scale
  # (these swap under coord_flip). Polar panels draw their angular/radial labels
  # inside the panel itself (no gutters), so the cartesian axis block is skipped.
  if (!polar) {
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
          rt
        )
      }
    }
  } # !polar

  scene <- .draw_strips(scene, built, lay, rt)

  # titles span the panel block; legend spans the panel rows. Polar panels have
  # no axis-title gutters, so the x/y titles are suppressed.
  if (!polar) {
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
      orient = "vertical"
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
      spec@labels$title,
      rt
    )
  }
  if (!is.na(lay$subtitle_row)) {
    scene <- .draw_subtitle(
      scene,
      lay$subtitle_row,
      lay$ncol_total,
      spec@labels$subtitle,
      rt
    )
  }
  if (!is.na(lay$caption_row)) {
    scene <- .draw_caption(
      scene,
      lay$caption_row,
      lay$ncol_total,
      spec@labels$caption,
      rt
    )
  }
  if (!is.na(lay$tag_row)) {
    scene <- .draw_tag(scene, lay$tag_row, lay$ncol_total, spec@labels$tag, rt)
  }

  scene <- vellum::pop(scene) # inner layout grid
  scene <- vellum::pop(scene) # centre cell
  vellum::pop(scene) # outer margin grid
}

# The body of the as_vellum_scene() method for a single plot.
.compile_plot <- function(spec) {
  .provenance_reset()
  scene <- vellum::vl_scene(
    width = spec@width,
    height = spec@height,
    dpi = spec@dpi,
    bg = "white",
    # Accessibility (WCAG 1.1.1): the plot title is the accessible name and the
    # alt text is the description. vellum emits these as an accessible SVG /
    # tagged PDF. Additive — geometry is unchanged. See R/alt.R.
    title = .alt_name(spec@labels),
    desc = .alt_desc_safe(spec)
  )
  scene <- .draw_plot(scene, spec)
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
S7::method(.as_vellum_scene, PlotSpec) <- function(x, ...) .compile_plot(x)

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
#' @return `path`, invisibly.
#' @examples
#' p <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
#' f <- tempfile(fileext = ".png")
#' render_plot(p, f)
#' render_plot(p, f, dpi = 300) # denser raster, same physical size
#' @export
render_plot <- function(plot, path, text = "native", dpi = NULL) {
  if (
    !S7::S7_inherits(plot, PlotSpec) && !S7::S7_inherits(plot, PlotComposition)
  ) {
    cli::cli_abort(
      "{.arg plot} must be a {.cls PlotSpec} or {.cls PlotComposition}."
    )
  }
  scene <- vellum::as_vellum_scene(plot)
  if (!is.null(dpi)) {
    .check_dpi(dpi)
    scene <- S7::set_props(scene, dpi = as.double(dpi))
  }
  vellum::render(scene, path, text = text)
}
