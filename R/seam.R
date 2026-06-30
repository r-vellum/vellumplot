#' @include classes.R coord.R compile-resolve.R compile-train.R compile-facet.R compile-layout.R compile-guides.R compile-marks.R
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
  rt <- .resolve_theme(.theme_of(spec))
  built <- .build_panels(spec)
  guides <- .legend_guides(built$scales)
  # coord_flip swaps which trained scale drives the horizontal vs vertical axis.
  co <- .coord_of(spec)
  flip <- identical(co@kind, "flip")
  polar <- identical(co@kind, "polar")
  hscale <- function(p) .hv_roles(p$x_sc, p$y_sc, flip)$h # horizontal (bottom)
  vscale <- function(p) .hv_roles(p$x_sc, p$y_sc, flip)$v # vertical (left)
  shared_hv <- .hv_roles(built$scales$x, built$scales$y, flip)
  hshared <- shared_hv$h
  vshared <- shared_hv$v
  lay <- .build_layout(built, guides, spec@labels, rt, flip, co)

  # Outer margin grid: absolute mm tracks around a single `null` cell holding the
  # real layout. (Done as a grid rather than an inset viewport because vellum
  # disallows the mixed npc/mm unit arithmetic an inset would need.)
  m <- rep_len(rt[["plot.margin"]] %||% 0, 4L) # (t, r, b, l) mm
  scene <- vellum::push(
    scene,
    vellum::viewport(
      layout = vellum::grid_layout(
        c(
          vellum::unit(m[4], "mm"),
          vellum::unit(1, "null"),
          vellum::unit(m[2], "mm")
        ),
        c(
          vellum::unit(m[1], "mm"),
          vellum::unit(1, "null"),
          vellum::unit(m[3], "mm")
        )
      )
    )
  )

  # plot background fills the whole page region (behind the margins too)
  pbg <- rt[["plot.background"]]
  if (!.is_blank(pbg)) {
    scene <- vellum::push(
      scene,
      vellum::viewport(row = 1, col = 1, rowspan = 3, colspan = 3)
    )
    scene <- vellum::draw(scene, vellum::rect_grob(gp = .el_gpar_rect(pbg)))
    scene <- vellum::pop(scene)
  }

  # the real layout lives in the centre cell, inset by the margins
  scene <- vellum::push(scene, vellum::viewport(row = 2, col = 2))
  scene <- vellum::push(
    scene,
    vellum::viewport(
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
      flip = flip,
      polar = NULL
    )
    if (polar) {
      ctx <- .polar_ctx(co, p$x_sc, p$y_sc)
      psc$polar <- ctx
      scene <- vellum::push(
        scene,
        vellum::viewport(
          row = lay$panel_row[p$r],
          col = lay$panel_col[p$c],
          xscale = c(-1, 1),
          yscale = c(-1, 1),
          clip = TRUE
        )
      )
      scene <- .draw_panel_polar(scene, ctx, rt)
    } else {
      scene <- vellum::push(
        scene,
        vellum::viewport(
          row = lay$panel_row[p$r],
          col = lay$panel_col[p$c],
          xscale = hsc$domain,
          yscale = vsc$domain,
          clip = TRUE
        )
      )
      scene <- .draw_panel_bg(scene, hsc, vsc, rt)
    }
    scene <- .compile_marks(scene, p$resolved, psc)
    scene <- vellum::pop(scene)
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
          vscale(p),
          rt
        )
      }
    } else {
      for (r in seq_len(lay$R)) {
        scene <- .draw_y_axis(
          scene,
          lay$panel_row[r],
          lay$ylabels_col[1],
          vshared,
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
          hscale(p),
          rt
        )
      }
    } else {
      for (cc in seq_len(lay$C)) {
        scene <- .draw_x_axis(
          scene,
          lay$xlabels_row[1],
          lay$panel_col[cc],
          hshared,
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
    scene <- .draw_legends(
      scene,
      list(row = 1, col = lay$legend_col, rowspan = length(lay$heights)),
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
  scene <- vellum::vl_scene(
    width = spec@width,
    height = spec@height,
    bg = "white"
  )
  .draw_plot(scene, spec)
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
        rt
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
          rt
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
          rot = 90
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
#' @return `path`, invisibly.
#' @examples
#' p <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
#' f <- tempfile(fileext = ".png")
#' render_plot(p, f)
#' @export
render_plot <- function(plot, path, text = "native") {
  if (
    !S7::S7_inherits(plot, PlotSpec) && !S7::S7_inherits(plot, PlotComposition)
  ) {
    cli::cli_abort(
      "{.arg plot} must be a {.cls PlotSpec} or {.cls PlotComposition}."
    )
  }
  vellum::render(vellum::as_vellum_scene(plot), path, text = text)
}
