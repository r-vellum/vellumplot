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
  flip <- identical(.coord_of(spec)@kind, "flip")
  hscale <- function(p) if (flip) p$y_sc else p$x_sc # horizontal (bottom)
  vscale <- function(p) if (flip) p$x_sc else p$y_sc # vertical (left)
  hshared <- if (flip) built$scales$y else built$scales$x
  vshared <- if (flip) built$scales$x else built$scales$y
  lay <- .build_layout(built, guides, spec@labels, rt, flip)

  scene <- vellum::push(
    scene,
    vellum::viewport(layout = vellum::grid_layout(lay$widths, lay$heights))
  )

  # plot background fills the whole page region (behind every panel/gutter)
  pbg <- rt[["plot.background"]]
  if (!.is_blank(pbg)) {
    scene <- vellum::push(
      scene,
      vellum::viewport(
        row = 1,
        col = 1,
        rowspan = length(lay$heights),
        colspan = length(lay$widths)
      )
    )
    scene <- vellum::draw(scene, vellum::rect_grob(gp = .el_gpar_rect(pbg)))
    scene <- vellum::pop(scene)
  }

  # panels: background + gridlines + marks, each in its own native scales
  for (p in built$panels) {
    hsc <- hscale(p)
    vsc <- vscale(p)
    psc <- list(
      x = p$x_sc,
      y = p$y_sc,
      color = built$scales$color,
      size = built$scales$size,
      shape = built$scales$shape,
      flip = flip
    )
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
    scene <- .compile_marks(scene, p$resolved, psc)
    scene <- vellum::pop(scene)
  }

  # axes: per panel when scales are free, otherwise once down the left / along
  # the bottom (drawn per panel row / column for alignment).
  # Left gutter shows the vertical scale; bottom gutter the horizontal scale
  # (these swap under coord_flip).
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

  scene <- .draw_strips(scene, built, lay, rt)

  # titles span the panel block; legend spans the panel rows
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
  if (!is.na(lay$legend_col)) {
    scene <- .draw_legends(
      scene,
      list(row = 1, col = lay$legend_col, rowspan = length(lay$heights)),
      guides,
      rt
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

  vellum::pop(scene)
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
