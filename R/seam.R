#' @include classes.R compile-resolve.R compile-train.R compile-layout.R compile-guides.R compile-marks.R
NULL

# The full compiler: spec -> resolve -> train -> layout -> guides -> marks ->
# vellum_scene. This is the body of the as_vellum_scene() method.
.compile_plot <- function(spec) {
  if (!length(spec@layers)) {
    cli::cli_abort("Nothing to draw: add a layer with {.fn mark_point} / {.fn mark_line}.")
  }
  resolved <- .resolve_layers(spec)
  scales <- .train_scales(spec, resolved)
  guides <- .legend_guides(scales)
  lay <- .build_layout(scales, guides)
  cells <- lay$cells

  scene <- vellum::vl_scene(width = spec@width, height = spec@height, bg = "white")
  scene <- vellum::push(scene, .vp(layout = vellum::grid_layout(lay$widths, lay$heights)))

  # panel: background + gridlines + marks, in native coordinates
  pc <- cells$panel
  scene <- vellum::push(scene, .vp(
    row = pc$row, col = pc$col,
    xscale = scales$x$domain, yscale = scales$y$domain, clip = TRUE, name = "panel"))
  scene <- .draw_panel_bg(scene, scales)
  scene <- .compile_marks(scene, resolved, scales)
  scene <- vellum::pop(scene)

  # guides
  scene <- .draw_y_axis(scene, cells$ylabel, scales)
  scene <- .draw_x_axis(scene, cells$xlabel, scales)
  scene <- .draw_y_title(scene, cells$ytitle, scales)
  scene <- .draw_x_title(scene, cells$xtitle, scales)
  if (!is.null(cells$legend)) scene <- .draw_legends(scene, cells$legend, guides)

  vellum::pop(scene)
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
  .check_plot(plot)
  vellum::render(vellum::as_vellum_scene(plot), path, text = text)
}
