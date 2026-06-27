#' @include classes.R
NULL

#' Start a plot specification
#'
#' `vplot()` begins a declarative, pipe-first plot. It captures the data and
#' page size and returns an inspectable [PlotSpec]; nothing is drawn until the
#' spec is compiled (via [render_plot()] or [vellum::as_vellum_scene()]). Build
#' the plot up with [mark_point()] / [mark_line()] / [mark_rule()] and the
#' `scale_*()` functions.
#'
#' @param data A data frame. Encoding expressions in `mark_*()` are evaluated
#'   against it with tidy evaluation.
#' @param width,height Page size in inches.
#' @return A [PlotSpec].
#' @examples
#' vplot(mtcars) |> mark_point(x = wt, y = mpg)
#' @export
vplot <- function(data, width = 6, height = 4) {
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame, not {.obj_type_friendly {data}}.")
  }
  PlotSpec(
    data = data,
    width = as.double(width),
    height = as.double(height)
  )
}
