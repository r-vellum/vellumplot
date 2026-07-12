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
#' @param dpi Output resolution in dots per inch. The exported PNG's pixel
#'   dimensions are `width * dpi` by `height * dpi`; raising `dpi` yields a
#'   denser image at the same physical size. Overridable at render time via
#'   [render_plot()]'s `dpi` argument.
#' @return A [PlotSpec].
#' @examples
#' vplot(mtcars) |> mark_point(x = wt, y = mpg)
#' @export
vplot <- function(data, width = 6, height = 4, dpi = 96) {
  if (!is.data.frame(data)) {
    cli::cli_abort(
      "{.arg data} must be a data frame, not {.obj_type_friendly {data}}."
    )
  }
  .check_dim(width, "width")
  .check_dim(height, "height")
  .check_dpi(dpi)
  PlotSpec(
    data = data,
    width = as.double(width),
    height = as.double(height),
    dpi = as.double(dpi)
  )
}

# A page dimension / resolution must be a single positive finite number.
# `!is.finite()` rejects NA and Inf (only reached once length == 1, so it never
# sees a vector).
.check_dim <- function(x, arg, call = rlang::caller_env()) {
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x) || x <= 0) {
    cli::cli_abort(
      "{.arg {arg}} must be a single positive number, not {.obj_type_friendly {x}}.",
      call = call
    )
  }
  invisible(x)
}

.check_dpi <- function(dpi, call = rlang::caller_env()) {
  .check_dim(dpi, "dpi", call = call)
}
