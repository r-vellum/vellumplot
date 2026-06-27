#' @include classes.R
NULL

# Append a scale, replacing any existing scale for the same aesthetic
# (last-one-wins).
.add_scale <- function(plot, scale) {
  keep <- !vapply(
    plot@scales,
    function(s) identical(s@aesthetic, scale@aesthetic),
    logical(1)
  )
  plot@scales <- c(plot@scales[keep], list(scale))
  plot
}

#' Position scales
#'
#' Declare a position scale to override the trained default. Without a
#' declaration, continuous position scales are trained automatically from the
#' data (range across all layers, with a small expansion).
#'
#' @param plot A [PlotSpec].
#' @param limits Numeric length-2 domain `c(min, max)`, or `NULL` to train from
#'   the data.
#' @param trans Transformation: `"identity"` (default) or `"log10"`.
#' @param name Axis title, or `NULL` to derive from the encoding.
#' @return The modified [PlotSpec].
#' @examples
#' vplot(mtcars) |> mark_point(x = wt, y = mpg) |> scale_x_continuous(limits = c(0, 6))
#' @export
scale_x_continuous <- function(
  plot,
  limits = NULL,
  trans = "identity",
  name = NULL
) {
  .check_plot(plot)
  .add_scale(
    plot,
    ScaleSpec(
      aesthetic = "x",
      type = if (identical(trans, "log10")) "log10" else "continuous",
      domain = limits,
      name = name
    )
  )
}

#' @rdname scale_x_continuous
#' @export
scale_y_continuous <- function(
  plot,
  limits = NULL,
  trans = "identity",
  name = NULL
) {
  .check_plot(plot)
  .add_scale(
    plot,
    ScaleSpec(
      aesthetic = "y",
      type = if (identical(trans, "log10")) "log10" else "continuous",
      domain = limits,
      name = name
    )
  )
}

#' Colour scales
#'
#' Declare a colour scale for the `color`/`fill` channel. Continuous data get a
#' perceptual ramp; discrete data get a qualitative palette. A legend is drawn
#' automatically when colour is mapped.
#'
#' @param plot A [PlotSpec].
#' @param palette For `scale_color_continuous()`, a vector of ramp stop colours;
#'   for `scale_color_discrete()`, a vector of category colours. `NULL` uses a
#'   sensible default.
#' @param name Legend title, or `NULL` to derive from the encoding.
#' @return The modified [PlotSpec].
#' @examples
#' vplot(mtcars) |> mark_point(x = wt, y = mpg, color = hp) |> scale_color_continuous()
#' @export
scale_color_continuous <- function(plot, palette = NULL, name = NULL) {
  .check_plot(plot)
  .add_scale(
    plot,
    ScaleSpec(
      aesthetic = "color",
      type = "continuous",
      palette = palette,
      name = name
    )
  )
}

#' @rdname scale_color_continuous
#' @export
scale_color_discrete <- function(plot, palette = NULL, name = NULL) {
  .check_plot(plot)
  .add_scale(
    plot,
    ScaleSpec(
      aesthetic = "color",
      type = "discrete",
      palette = palette,
      name = name
    )
  )
}
