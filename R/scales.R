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

# Shared builder for colour/fill scales.
.colour_scale <- function(
  plot,
  aesthetic,
  type,
  palette,
  breaks,
  labels,
  name
) {
  .check_plot(plot)
  .add_scale(
    plot,
    ScaleSpec(
      aesthetic = aesthetic,
      type = type,
      palette = palette,
      breaks = breaks,
      labels = labels,
      name = name
    )
  )
}

#' Position scales
#'
#' Declare a position scale to override the trained default. `scale_x_continuous()`
#' / `scale_y_continuous()` handle numeric (and date/time) axes;
#' `scale_x_discrete()` / `scale_y_discrete()` handle categorical (band) axes and
#' let you set the level order via `limits`.
#'
#' @param plot A [PlotSpec].
#' @param limits For continuous scales a numeric length-2 domain `c(min, max)`;
#'   for discrete scales a character vector of levels (sets order / subset).
#'   `NULL` trains from the data.
#' @param trans Transformation: `"identity"` (default), `"log10"`, `"sqrt"`,
#'   `"reverse"`, or a [scales::transform_log10()]-style transform object.
#' @param breaks,labels Explicit break positions (data units) and their labels,
#'   or `NULL` to compute them.
#' @param name Axis title, or `NULL` to derive from the encoding.
#' @return The modified [PlotSpec].
#' @examples
#' vplot(mtcars) |> mark_point(x = wt, y = mpg) |> scale_x_continuous(limits = c(0, 6))
#' vplot(mtcars) |> mark_point(x = wt, y = mpg) |> scale_y_continuous(trans = "sqrt")
#' @export
scale_x_continuous <- function(
  plot,
  limits = NULL,
  trans = "identity",
  breaks = NULL,
  labels = NULL,
  name = NULL
) {
  .check_plot(plot)
  .add_scale(
    plot,
    ScaleSpec(
      aesthetic = "x",
      type = "continuous",
      domain = limits,
      trans = trans,
      breaks = breaks,
      labels = labels,
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
  breaks = NULL,
  labels = NULL,
  name = NULL
) {
  .check_plot(plot)
  .add_scale(
    plot,
    ScaleSpec(
      aesthetic = "y",
      type = "continuous",
      domain = limits,
      trans = trans,
      breaks = breaks,
      labels = labels,
      name = name
    )
  )
}

#' @rdname scale_x_continuous
#' @export
scale_x_discrete <- function(plot, limits = NULL, name = NULL) {
  .check_plot(plot)
  .add_scale(
    plot,
    ScaleSpec(aesthetic = "x", type = "discrete", domain = limits, name = name)
  )
}

#' @rdname scale_x_continuous
#' @export
scale_y_discrete <- function(plot, limits = NULL, name = NULL) {
  .check_plot(plot)
  .add_scale(
    plot,
    ScaleSpec(aesthetic = "y", type = "discrete", domain = limits, name = name)
  )
}

#' Colour scales
#'
#' Declare a colour scale for the `color`/`fill` channel. Continuous data get a
#' perceptual ramp; discrete data get a qualitative palette. `scale_*_manual()`
#' sets exact colours, `scale_*_gradient()` a two-point ramp. The `fill` variants
#' are identical (colour and fill share one scale). A legend is drawn
#' automatically when colour is mapped.
#'
#' @param plot A [PlotSpec].
#' @param palette A vector of colours, or a single palette name passed to
#'   [grDevices::hcl.colors()] (e.g. `"viridis"`, `"Blues"`, `"Set 2"`; matched
#'   case/space-insensitively). `NULL` uses a sensible default.
#' @param values For `scale_*_manual()`, a vector of colours; if named, matched to
#'   data levels by name (unmatched levels draw grey).
#' @param low,high For `scale_*_gradient()`, the endpoint colours.
#' @param breaks,labels Explicit legend breaks / labels, or `NULL`.
#' @param name Legend title, or `NULL` to derive from the encoding.
#' @return The modified [PlotSpec].
#' @examples
#' vplot(mtcars) |>
#'   mark_point(x = wt, y = mpg, color = hp) |>
#'   scale_color_continuous(palette = "viridis")
#' @export
scale_color_continuous <- function(
  plot,
  palette = NULL,
  breaks = NULL,
  labels = NULL,
  name = NULL
) {
  .colour_scale(plot, "color", "continuous", palette, breaks, labels, name)
}

#' @rdname scale_color_continuous
#' @export
scale_color_discrete <- function(
  plot,
  palette = NULL,
  breaks = NULL,
  labels = NULL,
  name = NULL
) {
  .colour_scale(plot, "color", "discrete", palette, breaks, labels, name)
}

#' @rdname scale_color_continuous
#' @export
scale_color_manual <- function(plot, values, name = NULL) {
  .colour_scale(plot, "color", "discrete", values, NULL, NULL, name)
}

#' @rdname scale_color_continuous
#' @export
scale_color_gradient <- function(
  plot,
  low = "#132B43",
  high = "#56B1F7",
  name = NULL
) {
  .colour_scale(plot, "color", "continuous", c(low, high), NULL, NULL, name)
}

#' @rdname scale_color_continuous
#' @export
scale_fill_continuous <- function(
  plot,
  palette = NULL,
  breaks = NULL,
  labels = NULL,
  name = NULL
) {
  .colour_scale(plot, "fill", "continuous", palette, breaks, labels, name)
}

#' @rdname scale_color_continuous
#' @export
scale_fill_discrete <- function(
  plot,
  palette = NULL,
  breaks = NULL,
  labels = NULL,
  name = NULL
) {
  .colour_scale(plot, "fill", "discrete", palette, breaks, labels, name)
}

#' @rdname scale_color_continuous
#' @export
scale_fill_manual <- function(plot, values, name = NULL) {
  .colour_scale(plot, "fill", "discrete", values, NULL, NULL, name)
}

#' @rdname scale_color_continuous
#' @export
scale_fill_gradient <- function(
  plot,
  low = "#132B43",
  high = "#56B1F7",
  name = NULL
) {
  .colour_scale(plot, "fill", "continuous", c(low, high), NULL, NULL, name)
}

#' Size scale
#'
#' Declare the scale for a mapped `size` aesthetic: data values map linearly to a
#' point-size `range` (in mm). A size legend is drawn automatically.
#'
#' @param plot A [PlotSpec].
#' @param range Numeric length-2 output size range in mm (default `c(1, 6)`).
#' @param limits Numeric length-2 data domain, or `NULL` to train from the data.
#' @param breaks Explicit legend breaks, or `NULL`.
#' @param name Legend title, or `NULL` to derive from the encoding.
#' @return The modified [PlotSpec].
#' @examples
#' vplot(mtcars) |> mark_point(x = wt, y = mpg, size = hp) |> scale_size(range = c(1, 8))
#' @export
scale_size <- function(
  plot,
  range = c(1, 6),
  limits = NULL,
  breaks = NULL,
  name = NULL
) {
  .check_plot(plot)
  .add_scale(
    plot,
    ScaleSpec(
      aesthetic = "size",
      type = "continuous",
      domain = limits,
      range = range,
      breaks = breaks,
      name = name
    )
  )
}

#' Shape scale
#'
#' Declare the scale for a mapped (discrete) `shape` aesthetic. Levels cycle
#' through a default set of marker shapes unless `values` is given. A shape legend
#' is drawn automatically.
#'
#' @param plot A [PlotSpec].
#' @param values Character vector of shapes (each one of `"circle"`, `"square"`,
#'   `"triangle"`, `"diamond"`, `"plus"`, `"cross"`), or `NULL` for the default.
#' @param name Legend title, or `NULL` to derive from the encoding.
#' @return The modified [PlotSpec].
#' @examples
#' vplot(mtcars) |> mark_point(x = wt, y = mpg, shape = factor(cyl))
#' @export
scale_shape <- function(plot, values = NULL, name = NULL) {
  .check_plot(plot)
  .add_scale(
    plot,
    ScaleSpec(
      aesthetic = "shape",
      type = "discrete",
      palette = values,
      name = name
    )
  )
}
