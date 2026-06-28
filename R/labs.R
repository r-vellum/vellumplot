#' @include classes.R
NULL

#' Set plot titles and axis/legend labels
#'
#' Add plot-level text — a `title`, `subtitle`, `caption`, and `tag` — and
#' override the titles of individual axes and legends (`x`, `y`, `color`, `size`).
#' The title, subtitle, and tag are drawn in a band above the panels; the caption
#' in a band below. Repeated `labs()` calls merge, with later values winning.
#'
#' Axis/legend overrides set here are used unless a matching `scale_*(name = )`
#' is also given, which takes precedence. With neither, the title is derived from
#' the mapping.
#'
#' @param plot A [PlotSpec].
#' @param title,subtitle,caption,tag Plot-level text (or `NULL` to leave unset).
#' @param x,y,size Axis / legend title overrides for those aesthetics.
#' @param color,colour,fill Colour-scale title override; `colour` and `fill` are
#'   aliases for `color`.
#' @param ... Reserved; passing anything here is an error.
#' @return The modified [PlotSpec].
#' @examples
#' vplot(mtcars) |>
#'   mark_point(x = wt, y = mpg) |>
#'   labs(title = "Fuel economy", x = "Weight", y = "MPG")
#' @export
labs <- function(
  plot,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  tag = NULL,
  x = NULL,
  y = NULL,
  color = NULL,
  colour = NULL,
  fill = NULL,
  size = NULL,
  ...
) {
  .check_plot(plot)
  rlang::check_dots_empty()
  color <- color %||% colour %||% fill
  over <- list(
    title = title,
    subtitle = subtitle,
    caption = caption,
    tag = tag,
    x = x,
    y = y,
    color = color,
    size = size
  )
  over <- over[!vapply(over, is.null, logical(1))]
  plot@labels <- utils::modifyList(plot@labels, over)
  plot
}
