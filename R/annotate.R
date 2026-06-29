#' @include classes.R marks.R
NULL

#' Add a one-off annotation
#'
#' Draw a single mark (or a short vector of them) from values supplied directly,
#' rather than mapping a data column. The values become a small inline layer
#' `data` frame, so annotations are independent of the plot data (and repeat on
#' every facet panel). Supported `geom`s: `"text"`, `"label"`, `"point"`,
#' `"segment"`, and `"rect"`.
#'
#' @param plot A [PlotSpec].
#' @param geom The annotation geometry: one of `"text"`, `"label"`, `"point"`,
#'   `"segment"`, `"rect"`.
#' @param x,y Position (text/label/point; segment start).
#' @param xend,yend Segment end.
#' @param xmin,xmax,ymin,ymax Rectangle extent.
#' @param label Text to draw (text/label).
#' @param ... Constant aesthetics passed to the mark (e.g. `color`, `fill`,
#'   `alpha`, `size`).
#' @return The modified [PlotSpec].
#' @examples
#' vplot(mtcars) |>
#'   mark_point(x = wt, y = mpg) |>
#'   annotate("text", x = 4, y = 30, label = "note") |>
#'   annotate("rect", xmin = 3, xmax = 4, ymin = 15, ymax = 20, alpha = 0.2)
#' @export
annotate <- function(
  plot,
  geom,
  x = NULL,
  y = NULL,
  xend = NULL,
  yend = NULL,
  xmin = NULL,
  xmax = NULL,
  ymin = NULL,
  ymax = NULL,
  label = NULL,
  ...
) {
  .check_plot(plot)
  geom <- match.arg(
    geom,
    c("text", "label", "point", "segment", "rect")
  )
  switch(
    geom,
    text = ,
    label = {
      .req(geom, x = x, y = y, label = label)
      d <- data.frame(x = x, y = y, label = label, stringsAsFactors = FALSE)
      mk <- if (geom == "text") mark_text else mark_label
      mk(plot, x = x, y = y, label = label, ..., data = d)
    },
    point = {
      .req(geom, x = x, y = y)
      d <- data.frame(x = x, y = y)
      mark_point(plot, x = x, y = y, ..., data = d)
    },
    segment = {
      .req(geom, x = x, y = y, xend = xend, yend = yend)
      d <- data.frame(x = x, y = y, xend = xend, yend = yend)
      mark_segment(plot, x = x, y = y, xend = xend, yend = yend, ..., data = d)
    },
    rect = {
      .req(geom, xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)
      d <- data.frame(
        x = (xmin + xmax) / 2,
        y = (ymin + ymax) / 2,
        width = xmax - xmin,
        height = ymax - ymin
      )
      mark_tile(
        plot,
        x = x,
        y = y,
        width = width,
        height = height,
        ...,
        data = d
      )
    }
  )
}

# Error if any required annotation aesthetic is missing.
.req <- function(geom, ...) {
  args <- list(...)
  missing <- names(args)[vapply(args, is.null, logical(1))]
  if (length(missing)) {
    cli::cli_abort(
      "{.fn annotate} {.val {geom}} needs {.arg {missing}}."
    )
  }
}
