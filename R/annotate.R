#' @include classes.R marks.R
NULL

#' Add a one-off annotation
#'
#' Draw a single mark (or a short vector of them) from values supplied directly,
#' rather than mapping a data column. The values become a small inline layer
#' `data` frame, so annotations are independent of the plot data (and repeat on
#' every facet panel). Supported `geom`s: `"text"`, `"label"`, `"point"`,
#' `"segment"`, `"rect"`, `"grob"`, and `"sparkline"`.
#'
#' `"grob"` places an arbitrary vellum grob (or a [PlotSpec], e.g. a
#' [vsparkline()]) at data coordinate(s), in a box of physical size — a general
#' "glyph / chart in a panel" seam. `"sparkline"` is the convenience for the
#' common case: pass `values =` and it builds and places a [vsparkline()].
#'
#' @param plot A [PlotSpec].
#' @param geom The annotation geometry: one of `"text"`, `"label"`, `"point"`,
#'   `"segment"`, `"rect"`, `"grob"`, `"sparkline"`.
#' @param x,y Position (text/label/point/grob/sparkline; segment start).
#' @param xend,yend Segment end.
#' @param xmin,xmax,ymin,ymax Rectangle extent.
#' @param label Text to draw (text/label).
#' @param ... Constant aesthetics passed to the mark (e.g. `color`, `fill`,
#'   `alpha`, `size`). For `"grob"`: `grob =` (a vellum grob or `PlotSpec`). For
#'   `"sparkline"`: `values =` plus any [vsparkline()] argument. Both accept
#'   `width`/`height`/`units` (the box, default `20 x 6` mm) and
#'   `halign` (`"left"`/`"centre"`/`"right"`) / `valign`
#'   (`"top"`/`"centre"`/`"bottom"`) to anchor the box relative to `(x, y)`.
#' @return The modified [PlotSpec].
#' @examples
#' vplot(mtcars) |>
#'   mark_point(x = wt, y = mpg) |>
#'   annotate("text", x = 4, y = 30, label = "note") |>
#'   annotate("rect", xmin = 3, xmax = 4, ymin = 15, ymax = 20, alpha = 0.2)
#' \dontrun{
#' vplot(mtcars) |>
#'   mark_point(x = wt, y = mpg) |>
#'   annotate("sparkline", x = 4, y = 30, values = cumsum(rnorm(30)))
#' }
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
    c("text", "label", "point", "segment", "rect", "grob", "sparkline")
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
      # Carry the bounds through unchanged rather than converting to centre +
      # size here: `xmin = -Inf, xmax = Inf` would collapse to `x = NaN,
      # width = Inf` and the row would be dropped at draw time. The `rect`
      # emitter clamps infinite bounds to the panel edge, where the range is
      # known (see `.emit_rect`).
      d <- data.frame(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)
      mark_rect(
        plot,
        xmin = xmin,
        xmax = xmax,
        ymin = ymin,
        ymax = ymax,
        ...,
        data = d
      )
    },
    grob = {
      # Place an arbitrary vellum grob (or a PlotSpec, e.g. a vsparkline) at data
      # coordinate(s), sized in physical units -- a general "chart/glyph in a
      # panel" seam.
      .req(geom, x = x, y = y)
      args <- list(...)
      g <- args$grob
      if (is.null(g)) {
        cli::cli_abort(
          'annotate("grob") needs a {.arg grob} (a vellum grob or a {.cls PlotSpec})).'
        )
      }
      .annotate_grob(plot, x, y, g, args)
    },
    sparkline = {
      # A word-sized sparkline at data coordinate(s): build the vsparkline and
      # place it like a grob.
      .req(geom, x = x, y = y)
      args <- list(...)
      if (is.null(args$values)) {
        cli::cli_abort('annotate("sparkline") needs {.arg values}.')
      }
      place <- c("width", "height", "units", "halign", "valign")
      spark_args <- args[setdiff(names(args), c("values", place))]
      g <- do.call(vsparkline, c(list(values = args$values), spark_args))
      .annotate_grob(plot, x, y, g, args)
    }
  )
}

# Add a "grob" annotation layer: draw `g` (a vellum grob or a PlotSpec) at each
# (x, y), in a box of physical size `width` x `height` (in `units`), aligned by
# `halign`/`valign`. Carried through `stat_params`; drawn by `.emit_grob`.
.annotate_grob <- function(plot, x, y, g, args) {
  n <- max(length(x), length(y))
  d <- data.frame(x = rep_len(x, n), y = rep_len(y, n))
  .add_layer(
    plot,
    "grob",
    rlang::quos(x = x, y = y),
    stat_params = list(
      grob = g,
      width = args$width %||% 20,
      height = args$height %||% 6,
      units = args$units %||% "mm",
      halign = args$halign %||% "centre",
      valign = args$valign %||% "centre"
    ),
    data = d
  )
}

# A filled rectangle spanning [xmin, xmax] x [ymin, ymax]. Internal: the only
# entry point is `annotate("rect", ...)`. Unlike `mark_tile()` (centre + size),
# this keeps the corner bounds so a `-Inf`/`Inf` bound can be resolved to the
# panel edge at emit time.
mark_rect <- function(plot, ..., blend = NULL, sketch = NULL, data = NULL) {
  .check_plot(plot)
  .add_layer(
    plot,
    "rect",
    rlang::enquos(...),
    blend = blend,
    sketch = sketch,
    data = data
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
