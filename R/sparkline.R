#' @include classes.R vplot.R theme.R vgraph.R
NULL

# Length in `units` -> inches, for the sparkline's physical page size.
.spark_inches <- function(x, units) {
  switch(
    units,
    mm = x / 25.4,
    cm = x / 2.54,
    `in` = x,
    pt = x / 72,
    cli::cli_abort(
      "{.arg units} must be one of {.val {c('mm','cm','in','pt')}}."
    )
  )
}

#' Sparklines — tiny word-sized charts
#'
#' `vsparkline()` builds a compact, axis-free chart of a single numeric series,
#' sized in physical units (mm by default) so it reads as a *word-sized graphic*
#' (Tufte's sparkline) — for a table cell, a caption, or a dashboard tile. It is a
#' self-contained [PlotSpec]: render it with [render_plot()], drop it into a
#' composition with [inset()], or (soon) a table cell.
#'
#' Three shapes via `type`: a `"line"` trend (with optional dots on its extremes
#' or last point), a `"bar"` column micro-chart, and a `"winloss"` chart of equal
#' up/down bars about a baseline (for streaks of wins/losses, gains/drops).
#'
#' The chart fills its box with no axes, gridlines, labels, or legend.
#'
#' @param values A numeric vector (the series), length >= 2.
#' @param type `"line"` (default), `"bar"`, or `"winloss"`.
#' @param color Trend / bar colour. Default `"grey30"`.
#' @param points For `type = "line"`, which points get a dot: `"extremes"`
#'   (default -- the min and max), `"last"`, or `"none"`.
#' @param point_color Dot colour (default `"firebrick"`).
#' @param baseline For `"bar"`, the value bars grow from (default `0`); for
#'   `"winloss"`, the threshold separating a win (`>=`) from a loss (default `0`).
#' @param win_color,loss_color For `"winloss"`, the up / down bar colours
#'   (defaults blue / red).
#' @param linewidth Trend line width (default `1`).
#' @param point_size Dot diameter in mm (default `1.5`).
#' @param width,height,units Physical size of the sparkline; `units` is one of
#'   `"mm"` (default), `"cm"`, `"in"`, `"pt"`. Default `20 x 6` mm.
#' @param dpi Resolution for raster output.
#' @return A [PlotSpec].
#' @seealso [render_plot()], [inset()]
#' @examples
#' set.seed(1)
#' vsparkline(cumsum(rnorm(30)))
#' vsparkline(rpois(20, 5), type = "bar")
#' vsparkline(sample(c(-1, 1), 20, replace = TRUE), type = "winloss")
#' @export
vsparkline <- function(
  values,
  type = c("line", "bar", "winloss"),
  color = "grey30",
  points = c("extremes", "last", "none"),
  point_color = "firebrick",
  baseline = 0,
  win_color = "#2c7fb8",
  loss_color = "#d7301f",
  linewidth = 1,
  point_size = 1.5,
  width = 20,
  height = 6,
  units = "mm",
  dpi = 96
) {
  type <- match.arg(type)
  points <- match.arg(points)
  values <- as.numeric(values)
  if (length(values) < 2L || !any(is.finite(values))) {
    cli::cli_abort("{.arg values} must be a numeric vector of length >= 2.")
  }
  w_in <- .spark_inches(width, units)
  h_in <- .spark_inches(height, units)
  df <- data.frame(.i = seq_along(values), .v = values)
  p <- PlotSpec(
    data = df,
    coord = CoordSpec(kind = "cartesian"), # free aspect; chrome-free via the theme
    theme = .theme_vgraph(),
    width = as.double(w_in),
    height = as.double(h_in),
    dpi = as.double(dpi)
  )
  .add_layer(
    p,
    "sparkline",
    rlang::quos(x = .i, y = .v),
    stat_params = list(
      type = type,
      color = color,
      points = points,
      point_color = point_color,
      baseline = as.numeric(baseline),
      win_color = win_color,
      loss_color = loss_color,
      linewidth = linewidth,
      point_size = point_size
    )
  )
}
