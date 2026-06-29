#' @include classes.R elements.R theme-tree.R
NULL

# A theme is a *complete* element theme: a named list keyed by element slots
# (element_*() objects) plus scalar settings (see theme-tree.R). Presets build a
# complete theme; theme() / set_theme() element-merge a partial onto it. The
# compile pipeline resolves it once (.resolve_theme) before drawing.

# The default (gray) theme. Axis/legend titles inherit black from the text root;
# the tick/line/minor elements carry their intended values for the drawers.
.theme_gray_complete <- function() {
  list(
    text = element_text(colour = "black", size = 11),
    plot.title = element_text(size = 14, colour = "grey10"),
    plot.subtitle = element_text(size = 11, colour = "grey30"),
    plot.caption = element_text(size = 9, colour = "grey30"),
    plot.tag = element_text(size = 12, colour = "grey10"),
    legend.title = element_text(size = 10),
    axis.text = element_text(size = 9, colour = "grey20"),
    legend.text = element_text(size = 9, colour = "grey20"),
    strip.text = element_text(size = 9, colour = "grey10"),
    line = element_line(colour = "white", linewidth = 1),
    panel.grid.minor = element_line(linewidth = 0.5),
    axis.ticks = element_line(colour = "grey20"),
    axis.line = element_blank(),
    rect = element_rect(fill = NA, colour = NA),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "grey92", colour = NA),
    strip.background = element_rect(fill = "grey85", colour = NA),
    legend.background = element_blank(),
    legend.key = element_blank()
  )
}

.theme_default <- function() .theme_gray_complete()

# The resolved-or-default complete theme for a spec.
.theme_of <- function(spec) spec@theme %||% .theme_default()

# Legacy NA-as-"draw nothing" colour -> element (for set_theme back-compat).
.legacy_rect <- function(col) {
  if (is.na(col)) element_blank() else element_rect(fill = col, colour = NA)
}
.legacy_line <- function(col) {
  if (is.na(col)) element_blank() else element_line(colour = col)
}

#' Plot themes
#'
#' Control the non-data look of a plot. `theme_gray()` is the default (grey panel,
#' white gridlines); `theme_minimal()` drops the panel fill for light gridlines on
#' the page; `theme_bw()` is a white panel with light grey gridlines;
#' `theme_classic()` has axis lines and no gridlines; `theme_void()` strips
#' everything but the marks, legend, and titles.
#'
#' [theme()] overrides individual elements on top of the current theme using
#' [element_text()] / [element_line()] / [element_rect()] / [element_blank()].
#' `set_theme()` is a small back-compatible shortcut for the most common colours.
#'
#' @param plot A [PlotSpec].
#' @param ... Named theme elements, e.g. `plot.title = element_text(size = 16)`,
#'   `panel.grid.minor = element_blank()`, or settings like
#'   `legend.position = "none"`.
#' @param panel_bg,grid_col,label_col,strip_bg Colours (or `NA` to draw nothing)
#'   for the panel background, gridlines, axis-label/legend text, and facet strip
#'   background.
#' @return The modified [PlotSpec].
#' @examples
#' vplot(mtcars) |> mark_point(x = wt, y = mpg) |> theme_minimal()
#' vplot(mtcars) |>
#'   mark_point(x = wt, y = mpg) |>
#'   theme(panel.grid.minor = element_blank(), legend.position = "none")
#' @export
theme_gray <- function(plot) {
  .check_plot(plot)
  plot@theme <- .theme_gray_complete()
  plot
}

#' @rdname theme_gray
#' @export
theme_minimal <- function(plot) {
  .check_plot(plot)
  plot@theme <- .merge_theme(
    .theme_gray_complete(),
    list(
      panel.background = element_blank(),
      panel.grid = element_line(colour = "grey92"),
      strip.background = element_blank(),
      axis.text = element_text(colour = "grey30"),
      legend.text = element_text(colour = "grey30"),
      axis.ticks = element_blank()
    )
  )
  plot
}

#' @rdname theme_gray
#' @export
theme_bw <- function(plot) {
  .check_plot(plot)
  plot@theme <- .merge_theme(
    .theme_gray_complete(),
    list(
      panel.background = element_rect(fill = "white", colour = NA),
      panel.grid = element_line(colour = "grey90")
    )
  )
  plot
}

#' @rdname theme_gray
#' @export
theme_classic <- function(plot) {
  .check_plot(plot)
  plot@theme <- .merge_theme(
    .theme_gray_complete(),
    list(
      panel.background = element_rect(fill = "white", colour = NA),
      panel.grid = element_blank(),
      axis.line = element_line(colour = "black"),
      axis.ticks = element_line(colour = "black"),
      strip.background = element_blank()
    )
  )
  plot
}

#' @rdname theme_gray
#' @export
theme_void <- function(plot) {
  .check_plot(plot)
  plot@theme <- .merge_theme(
    .theme_gray_complete(),
    list(
      panel.background = element_blank(),
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      axis.line = element_blank(),
      strip.background = element_blank()
    )
  )
  plot
}

#' @rdname theme_gray
#' @export
theme <- function(plot, ...) {
  .check_plot(plot)
  args <- rlang::list2(...)
  .validate_theme_args(args)
  plot@theme <- .merge_theme(.theme_of(plot), args)
  plot
}

#' @rdname theme_gray
#' @export
set_theme <- function(
  plot,
  panel_bg = NULL,
  grid_col = NULL,
  label_col = NULL,
  strip_bg = NULL
) {
  .check_plot(plot)
  over <- list()
  if (!is.null(panel_bg)) {
    over$panel.background <- .legacy_rect(panel_bg)
  }
  if (!is.null(grid_col)) {
    over$panel.grid <- .legacy_line(grid_col)
  }
  if (!is.null(label_col)) {
    over$axis.text <- element_text(colour = label_col)
    over$legend.text <- element_text(colour = label_col)
  }
  if (!is.null(strip_bg)) {
    over$strip.background <- .legacy_rect(strip_bg)
  }
  plot@theme <- .merge_theme(.theme_of(plot), over)
  plot
}
