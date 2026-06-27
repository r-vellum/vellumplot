#' @include classes.R
NULL

# The default theme (the look v1 shipped with). A theme is a plain named list of
# visual settings consumed by the guide/panel drawers. `NA` for `panel_bg` /
# `grid_col` / `strip_bg` means "draw nothing".
.theme_default <- function() {
  list(panel_bg = "grey92", grid_col = "white", label_col = "grey20",
       strip_bg = "grey85")
}

# The resolved theme for a spec (its theme, or the default).
.theme_of <- function(spec) spec@theme %||% .theme_default()

#' Plot themes
#'
#' Control the non-data look of a plot. `theme_gray()` is the default (grey panel,
#' white gridlines); `theme_minimal()` drops the panel fill for light gridlines on
#' the page; `theme_bw()` is a white panel with light grey gridlines.
#' `set_theme()` overrides individual settings on top of the current theme.
#'
#' @param plot A [PlotSpec].
#' @param panel_bg,grid_col,label_col,strip_bg Colours (or `NA` to draw nothing)
#'   for the panel background, gridlines, axis-label/legend text, and facet strip
#'   background.
#' @return The modified [PlotSpec].
#' @examples
#' vplot(mtcars) |> mark_point(x = wt, y = mpg) |> theme_minimal()
#' vplot(mtcars) |> mark_point(x = wt, y = mpg) |> set_theme(panel_bg = "white")
#' @export
theme_gray <- function(plot) {
  .check_plot(plot)
  plot@theme <- .theme_default()
  plot
}

#' @rdname theme_gray
#' @export
theme_minimal <- function(plot) {
  .check_plot(plot)
  plot@theme <- list(panel_bg = NA, grid_col = "grey92", label_col = "grey30",
                     strip_bg = NA)
  plot
}

#' @rdname theme_gray
#' @export
theme_bw <- function(plot) {
  .check_plot(plot)
  plot@theme <- list(panel_bg = "white", grid_col = "grey90", label_col = "grey20",
                     strip_bg = "grey85")
  plot
}

#' @rdname theme_gray
#' @export
set_theme <- function(plot, panel_bg = NULL, grid_col = NULL, label_col = NULL,
                      strip_bg = NULL) {
  .check_plot(plot)
  base <- .theme_of(plot)
  over <- list(panel_bg = panel_bg, grid_col = grid_col,
               label_col = label_col, strip_bg = strip_bg)
  over <- over[!vapply(over, is.null, logical(1))]
  plot@theme <- utils::modifyList(base, over)
  plot
}
