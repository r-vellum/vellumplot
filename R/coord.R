#' @include classes.R
NULL

# A coordinate system: `kind` ("cartesian" | "flip") plus optional view-window
# limits. `flip` swaps the x and y axes at render time; `xlim`/`ylim` zoom the
# view (clipping marks, never dropping data). Aspect lock (coord_fixed) is
# deferred until vellum gains an aspect-respect layout.
CoordSpec <- S7::new_class(
  "CoordSpec",
  package = "vellumplot",
  properties = list(
    kind = S7::new_property(S7::class_character, default = "cartesian"),
    xlim = S7::new_property(S7::class_any, default = NULL),
    ylim = S7::new_property(S7::class_any, default = NULL)
  )
)

# The coord for a spec, or the cartesian default.
.coord_of <- function(spec) spec@coord %||% CoordSpec(kind = "cartesian")

#' Coordinate systems
#'
#' `coord_cartesian()` is the default Cartesian system; pass `xlim`/`ylim` to
#' **zoom** the view (out-of-range marks are clipped, not dropped — unlike a
#' `scale_*(limits=)`, which here behaves the same but is the data-scale's job).
#' `coord_flip()` swaps the x and y axes, e.g. for horizontal bars. Coordinate
#' limits take precedence over scale limits.
#'
#' @param plot A [PlotSpec].
#' @param xlim,ylim Length-2 view-window limits, or `NULL` to use the trained
#'   range.
#' @return The modified [PlotSpec].
#' @examples
#' vplot(mtcars) |> mark_point(x = wt, y = mpg) |> coord_cartesian(xlim = c(2, 4))
#' vplot(mtcars) |> mark_bar(x = factor(cyl)) |> coord_flip()
#' @export
coord_cartesian <- function(plot, xlim = NULL, ylim = NULL) {
  .check_plot(plot)
  plot@coord <- CoordSpec(kind = "cartesian", xlim = xlim, ylim = ylim)
  plot
}

#' @rdname coord_cartesian
#' @export
coord_flip <- function(plot, xlim = NULL, ylim = NULL) {
  .check_plot(plot)
  plot@coord <- CoordSpec(kind = "flip", xlim = xlim, ylim = ylim)
  plot
}
