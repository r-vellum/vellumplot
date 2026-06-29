#' @include classes.R
NULL

# A coordinate system: `kind` ("cartesian" | "flip" | "fixed") plus optional
# view-window limits. `flip` swaps the x and y axes at render time; `xlim`/`ylim`
# zoom the view (clipping marks, never dropping data); `fixed` aspect-locks the
# panel so `ratio` data units on y occupy the same device length as one on x.
CoordSpec <- S7::new_class(
  "CoordSpec",
  package = "vellumplot",
  properties = list(
    kind = S7::new_property(S7::class_character, default = "cartesian"),
    xlim = S7::new_property(S7::class_any, default = NULL),
    ylim = S7::new_property(S7::class_any, default = NULL),
    ratio = S7::new_property(S7::class_any, default = NULL)
  )
)

# The coord for a spec, or the cartesian default.
.coord_of <- function(spec) spec@coord %||% CoordSpec(kind = "cartesian")

#' Coordinate systems
#'
#' `coord_cartesian()` is the default Cartesian system; pass `xlim`/`ylim` to
#' **zoom** the view (out-of-range marks are clipped, not dropped — unlike a
#' `scale_*(limits=)`, which here behaves the same but is the data-scale's job).
#' `coord_flip()` swaps the x and y axes, e.g. for horizontal bars.
#' `coord_fixed()` / `coord_equal()` lock the aspect ratio so `ratio` data units
#' on the y axis occupy the same physical length as one unit on x (the panel
#' shrinks to fit and is centred). Coordinate limits take precedence over scale
#' limits.
#'
#' @param plot A [PlotSpec].
#' @param xlim,ylim Length-2 view-window limits, or `NULL` to use the trained
#'   range.
#' @param ratio Aspect ratio for `coord_fixed()`: the device length of one y unit
#'   relative to one x unit (default `1`).
#' @return The modified [PlotSpec].
#' @examples
#' vplot(mtcars) |> mark_point(x = wt, y = mpg) |> coord_cartesian(xlim = c(2, 4))
#' vplot(mtcars) |> mark_bar(x = factor(cyl)) |> coord_flip()
#' vplot(mtcars) |> mark_point(x = wt, y = mpg) |> coord_fixed()
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

#' @rdname coord_cartesian
#' @export
coord_fixed <- function(plot, ratio = 1, xlim = NULL, ylim = NULL) {
  .check_plot(plot)
  plot@coord <- CoordSpec(
    kind = "fixed",
    xlim = xlim,
    ylim = ylim,
    ratio = ratio
  )
  plot
}

#' @rdname coord_cartesian
#' @export
coord_equal <- function(plot, ratio = 1, xlim = NULL, ylim = NULL) {
  coord_fixed(plot, ratio = ratio, xlim = xlim, ylim = ylim)
}
