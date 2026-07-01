#' @include classes.R
NULL

# A coordinate system: `kind` ("cartesian" | "flip" | "fixed" | "polar" | "sf")
# plus optional view-window limits. `flip` swaps the x and y axes at render time;
# `xlim`/`ylim` zoom the view (clipping marks, never dropping data); `fixed`
# aspect-locks the panel so one data unit on y occupies `ratio` times the device
# length of one unit on x; `polar` projects one position aesthetic to angle and the other to
# radius (`theta` picks which is the angle; `start`/`direction` orient it; `rmin`
# is the inner-hole radius for a donut, as a fraction of the rim radius).
CoordSpec <- S7::new_class(
  "CoordSpec",
  package = "vellumplot",
  properties = list(
    kind = S7::new_property(S7::class_character, default = "cartesian"),
    xlim = S7::new_property(S7::class_any, default = NULL),
    ylim = S7::new_property(S7::class_any, default = NULL),
    ratio = S7::new_property(S7::class_any, default = NULL),
    theta = S7::new_property(S7::class_character, default = "x"),
    start = S7::new_property(S7::class_double, default = 0),
    direction = S7::new_property(S7::class_double, default = 1),
    rmin = S7::new_property(S7::class_double, default = 0),
    crs = S7::new_property(S7::class_any, default = NULL) # coord_sf target CRS
  )
)

# The coord for a spec, or the cartesian default.
.coord_of <- function(spec) spec@coord %||% CoordSpec(kind = "cartesian")

# Build the per-panel polar context from a coord and the panel's trained x/y
# scales. The theta aesthetic drives the angle, the other drives the radius.
# Angles follow ggplot2's familiar convention (zero at twelve o'clock, clockwise
# for direction = 1) but are emitted in vellum's radians (zero at three o'clock,
# CCW) via `ang_frac()`. The rim sits at `rmax` (< 1) so labels fit inside the
# clipped square panel; `rmin` is the donut hole. `theta_map`/`r_map` convert a
# trained-scale native value to an angle / panel-native radius; emitters and the
# polar guide drawer use these and never see the convention math.
.polar_ctx <- function(co, x_sc, y_sc) {
  theta_aes <- co@theta
  tsc <- if (theta_aes == "x") x_sc else y_sc
  rsc <- if (theta_aes == "x") y_sc else x_sc
  rmax <- 0.8
  rmin <- (co@rmin %||% 0) * rmax
  start <- co@start
  dir <- co@direction
  tdom <- tsc$domain
  rdom <- rsc$domain
  ang_frac <- function(frac) pi / 2 - dir * (start + 2 * pi * frac)
  list(
    theta_aes = theta_aes,
    start = start,
    direction = dir,
    theta_dom = tdom,
    r_dom = rdom,
    rmin = rmin,
    rmax = rmax,
    theta_sc = tsc,
    r_sc = rsc,
    ang_frac = ang_frac,
    theta_map = function(v) ang_frac((v - tdom[1]) / (tdom[2] - tdom[1])),
    r_map = function(v) {
      rmin + ((v - rdom[1]) / (rdom[2] - rdom[1])) * (rmax - rmin)
    }
  )
}

# Horizontal / vertical roles of an (x, y) pair under coord_flip: flip swaps the
# horizontal (bottom) and vertical (left) axes. The single source of truth for
# the swap, shared by the layout builder and the seam.
.hv_roles <- function(x, y, flip) {
  if (flip) list(h = y, v = x) else list(h = x, v = y)
}

#' Coordinate systems
#'
#' `coord_cartesian()` is the default Cartesian system; pass `xlim`/`ylim` to
#' **zoom** the view (out-of-range marks are clipped, not dropped — unlike a
#' `scale_*(limits=)`, which here behaves the same but is the data-scale's job).
#' `coord_flip()` swaps the x and y axes, e.g. for horizontal bars.
#' `coord_fixed()` / `coord_equal()` lock the aspect ratio so one data unit on
#' the y axis occupies `ratio` times the physical length of one unit on x (the
#' panel shrinks to fit and is centred). Coordinate limits take precedence over
#' scale limits.
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

#' Polar coordinates
#'
#' `coord_polar()` projects the panel into polar space: one position aesthetic
#' becomes the angle and the other the radius. With `theta = "x"` (the default)
#' the x aesthetic maps to angle and y to radius — a categorical bar chart
#' becomes a wind-rose / coxcomb, a line becomes a radar/spider trace, a point
#' cloud is positioned by `(angle, radius)`. With `theta = "y"` a stacked bar
#' becomes a pie (see also the [mark_pie()] / [mark_donut()] shortcuts). The
#' panel is locked to a square. Lines, areas, and ribbons are interpolated into
#' smooth arcs.
#'
#' @param plot A [PlotSpec].
#' @param theta Which position aesthetic drives the angle: `"x"` (default) or
#'   `"y"`.
#' @param start Angular offset of the zero position, in radians (`0` places the
#'   first value at twelve o'clock).
#' @param direction Winding direction: `1` for clockwise (default), `-1` for
#'   counter-clockwise.
#' @return The modified [PlotSpec].
#' @seealso [mark_pie()], [mark_donut()]
#' @examples
#' vplot(mtcars) |> mark_bar(x = factor(cyl)) |> coord_polar(theta = "x")
#' @export
coord_polar <- function(plot, theta = "x", start = 0, direction = 1) {
  .check_plot(plot)
  theta <- rlang::arg_match0(theta, c("x", "y"))
  if (!direction %in% c(1, -1)) {
    cli::cli_abort("{.arg direction} must be {.val 1} or {.val -1}.")
  }
  plot@coord <- CoordSpec(
    kind = "polar",
    theta = theta,
    start = as.double(start),
    direction = as.double(direction)
  )
  plot
}

#' Map coordinate system
#'
#' `coord_sf()` is the coordinate system for [mark_sf()] maps. Before scale
#' training it reprojects every `sf` layer to a common CRS (via
#' `sf::st_transform()`), so the renderer only ever sees projected Cartesian
#' coordinates, and it locks the panel aspect ratio so the map is not stretched:
#' `1` for a projected CRS, and the equirectangular correction
#' `1/cos(mean_latitude)` for unprojected longitude/latitude data.
#'
#' `sf` is an optional dependency (in `Suggests`); `coord_sf()` errors with an
#' install hint if it is not available at render time.
#'
#' @param plot A [PlotSpec].
#' @param crs Target coordinate reference system to project all layers into
#'   (anything `sf::st_crs()` accepts, e.g. an EPSG code, `"OGC:CRS84"`, or a
#'   proj/WKT string). `NULL` (default) uses the CRS of the first `sf` layer. For
#'   guaranteed longitude/latitude order, use `"OGC:CRS84"` rather than
#'   `EPSG:4326`. For choropleths, prefer an equal-area CRS.
#' @param xlim,ylim Length-2 view-window limits in the target CRS, or `NULL`.
#' @return The modified [PlotSpec].
#' @seealso [mark_sf()]
#' @examples
#' \dontrun{
#' nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
#' vplot(nc) |> mark_sf(fill = BIR74) |> coord_sf(crs = "OGC:CRS84")
#' }
#' @export
coord_sf <- function(plot, crs = NULL, xlim = NULL, ylim = NULL) {
  .check_plot(plot)
  plot@coord <- CoordSpec(kind = "sf", crs = crs, xlim = xlim, ylim = ylim)
  plot
}
