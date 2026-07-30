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
    end = S7::new_property(S7::class_any, default = NULL), # polar/radial arc end angle (rad); NULL = full circle
    direction = S7::new_property(S7::class_double, default = 1),
    rmin = S7::new_property(S7::class_double, default = 0),
    crs = S7::new_property(S7::class_any, default = NULL), # coord_sf target CRS
    graticule = S7::new_property(S7::class_any, default = NULL), # coord_sf lon/lat grid spec (list) or NULL
    # coord_trans per-axis display transform (name or scales::transform_* object).
    xtrans = S7::new_property(S7::class_any, default = "identity"),
    ytrans = S7::new_property(S7::class_any, default = "identity")
  )
)

# The coord for a spec, or the cartesian default.
.coord_of <- function(spec) spec@coord %||% CoordSpec(kind = "cartesian")

# Validate a central-hole radius fraction. Shared by coord_radial() and the
# vhierarchy() sunburst so they reject the same range identically.
.check_inner_radius <- function(
  x,
  arg = "inner_radius",
  call = rlang::caller_env()
) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || x < 0 || x >= 1) {
    cli::cli_abort(
      "{.arg {arg}} must be a single number in {.code [0, 1)}.",
      call = call
    )
  }
}

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
  # Angular sweep: a full turn (coord_polar), or `end - start` for a partial arc
  # (coord_radial(end=), e.g. a half-circle gauge).
  sweep <- if (is.null(co@end)) 2 * pi else (co@end - start)
  ang_frac <- function(frac) pi / 2 - dir * (start + sweep * frac)
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
    # Guard degenerate (single-value / empty) domains: a zero-width span would
    # otherwise divide by zero and emit non-finite angles/radii. Map everything
    # to the domain start (fraction 0) in that case.
    theta_map = function(v) {
      span <- tdom[2] - tdom[1]
      ang_frac(if (isTRUE(span > 0)) (v - tdom[1]) / span else 0)
    },
    r_map = function(v) {
      span <- rdom[2] - rdom[1]
      frac <- if (isTRUE(span > 0)) (v - rdom[1]) / span else 0
      rmin + frac * (rmax - rmin)
    }
  )
}

# --- coord_trans (nonlinear display remap) ---------------------------------
# Unlike scale_*(trans=), which transforms the scale (breaks chosen in
# transformed space, data rescaled), coord_trans leaves the trained scale alone
# and warps only the final data->position mapping: gridlines/ticks relocate but
# their labels keep the original data values, and straight lines curve.

# Resolve a coord_trans axis argument to a forward transform function `f(x)`.
# Reuses the position-transform registry (`.TRANSFORMS`, compile-train.R); also
# accepts a `scales::transform_*()` object (its `$transform`). NULL -> identity.
.resolve_coord_trans <- function(t, arg) {
  if (is.null(t)) {
    return(function(x) x)
  }
  if (is.character(t)) {
    tr <- .TRANSFORMS[[t]]
    if (is.null(tr)) {
      cli::cli_abort(c(
        "Unknown {.arg {arg}} transform {.val {t}}.",
        i = "Use one of {.or {.val {names(.TRANSFORMS)}}} or a {.fn scales::transform_*} object."
      ))
    }
    return(tr$transform)
  }
  if (is.list(t) && is.function(t$transform)) {
    return(t$transform)
  }
  cli::cli_abort(
    "{.arg {arg}} must be a transform name or a {.fn scales::transform_*} object."
  )
}

# Is an axis transform linear (identity/reverse)? Linear axes need no polyline
# densification (a straight data segment stays straight); nonlinear ones curve.
.is_linear_trans <- function(t) {
  is.null(t) || (is.character(t) && t %in% c("identity", "reverse"))
}

# Per-panel coord_trans context: the forward maps and the warped native domain
# (which the panel viewport uses). `x_lin`/`y_lin` gate densification.
.trans_ctx <- function(co, x_sc, y_sc) {
  fx <- .resolve_coord_trans(co@xtrans, "x")
  fy <- .resolve_coord_trans(co@ytrans, "y")
  # An out-of-domain transform (e.g. log of a non-positive value) yields NaN with
  # a warning; suppress it here and report the real problem via the finite check.
  xdom <- suppressWarnings(fx(x_sc$domain))
  ydom <- suppressWarnings(fy(y_sc$domain))
  if (!all(is.finite(xdom))) {
    cli::cli_abort(c(
      "The x range is outside the {.fn coord_trans} x transform's domain.",
      i = "A log transform, for example, needs strictly positive values."
    ))
  }
  if (!all(is.finite(ydom))) {
    cli::cli_abort(c(
      "The y range is outside the {.fn coord_trans} y transform's domain.",
      i = "A log transform, for example, needs strictly positive values."
    ))
  }
  list(
    x_map = fx,
    y_map = fy,
    x_dom = xdom,
    y_dom = ydom,
    x_lin = .is_linear_trans(co@xtrans),
    y_lin = .is_linear_trans(co@ytrans)
  )
}

# A display copy of a trained scale with its break/domain positions warped by `f`
# but its labels kept — the panel background, gridlines, and axis drawers read
# this so ticks relocate while their text stays at the original data values.
.warp_scale <- function(sc, f) {
  sc$domain <- f(sc$domain)
  sc$breaks <- f(sc$breaks)
  sc
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
  .check_continuous_limits(xlim, "xlim")
  .check_continuous_limits(ylim, "ylim")
  plot@coord <- CoordSpec(kind = "cartesian", xlim = xlim, ylim = ylim)
  plot
}

#' @rdname coord_cartesian
#' @export
coord_flip <- function(plot, xlim = NULL, ylim = NULL) {
  .check_plot(plot)
  .check_continuous_limits(xlim, "xlim")
  .check_continuous_limits(ylim, "ylim")
  plot@coord <- CoordSpec(kind = "flip", xlim = xlim, ylim = ylim)
  plot
}

#' @rdname coord_cartesian
#' @export
coord_fixed <- function(plot, ratio = 1, xlim = NULL, ylim = NULL) {
  .check_plot(plot)
  .check_continuous_limits(xlim, "xlim")
  .check_continuous_limits(ylim, "ylim")
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

#' @rdname coord_polar
#'
#' @description
#' `coord_radial()` is a fuller polar system (ggplot2 3.5's name): besides
#' `theta`/`start`/`direction` it takes `end` to sweep only a **partial arc**
#' (e.g. a half-circle gauge) and `inner_radius` to open a **donut hole**. With
#' `end = NULL` and `inner_radius = 0` it is identical to `coord_polar()`.
#'
#' @param end Arc end angle in radians; `NULL` (default) sweeps a full turn from
#'   `start`. Set it for a partial arc — e.g. `start = -pi/2, end = pi/2` for a
#'   semicircular gauge.
#' @param inner_radius Radius of the central hole as a fraction of the outer
#'   radius (`0`–`1`); `0` (default) is a filled disc, `> 0` a donut/ring.
#' @examples
#' vplot(mtcars) |>
#'   mark_bar(x = factor(cyl)) |>
#'   coord_radial(theta = "x", inner_radius = 0.3)
#' @export
coord_radial <- function(
  plot,
  theta = "x",
  start = 0,
  end = NULL,
  direction = 1,
  inner_radius = 0
) {
  .check_plot(plot)
  theta <- rlang::arg_match0(theta, c("x", "y"))
  if (!direction %in% c(1, -1)) {
    cli::cli_abort("{.arg direction} must be {.val 1} or {.val -1}.")
  }
  .check_inner_radius(inner_radius)
  plot@coord <- CoordSpec(
    kind = "polar",
    theta = theta,
    start = as.double(start),
    end = if (is.null(end)) NULL else as.double(end),
    direction = as.double(direction),
    rmin = as.double(inner_radius)
  )
  plot
}

#' Transformed coordinate system
#'
#' `coord_trans()` applies a nonlinear transform to the **display** of one or both
#' position axes, *after* the scale has trained. This differs from a
#' `scale_*(trans=)`: a scale transform rescales the data and picks its breaks in
#' the transformed space (so a log scale is labelled `1, 10, 100`), whereas
#' `coord_trans()` keeps the trained breaks at their original data values and only
#' warps *where* they are drawn — so the axis is still labelled with the raw
#' values, the gridlines bunch up, and straight lines curve. Use it to show data
#' on, say, a log display without relabelling the axis in powers of ten.
#'
#' The transform is separable per axis. Each of `x`/`y` is a transform name
#' (`"identity"`, `"log10"`, `"sqrt"`) or a `scales::transform_*()` object. It is
#' intended for continuous position axes; `"reverse"` is better expressed as
#' `scale_*(trans = "reverse")`.
#'
#' @param plot A [PlotSpec].
#' @param x,y Display transform for that axis (default `"identity"`).
#' @return The modified [PlotSpec].
#' @examples
#' vplot(mtcars) |> mark_point(x = wt, y = mpg) |> coord_trans(y = "log10")
#' @export
coord_trans <- function(plot, x = "identity", y = "identity") {
  .check_plot(plot)
  # Fail fast on an unknown transform name (rather than at render time).
  .resolve_coord_trans(x, "x")
  .resolve_coord_trans(y, "y")
  # `reverse` is only a display flip; coord_trans warps by a forward map and has
  # nowhere to apply the flag, so it would be a silent no-op. Reject it and point
  # to the scale, which does support it.
  .reject_coord_reverse(x, "x")
  .reject_coord_reverse(y, "y")
  plot@coord <- CoordSpec(kind = "trans", xtrans = x, ytrans = y)
  plot
}

# Refuse a `reverse` transform (name or a transform object) for coord_trans.
.reject_coord_reverse <- function(t, arg) {
  is_rev <- (is.character(t) && identical(t, "reverse")) ||
    (is.list(t) && identical(t$name, "reverse"))
  if (is_rev) {
    scale_fn <- paste0("scale_", arg, "_continuous(trans = \"reverse\")")
    cli::cli_abort(c(
      "{.fn coord_trans} does not support a {.val reverse} {.arg {arg}} transform.",
      i = "It would be a silent no-op; reverse the axis with {.code {scale_fn}} instead."
    ))
  }
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
#' @param graticule Draw meridians and parallels behind the map. `FALSE`
#'   (default) draws none; `TRUE` picks round longitude/latitude breaks
#'   automatically; a list `list(lon = , lat = )` sets the breaks explicitly (in
#'   degrees). For a projected CRS the lines are reprojected and therefore curved;
#'   for unprojected longitude/latitude they are straight.
#' @param graticule_labels Label the graticule lines with their degree values
#'   (default `TRUE`); ignored when `graticule` is `FALSE`.
#' @return The modified [PlotSpec].
#' @seealso [mark_sf()], [mark_scalebar()], [mark_compass()]
#' @examples
#' \dontrun{
#' nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
#' vplot(nc) |> mark_sf(fill = BIR74) |> coord_sf(crs = "OGC:CRS84")
#' vplot(nc) |> mark_sf(fill = BIR74) |> coord_sf(crs = 3857, graticule = TRUE)
#' }
#' @export
coord_sf <- function(
  plot,
  crs = NULL,
  xlim = NULL,
  ylim = NULL,
  graticule = FALSE,
  graticule_labels = TRUE
) {
  .check_plot(plot)
  .check_continuous_limits(xlim, "xlim")
  .check_continuous_limits(ylim, "ylim")
  grat <- if (isTRUE(graticule)) {
    list(labels = graticule_labels)
  } else if (is.list(graticule)) {
    utils::modifyList(list(labels = graticule_labels), graticule)
  } else {
    NULL
  }
  plot@coord <- CoordSpec(
    kind = "sf",
    crs = crs,
    xlim = xlim,
    ylim = ylim,
    graticule = grat
  )
  plot
}
