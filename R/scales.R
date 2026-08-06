#' @include classes.R
NULL

# Append a scale, replacing any existing scale for the same aesthetic
# (last-one-wins).
.add_scale <- function(plot, scale) {
  keep <- !vapply(
    plot@scales,
    function(s) identical(s@aesthetic, scale@aesthetic),
    logical(1)
  )
  plot@scales <- c(plot@scales[keep], list(scale))
  plot
}

# Validate a continuous scale bound (`limits` or an output `range`). `NULL` means
# "derive from the data" and is always allowed. Otherwise it must be a length-2
# vector `c(min, max)`; `numeric = TRUE` additionally requires it to be numeric
# (for size/alpha/edge-width output ranges and their data limits, which are never
# dates). Without this, a malformed bound (a length-1 or length-3 vector) slips
# into the training code as `diff(rng)` / `is.finite(rng)` inside `if()`/`||` and
# aborts with a cryptic low-level message instead of a clear one.
.check_continuous_limits <- function(
  x,
  arg = "limits",
  numeric = FALSE,
  lower = NULL,
  upper = NULL,
  call = rlang::caller_env()
) {
  if (is.null(x)) {
    return(invisible(x))
  }
  if (length(x) != 2L) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be a length-2 vector {.code c(min, max)} or {.code NULL}.",
        i = "Got {length(x)} value{?s}."
      ),
      call = call
    )
  }
  if (numeric && !is.numeric(x)) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be numeric.",
        i = "Got {.obj_type_friendly {x}}."
      ),
      call = call
    )
  }
  # A non-finite entry in a numeric range/limit would make the `<`/`>` bound
  # checks below return NA (and abort with a cryptic base-R message); reject it
  # with a clear one instead.
  if (numeric && any(!is.finite(x))) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be finite.",
        i = "Got {.val {x}}."
      ),
      call = call
    )
  }
  # Output-range bounds (e.g. opacity in [0, 1], size/width non-negative): reject
  # a range outside the aesthetic's valid interval up front rather than emitting
  # silently invalid opacities/sizes.
  if (numeric && !is.null(lower) && any(x < lower)) {
    cli::cli_abort(
      "{.arg {arg}} must be >= {lower}; got {.val {x}}.",
      call = call
    )
  }
  if (numeric && !is.null(upper) && any(x > upper)) {
    cli::cli_abort(
      "{.arg {arg}} must be <= {upper}; got {.val {x}}.",
      call = call
    )
  }
  invisible(x)
}

# Normalise a `sec_axis()`/`dup_axis()` `transform` to a plain forward function
# mapping primary data values to secondary data values. Accepts a formula
# (`~ . * 2`), a function, or a `scales::transform_*()` object.
.as_sec_fun <- function(transform) {
  if (rlang::is_formula(transform) || is.function(transform)) {
    return(rlang::as_function(transform))
  }
  if (is.list(transform) && is.function(transform$transform)) {
    return(transform$transform)
  }
  cli::cli_abort(c(
    "{.arg transform} must be a formula, a function, or a {.fn scales::transform_*} object.",
    i = "For example {.code sec_axis(~ . * 1.8 + 32)} or {.code sec_axis(scales::transform_log10())}."
  ))
}

# Validate the `sec.axis =` argument of a position scale.
.check_sec_axis <- function(sec) {
  if (is.null(sec)) {
    return(NULL)
  }
  if (!S7::S7_inherits(sec, SecAxisSpec)) {
    cli::cli_abort(
      "{.arg sec.axis} must be created with {.fn sec_axis} or {.fn dup_axis}, or be {.code NULL}."
    )
  }
  sec
}

# Axis-expansion padding for a continuous position scale. `NULL` keeps the
# default 5% multiplicative pad. A numeric of length 1 or 2 is `c(mult, add)`:
# a proportion of the data range plus a constant in data units (either may be 0),
# applied to both ends -- so `expand = c(0, 0)` clamps the axis to the data.
.check_expand <- function(expand) {
  if (is.null(expand)) {
    return(NULL)
  }
  if (
    !is.numeric(expand) ||
      length(expand) < 1L ||
      length(expand) > 2L ||
      anyNA(expand) ||
      any(expand < 0)
  ) {
    cli::cli_abort(
      "{.arg expand} must be {.code NULL} or a non-negative numeric {.code c(mult, add)} (length 1 or 2)."
    )
  }
  as.numeric(expand)
}

# Resolve a continuous position scale's expansion to a padded domain. Reads the
# `expand` slot (c(mult, add)); falls back to the historic 5% multiplicative pad.
.expand_domain <- function(rng, scalespec) {
  e <- if (!is.null(scalespec)) scalespec@expand else NULL
  if (is.null(e)) {
    return(scales::expand_range(rng, mul = 0.05))
  }
  scales::expand_range(
    rng,
    mul = e[[1L]],
    add = if (length(e) >= 2L) e[[2L]] else 0
  )
}

# Shared builder for colour/fill scales.
.colour_scale <- function(
  plot,
  aesthetic,
  type,
  palette,
  breaks,
  labels,
  name,
  midpoint = NULL,
  domain = NULL
) {
  .check_plot(plot)
  .add_scale(
    plot,
    ScaleSpec(
      aesthetic = aesthetic,
      type = type,
      palette = palette,
      breaks = breaks,
      labels = labels,
      name = name,
      midpoint = midpoint,
      domain = domain
    )
  )
}

#' Position scales
#'
#' Declare a position scale to override the trained default. `scale_x_continuous()`
#' / `scale_y_continuous()` handle numeric (and date/time) axes;
#' `scale_x_discrete()` / `scale_y_discrete()` handle categorical (band) axes and
#' let you set the level order via `limits`.
#'
#' @param plot A [PlotSpec].
#' @param limits For continuous scales a numeric length-2 domain `c(min, max)`;
#'   for discrete scales a character vector of levels (sets order / subset).
#'   `NULL` trains from the data.
#' @param trans Transformation: `"identity"` (default), `"log10"`, `"sqrt"`,
#'   `"symlog"` (symmetric log --- linear through zero, logarithmic in the tails,
#'   so signed data and zero/negatives read on one axis), `"reverse"`, or a
#'   [scales::transform_log10()]-style transform object.
#' @param breaks,labels Explicit break positions (data units) and their labels,
#'   or `NULL` to compute them.
#' @param name Axis title, or `NULL` to derive from the encoding.
#' @param expand Axis padding beyond the data. `NULL` (default) keeps the usual
#'   5% breathing room; a numeric `c(mult, add)` sets a proportion of the range
#'   plus a constant (either may be `0`), applied to both ends. Both act in the
#'   scale's *transformed* space, so on a `trans = "log10"` axis `add` is in log
#'   units (matching how the axis is drawn). Use `expand = c(0, 0)` to clamp the
#'   axis exactly to the data (e.g. a bar chart's baseline), or `expand = c(0.1,
#'   0)` for a wider margin.
#' @param sec.axis A secondary axis from [sec_axis()] / [dup_axis()], drawn on
#'   the opposite edge, or `NULL` for none. Continuous Cartesian plots only (see
#'   [sec_axis()] for the current limitations).
#' @return The modified [PlotSpec].
#' @examples
#' vplot(mtcars) |> mark_point(x = wt, y = mpg) |> scale_x_continuous(limits = c(0, 6))
#' vplot(mtcars) |> mark_point(x = wt, y = mpg) |> scale_y_continuous(trans = "sqrt")
#' @export
scale_x_continuous <- function(
  plot,
  limits = NULL,
  trans = "identity",
  breaks = NULL,
  labels = NULL,
  name = NULL,
  expand = NULL,
  sec.axis = NULL
) {
  .continuous_position_scale(
    plot,
    "x",
    limits,
    trans,
    breaks,
    labels,
    name,
    expand,
    sec.axis
  )
}

#' @rdname scale_x_continuous
#' @export
scale_y_continuous <- function(
  plot,
  limits = NULL,
  trans = "identity",
  breaks = NULL,
  labels = NULL,
  name = NULL,
  expand = NULL,
  sec.axis = NULL
) {
  .continuous_position_scale(
    plot,
    "y",
    limits,
    trans,
    breaks,
    labels,
    name,
    expand,
    sec.axis
  )
}

# Shared body of scale_x_continuous() / scale_y_continuous() (identical but for
# the aesthetic).
.continuous_position_scale <- function(
  plot,
  aesthetic,
  limits,
  trans,
  breaks,
  labels,
  name,
  expand,
  sec.axis
) {
  .check_plot(plot)
  .check_continuous_limits(limits, "limits")
  .add_scale(
    plot,
    ScaleSpec(
      aesthetic = aesthetic,
      type = "continuous",
      domain = limits,
      trans = trans,
      breaks = breaks,
      labels = labels,
      name = name,
      expand = .check_expand(expand),
      sec_axis = .check_sec_axis(sec.axis)
    )
  )
}

#' Secondary axes
#'
#' Add a secondary axis to a continuous position scale: a second set of ticks and
#' labels on the opposite edge (top for `x`, right for `y`), computed as a 1:1
#' monotonic transform of the primary axis. Pass the result to the `sec.axis`
#' argument of [scale_x_continuous()] / [scale_y_continuous()]. This is a
#' labelling convenience (unit conversions, count/proportion dual readouts,
#' duplicating an axis on a wide plot) --- **not** an independent second axis with
#' its own data.
#'
#' `sec_axis()` maps the primary axis through `transform`; `dup_axis()` is the
#' identity special case (a plain duplicate) with tidy defaults.
#'
#' @section Limitations (current version):
#' Secondary axes are supported only on continuous position scales under the
#' default Cartesian coordinate system, with **shared** scales across facets.
#' Combining `sec.axis` with [coord_flip()] / [coord_polar()] / [coord_trans()],
#' with free facet scales, or with [add_marginal()] raises an error. In a plot
#' composition (`|` / `/`) the secondary axis is not drawn.
#'
#' @param transform The primary-to-secondary mapping: a formula using `.`
#'   (e.g. `~ . * 1.8 + 32`), a function, or a [scales::transform_log10()]-style
#'   transform object. Must be monotonic over the axis range. Defaults to the
#'   identity, `~ .`.
#' @param name Secondary axis title, or `NULL` for none.
#' @param breaks Break positions in **secondary** units, or `NULL` to compute
#'   them.
#' @param labels A character vector of labels (one per break) or a labelling
#'   function, or `NULL` for the default number format.
#' @return A `SecAxisSpec` to pass to `sec.axis`.
#' @seealso [scale_x_continuous()]
#' @examples
#' # Celsius with a Fahrenheit axis on top
#' vplot(data.frame(t = 0:100, y = (0:100)^2)) |>
#'   mark_line(x = t, y = y) |>
#'   scale_x_continuous(name = "°C", sec.axis = sec_axis(~ . * 1.8 + 32, name = "°F"))
#'
#' # Duplicate the y axis on the right
#' vplot(mtcars) |>
#'   mark_point(x = wt, y = mpg) |>
#'   scale_y_continuous(sec.axis = dup_axis())
#' @export
sec_axis <- function(
  transform = ~.,
  name = NULL,
  breaks = NULL,
  labels = NULL
) {
  SecAxisSpec(
    transform = .as_sec_fun(transform),
    name = name,
    breaks = breaks,
    labels = labels,
    dup = FALSE
  )
}

#' @rdname sec_axis
#' @export
dup_axis <- function(
  transform = ~.,
  name = NULL,
  breaks = NULL,
  labels = NULL
) {
  SecAxisSpec(
    transform = .as_sec_fun(transform),
    name = name,
    breaks = breaks,
    labels = labels,
    dup = TRUE
  )
}

#' @rdname scale_x_continuous
#' @export
scale_x_discrete <- function(plot, limits = NULL, name = NULL) {
  .check_plot(plot)
  .add_scale(
    plot,
    ScaleSpec(aesthetic = "x", type = "discrete", domain = limits, name = name)
  )
}

#' @rdname scale_x_continuous
#' @export
scale_y_discrete <- function(plot, limits = NULL, name = NULL) {
  .check_plot(plot)
  .add_scale(
    plot,
    ScaleSpec(aesthetic = "y", type = "discrete", domain = limits, name = name)
  )
}

#' Date and time position scales
#'
#' Position scales for `Date` (`scale_*_date`), date-time `POSIXct`
#' (`scale_*_datetime`), and time-of-day / `difftime` / `hms`
#' (`scale_*_time`) axes. A `Date`/`POSIXct` column already gets a sensible date
#' axis automatically; declare one of these to control the break interval or the
#' label format.
#'
#' @param plot A [PlotSpec].
#' @param limits Length-2 vector giving the axis range (same class as the data),
#'   or `NULL` to train from the data.
#' @param date_breaks A break interval string, e.g. `"1 month"`, `"2 weeks"`,
#'   `"10 years"` (passed to [scales::breaks_width()]). `NULL` uses `pretty()`.
#' @param date_labels A [base::strftime()] format string for the tick labels, e.g.
#'   `"%b %Y"`. `NULL` uses the default format.
#' @param breaks,labels Explicit break positions / labels (override
#'   `date_breaks`/`date_labels`), or `NULL`.
#' @param name Axis title, or `NULL` to derive from the encoding.
#' @return The modified [PlotSpec].
#' @examples
#' df <- data.frame(day = as.Date("2020-01-01") + 0:364, y = cumsum(rnorm(365)))
#' vplot(df) |>
#'   mark_line(x = day, y = y) |>
#'   scale_x_date(date_breaks = "3 months", date_labels = "%b %Y")
#' @export
scale_x_date <- function(
  plot,
  limits = NULL,
  date_breaks = NULL,
  date_labels = NULL,
  breaks = NULL,
  labels = NULL,
  name = NULL
) {
  .date_scale(
    plot,
    "x",
    "date",
    limits,
    date_breaks,
    date_labels,
    breaks,
    labels,
    name
  )
}

#' @rdname scale_x_date
#' @export
scale_y_date <- function(
  plot,
  limits = NULL,
  date_breaks = NULL,
  date_labels = NULL,
  breaks = NULL,
  labels = NULL,
  name = NULL
) {
  .date_scale(
    plot,
    "y",
    "date",
    limits,
    date_breaks,
    date_labels,
    breaks,
    labels,
    name
  )
}

#' @rdname scale_x_date
#' @export
scale_x_datetime <- function(
  plot,
  limits = NULL,
  date_breaks = NULL,
  date_labels = NULL,
  breaks = NULL,
  labels = NULL,
  name = NULL
) {
  .date_scale(
    plot,
    "x",
    "datetime",
    limits,
    date_breaks,
    date_labels,
    breaks,
    labels,
    name
  )
}

#' @rdname scale_x_date
#' @export
scale_y_datetime <- function(
  plot,
  limits = NULL,
  date_breaks = NULL,
  date_labels = NULL,
  breaks = NULL,
  labels = NULL,
  name = NULL
) {
  .date_scale(
    plot,
    "y",
    "datetime",
    limits,
    date_breaks,
    date_labels,
    breaks,
    labels,
    name
  )
}

#' @rdname scale_x_date
#' @export
scale_x_time <- function(
  plot,
  limits = NULL,
  date_breaks = NULL,
  date_labels = NULL,
  breaks = NULL,
  labels = NULL,
  name = NULL
) {
  .date_scale(
    plot,
    "x",
    "time",
    limits,
    date_breaks,
    date_labels,
    breaks,
    labels,
    name
  )
}

#' @rdname scale_x_date
#' @export
scale_y_time <- function(
  plot,
  limits = NULL,
  date_breaks = NULL,
  date_labels = NULL,
  breaks = NULL,
  labels = NULL,
  name = NULL
) {
  .date_scale(
    plot,
    "y",
    "time",
    limits,
    date_breaks,
    date_labels,
    breaks,
    labels,
    name
  )
}

.date_scale <- function(
  plot,
  aesthetic,
  type,
  limits,
  date_breaks,
  date_labels,
  breaks,
  labels,
  name
) {
  .check_plot(plot)
  .add_scale(
    plot,
    ScaleSpec(
      aesthetic = aesthetic,
      type = type,
      domain = limits,
      breaks = breaks,
      labels = labels,
      date_breaks = date_breaks,
      date_labels = date_labels,
      name = name
    )
  )
}

# Canonicalise an aesthetic name for a limits shortcut (British spelling, `fill`
# shares the colour scale).
.canonical_lim_aes <- function(aesthetic) {
  switch(
    aesthetic,
    colour = "color",
    fill = "color",
    linewidth = "edge_width",
    aesthetic
  )
}

# The shared engine behind lims()/xlim()/ylim(): declare a scale for `aesthetic`
# carrying `limits` as its domain. The scale kind is inferred from the limits
# value (character/factor -> discrete, else continuous), which matches how the
# data itself would train, so a discrete colour limit stays discrete etc.
.lim_scale <- function(plot, aesthetic, limits) {
  if (is.null(limits)) {
    return(plot)
  }
  aesthetic <- .canonical_lim_aes(aesthetic)
  discrete <- is.character(limits) || is.factor(limits)
  dom <- if (is.factor(limits)) as.character(limits) else limits
  if (!discrete) {
    .check_continuous_limits(dom, "limits")
  }
  .add_scale(
    plot,
    ScaleSpec(
      aesthetic = aesthetic,
      type = if (discrete) "discrete" else "continuous",
      domain = dom
    )
  )
}

#' Set scale limits with a shortcut
#'
#' Convenience wrappers that declare a scale carrying only its limits, instead of
#' spelling out a full `scale_*()`. `xlim()` / `ylim()` set the position range;
#' `lims()` sets limits for any named aesthetic. They are shortcuts for
#' `scale_*(limits = ...)`: like there, an explicit range sets the trained
#' (view) window and marks outside it are clipped.
#'
#' @param plot A [PlotSpec].
#' @param ... For `xlim()`/`ylim()`, the limits: two numbers `xlim(0, 10)` (or a
#'   length-2 vector) for a continuous axis, or a set of levels
#'   `xlim("a", "b", "c")` for a discrete one. For `lims()`, named limit vectors,
#'   one per aesthetic, e.g. `lims(x = c(0, 10), color = c(0, 100))`.
#' @return The modified [PlotSpec].
#' @seealso [scale_x_continuous()]
#' @examples
#' vplot(mtcars) |> mark_point(x = wt, y = mpg) |> xlim(0, 6)
#' vplot(mtcars) |> mark_point(x = wt, y = mpg) |> lims(x = c(0, 6), y = c(10, 35))
#' @export
lims <- function(plot, ...) {
  .check_plot(plot)
  args <- list(...)
  nm <- names(args)
  if (is.null(nm) || any(!nzchar(nm))) {
    cli::cli_abort("All arguments to {.fn lims} must be named by aesthetic.")
  }
  for (aes in nm) {
    plot <- .lim_scale(plot, aes, args[[aes]])
  }
  plot
}

#' @rdname lims
#' @export
xlim <- function(plot, ...) {
  .check_plot(plot)
  .lim_scale(plot, "x", c(...))
}

#' @rdname lims
#' @export
ylim <- function(plot, ...) {
  .check_plot(plot)
  .lim_scale(plot, "y", c(...))
}

#' Colour scales
#'
#' Declare a colour scale for the `color`/`fill` channel. Continuous data get a
#' perceptual ramp; discrete data get a qualitative palette. `scale_*_manual()`
#' sets exact colours, `scale_*_gradient()` a two-point ramp, and
#' `scale_*_gradient2()` a diverging three-point ramp (`low`--`mid`--`high`)
#' centred on `midpoint` --- for signed or anomaly data where a meaningful zero
#' should sit at the neutral colour. The `fill` variants are identical (colour and
#' fill share one scale). A legend is drawn automatically when colour is mapped.
#'
#' Continuous and binned ramps built from a plain colour vector are interpolated
#' in the perceptually-uniform **Oklab** space, so they avoid the muddy, over-dark
#' midtones and hue drift of sRGB blending. Designed perceptual palettes (the
#' default, and `hcl.colors()` names) are already uniform and unaffected. Set
#' `options(vellumplot.color.interpolation = "srgb")` (or `"lab"`) to change the
#' blend space globally.
#'
#' @param plot A [PlotSpec].
#' @param palette A vector of colours, or a single palette name passed to
#'   [grDevices::hcl.colors()] (e.g. `"Batlow"`, `"Blues"`, `"Set 2"`; matched
#'   case/space-insensitively). `NULL` uses a sensible default.
#' @param values For `scale_*_manual()`, a vector of colours; if named, matched to
#'   data levels by name (unmatched levels draw grey).
#' @param colours For `scale_*_gradientn()`, an n-stop vector of colours the
#'   continuous ramp interpolates through.
#' @param limits For the continuous colour scales, a length-2 numeric
#'   `c(min, max)` fixing the mapped data range (the direct form of
#'   `lims(color = ...)`).
#' @param low,high For `scale_*_gradient()`/`scale_*_gradient2()`, the endpoint
#'   colours.
#' @param mid For `scale_*_gradient2()`, the midpoint colour.
#' @param midpoint For `scale_*_gradient2()`, the data value placed at `mid`
#'   (default `0`); values above and below diverge to `high` and `low`.
#' @param breaks,labels Explicit legend breaks / labels, or `NULL`.
#' @param name Legend title, or `NULL` to derive from the encoding.
#' @return The modified [PlotSpec].
#' @examples
#' vplot(mtcars) |>
#'   mark_point(x = wt, y = mpg, color = hp) |>
#'   scale_color_continuous(palette = "Batlow")
#' @export
scale_color_continuous <- function(
  plot,
  palette = NULL,
  limits = NULL,
  breaks = NULL,
  labels = NULL,
  name = NULL
) {
  .check_continuous_limits(limits, "limits", numeric = TRUE)
  .colour_scale(
    plot,
    "color",
    "continuous",
    palette,
    breaks,
    labels,
    name,
    domain = limits
  )
}

#' @rdname scale_color_continuous
#' @export
scale_color_discrete <- function(
  plot,
  palette = NULL,
  breaks = NULL,
  labels = NULL,
  name = NULL
) {
  .colour_scale(plot, "color", "discrete", palette, breaks, labels, name)
}

# Validate the `values` supplied to a manual colour/fill scale.
.check_manual_values <- function(values, arg = "values") {
  if (missing(values) || !is.character(values) || !length(values)) {
    cli::cli_abort(
      "{.arg {arg}} must be a non-empty character vector of colours."
    )
  }
}

#' @rdname scale_color_continuous
#' @export
scale_color_manual <- function(plot, values, name = NULL) {
  .check_manual_values(values)
  .colour_scale(plot, "color", "discrete", values, NULL, NULL, name)
}

#' @rdname scale_color_continuous
#' @export
scale_color_gradient <- function(
  plot,
  low = "#132B43",
  high = "#56B1F7",
  name = NULL
) {
  .colour_scale(plot, "color", "continuous", c(low, high), NULL, NULL, name)
}

#' @rdname scale_color_continuous
#' @export
scale_color_gradient2 <- function(
  plot,
  low = "#832424",
  mid = "#FFFFFF",
  high = "#3A3A98",
  midpoint = 0,
  name = NULL
) {
  .colour_scale(
    plot,
    "color",
    "continuous",
    c(low, mid, high),
    NULL,
    NULL,
    name,
    midpoint = midpoint
  )
}

#' @rdname scale_color_continuous
#' @export
scale_fill_continuous <- function(
  plot,
  palette = NULL,
  limits = NULL,
  breaks = NULL,
  labels = NULL,
  name = NULL
) {
  .check_continuous_limits(limits, "limits", numeric = TRUE)
  .colour_scale(
    plot,
    "fill",
    "continuous",
    palette,
    breaks,
    labels,
    name,
    domain = limits
  )
}

#' @rdname scale_color_continuous
#' @export
scale_color_gradientn <- function(plot, colours, name = NULL) {
  .check_manual_values(colours, "colours")
  .colour_scale(plot, "color", "continuous", colours, NULL, NULL, name)
}

#' @rdname scale_color_continuous
#' @export
scale_fill_gradientn <- function(plot, colours, name = NULL) {
  .check_manual_values(colours, "colours")
  .colour_scale(plot, "fill", "continuous", colours, NULL, NULL, name)
}

#' @rdname scale_color_continuous
#' @export
scale_colour_gradientn <- scale_color_gradientn

#' @rdname scale_color_continuous
#' @export
scale_fill_discrete <- function(
  plot,
  palette = NULL,
  breaks = NULL,
  labels = NULL,
  name = NULL
) {
  .colour_scale(plot, "fill", "discrete", palette, breaks, labels, name)
}

#' @rdname scale_color_continuous
#' @export
scale_fill_manual <- function(plot, values, name = NULL) {
  .check_manual_values(values)
  .colour_scale(plot, "fill", "discrete", values, NULL, NULL, name)
}

#' @rdname scale_color_continuous
#' @export
scale_fill_gradient <- function(
  plot,
  low = "#132B43",
  high = "#56B1F7",
  name = NULL
) {
  .colour_scale(plot, "fill", "continuous", c(low, high), NULL, NULL, name)
}

#' @rdname scale_color_continuous
#' @export
scale_fill_gradient2 <- function(
  plot,
  low = "#832424",
  mid = "#FFFFFF",
  high = "#3A3A98",
  midpoint = 0,
  name = NULL
) {
  .colour_scale(
    plot,
    "fill",
    "continuous",
    c(low, mid, high),
    NULL,
    NULL,
    name,
    midpoint = midpoint
  )
}

# Map a viridis `option` (a name or ggplot2's A–H letter) to the matching
# `grDevices::hcl.colors()` palette name; unknown options fall back to Viridis.
.viridis_pal <- function(option) {
  # grDevices::hcl.colors() carries Viridis/Plasma/Inferno/Cividis/Rocket/Mako
  # (not viridisLite's Magma or Turbo); map ggplot2's letters onto what exists,
  # falling back to Viridis.
  switch(
    tolower(option),
    b = ,
    inferno = "Inferno",
    c = ,
    plasma = "Plasma",
    d = ,
    viridis = "Viridis",
    e = ,
    cividis = "Cividis",
    f = ,
    rocket = "Rocket",
    g = ,
    mako = "Mako",
    "Viridis"
  )
}

#' Viridis and ColorBrewer scales
#'
#' Named convenience constructors for popular palettes, easing migration from
#' ggplot2. They are thin wrappers over the `palette =` argument of
#' [scale_color_continuous()] / [scale_color_discrete()] (any
#' [grDevices::hcl.colors()] name works there directly).
#'
#' * `scale_*_viridis_c()` / `scale_*_viridis_d()` — the perceptually-uniform,
#'   colour-vision-deficient-safe viridis maps, continuous (`_c`) or discrete
#'   (`_d`). `option` picks the map: `"viridis"`, `"plasma"`, `"inferno"`,
#'   `"cividis"`, `"rocket"`, or `"mako"` (unknown options fall back to viridis).
#' * `scale_*_brewer()` — a discrete ColorBrewer-style qualitative/sequential
#'   palette; `scale_*_distiller()` — the same ramp interpolated for continuous
#'   data. `palette` is any `hcl.colors()` name (e.g. `"Blues"`, `"Set 2"`).
#'
#' @param plot A [PlotSpec].
#' @param option Which viridis map (see above).
#' @param palette A `grDevices::hcl.colors()` palette name.
#' @param name,breaks,labels As for [scale_color_continuous()].
#' @return The modified [PlotSpec].
#' @seealso [scale_color_continuous()]
#' @examples
#' vplot(mtcars) |> mark_point(x = wt, y = mpg, color = hp) |> scale_color_viridis_c()
#' vplot(mtcars) |>
#'   mark_point(x = wt, y = mpg, color = factor(cyl)) |>
#'   scale_color_brewer(palette = "Set 2")
#' @name scale_viridis
NULL

#' @rdname scale_viridis
#' @export
scale_color_viridis_c <- function(
  plot,
  option = "viridis",
  breaks = NULL,
  labels = NULL,
  name = NULL
) {
  .colour_scale(
    plot,
    "color",
    "continuous",
    .viridis_pal(option),
    breaks,
    labels,
    name
  )
}

#' @rdname scale_viridis
#' @export
scale_color_viridis_d <- function(
  plot,
  option = "viridis",
  breaks = NULL,
  labels = NULL,
  name = NULL
) {
  .colour_scale(
    plot,
    "color",
    "discrete",
    .viridis_pal(option),
    breaks,
    labels,
    name
  )
}

#' @rdname scale_viridis
#' @export
scale_fill_viridis_c <- function(
  plot,
  option = "viridis",
  breaks = NULL,
  labels = NULL,
  name = NULL
) {
  .colour_scale(
    plot,
    "fill",
    "continuous",
    .viridis_pal(option),
    breaks,
    labels,
    name
  )
}

#' @rdname scale_viridis
#' @export
scale_fill_viridis_d <- function(
  plot,
  option = "viridis",
  breaks = NULL,
  labels = NULL,
  name = NULL
) {
  .colour_scale(
    plot,
    "fill",
    "discrete",
    .viridis_pal(option),
    breaks,
    labels,
    name
  )
}

#' @rdname scale_viridis
#' @export
scale_color_brewer <- function(
  plot,
  palette = "Blues",
  breaks = NULL,
  labels = NULL,
  name = NULL
) {
  .colour_scale(plot, "color", "discrete", palette, breaks, labels, name)
}

#' @rdname scale_viridis
#' @export
scale_fill_brewer <- function(
  plot,
  palette = "Blues",
  breaks = NULL,
  labels = NULL,
  name = NULL
) {
  .colour_scale(plot, "fill", "discrete", palette, breaks, labels, name)
}

#' @rdname scale_viridis
#' @export
scale_color_distiller <- function(
  plot,
  palette = "Blues",
  breaks = NULL,
  labels = NULL,
  name = NULL
) {
  .colour_scale(plot, "color", "continuous", palette, breaks, labels, name)
}

#' @rdname scale_viridis
#' @export
scale_fill_distiller <- function(
  plot,
  palette = "Blues",
  breaks = NULL,
  labels = NULL,
  name = NULL
) {
  .colour_scale(plot, "fill", "continuous", palette, breaks, labels, name)
}

# British-spelling aliases: `colour` is honoured everywhere `color` is (the
# `mark_*()` encodings, `labs()`, `element_*()`), so the scale constructors take
# the alias too.
#' @rdname scale_color_continuous
#' @export
scale_colour_continuous <- scale_color_continuous

#' @rdname scale_color_continuous
#' @export
scale_colour_discrete <- scale_color_discrete

#' @rdname scale_color_continuous
#' @export
scale_colour_manual <- scale_color_manual

#' @rdname scale_color_continuous
#' @export
scale_colour_gradient <- scale_color_gradient

#' @rdname scale_color_continuous
#' @export
scale_colour_gradient2 <- scale_color_gradient2

#' @rdname scale_viridis
#' @export
scale_colour_viridis_c <- scale_color_viridis_c

#' @rdname scale_viridis
#' @export
scale_colour_viridis_d <- scale_color_viridis_d

#' @rdname scale_viridis
#' @export
scale_colour_brewer <- scale_color_brewer

#' @rdname scale_viridis
#' @export
scale_colour_distiller <- scale_color_distiller

# The shared engine behind the continuous output-range scales (size / alpha /
# edge_width / edge_alpha): validate the data `limits` and the output `range`
# (non-negative, and <= `upper` when the aesthetic is bounded like opacity), then
# declare a continuous scale for `aesthetic`.
.continuous_range_scale <- function(
  plot,
  aesthetic,
  range,
  limits,
  breaks,
  name,
  upper = NULL
) {
  .check_plot(plot)
  .check_continuous_limits(limits, "limits", numeric = TRUE)
  .check_continuous_limits(
    range,
    "range",
    numeric = TRUE,
    lower = 0,
    upper = upper
  )
  .add_scale(
    plot,
    ScaleSpec(
      aesthetic = aesthetic,
      type = "continuous",
      domain = limits,
      range = range,
      breaks = breaks,
      name = name
    )
  )
}

#' Size scale
#'
#' Declare the scale for a mapped `size` aesthetic: data values map linearly to a
#' point-size `range` (in mm). A size legend is drawn automatically.
#'
#' @param plot A [PlotSpec].
#' @param range Numeric length-2 output size range in mm, or `NULL` for the
#'   default `c(1, 4)`.
#' @param limits Numeric length-2 data domain, or `NULL` to train from the data.
#' @param breaks Explicit legend breaks, or `NULL`.
#' @param name Legend title, or `NULL` to derive from the encoding.
#' @return The modified [PlotSpec].
#' @examples
#' vplot(mtcars) |> mark_point(x = wt, y = mpg, size = hp) |> scale_size(range = c(1, 8))
#' @export
scale_size <- function(
  plot,
  range = NULL,
  limits = NULL,
  breaks = NULL,
  name = NULL
) {
  .continuous_range_scale(plot, "size", range, limits, breaks, name)
}

#' @rdname scale_size
#'
#' @description
#' `scale_size_area()` maps the data value to the marker's **area** rather than
#' its radius, with value `0` at size `0` — the perceptually honest default for
#' bubble charts (a value twice as large looks twice as big in ink). `max_size`
#' is the size (mm) of the largest value.
#'
#' @param max_size For `scale_size_area()`, the point size (mm) of the largest
#'   value.
#' @examples
#' vplot(mtcars) |>
#'   mark_point(x = wt, y = mpg, size = hp) |>
#'   scale_size_area(max_size = 10)
#' @export
scale_size_area <- function(
  plot,
  max_size = 6,
  limits = NULL,
  breaks = NULL,
  name = NULL
) {
  .check_plot(plot)
  .check_continuous_limits(limits, "limits", numeric = TRUE)
  .check_dim(max_size, "max_size")
  .add_scale(
    plot,
    ScaleSpec(
      aesthetic = "size",
      type = "area",
      domain = limits,
      range = c(0, as.numeric(max_size)),
      breaks = breaks,
      name = name
    )
  )
}

#' Edge-width scale
#'
#' Declare the scale for a mapped edge `linewidth` aesthetic (e.g.
#' `mark_edges(linewidth = weight)` on a [vgraph()] plot). The data range is
#' rescaled to a line-width range and an edge-width legend is drawn automatically.
#'
#' @param plot A [PlotSpec].
#' @param range Output line-width range `c(min, max)`, or `NULL` for the
#'   default `c(0.3, 3)`.
#' @param limits Data limits `c(min, max)`, or `NULL` to train from the data.
#' @param breaks Explicit legend breaks, or `NULL`.
#' @param name Legend title, or `NULL` to derive from the encoding.
#' @return The modified [PlotSpec].
#' @seealso [mark_edges()], [vgraph()]
#' @examples
#' \dontrun{
#' g <- igraph::sample_gnp(20, 0.2)
#' g <- igraph::set_edge_attr(g, "w", value = runif(igraph::ecount(g)))
#' vgraph(g) |> mark_edges(linewidth = w) |> mark_nodes() |> scale_edge_width(range = c(0.3, 4))
#' }
#' @export
scale_edge_width <- function(
  plot,
  range = NULL,
  limits = NULL,
  breaks = NULL,
  name = NULL
) {
  .continuous_range_scale(plot, "edge_width", range, limits, breaks, name)
}

#' Edge colour / alpha / line-type scales
#'
#' Declare the scale for a mapped edge aesthetic on a [vgraph()] plot -- the
#' colour (`mark_edges(color = )`), opacity (`alpha = `), or line type
#' (`linetype = `) of the edges. These are the edge counterparts of
#' [scale_color_continuous()] / [scale_alpha()] / [scale_linetype()]: an edge
#' aesthetic is trained and legended **independently of the node scales**, so a
#' figure can map, say, node fill to a discrete community *and* edge colour to a
#' continuous weight without the two collapsing into one legend. Each draws its
#' own legend automatically.
#'
#' `scale_edge_color()` infers discrete vs continuous from the mapped data (a
#' factor/character trains a discrete swatch legend; a number a colour bar), the
#' same as the node colour scale. `scale_edge_colour()` is a British-spelling
#' alias.
#'
#' @param plot A [PlotSpec], normally from [vgraph()].
#' @param palette For `scale_edge_color()`, a vector of colours or a single
#'   palette name (as in [scale_color_continuous()]); `NULL` for a default.
#' @param values For `scale_edge_linetype()`, a character vector of line types
#'   (as in [scale_linetype()]); `NULL` for the default palette.
#' @param range For `scale_edge_alpha()`, the output opacity range `c(min, max)`,
#'   or `NULL` for the default.
#' @param limits,breaks,labels Data limits, explicit legend breaks, and explicit
#'   labels, or `NULL` to derive from the data.
#' @param midpoint For `scale_edge_color()`, the data value placed at the ramp's
#'   midpoint (a diverging scale); `NULL` for an ordinary ramp.
#' @param name Legend title, or `NULL` to derive from the encoding.
#' @return The modified [PlotSpec].
#' @seealso [mark_edges()], [scale_edge_width()], [vgraph()]
#' @examples
#' \dontrun{
#' g <- igraph::make_graph("Zachary")
#' g <- igraph::set_edge_attr(g, "w", value = runif(igraph::ecount(g)))
#' vgraph(g) |>
#'   mark_edges(color = w) |>
#'   mark_nodes(fill = factor(igraph::membership(igraph::cluster_louvain(g)))) |>
#'   scale_edge_color(palette = "Grays")
#' }
#' @name scale_edge
NULL

#' @rdname scale_edge
#' @export
scale_edge_color <- function(
  plot,
  palette = NULL,
  breaks = NULL,
  labels = NULL,
  name = NULL,
  midpoint = NULL
) {
  .colour_scale(
    plot,
    "edge_color",
    "",
    palette,
    breaks,
    labels,
    name,
    midpoint = midpoint
  )
}

#' @rdname scale_edge
#' @export
scale_edge_colour <- scale_edge_color

#' @rdname scale_edge
#' @export
scale_edge_alpha <- function(
  plot,
  range = NULL,
  limits = NULL,
  breaks = NULL,
  name = NULL
) {
  .continuous_range_scale(
    plot,
    "edge_alpha",
    range,
    limits,
    breaks,
    name,
    upper = 1
  )
}

# The shared engine behind the discrete-palette scales (shape / linetype /
# edge_linetype): validate `values` against the aesthetic's fixed palette, then
# declare a discrete scale. `{qty(bad)}` keeps the count (not `noun`) governing
# the `{?s}` pluralisation.
.discrete_palette_scale <- function(
  plot,
  aesthetic,
  values,
  valid,
  noun,
  name,
  svg_ok = FALSE
) {
  .check_plot(plot)
  if (!is.null(values)) {
    bad <- setdiff(as.character(values), valid)
    # For `shape`, a value that is not a built-in marker is an SVG icon -- a
    # `.svg` file, or a path `d` (which begins with a moveto). Both are
    # legitimate, so don't reject them; a genuine typo still errors.
    if (svg_ok && length(bad)) {
      is_svg <- grepl("\\.svg$", bad, ignore.case = TRUE) |
        grepl("^\\s*[Mm][\\s0-9.,+-]", bad, perl = TRUE)
      bad <- bad[!is_svg]
    }
    if (length(bad)) {
      # Pluralise the noun in R (the count governs, not cli's `{?s}` which would
      # attach to the length-1 `{noun}` interpolation before it).
      label <- if (length(bad) > 1L) paste0(noun, "s") else noun
      cli::cli_abort(c(
        "Unknown {label} in {.arg values}: {.val {bad}}.",
        i = "Use {.or {.val {valid}}}."
      ))
    }
  }
  .add_scale(
    plot,
    ScaleSpec(
      aesthetic = aesthetic,
      type = "discrete",
      palette = values,
      name = name
    )
  )
}

#' @rdname scale_edge
#' @export
scale_edge_linetype <- function(plot, values = NULL, name = NULL) {
  .discrete_palette_scale(
    plot,
    "edge_linetype",
    values,
    .LINETYPE_PALETTE,
    "line type",
    name
  )
}

#' Shape scale
#'
#' Declare the scale for a mapped (discrete) `shape` aesthetic. Levels cycle
#' through a default set of marker shapes unless `values` is given. A shape legend
#' is drawn automatically.
#'
#' @param plot A [PlotSpec].
#' @param values Character vector of shapes, or `NULL` for the default. Each is
#'   either a built-in marker (`"circle"`, `"square"`, `"triangle"`,
#'   `"diamond"`, `"plus"`, `"cross"`, `"triangle_down"`, `"star"`) **or an SVG
#'   icon** — a path `d` string (what icon sets ship) or a path to a `.svg`
#'   file. SVG values are drawn as crisp vector markers (via
#'   [vellum::svg_grob()]) and the legend shows the icon; the `size` aesthetic
#'   scales them.
#' @param name Legend title, or `NULL` to derive from the encoding.
#' @return The modified [PlotSpec].
#' @examples
#' vplot(mtcars) |> mark_point(x = wt, y = mpg, shape = factor(cyl))
#' # SVG icon markers, one per level:
#' star <- "M12 2l3 7h7l-5.5 4.5 2 7-6.5-4.5-6.5 4.5 2-7L2 9h7z"
#' vplot(mtcars) |>
#'   mark_point(x = wt, y = mpg, shape = factor(cyl)) |>
#'   scale_shape(values = c(star, star, star))
#' @export
scale_shape <- function(plot, values = NULL, name = NULL) {
  .discrete_palette_scale(
    plot,
    "shape",
    values,
    .SHAPE_PALETTE,
    "shape",
    name,
    svg_ok = TRUE
  )
}

#' Pattern (texture) scale
#'
#' Map a discrete variable to fill **textures** -- the greyscale- and
#' colour-vision-safe companion of a fill palette -- via a filled mark's `pattern`
#' aesthetic: `mark_bar(x = g, y = n, pattern = series)`. Each level takes a
#' distinct [pattern_stripe()]-family texture. Applies to the filled marks (bar,
#' area, tile, rect, boxplot, violin, hull).
#'
#' @param plot A [PlotSpec].
#' @param values Optional textures, one per level: a character vector of builder
#'   names (`"stripe"`, `"crosshatch"`, `"grid"`, `"dot"`, `"checker"`), or a list
#'   of `vellum_pattern` objects (e.g. from [pattern_stripe()]). `NULL` (default)
#'   cycles a built-in palette of distinct textures.
#' @param name Optional legend title.
#' @return The modified [PlotSpec].
#' @seealso [pattern_stripe()], [scale_shape()]
#' @examples
#' df <- data.frame(g = c("a", "b", "c"), n = c(3, 5, 2))
#' vplot(df) |> mark_bar(x = g, y = n, pattern = g) |> scale_pattern()
#' @export
scale_pattern <- function(plot, values = NULL, name = NULL) {
  .check_plot(plot)
  .add_scale(
    plot,
    ScaleSpec(
      aesthetic = "pattern",
      type = "discrete",
      palette = values,
      name = name
    )
  )
}

#' Identity scales
#'
#' Use the data values *directly* as the aesthetic — no training and no legend.
#' The mapped column must already contain valid aesthetic values: colours for
#' `scale_color_identity()` / `scale_fill_identity()`, sizes in mm for
#' `scale_size_identity()`, and shape names for `scale_shape_identity()`. Useful
#' when a column already holds the exact colours/sizes you want to draw.
#'
#' @param plot A [PlotSpec].
#' @return The modified [PlotSpec].
#' @examples
#' df <- data.frame(x = 1:3, y = 1:3, col = c("red", "green", "blue"))
#' vplot(df) |> mark_point(x = x, y = y, color = col) |> scale_color_identity()
#' @export
scale_color_identity <- function(plot) {
  .check_plot(plot)
  .add_scale(plot, ScaleSpec(aesthetic = "color", type = "identity"))
}

#' @rdname scale_color_identity
#' @export
scale_fill_identity <- function(plot) {
  .check_plot(plot)
  .add_scale(plot, ScaleSpec(aesthetic = "color", type = "identity"))
}

#' @rdname scale_color_identity
#' @export
scale_colour_identity <- scale_color_identity

#' @rdname scale_color_identity
#' @export
scale_size_identity <- function(plot) {
  .check_plot(plot)
  .add_scale(plot, ScaleSpec(aesthetic = "size", type = "identity"))
}

#' @rdname scale_color_identity
#' @export
scale_shape_identity <- function(plot) {
  .check_plot(plot)
  .add_scale(plot, ScaleSpec(aesthetic = "shape", type = "identity"))
}

#' @rdname scale_color_identity
#' @export
scale_alpha_identity <- function(plot) {
  .check_plot(plot)
  .add_scale(plot, ScaleSpec(aesthetic = "alpha", type = "identity"))
}

#' @rdname scale_color_identity
#' @export
scale_linetype_identity <- function(plot) {
  .check_plot(plot)
  .add_scale(plot, ScaleSpec(aesthetic = "linetype", type = "identity"))
}

#' Alpha (opacity) scale
#'
#' Declare the scale for a mapped `alpha` aesthetic: data values map to an
#' opacity `range` in `[0, 1]`. An alpha legend is drawn automatically.
#'
#' @param plot A [PlotSpec].
#' @param range Numeric length-2 output opacity range, or `NULL` for the
#'   default `c(0.1, 1)`.
#' @param limits Numeric length-2 data domain, or `NULL` to train from the data.
#' @param breaks Explicit legend breaks, or `NULL`.
#' @param name Legend title, or `NULL` to derive from the encoding.
#' @return The modified [PlotSpec].
#' @examples
#' vplot(mtcars) |> mark_point(x = wt, y = mpg, alpha = hp) |> scale_alpha(range = c(0.2, 1))
#' @export
scale_alpha <- function(
  plot,
  range = NULL,
  limits = NULL,
  breaks = NULL,
  name = NULL
) {
  .continuous_range_scale(plot, "alpha", range, limits, breaks, name, upper = 1)
}

#' @rdname scale_alpha
#' @export
scale_alpha_continuous <- scale_alpha

#' Line-type scale
#'
#' Declare the scale for a mapped (discrete) `linetype` aesthetic. Levels cycle
#' through a default set of line types unless `values` is given. A line-type
#' legend is drawn automatically. Applies to line-like marks (`mark_line()`,
#' `mark_step()`).
#'
#' @param plot A [PlotSpec].
#' @param values Character vector of line types (each one of `"solid"`,
#'   `"dashed"`, `"dotted"`, `"dotdash"`, `"longdash"`, `"twodash"`), or `NULL`
#'   for the default set.
#' @param name Legend title, or `NULL` to derive from the encoding.
#' @return The modified [PlotSpec].
#' @examples
#' df <- data.frame(x = rep(1:10, 2), y = rnorm(20), g = rep(c("a", "b"), each = 10))
#' vplot(df) |> mark_line(x = x, y = y, linetype = g)
#' @export
scale_linetype <- function(plot, values = NULL, name = NULL) {
  .discrete_palette_scale(
    plot,
    "linetype",
    values,
    .LINETYPE_PALETTE,
    "line type",
    name
  )
}

#' @rdname scale_linetype
#' @export
scale_linetype_discrete <- scale_linetype
