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

# Shared builder for colour/fill scales.
.colour_scale <- function(
  plot,
  aesthetic,
  type,
  palette,
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
      palette = palette,
      breaks = breaks,
      labels = labels,
      name = name
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
#'   `"reverse"`, or a [scales::transform_log10()]-style transform object.
#' @param breaks,labels Explicit break positions (data units) and their labels,
#'   or `NULL` to compute them.
#' @param name Axis title, or `NULL` to derive from the encoding.
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
  name = NULL
) {
  .check_plot(plot)
  .add_scale(
    plot,
    ScaleSpec(
      aesthetic = "x",
      type = "continuous",
      domain = limits,
      trans = trans,
      breaks = breaks,
      labels = labels,
      name = name
    )
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
  name = NULL
) {
  .check_plot(plot)
  .add_scale(
    plot,
    ScaleSpec(
      aesthetic = "y",
      type = "continuous",
      domain = limits,
      trans = trans,
      breaks = breaks,
      labels = labels,
      name = name
    )
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
#' @param date_labels A [strftime()] format string for the tick labels, e.g.
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
  if (!discrete && length(dom) != 2L) {
    cli::cli_abort(c(
      "Continuous limits must be a length-2 vector {.code c(min, max)}.",
      i = "Got {length(dom)} value{?s} for {.field {aesthetic}}."
    ))
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
#' sets exact colours, `scale_*_gradient()` a two-point ramp. The `fill` variants
#' are identical (colour and fill share one scale). A legend is drawn
#' automatically when colour is mapped.
#'
#' @param plot A [PlotSpec].
#' @param palette A vector of colours, or a single palette name passed to
#'   [grDevices::hcl.colors()] (e.g. `"Batlow"`, `"Blues"`, `"Set 2"`; matched
#'   case/space-insensitively). `NULL` uses a sensible default.
#' @param values For `scale_*_manual()`, a vector of colours; if named, matched to
#'   data levels by name (unmatched levels draw grey).
#' @param low,high For `scale_*_gradient()`, the endpoint colours.
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
  breaks = NULL,
  labels = NULL,
  name = NULL
) {
  .colour_scale(plot, "color", "continuous", palette, breaks, labels, name)
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
scale_fill_continuous <- function(
  plot,
  palette = NULL,
  breaks = NULL,
  labels = NULL,
  name = NULL
) {
  .colour_scale(plot, "fill", "continuous", palette, breaks, labels, name)
}

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
  .check_plot(plot)
  .add_scale(
    plot,
    ScaleSpec(
      aesthetic = "size",
      type = "continuous",
      domain = limits,
      range = range,
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
  .check_plot(plot)
  .add_scale(
    plot,
    ScaleSpec(
      aesthetic = "edge_width",
      type = "continuous",
      domain = limits,
      range = range,
      breaks = breaks,
      name = name
    )
  )
}

#' Shape scale
#'
#' Declare the scale for a mapped (discrete) `shape` aesthetic. Levels cycle
#' through a default set of marker shapes unless `values` is given. A shape legend
#' is drawn automatically.
#'
#' @param plot A [PlotSpec].
#' @param values Character vector of shapes (each one of `"circle"`, `"square"`,
#'   `"triangle"`, `"diamond"`, `"plus"`, `"cross"`), or `NULL` for the default.
#' @param name Legend title, or `NULL` to derive from the encoding.
#' @return The modified [PlotSpec].
#' @examples
#' vplot(mtcars) |> mark_point(x = wt, y = mpg, shape = factor(cyl))
#' @export
scale_shape <- function(plot, values = NULL, name = NULL) {
  .check_plot(plot)
  if (!is.null(values)) {
    valid <- .SHAPE_PALETTE
    bad <- setdiff(as.character(values), valid)
    if (length(bad)) {
      cli::cli_abort(c(
        "Unknown shape{?s} in {.arg values}: {.val {bad}}.",
        i = "Use {.or {.val {valid}}}."
      ))
    }
  }
  .add_scale(
    plot,
    ScaleSpec(
      aesthetic = "shape",
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
  .check_plot(plot)
  .add_scale(
    plot,
    ScaleSpec(
      aesthetic = "alpha",
      type = "continuous",
      domain = limits,
      range = range,
      breaks = breaks,
      name = name
    )
  )
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
  .check_plot(plot)
  if (!is.null(values)) {
    valid <- .LINETYPE_PALETTE
    bad <- setdiff(as.character(values), valid)
    if (length(bad)) {
      cli::cli_abort(c(
        "Unknown line type{?s} in {.arg values}: {.val {bad}}.",
        i = "Use {.or {.val {valid}}}."
      ))
    }
  }
  .add_scale(
    plot,
    ScaleSpec(
      aesthetic = "linetype",
      type = "discrete",
      palette = values,
      name = name
    )
  )
}

#' @rdname scale_linetype
#' @export
scale_linetype_discrete <- scale_linetype
