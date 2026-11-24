#' @include classes.R
NULL

# --- living beside ggplot2 ----------------------------------------------------
#
# ggplot2 looks two kinds of constructor up *by name* from the plot's
# environment, i.e. along the search path: the default scale of each mapped
# aesthetic (`scale_colour_continuous()`, `scale_x_discrete()`, ...) and a guide
# given as a string (`guide = "legend"`, the default of every discrete scale).
# With vellumplot attached after ggplot2 that lookup finds vellumplot's
# constructor of the same name and calls it with no arguments, which breaks the
# ggplot. Each constructor ggplot2 can reach this way therefore starts with
# `if (.ggplot2_asks()) return(.ggplot2_own("<name>"))`: called from inside
# ggplot2's namespace, it hands back ggplot2's own scale or guide.

# Is the caller of the calling constructor ggplot2's own code?
.ggplot2_asks <- function(env = parent.frame(2)) {
  top <- topenv(env)
  isNamespace(top) && identical(getNamespaceName(top), c(name = "ggplot2"))
}

# ggplot2's constructor `name`, called with its defaults.
.ggplot2_own <- function(name) getExportedValue("ggplot2", name)()

# Append a scale, replacing any existing scale for the same aesthetic
# (last-one-wins).
.add_scale <- function(plot, scale) {
  keep <- !vapply(
    plot@scales,
    function(s) identical(s@aesthetic, scale@aesthetic),
    logical(1)
  )
  # A guide set earlier with `guides()` survives a later scale declaration
  # that does not set its own (as ggplot2 keeps `guides()` apart from scales).
  if (is.null(scale@guide) && !all(keep)) {
    old <- plot@scales[!keep]
    scale@guide <- old[[length(old)]]@guide
  }
  plot@scales <- c(plot@scales[keep], list(scale))
  plot
}

# Set `drop` on the scale just declared for `aesthetic` (a discrete legend
# scale): `TRUE` (the default, as in ggplot2) drops factor levels no row uses
# from the legend and the palette, `FALSE` keeps every level (VP-91).
.with_drop <- function(plot, aesthetic, drop, call = rlang::caller_env()) {
  if (!is.logical(drop) || length(drop) != 1L || is.na(drop)) {
    cli::cli_abort("{.arg drop} must be {.code TRUE} or {.code FALSE}.", call = call)
  }
  if (isTRUE(drop)) {
    return(plot)
  }
  i <- which(vapply(
    plot@scales,
    function(s) identical(s@aesthetic, aesthetic),
    logical(1)
  ))
  if (length(i)) {
    i <- i[length(i)]
    sc <- plot@scales[[i]]
    sc@drop <- FALSE
    plot@scales[[i]] <- sc
  }
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
  domain = NULL,
  oob = NULL
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
      domain = domain,
      oob = oob
    )
  )
}

# Shared builder for the named continuous colour scales (gradient, gradient2,
# gradientn, viridis_c, distiller): validates `limits` and `oob`.
.continuous_colour_scale <- function(
  plot,
  aesthetic,
  palette,
  name,
  breaks,
  labels,
  limits,
  na.value,
  oob,
  midpoint = NULL,
  call = rlang::caller_env()
) {
  .check_plot(plot)
  .check_continuous_limits(limits, "limits", numeric = TRUE, call = call)
  .add_scale(
    plot,
    ScaleSpec(
      aesthetic = aesthetic,
      type = "continuous",
      palette = palette,
      breaks = breaks,
      labels = labels,
      name = name,
      midpoint = midpoint,
      domain = limits,
      na_value = na.value,
      oob = .check_oob(oob, call = call)
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
#'   or `NULL` to compute them. On a discrete scale `breaks` is the levels that
#'   get a tick, and `labels` a character vector (one per break, or named by
#'   level) or a function of the levels.
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
#' @param oob What to do with values outside `limits`, as in
#'   [scales::oob_censor()] and friends: `"censor"` drops them (they become
#'   `NA` and are not drawn), `"squish"` moves them onto the nearest limit,
#'   `"keep"` draws them where they fall, clipped by the panel. A function
#'   `function(x, range)` such as [scales::oob_squish_any()] also works. `NULL`
#'   (default) is `"keep"` on a position axis. Infinite values are never out
#'   of bounds: `x = Inf` sits on the panel edge.
#' @param drop For discrete scales, drop factor levels that do not occur in
#'   the data (default `TRUE`, as in ggplot2). `limits` overrides it.
#' @param guide The axis guide: [guide_axis()] to stagger, rotate or thin the
#'   labels, `"none"` for no labels or ticks, or `NULL` (default) for the plain
#'   axis. Same as `guides(x = )` / `guides(y = )`.
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
  sec.axis = NULL,
  oob = NULL,
  guide = NULL
) {
  if (.ggplot2_asks()) return(.ggplot2_own("scale_x_continuous"))
  plot <- .continuous_position_scale(
    plot,
    "x",
    limits,
    trans,
    breaks,
    labels,
    name,
    expand,
    sec.axis,
    oob
  )
  .with_axis_guide(plot, "x", guide)
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
  sec.axis = NULL,
  oob = NULL,
  guide = NULL
) {
  if (.ggplot2_asks()) return(.ggplot2_own("scale_y_continuous"))
  plot <- .continuous_position_scale(
    plot,
    "y",
    limits,
    trans,
    breaks,
    labels,
    name,
    expand,
    sec.axis,
    oob
  )
  .with_axis_guide(plot, "y", guide)
}

#' @rdname scale_x_continuous
#' @description
#' `scale_x_log10()`, `scale_x_sqrt()` and `scale_x_reverse()` (and their `y`
#' forms) are shortcuts for `scale_x_continuous(trans = "log10")` and so on,
#' as in ggplot2; `...` takes the other arguments of `scale_x_continuous()`.
#' @param ... For the transform shortcuts, further arguments to
#'   `scale_x_continuous()` / `scale_y_continuous()` (not `trans`).
#' @export
scale_x_log10 <- function(plot, ...) {
  scale_x_continuous(plot, ..., trans = "log10")
}

#' @rdname scale_x_continuous
#' @export
scale_x_sqrt <- function(plot, ...) {
  scale_x_continuous(plot, ..., trans = "sqrt")
}

#' @rdname scale_x_continuous
#' @export
scale_x_reverse <- function(plot, ...) {
  scale_x_continuous(plot, ..., trans = "reverse")
}

#' @rdname scale_x_continuous
#' @export
scale_y_log10 <- function(plot, ...) {
  scale_y_continuous(plot, ..., trans = "log10")
}

#' @rdname scale_x_continuous
#' @export
scale_y_sqrt <- function(plot, ...) {
  scale_y_continuous(plot, ..., trans = "sqrt")
}

#' @rdname scale_x_continuous
#' @export
scale_y_reverse <- function(plot, ...) {
  scale_y_continuous(plot, ..., trans = "reverse")
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
  sec.axis,
  oob = NULL
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
      sec_axis = .check_sec_axis(sec.axis),
      oob = .check_oob(oob)
    )
  )
}

# Shared body of scale_x_discrete() / scale_y_discrete().
.discrete_position_scale <- function(
  plot,
  aesthetic,
  limits,
  name,
  breaks,
  labels,
  drop
) {
  .check_plot(plot)
  if (!is.logical(drop) || length(drop) != 1L || is.na(drop)) {
    cli::cli_abort("{.arg drop} must be {.code TRUE} or {.code FALSE}.")
  }
  if (
    !is.null(labels) &&
      !is.function(labels) &&
      !is.character(labels) &&
      !is.numeric(labels)
  ) {
    cli::cli_abort(
      "{.arg labels} must be a character vector, a function, or {.code NULL}."
    )
  }
  .add_scale(
    plot,
    ScaleSpec(
      aesthetic = aesthetic,
      type = "discrete",
      domain = limits,
      name = name,
      breaks = breaks,
      labels = labels,
      drop = drop
    )
  )
}

# Check a scale's `oob` argument: NULL, one of the handler names, or a
# `function(x, range)`. scales' own handlers are stored by name, so a spec that
# uses them still serializes.
.check_oob <- function(oob, call = rlang::caller_env()) {
  if (is.null(oob)) {
    return(NULL)
  }
  if (is.function(oob)) {
    known <- list(
      censor = scales::oob_censor,
      squish = scales::oob_squish,
      keep = scales::oob_keep
    )
    for (nm in names(known)) {
      if (identical(oob, known[[nm]])) {
        return(nm)
      }
    }
    return(oob)
  }
  if (!is.character(oob) || length(oob) != 1L) {
    cli::cli_abort(
      "{.arg oob} must be {.val censor}, {.val squish}, {.val keep}, or a function.",
      call = call
    )
  }
  rlang::arg_match0(oob, c("censor", "squish", "keep"), "oob", call)
}

# Logical per row of `data`: does a position scale with `oob` set (to
# "censor" or a function) map one of `layer`'s continuous position channels
# outside its numeric limits to NA? Such rows are removed before the stat and
# counted into the "Removed n rows" warning, as for missing positions (see
# `.missing_position_rows()`); only the marks that draw one element per row
# drop them (a path keeps its gap).
.censored_position_rows <- function(spec, layer, data) {
  n <- nrow(data)
  out <- rep(FALSE, n)
  if (!n || !layer@mark %in% .NA_REMOVE_MARKS) {
    return(out)
  }
  is_num <- function(v) is.numeric(v) || inherits(v, c("Date", "POSIXt"))
  for (axis in names(.DIAG_POS_CHANNELS)) {
    sc <- .scale_for(spec, axis)
    if (is.null(sc) || is.null(sc@oob) || identical(sc@oob, "keep")) {
      next
    }
    dom <- sc@domain
    if (length(dom) != 2L || !is_num(dom)) {
      next
    }
    chans <- intersect(.DIAG_POS_CHANNELS[[axis]], names(layer@encoding))
    for (nm in chans) {
      ch <- layer@encoding[[nm]]
      if (isTRUE(ch@after)) {
        next
      }
      v <- rlang::eval_tidy(ch@expr, data = data)
      if (length(v) == n && is_num(v)) {
        out <- out | .diag_oob(sc, as.numeric(v), as.numeric(dom))$gone
      }
    }
  }
  out
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
scale_x_discrete <- function(
  plot,
  limits = NULL,
  name = NULL,
  breaks = NULL,
  labels = NULL,
  drop = TRUE,
  guide = NULL
) {
  if (.ggplot2_asks()) return(.ggplot2_own("scale_x_discrete"))
  plot <- .discrete_position_scale(
    plot,
    "x",
    limits,
    name,
    breaks,
    labels,
    drop
  )
  .with_axis_guide(plot, "x", guide)
}

#' @rdname scale_x_continuous
#' @export
scale_y_discrete <- function(
  plot,
  limits = NULL,
  name = NULL,
  breaks = NULL,
  labels = NULL,
  drop = TRUE,
  guide = NULL
) {
  if (.ggplot2_asks()) return(.ggplot2_own("scale_y_discrete"))
  plot <- .discrete_position_scale(
    plot,
    "y",
    limits,
    name,
    breaks,
    labels,
    drop
  )
  .with_axis_guide(plot, "y", guide)
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
#' @param guide The axis guide: [guide_axis()] to stagger, rotate or thin the
#'   labels, `"none"` for no labels or ticks, or `NULL` (default) for the plain
#'   axis. Same as `guides(x = )` / `guides(y = )`.
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
  name = NULL,
  guide = NULL
) {
  if (.ggplot2_asks()) return(.ggplot2_own("scale_x_date"))
  .date_scale(
    plot,
    "x",
    "date",
    limits,
    date_breaks,
    date_labels,
    breaks,
    labels,
    name,
    guide
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
  name = NULL,
  guide = NULL
) {
  if (.ggplot2_asks()) return(.ggplot2_own("scale_y_date"))
  .date_scale(
    plot,
    "y",
    "date",
    limits,
    date_breaks,
    date_labels,
    breaks,
    labels,
    name,
    guide
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
  name = NULL,
  guide = NULL
) {
  if (.ggplot2_asks()) return(.ggplot2_own("scale_x_datetime"))
  .date_scale(
    plot,
    "x",
    "datetime",
    limits,
    date_breaks,
    date_labels,
    breaks,
    labels,
    name,
    guide
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
  name = NULL,
  guide = NULL
) {
  if (.ggplot2_asks()) return(.ggplot2_own("scale_y_datetime"))
  .date_scale(
    plot,
    "y",
    "datetime",
    limits,
    date_breaks,
    date_labels,
    breaks,
    labels,
    name,
    guide
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
  name = NULL,
  guide = NULL
) {
  if (.ggplot2_asks()) return(.ggplot2_own("scale_x_time"))
  .date_scale(
    plot,
    "x",
    "time",
    limits,
    date_breaks,
    date_labels,
    breaks,
    labels,
    name,
    guide
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
  name = NULL,
  guide = NULL
) {
  if (.ggplot2_asks()) return(.ggplot2_own("scale_y_time"))
  .date_scale(
    plot,
    "y",
    "time",
    limits,
    date_breaks,
    date_labels,
    breaks,
    labels,
    name,
    guide
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
  name,
  guide = NULL
) {
  .check_plot(plot)
  plot <- .add_scale(
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
  .with_axis_guide(plot, aesthetic, guide)
}

# Canonicalise an aesthetic name for a limits shortcut (British spelling).
.canonical_lim_aes <- function(aesthetic) {
  switch(
    aesthetic,
    colour = "color",
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
#' Declare the scale for the `color` or the `fill` channel. Continuous data get a
#' perceptual ramp; discrete data get a qualitative palette. `scale_*_manual()`
#' sets exact colours, `scale_*_gradient()` a two-point ramp, and
#' `scale_*_gradient2()` a diverging three-point ramp (`low`--`mid`--`high`)
#' centred on `midpoint` --- for signed or anomaly data where a meaningful zero
#' should sit at the neutral colour. The `fill` variants set the fill scale the
#' same way: `color` and `fill` are separate scales with separate legends, as in
#' ggplot2, drawn as one legend when they show the same title and keys. A
#' legend is drawn automatically when colour or fill is mapped.
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
#' @param limits For the continuous colour scales (including the gradient
#'   ones), a length-2 numeric `c(min, max)` fixing the mapped data range (the
#'   direct form of `lims(color = ...)`). For `scale_*_manual()`, the levels
#'   the scale covers.
#' @param low,high For `scale_*_gradient()`/`scale_*_gradient2()`, the endpoint
#'   colours.
#' @param mid For `scale_*_gradient2()`, the midpoint colour.
#' @param midpoint For `scale_*_gradient2()`, the data value placed at `mid`
#'   (default `0`); values above and below diverge to `high` and `low`.
#' @param breaks,labels Explicit legend breaks / labels, or `NULL`.
#' @param name Legend title, or `NULL` to derive from the encoding.
#' @param oob For the continuous scales (`scale_*_continuous()`,
#'   `_gradient()`, `_gradient2()`, `_gradientn()`), what to do with values
#'   outside `limits`: `"censor"` gives them the NA colour, `"squish"` the
#'   colour of the nearest limit, `"keep"` the same (a ramp cannot extend past
#'   its ends), or a `function(x, range)`. `NULL` (default) squishes.
#' @param na.value For the gradient scales, the colour of missing (and
#'   censored) values; for `scale_*_manual()`, also of levels outside
#'   `limits`. `NULL` keeps the default grey.
#' @param drop For the discrete scales, drop factor levels that no row uses
#'   (default `TRUE`, as in ggplot2): they get no legend key and no palette
#'   entry, so the colours go to the levels that remain. `FALSE` keeps every
#'   level. `limits` overrides it.
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
  name = NULL,
  oob = NULL
) {
  if (.ggplot2_asks()) return(.ggplot2_own("scale_color_continuous"))
  .check_continuous_limits(limits, "limits", numeric = TRUE)
  .colour_scale(
    plot,
    "color",
    "continuous",
    palette,
    breaks,
    labels,
    name,
    domain = limits,
    oob = .check_oob(oob)
  )
}

#' @rdname scale_color_continuous
#' @export
scale_color_discrete <- function(
  plot,
  palette = NULL,
  breaks = NULL,
  labels = NULL,
  name = NULL,
  drop = TRUE
) {
  if (.ggplot2_asks()) return(.ggplot2_own("scale_color_discrete"))
  plot <- .colour_scale(plot, "color", "discrete", palette, breaks, labels, name)
  .with_drop(plot, "color", drop)
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
scale_color_manual <- function(
  plot,
  values,
  name = NULL,
  breaks = NULL,
  labels = NULL,
  limits = NULL,
  na.value = NULL,
  drop = TRUE
) {
  .check_manual_values(values)
  plot <- .discrete_colour_scale(
    plot,
    "color",
    values,
    name,
    breaks,
    labels,
    limits,
    na.value
  )
  .with_drop(plot, "color", drop)
}

#' @rdname scale_color_continuous
#' @export
scale_color_gradient <- function(
  plot,
  low = "#132B43",
  high = "#56B1F7",
  name = NULL,
  limits = NULL,
  breaks = NULL,
  labels = NULL,
  na.value = NULL,
  oob = NULL
) {
  .continuous_colour_scale(
    plot,
    "color",
    c(low, high),
    name,
    breaks,
    labels,
    limits,
    na.value,
    oob
  )
}

#' @rdname scale_color_continuous
#' @export
scale_color_gradient2 <- function(
  plot,
  low = "#832424",
  mid = "#FFFFFF",
  high = "#3A3A98",
  midpoint = 0,
  name = NULL,
  limits = NULL,
  breaks = NULL,
  labels = NULL,
  na.value = NULL,
  oob = NULL
) {
  .continuous_colour_scale(
    plot,
    "color",
    c(low, mid, high),
    name,
    breaks,
    labels,
    limits,
    na.value,
    oob,
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
  name = NULL,
  oob = NULL
) {
  if (.ggplot2_asks()) return(.ggplot2_own("scale_fill_continuous"))
  .check_continuous_limits(limits, "limits", numeric = TRUE)
  .colour_scale(
    plot,
    "fill",
    "continuous",
    palette,
    breaks,
    labels,
    name,
    domain = limits,
    oob = .check_oob(oob)
  )
}

#' @rdname scale_color_continuous
#' @export
scale_color_gradientn <- function(
  plot,
  colours,
  name = NULL,
  limits = NULL,
  breaks = NULL,
  labels = NULL,
  na.value = NULL,
  oob = NULL
) {
  .check_manual_values(colours, "colours")
  .continuous_colour_scale(
    plot,
    "color",
    colours,
    name,
    breaks,
    labels,
    limits,
    na.value,
    oob
  )
}

#' @rdname scale_color_continuous
#' @export
scale_fill_gradientn <- function(
  plot,
  colours,
  name = NULL,
  limits = NULL,
  breaks = NULL,
  labels = NULL,
  na.value = NULL,
  oob = NULL
) {
  .check_manual_values(colours, "colours")
  .continuous_colour_scale(
    plot,
    "fill",
    colours,
    name,
    breaks,
    labels,
    limits,
    na.value,
    oob
  )
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
  name = NULL,
  drop = TRUE
) {
  if (.ggplot2_asks()) return(.ggplot2_own("scale_fill_discrete"))
  plot <- .colour_scale(plot, "fill", "discrete", palette, breaks, labels, name)
  .with_drop(plot, "fill", drop)
}

#' @rdname scale_color_continuous
#' @export
scale_fill_manual <- function(
  plot,
  values,
  name = NULL,
  breaks = NULL,
  labels = NULL,
  limits = NULL,
  na.value = NULL,
  drop = TRUE
) {
  .check_manual_values(values)
  plot <- .discrete_colour_scale(
    plot,
    "fill",
    values,
    name,
    breaks,
    labels,
    limits,
    na.value
  )
  .with_drop(plot, "fill", drop)
}

#' @rdname scale_color_continuous
#' @export
scale_fill_gradient <- function(
  plot,
  low = "#132B43",
  high = "#56B1F7",
  name = NULL,
  limits = NULL,
  breaks = NULL,
  labels = NULL,
  na.value = NULL,
  oob = NULL
) {
  .continuous_colour_scale(
    plot,
    "fill",
    c(low, high),
    name,
    breaks,
    labels,
    limits,
    na.value,
    oob
  )
}

#' @rdname scale_color_continuous
#' @export
scale_fill_gradient2 <- function(
  plot,
  low = "#832424",
  mid = "#FFFFFF",
  high = "#3A3A98",
  midpoint = 0,
  name = NULL,
  limits = NULL,
  breaks = NULL,
  labels = NULL,
  na.value = NULL,
  oob = NULL
) {
  .continuous_colour_scale(
    plot,
    "fill",
    c(low, mid, high),
    name,
    breaks,
    labels,
    limits,
    na.value,
    oob,
    midpoint = midpoint
  )
}

#' Hue, grey and ordinal colour scales
#'
#' ggplot2's other discrete colour scales, for code migrating from it.
#'
#' * `scale_*_hue()` --- evenly spaced hues around the HCL colour wheel
#'   (ggplot2's default discrete palette, via [scales::pal_hue()]).
#' * `scale_*_grey()` --- a grey ramp from `start` to `end` (0 is black, 1
#'   white), via [scales::pal_grey()].
#' * `scale_*_ordinal()` --- the viridis palette ggplot2 uses for an ordered
#'   factor (as [scale_color_viridis_d()]).
#'
#' The colours depend on the number of levels, so they are drawn when the plot
#' is compiled.
#'
#' @param plot A [PlotSpec].
#' @param h Range of hues, in degrees on the colour wheel.
#' @param c,l Chroma and luminance of the hues.
#' @param h.start Hue to start at.
#' @param direction `1` runs clockwise round the wheel, `-1` anticlockwise.
#' @param start,end Grey levels of the first and last colour, in `[0, 1]`.
#' @param option For `scale_*_ordinal()`, the viridis map (see
#'   [scale_color_viridis_d()]).
#' @param name Legend title, or `NULL` to derive from the encoding.
#' @param breaks,labels The levels the legend shows and their labels (one per
#'   shown level, a vector named by level, or a function), or `NULL`.
#' @param limits The levels the scale covers, or `NULL` for those in the data.
#' @param na.value Colour for missing values (and levels outside `limits`).
#' @param drop For the discrete scales, drop factor levels that no row uses
#'   (default `TRUE`, as in ggplot2): they get no legend key and no palette
#'   entry, so the colours go to the levels that remain. `FALSE` keeps every
#'   level. `limits` overrides it.
#' @return The modified [PlotSpec].
#' @seealso [scale_color_discrete()], [scale_viridis]
#' @examples
#' vplot(mtcars) |>
#'   mark_point(x = wt, y = mpg, color = factor(cyl)) |>
#'   scale_color_hue(l = 45)
#' vplot(mtcars) |>
#'   mark_bar(x = factor(gear), fill = factor(cyl)) |>
#'   scale_fill_grey()
#' @name scale_hue
NULL

# A discrete palette built from a palette *function* of the level count
# (pal_hue(), pal_grey()) is stored on the ScaleSpec as a string token,
# "<kind>:<arg>:<arg>...", like the brewer tokens: the colours depend on the
# trained level count, and a string keeps the spec serializable.
.PALETTE_TOKEN <- "^(hue|grey):"

.palette_token <- function(kind, ...) {
  args <- c(...)
  paste0(
    kind,
    ":",
    paste(format(args, digits = 15, trim = TRUE), collapse = ":")
  )
}

# `k` colours from a hue/grey palette token, or NULL when `x` is not one.
.token_colours <- function(x, k) {
  if (!is.character(x) || length(x) != 1L || !grepl(.PALETTE_TOKEN, x)) {
    return(NULL)
  }
  parts <- strsplit(x, ":", fixed = TRUE)[[1L]]
  a <- as.numeric(parts[-1L])
  switch(
    parts[[1L]],
    hue = scales::pal_hue(
      h = a[1:2],
      c = a[3],
      l = a[4],
      h.start = a[5],
      direction = a[6]
    )(k),
    grey = scales::pal_grey(start = a[1], end = a[2])(k)
  )
}

.check_unit_interval <- function(x, arg, call = rlang::caller_env()) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || x < 0 || x > 1) {
    cli::cli_abort("{.arg {arg}} must be a number in [0, 1].", call = call)
  }
}

.discrete_colour_scale <- function(
  plot,
  aesthetic,
  palette,
  name,
  breaks,
  labels,
  limits,
  na.value,
  call = rlang::caller_env()
) {
  .check_plot(plot)
  .check_manual_labels(labels, call = call)
  .add_scale(
    plot,
    ScaleSpec(
      aesthetic = aesthetic,
      type = "discrete",
      palette = palette,
      name = name,
      breaks = breaks,
      labels = labels,
      domain = .check_discrete_limits(limits, call = call),
      na_value = na.value
    )
  )
}

.hue_scale <- function(
  plot,
  aesthetic,
  h,
  c,
  l,
  h.start,
  direction,
  name,
  breaks,
  labels,
  limits,
  na.value,
  call = rlang::caller_env()
) {
  if (!is.numeric(h) || length(h) != 2L || anyNA(h)) {
    cli::cli_abort(
      "{.arg h} must be a numeric range {.code c(from, to)}.",
      call = call
    )
  }
  for (a in list(list(c, "c"), list(l, "l"), list(h.start, "h.start"))) {
    if (!is.numeric(a[[1]]) || length(a[[1]]) != 1L || is.na(a[[1]])) {
      cli::cli_abort("{.arg {a[[2]]}} must be a single number.", call = call)
    }
  }
  if (
    !identical(as.numeric(direction), 1) &&
      !identical(as.numeric(direction), -1)
  ) {
    cli::cli_abort(
      "{.arg direction} must be {.val {1}} or {.val {-1}}.",
      call = call
    )
  }
  .discrete_colour_scale(
    plot,
    aesthetic,
    .palette_token("hue", h, c, l, h.start, direction),
    name,
    breaks,
    labels,
    limits,
    na.value,
    call = call
  )
}

#' @rdname scale_hue
#' @export
scale_color_hue <- function(
  plot,
  h = c(0, 360) + 15,
  c = 100,
  l = 65,
  h.start = 0,
  direction = 1,
  name = NULL,
  breaks = NULL,
  labels = NULL,
  limits = NULL,
  na.value = "grey50",
  drop = TRUE
) {
  plot <- .hue_scale(
    plot,
    "color",
    h,
    c,
    l,
    h.start,
    direction,
    name,
    breaks,
    labels,
    limits,
    na.value
  )
  .with_drop(plot, "color", drop)
}

#' @rdname scale_hue
#' @export
scale_colour_hue <- scale_color_hue

#' @rdname scale_hue
#' @export
scale_fill_hue <- function(
  plot,
  h = c(0, 360) + 15,
  c = 100,
  l = 65,
  h.start = 0,
  direction = 1,
  name = NULL,
  breaks = NULL,
  labels = NULL,
  limits = NULL,
  na.value = "grey50",
  drop = TRUE
) {
  plot <- .hue_scale(
    plot,
    "fill",
    h,
    c,
    l,
    h.start,
    direction,
    name,
    breaks,
    labels,
    limits,
    na.value
  )
  .with_drop(plot, "fill", drop)
}

.grey_scale <- function(
  plot,
  aesthetic,
  start,
  end,
  name,
  breaks,
  labels,
  limits,
  na.value,
  call = rlang::caller_env()
) {
  .check_unit_interval(start, "start", call = call)
  .check_unit_interval(end, "end", call = call)
  .discrete_colour_scale(
    plot,
    aesthetic,
    .palette_token("grey", start, end),
    name,
    breaks,
    labels,
    limits,
    na.value,
    call = call
  )
}

#' @rdname scale_hue
#' @export
scale_color_grey <- function(
  plot,
  start = 0.2,
  end = 0.8,
  name = NULL,
  breaks = NULL,
  labels = NULL,
  limits = NULL,
  na.value = "red",
  drop = TRUE
) {
  plot <- .grey_scale(plot, "color", start, end, name, breaks, labels, limits, na.value)
  .with_drop(plot, "color", drop)
}

#' @rdname scale_hue
#' @export
scale_colour_grey <- scale_color_grey

#' @rdname scale_hue
#' @export
scale_fill_grey <- function(
  plot,
  start = 0.2,
  end = 0.8,
  name = NULL,
  breaks = NULL,
  labels = NULL,
  limits = NULL,
  na.value = "red",
  drop = TRUE
) {
  plot <- .grey_scale(plot, "fill", start, end, name, breaks, labels, limits, na.value)
  .with_drop(plot, "fill", drop)
}

#' @rdname scale_hue
#' @export
scale_color_ordinal <- function(
  plot,
  option = "viridis",
  name = NULL,
  breaks = NULL,
  labels = NULL,
  limits = NULL,
  na.value = "grey50",
  drop = TRUE
) {
  if (.ggplot2_asks()) return(.ggplot2_own("scale_color_ordinal"))
  plot <- .discrete_colour_scale(
    plot,
    "color",
    .viridis_pal(option),
    name,
    breaks,
    labels,
    limits,
    na.value
  )
  .with_drop(plot, "color", drop)
}

#' @rdname scale_hue
#' @export
scale_colour_ordinal <- scale_color_ordinal

#' @rdname scale_hue
#' @export
scale_fill_ordinal <- function(
  plot,
  option = "viridis",
  name = NULL,
  breaks = NULL,
  labels = NULL,
  limits = NULL,
  na.value = "grey50",
  drop = TRUE
) {
  if (.ggplot2_asks()) return(.ggplot2_own("scale_fill_ordinal"))
  plot <- .discrete_colour_scale(
    plot,
    "fill",
    .viridis_pal(option),
    name,
    breaks,
    labels,
    limits,
    na.value
  )
  .with_drop(plot, "fill", drop)
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
#' * `scale_*_brewer()` — a discrete ColorBrewer palette (sequential,
#'   diverging or qualitative, via [scales::pal_brewer()]);
#'   `scale_*_distiller()` — the same palette's 7-class ramp interpolated for
#'   continuous data. `palette` is a ColorBrewer name (`"Set1"`, `"Dark2"`,
#'   `"Paired"`, `"Blues"`, `"RdBu"`, `"Spectral"`, ...; matched ignoring case,
#'   spaces and punctuation, so `"Set 2"` is `"Set2"`) or an index into the
#'   palettes of `type`. Any other [grDevices::hcl.colors()] name (e.g.
#'   `"Batlow"`) is passed through unchanged, as by [scale_color_discrete()] /
#'   [scale_color_continuous()].
#'
#' @param plot A [PlotSpec].
#' @param option Which viridis map (see above).
#' @param palette A ColorBrewer palette name or index (see above), or a
#'   `grDevices::hcl.colors()` palette name.
#' @param type When `palette` is a number, which ColorBrewer family it indexes:
#'   `"seq"` (sequential), `"div"` (diverging) or `"qual"` (qualitative).
#' @param direction `1` keeps a ColorBrewer palette's order, `-1` reverses it.
#'   `scale_*_distiller()` defaults to `-1`, as in ggplot2, so high values are
#'   light. Ignored for non-ColorBrewer palette names.
#' @param name,breaks,labels As for [scale_color_continuous()].
#' @param limits,oob,na.value For the continuous scales (`_viridis_c()`,
#'   `_distiller()`), the mapped data range `c(min, max)`, what to do with
#'   values outside it (`"censor"`, `"squish"`, `"keep"` or a function; `NULL`,
#'   the default, squishes) and the colour of missing values, as for
#'   [scale_color_continuous()].
#' @param drop For the discrete scales, drop factor levels that no row uses
#'   (default `TRUE`, as in ggplot2): they get no legend key and no palette
#'   entry, so the colours go to the levels that remain. `FALSE` keeps every
#'   level. `limits` overrides it.
#' @return The modified [PlotSpec].
#' @seealso [scale_color_continuous()]
#' @examples
#' vplot(mtcars) |> mark_point(x = wt, y = mpg, color = hp) |> scale_color_viridis_c()
#' vplot(mtcars) |>
#'   mark_point(x = wt, y = mpg, color = factor(cyl)) |>
#'   scale_color_brewer(palette = "Set1")
#' vplot(mtcars) |>
#'   mark_point(x = wt, y = mpg, color = hp) |>
#'   scale_color_distiller(palette = "Spectral")
#' @name scale_viridis
NULL

#' @rdname scale_viridis
#' @export
scale_color_viridis_c <- function(
  plot,
  option = "viridis",
  breaks = NULL,
  labels = NULL,
  name = NULL,
  limits = NULL,
  na.value = NULL,
  oob = NULL
) {
  .continuous_colour_scale(
    plot,
    "color",
    .viridis_pal(option),
    name,
    breaks,
    labels,
    limits,
    na.value,
    oob
  )
}

#' @rdname scale_viridis
#' @export
scale_color_viridis_d <- function(
  plot,
  option = "viridis",
  breaks = NULL,
  labels = NULL,
  name = NULL,
  drop = TRUE
) {
  plot <- .colour_scale(
    plot,
    "color",
    "discrete",
    .viridis_pal(option),
    breaks,
    labels,
    name
  )
  .with_drop(plot, "color", drop)
}

#' @rdname scale_viridis
#' @export
scale_fill_viridis_c <- function(
  plot,
  option = "viridis",
  breaks = NULL,
  labels = NULL,
  name = NULL,
  limits = NULL,
  na.value = NULL,
  oob = NULL
) {
  .continuous_colour_scale(
    plot,
    "fill",
    .viridis_pal(option),
    name,
    breaks,
    labels,
    limits,
    na.value,
    oob
  )
}

#' @rdname scale_viridis
#' @export
scale_fill_viridis_d <- function(
  plot,
  option = "viridis",
  breaks = NULL,
  labels = NULL,
  name = NULL,
  drop = TRUE
) {
  plot <- .colour_scale(
    plot,
    "fill",
    "discrete",
    .viridis_pal(option),
    breaks,
    labels,
    name
  )
  .with_drop(plot, "fill", drop)
}

#' @rdname scale_viridis
#' @export
scale_color_brewer <- function(
  plot,
  palette = "Blues",
  breaks = NULL,
  labels = NULL,
  name = NULL,
  type = "seq",
  direction = 1,
  drop = TRUE
) {
  palette <- .brewer_scale_palette(palette, type, direction, "discrete")
  plot <- .colour_scale(plot, "color", "discrete", palette, breaks, labels, name)
  .with_drop(plot, "color", drop)
}

#' @rdname scale_viridis
#' @export
scale_fill_brewer <- function(
  plot,
  palette = "Blues",
  breaks = NULL,
  labels = NULL,
  name = NULL,
  type = "seq",
  direction = 1,
  drop = TRUE
) {
  palette <- .brewer_scale_palette(palette, type, direction, "discrete")
  plot <- .colour_scale(plot, "fill", "discrete", palette, breaks, labels, name)
  .with_drop(plot, "fill", drop)
}

#' @rdname scale_viridis
#' @export
scale_color_distiller <- function(
  plot,
  palette = "Blues",
  breaks = NULL,
  labels = NULL,
  name = NULL,
  type = "seq",
  direction = -1,
  limits = NULL,
  na.value = NULL,
  oob = NULL
) {
  palette <- .brewer_scale_palette(palette, type, direction, "continuous")
  .continuous_colour_scale(
    plot,
    "color",
    palette,
    name,
    breaks,
    labels,
    limits,
    na.value,
    oob
  )
}

#' @rdname scale_viridis
#' @export
scale_fill_distiller <- function(
  plot,
  palette = "Blues",
  breaks = NULL,
  labels = NULL,
  name = NULL,
  type = "seq",
  direction = -1,
  limits = NULL,
  na.value = NULL,
  oob = NULL
) {
  palette <- .brewer_scale_palette(palette, type, direction, "continuous")
  .continuous_colour_scale(
    plot,
    "fill",
    palette,
    name,
    breaks,
    labels,
    limits,
    na.value,
    oob
  )
}

# ColorBrewer palettes by family, in the order scales::pal_brewer() indexes them
# (so a numeric `palette` picks the same palette as in ggplot2).
.BREWER_PALS <- list(
  seq = c(
    "Blues",
    "BuGn",
    "BuPu",
    "GnBu",
    "Greens",
    "Greys",
    "Oranges",
    "OrRd",
    "PuBu",
    "PuBuGn",
    "PuRd",
    "Purples",
    "RdPu",
    "Reds",
    "YlGn",
    "YlGnBu",
    "YlOrBr",
    "YlOrRd"
  ),
  div = c(
    "BrBG",
    "PiYG",
    "PRGn",
    "PuOr",
    "RdBu",
    "RdGy",
    "RdYlBu",
    "RdYlGn",
    "Spectral"
  ),
  qual = c(
    "Accent",
    "Dark2",
    "Paired",
    "Pastel1",
    "Pastel2",
    "Set1",
    "Set2",
    "Set3"
  )
)

# The canonical ColorBrewer name `x` refers to (matched like hcl names: case,
# spaces and punctuation ignored), or NA when `x` is not one.
.brewer_name <- function(x) {
  if (!is.character(x) || length(x) != 1L || is.na(x)) {
    return(NA_character_)
  }
  all <- unlist(.BREWER_PALS, use.names = FALSE)
  all[match(.norm_pal(x), .norm_pal(all))]
}

# A discrete ColorBrewer palette is stored on the ScaleSpec as a plain string
# token, "brewer:<Name>" or "brewer:<Name>:reverse": the colours depend on the
# trained level count (ColorBrewer's 3-class Blues is not the first three of its
# 9-class Blues), so they are only drawn at compile time, by .discrete_colours().
# A string keeps the spec serializable.
.BREWER_TOKEN <- "^brewer:"
.is_brewer_pal <- function(x) {
  is.character(x) &&
    length(x) == 1L &&
    !is.na(x) &&
    (grepl(.BREWER_TOKEN, x) || !is.na(.brewer_name(x)))
}

# Split a brewer token (or bare ColorBrewer name) into its name and direction.
.brewer_parts <- function(x) {
  if (!grepl(.BREWER_TOKEN, x)) {
    return(list(name = .brewer_name(x), direction = 1))
  }
  parts <- strsplit(sub(.BREWER_TOKEN, "", x), ":", fixed = TRUE)[[1L]]
  name <- .brewer_name(parts[[1L]])
  if (is.na(name)) {
    cli::cli_abort("{.val {x}} is not a known ColorBrewer palette.")
  }
  rev <- length(parts) > 1L && identical(parts[[2L]], "reverse")
  list(name = name, direction = if (rev) -1 else 1)
}

# `k` discrete colours from a ColorBrewer palette. Past the palette's maximum
# the colours recycle, with a warning (ColorBrewer defines no more classes).
.brewer_colours <- function(palette, k) {
  if (k == 0L) {
    return(character())
  }
  bp <- .brewer_parts(palette)
  pal <- scales::pal_brewer(palette = bp$name, direction = bp$direction)
  max_k <- scales::palette_nlevels(pal)
  if (k <= max_k) {
    return(pal(k))
  }
  cli::cli_warn(c(
    "ColorBrewer palette {.val {bp$name}} has at most {max_k} colours.",
    i = "Its colours are recycled for the {k} levels."
  ))
  rep_len(pal(max_k), k)
}

# Continuous ramp stops for a ColorBrewer palette: its 7-class colours (as
# ggplot2's distiller uses), optionally reversed.
.brewer_stops <- function(palette, direction = NULL) {
  bp <- .brewer_parts(palette)
  scales::pal_brewer(
    palette = bp$name,
    direction = direction %||% bp$direction
  )(7L)
}

# Resolve the `palette` of scale_*_brewer() / scale_*_distiller(): a
# ColorBrewer name or index becomes a brewer token (discrete) or the 7-class
# ramp (continuous), honouring `direction`; any other value (an hcl.pals() name,
# colours) passes through unchanged.
.brewer_scale_palette <- function(
  palette,
  type,
  direction,
  scale_type,
  call = rlang::caller_env()
) {
  if (
    !is.numeric(direction) ||
      length(direction) != 1L ||
      !direction %in% c(-1, 1)
  ) {
    cli::cli_abort(
      "{.arg direction} must be {.val {1}} or {.val {-1}}.",
      call = call
    )
  }
  if (is.numeric(palette)) {
    type <- rlang::arg_match0(type, names(.BREWER_PALS), error_call = call)
    pals <- .BREWER_PALS[[type]]
    if (
      length(palette) != 1L ||
        is.na(palette) ||
        !palette %in% seq_along(pals)
    ) {
      cli::cli_abort(
        "A numeric {.arg palette} must be a whole number from 1 to {length(pals)} for {.code type = {.val {type}}}.",
        call = call
      )
    }
    name <- pals[[palette]]
  } else {
    name <- .brewer_name(palette)
  }
  if (is.na(name)) {
    return(palette)
  }
  if (identical(scale_type, "continuous")) {
    return(.brewer_stops(name, direction))
  }
  paste0("brewer:", name, if (direction == -1) ":reverse")
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
  upper = NULL,
  oob = NULL
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
      name = name,
      oob = .check_oob(oob)
    )
  )
}

#' Size scale
#'
#' Declare the scale for a mapped `size` aesthetic: data values map linearly to a
#' point-size `range` (in mm). A size legend is drawn automatically.
#'
#' A factor, character or logical mapped to `size` gets a discrete scale: the
#' levels spread evenly (by area) over `range`, the legend keys are the levels,
#' and `breaks` picks which of them it shows. As in ggplot2, an unordered
#' variable warns that size is not advised for it; an ordered factor does not.
#'
#' @param plot A [PlotSpec].
#' @param range Numeric length-2 output size range in mm, or `NULL` for the
#'   default `c(1, 4)`.
#' @param limits Numeric length-2 data domain, or `NULL` to train from the data.
#' @param breaks Explicit legend breaks, or `NULL`.
#' @param name Legend title, or `NULL` to derive from the encoding.
#' @param oob What to do with values outside `limits`: `"censor"` (they get
#'   no size, so the mark is not drawn), `"squish"` (the size of the
#'   nearest limit), `"keep"` (extrapolated past the range), or a
#'   `function(x, range)`. `NULL` (default) keeps them.
#' @return The modified [PlotSpec].
#' @examples
#' vplot(mtcars) |> mark_point(x = wt, y = mpg, size = hp) |> scale_size(range = c(1, 8))
#' @export
scale_size <- function(
  plot,
  range = NULL,
  limits = NULL,
  breaks = NULL,
  name = NULL,
  oob = NULL
) {
  if (.ggplot2_asks()) return(.ggplot2_own("scale_size"))
  .continuous_range_scale(plot, "size", range, limits, breaks, name, oob = oob)
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

#' @rdname scale_size
#' @export
scale_size_continuous <- scale_size

#' @rdname scale_size
#'
#' @description
#' `scale_radius()` is ggplot2's radius-proportional size scale. vellumplot's
#' `scale_size()` already maps the value linearly to the marker size, so it
#' is `scale_size()` with ggplot2's default `range = c(1, 6)`.
#' @export
scale_radius <- function(
  plot,
  range = c(1, 6),
  limits = NULL,
  breaks = NULL,
  name = NULL,
  oob = NULL
) {
  scale_size(
    plot,
    range = range,
    limits = limits,
    breaks = breaks,
    name = name,
    oob = oob
  )
}

#' @rdname scale_size
#'
#' @description
#' `scale_size_binned()` cuts the variable into classes and gives each class
#' one size, the size of its centre on an area scale (as ggplot2's
#' `scale_size_binned()`); the legend keys the classes. Without `breaks` the
#' class boundaries are `n.breaks` "nice" values inside the data range, and the
#' outer classes are widened to whole bins, as in ggplot2.
#'
#' @param n.breaks For `scale_size_binned()`, the approximate number of class
#'   boundaries (default 5) when `breaks` is `NULL`.
#' @param labels For `scale_size_binned()` and the discrete forms, legend
#'   labels (one per key, or a function), or `NULL`.
#' @export
scale_size_binned <- function(
  plot,
  range = NULL,
  limits = NULL,
  breaks = NULL,
  labels = NULL,
  name = NULL,
  n.breaks = NULL
) {
  if (.ggplot2_asks()) return(.ggplot2_own("scale_size_binned"))
  .check_plot(plot)
  .check_continuous_limits(limits, "limits", numeric = TRUE)
  .check_continuous_limits(range, "range", numeric = TRUE, lower = 0)
  .check_binned_args(n.breaks, "extended")
  .add_scale(
    plot,
    ScaleSpec(
      aesthetic = "size",
      type = "binned",
      domain = limits,
      range = range,
      breaks = breaks,
      labels = labels,
      style = "extended",
      n = n.breaks,
      name = name
    )
  )
}

#' @rdname scale_size
#'
#' @description
#' `scale_size_manual()` sets the size (mm) of each level of a discrete
#' variable directly via `values` (named values match levels by name).
#' `scale_size_discrete()` / `scale_size_ordinal()` spread the levels evenly
#' (by area) over `range`; the discrete form warns that size is not advised for
#' an unordered variable, the ordinal form does not.
#'
#' @param values For `scale_size_manual()`, the sizes in mm, one per level (or
#'   named by level).
#' @param na.value For the discrete forms, the size drawn for missing values,
#'   or `NULL` to drop them.
#' @param drop For the discrete forms, drop factor levels that no row uses
#'   (default `TRUE`, as in ggplot2): they get no legend key and no size, so the
#'   range is spread over the levels that remain. `FALSE` keeps every level.
#'   `limits` overrides it.
#' @examples
#' vplot(mtcars) |>
#'   mark_point(x = wt, y = mpg, size = factor(cyl)) |>
#'   scale_size_manual(values = c(2, 4, 8))
#' @export
scale_size_manual <- function(
  plot,
  values,
  breaks = NULL,
  labels = NULL,
  name = NULL,
  limits = NULL,
  na.value = NULL,
  drop = TRUE
) {
  .check_manual_present(values)
  plot <- .discrete_range_scale(
    plot,
    "size",
    "discrete",
    values = .check_manual_numbers(values, lower = 0),
    breaks = breaks,
    labels = labels,
    name = name,
    limits = limits,
    na_value = na.value
  )
  .with_drop(plot, "size", drop)
}

#' @rdname scale_size
#' @export
scale_size_discrete <- function(
  plot,
  range = NULL,
  breaks = NULL,
  labels = NULL,
  name = NULL,
  limits = NULL,
  na.value = NULL,
  drop = TRUE
) {
  if (.ggplot2_asks()) return(.ggplot2_own("scale_size_discrete"))
  plot <- .discrete_range_scale(
    plot,
    "size",
    "discrete",
    range = range,
    breaks = breaks,
    labels = labels,
    name = name,
    limits = limits,
    na_value = na.value
  )
  .with_drop(plot, "size", drop)
}

#' @rdname scale_size
#' @export
scale_size_ordinal <- function(
  plot,
  range = NULL,
  breaks = NULL,
  labels = NULL,
  name = NULL,
  limits = NULL,
  na.value = NULL,
  drop = TRUE
) {
  if (.ggplot2_asks()) return(.ggplot2_own("scale_size_ordinal"))
  plot <- .discrete_range_scale(
    plot,
    "size",
    "ordinal",
    range = range,
    breaks = breaks,
    labels = labels,
    name = name,
    limits = limits,
    na_value = na.value
  )
  .with_drop(plot, "size", drop)
}

# The shared engine behind the discrete size / alpha scales: manual `values`,
# or levels spread over an output `range`; `type` "ordinal" never warns that
# the aesthetic implies an order.
.discrete_range_scale <- function(
  plot,
  aesthetic,
  type,
  values = NULL,
  range = NULL,
  breaks = NULL,
  labels = NULL,
  name = NULL,
  limits = NULL,
  na_value = NULL,
  upper = NULL,
  call = rlang::caller_env()
) {
  .check_plot(plot)
  .check_manual_labels(labels, call = call)
  .check_continuous_limits(
    range,
    "range",
    numeric = TRUE,
    lower = 0,
    upper = upper,
    call = call
  )
  if (!is.null(na_value)) {
    na_value <- .check_manual_numbers(
      na_value,
      "na.value",
      lower = 0,
      upper = upper,
      call = call
    )
  }
  .add_scale(
    plot,
    ScaleSpec(
      aesthetic = aesthetic,
      type = type,
      palette = values,
      range = range,
      breaks = breaks,
      labels = labels,
      name = name,
      domain = .check_discrete_limits(limits, call = call),
      na_value = na_value
    )
  )
}

# Numeric manual values (sizes, opacities), each within [lower, upper].
.check_manual_numbers <- function(
  x,
  arg = "values",
  lower = NULL,
  upper = NULL,
  call = rlang::caller_env()
) {
  if (!is.numeric(x) || anyNA(x)) {
    cli::cli_abort(
      "{.arg {arg}} must be numeric, without missing values.",
      call = call
    )
  }
  if (
    (!is.null(lower) && any(x < lower)) || (!is.null(upper) && any(x > upper))
  ) {
    cli::cli_abort(
      "{.arg {arg}} must lie in [{lower %||% -Inf}, {upper %||% Inf}].",
      call = call
    )
  }
  x
}

#' Line-width scale
#'
#' Declare the scale for a mapped `linewidth` aesthetic on a stroked mark --
#' `mark_line()`, `mark_step()`, or `mark_segment()`. Data values are rescaled
#' linearly onto a line-width `range` (in `lwd` units, as everywhere else in the
#' package) and a line-width legend is drawn automatically.
#'
#' On `mark_line()` and `mark_step()`, a mapped `linewidth` draws each series as
#' **one continuous stroke whose width tapers smoothly** between the values at
#' its vertices (via vellum's `lines_grob(lwd_profile = )`). The stroke is built
#' as a filled outline at render time, so it looks the same in PNG, SVG and PDF
#' and stays one element per series in [vellum::scene_model()] and the SVG. A
#' constant `linewidth =` is unaffected and still draws an ordinary stroke.
#'
#' A tapered stroke has one colour and no dash pattern, so a series falls back to
#' **one segment per span** -- each at the mean of its two endpoint widths, so
#' the width steps at every vertex -- when it also maps a continuous colour that
#' varies along the series (each segment then takes its own colour), uses a dashed or dotted `linetype`, is
#' drawn with a sketch theme, or has a missing width. That fallback emits one
#' path element per segment, worth knowing before combining those with tens of
#' thousands of rows. `mark_segment()` is already one element per row and takes
#' one width per segment.
#'
#' For edges on a [vgraph()] plot, use [scale_edge_width()] instead: an edge's
#' width is trained and legended independently of the line-width scale. That also
#' means `guides(linewidth = )` and `lims(linewidth = )` address *this* scale;
#' use `guides(edge_width = )` / `lims(edge_width = )` for a graph's edges.
#'
#' A missing (`NA`) width takes its segment's width from whichever endpoint is
#' known, so one missing value thins the line rather than erasing the two
#' segments that meet at it. A segment missing a width at both ends is dropped.
#' (A series with a missing width is drawn per segment, as above.)
#'
#' @param plot A [PlotSpec].
#' @param range Output line-width range `c(min, max)`, or `NULL` for the
#'   default `c(0.5, 4)`.
#' @param limits Data limits `c(min, max)`, or `NULL` to train from the data.
#' @param breaks Explicit legend breaks, or `NULL`.
#' @param name Legend title, or `NULL` to derive from the encoding.
#' @return The modified [PlotSpec].
#' @seealso [mark_line()], [mark_step()], [mark_segment()], [scale_edge_width()]
#' @examples
#' vplot(pressure) |>
#'   mark_line(x = temperature, y = pressure, linewidth = pressure) |>
#'   scale_linewidth(range = c(0.5, 5))
#' @export
scale_linewidth <- function(
  plot,
  range = NULL,
  limits = NULL,
  breaks = NULL,
  name = NULL
) {
  if (.ggplot2_asks()) return(.ggplot2_own("scale_linewidth"))
  .continuous_range_scale(plot, "linewidth", range, limits, breaks, name)
}

#' @rdname scale_linewidth
#' @export
scale_linewidth_continuous <- scale_linewidth

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
# edge_linetype, and their `_manual()` forms): validate `values` against the
# aesthetic's fixed palette, then declare a discrete scale. `breaks` picks the
# levels the legend shows, `labels` relabels them, `limits` fixes the level set
# (data outside it draws as `na.value`) and `na_value` is the value drawn for
# missing data. Named `values` are matched to levels by name.
.discrete_palette_scale <- function(
  plot,
  aesthetic,
  values,
  valid,
  noun,
  name,
  svg_ok = FALSE,
  breaks = NULL,
  labels = NULL,
  limits = NULL,
  na_value = NULL,
  call = rlang::caller_env()
) {
  .check_plot(plot)
  .check_manual_labels(labels, call = call)
  for (v in list(values, na_value)) {
    if (is.null(v)) {
      next
    }
    bad <- setdiff(as.character(v), valid)
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
      cli::cli_abort(
        c(
          "Unknown {label} in {.arg values}: {.val {bad}}.",
          i = "Use {.or {.val {valid}}}."
        ),
        call = call
      )
    }
  }
  .add_scale(
    plot,
    ScaleSpec(
      aesthetic = aesthetic,
      type = "discrete",
      palette = values,
      name = name,
      breaks = breaks,
      labels = labels,
      domain = .check_discrete_limits(limits, call = call),
      na_value = na_value
    )
  )
}

# `labels` of a discrete (manual) scale: NULL, a vector (one per shown break,
# or named by level), or a function of the shown levels.
.check_manual_labels <- function(labels, call = rlang::caller_env()) {
  if (
    !is.null(labels) &&
      !is.function(labels) &&
      !is.character(labels) &&
      !is.numeric(labels)
  ) {
    cli::cli_abort(
      "{.arg labels} must be a character vector, a function, or {.code NULL}.",
      call = call
    )
  }
  invisible(labels)
}

# `limits` of a discrete scale: NULL or the levels (a character vector or a
# factor, whose levels are taken in order).
.check_discrete_limits <- function(limits, call = rlang::caller_env()) {
  if (is.null(limits)) {
    return(NULL)
  }
  if (is.factor(limits)) {
    limits <- levels(limits)
  }
  if (!is.atomic(limits) || !length(limits)) {
    cli::cli_abort(
      "{.arg limits} must be a vector of levels or {.code NULL}.",
      call = call
    )
  }
  as.character(limits)
}

# The levels a discrete legend scale trains: its `limits` when set, else the
# data's levels in factor order. As in ggplot2, a factor level no row uses is
# dropped (`drop = TRUE`, the default), so it gets neither a legend key nor a
# palette colour -- the colours go to the levels that remain (VP-91); the
# scale's `drop = FALSE` keeps every level. `values` pools every panel, so a
# level used in only some facet panels stays.
.discrete_scale_levels <- function(scalespec, values) {
  dom <- if (!is.null(scalespec)) scalespec@domain else NULL
  if (is.character(dom) || is.factor(dom)) {
    return(as.character(dom))
  }
  levs <- .cat_levels(values)
  if (is.null(scalespec) || !isFALSE(scalespec@drop)) {
    vals <- if (is.list(values)) values else list(values)
    seen <- unique(unlist(lapply(vals, as.character), use.names = FALSE))
    levs <- levs[levs %in% seen]
  }
  levs
}

# The legend keys of a discrete scale: the levels `breaks` selects (unknown
# breaks dropped; all levels by default) and their labels -- from a function of
# the levels, a vector named by level (unnamed levels keep their name), or a
# plain vector with one entry per shown key.
.discrete_legend_keys <- function(scalespec, all_levels, aes) {
  ub <- if (!is.null(scalespec)) scalespec@breaks else NULL
  levels <- if (is.null(ub)) {
    all_levels
  } else {
    ub <- as.character(ub)
    ub[ub %in% all_levels]
  }
  ul <- if (!is.null(scalespec)) scalespec@labels else NULL
  if (is.function(ul)) {
    ul <- ul(levels)
  } else if (!is.null(names(ul))) {
    hit <- levels %in% names(ul)
    if (.is_rich_list(ul)) {
      lab <- as.list(levels)
      lab[hit] <- ul[levels[hit]]
    } else {
      lab <- levels
      lab[hit] <- as.character(ul[levels[hit]])
    }
    ul <- lab
  }
  list(levels = levels, labels = .match_labels(ul, levels, aes) %||% levels)
}

# Palette values for `levels`: a vector named by level is looked up by name
# (an unmatched level gets NA, drawn as the scale's `na.value`); an unnamed one
# is taken in order. The caller has already checked there are enough values.
.palette_for_levels <- function(pal, levels) {
  if (!is.null(names(pal)) && all(nzchar(names(pal)))) {
    return(unname(pal[levels]))
  }
  rep_len(unname(pal), length(levels))
}

# ggplot2's numeric shape codes (R `pch`) as vellum marker names. The open
# codes 0-14 become "<marker> open" (stroke only, in the point colour), the
# solid codes 15-20 a marker filled with the point colour, and the bordered
# codes 21-25 "<marker> filled" (`fill` inside a `color` outline), as ggplot2
# draws them (VP-103). Of the solid codes R outlines only 19 and 20, so 15-18
# are "<marker> bare" (no outline), and 20, the bullet, is "circle small" (2/3
# the size of 19, VP-106). Composite open glyphs (7, 9-14) take the nearest
# open marker; 3 and 4 are the line glyphs "plus" and "cross"; the asterisk (8)
# is an open star. Unknown codes are circles.
.pch_to_shape <- function(x) {
  if (!is.numeric(x)) {
    return(x)
  }
  map <- c(
    "0" = "square open",
    "1" = "circle open",
    "2" = "triangle open",
    "3" = "plus open",
    "4" = "cross open",
    "5" = "diamond open",
    "6" = "triangle_down open",
    "7" = "square_cross open",
    "8" = "asterisk open",
    "9" = "diamond_plus open",
    "10" = "circle_plus open",
    "11" = "triangles open",
    "12" = "square_plus open",
    "13" = "circle_cross open",
    "14" = "square_triangle open",
    "15" = "square bare",
    "16" = "circle bare",
    "17" = "triangle bare",
    "18" = "diamond bare",
    "19" = "circle",
    "20" = "circle small",
    "21" = "circle filled",
    "22" = "square filled",
    "23" = "diamond filled",
    "24" = "triangle filled",
    "25" = "triangle_down filled"
  )
  out <- unname(map[as.character(x)])
  out[is.na(out)] <- "circle"
  names(out) <- names(x)
  out
}

# Every built-in shape name a scale accepts: the markers and their open and
# bordered variants (VP-103), and the outline-free and small solid ones the
# pch codes 15-18 and 20 name (VP-106), and the composite markers of pch
# 7-14 (VP-115).
.SHAPE_VALID <- c(
  .SHAPE_PALETTE,
  paste(.SHAPE_PALETTE, "open"),
  paste(.SHAPE_PALETTE, "filled"),
  paste(.SHAPE_PALETTE, "bare"),
  paste(.SHAPE_PALETTE, "small"),
  paste(.COMPOSITE_SHAPES, "open")
)

# ggplot2's numeric line types (0 = blank, 1 = solid, 2 = dashed, ...) as names.
# vellum has no blank line, so 0 is solid.
.lty_to_linetype <- function(x) {
  if (!is.numeric(x)) {
    return(x)
  }
  out <- c("solid", .LINETYPE_PALETTE)[pmin(pmax(as.integer(x), 0L), 6L) + 1L]
  names(out) <- names(x)
  out
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
#'
#'   For `scale_shape_manual()` `values` is required, and may also be
#'   ggplot2's numeric shape codes (`16`, `17`, ...), which map to the nearest
#'   marker. As in ggplot2, the open codes (0-14) draw an outline in the point
#'   colour, the solid ones (15-20) fill with it, and the bordered ones (21-25)
#'   fill with `fill` inside a `color` outline (unfilled when no `fill` is
#'   given). Of the solid codes, 15-18 have no outline and 20 is 2/3 the size
#'   of 19, as in ggplot2. Each code draws at R's size for it: the triangles
#'   are larger than the circle (equal in area), the bordered square (22) a
#'   little smaller than the open and solid ones, and so on. By name, these are
#'   `"circle open"`, `"circle filled"`, `"circle bare"` (no outline),
#'   `"circle small"` and so on for every built-in marker; the plain names
#'   (`"triangle"`) keep vellum's own sizes. Named values are matched to levels
#'   by name.
#' @param name Legend title, or `NULL` to derive from the encoding.
#' @param breaks The levels the legend shows (and their order), or `NULL` for
#'   all of them.
#' @param labels Legend labels: one per shown level, a vector named by level,
#'   or a function of the levels; `NULL` keeps the levels.
#' @param limits The levels the scale covers (a character vector), or `NULL`
#'   for the levels in the data. Data outside `limits` draws as `na.value`.
#' @param na.value The shape drawn for missing values (default a circle).
#' @param drop For the discrete scales, drop factor levels that no row uses
#'   (default `TRUE`, as in ggplot2): they get no legend key and no palette
#'   entry, so the colours go to the levels that remain. `FALSE` keeps every
#'   level. `limits` overrides it.
#' @return The modified [PlotSpec].
#' @examples
#' vplot(mtcars) |> mark_point(x = wt, y = mpg, shape = factor(cyl))
#' # SVG icon markers, one per level:
#' star <- "M12 2l3 7h7l-5.5 4.5 2 7-6.5-4.5-6.5 4.5 2-7L2 9h7z"
#' vplot(mtcars) |>
#'   mark_point(x = wt, y = mpg, shape = factor(cyl)) |>
#'   scale_shape(values = c(star, star, star))
#' vplot(mtcars) |>
#'   mark_point(x = wt, y = mpg, shape = factor(cyl)) |>
#'   scale_shape_manual(values = c("4" = 16, "6" = 17, "8" = 15))
#' @export
scale_shape <- function(
  plot,
  values = NULL,
  name = NULL,
  breaks = NULL,
  labels = NULL,
  limits = NULL,
  na.value = NULL,
  drop = TRUE
) {
  if (.ggplot2_asks()) {
    return(.ggplot2_own("scale_shape"))
  }
  plot <- .discrete_palette_scale(
    plot,
    "shape",
    .pch_to_shape(values),
    .SHAPE_VALID,
    "shape",
    name,
    svg_ok = TRUE,
    breaks = breaks,
    labels = labels,
    limits = limits,
    na_value = .pch_to_shape(na.value)
  )
  .with_drop(plot, "shape", drop)
}

#' @rdname scale_shape
#' @export
scale_shape_manual <- function(
  plot,
  values,
  breaks = NULL,
  labels = NULL,
  name = NULL,
  limits = NULL,
  na.value = NULL,
  drop = TRUE
) {
  .check_manual_present(values)
  plot <- scale_shape(
    plot,
    values = values,
    name = name,
    breaks = breaks,
    labels = labels,
    limits = limits,
    na.value = na.value
  )
  .with_drop(plot, "shape", drop)
}

#' @rdname scale_shape
#' @export
scale_shape_discrete <- scale_shape

# A `_manual()` scale needs its `values`.
.check_manual_present <- function(values, call = rlang::caller_env()) {
  if (missing(values) || is.null(values) || !length(values)) {
    cli::cli_abort("{.arg values} must be a non-empty vector.", call = call)
  }
  invisible(values)
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
  if (.ggplot2_asks()) return(.ggplot2_own("scale_color_identity"))
  .check_plot(plot)
  .add_scale(plot, ScaleSpec(aesthetic = "color", type = "identity"))
}

#' @rdname scale_color_identity
#' @export
scale_fill_identity <- function(plot) {
  if (.ggplot2_asks()) return(.ggplot2_own("scale_fill_identity"))
  .check_plot(plot)
  .add_scale(plot, ScaleSpec(aesthetic = "fill", type = "identity"))
}

#' @rdname scale_color_identity
#' @export
scale_colour_identity <- scale_color_identity

#' @rdname scale_color_identity
#' @export
scale_size_identity <- function(plot) {
  if (.ggplot2_asks()) return(.ggplot2_own("scale_size_identity"))
  .check_plot(plot)
  .add_scale(plot, ScaleSpec(aesthetic = "size", type = "identity"))
}

#' @rdname scale_color_identity
#' @export
scale_shape_identity <- function(plot) {
  if (.ggplot2_asks()) return(.ggplot2_own("scale_shape_identity"))
  .check_plot(plot)
  .add_scale(plot, ScaleSpec(aesthetic = "shape", type = "identity"))
}

#' @rdname scale_color_identity
#' @export
scale_alpha_identity <- function(plot) {
  if (.ggplot2_asks()) return(.ggplot2_own("scale_alpha_identity"))
  .check_plot(plot)
  .add_scale(plot, ScaleSpec(aesthetic = "alpha", type = "identity"))
}

#' @rdname scale_color_identity
#' @export
scale_linetype_identity <- function(plot) {
  if (.ggplot2_asks()) return(.ggplot2_own("scale_linetype_identity"))
  .check_plot(plot)
  .add_scale(plot, ScaleSpec(aesthetic = "linetype", type = "identity"))
}

#' Alpha (opacity) scale
#'
#' Declare the scale for a mapped `alpha` aesthetic: data values map to an
#' opacity `range` in `[0, 1]`. An alpha legend is drawn automatically.
#'
#' A factor, character or logical mapped to `alpha` gets a discrete scale, its
#' levels spread evenly over `range`, with a warning for an unordered variable
#' (as in ggplot2).
#'
#' @param plot A [PlotSpec].
#' @param range Numeric length-2 output opacity range, or `NULL` for the
#'   default `c(0.1, 1)`.
#' @param limits Numeric length-2 data domain, or `NULL` to train from the data.
#' @param breaks Explicit legend breaks, or `NULL`.
#' @param name Legend title, or `NULL` to derive from the encoding.
#' @param oob What to do with values outside `limits`: `"censor"` (they get
#'   no opacity, so the mark is not drawn), `"squish"` (the opacity of the
#'   nearest limit), `"keep"` (extrapolated past the range), or a
#'   `function(x, range)`. `NULL` (default) keeps them.
#' @return The modified [PlotSpec].
#' @examples
#' vplot(mtcars) |> mark_point(x = wt, y = mpg, alpha = hp) |> scale_alpha(range = c(0.2, 1))
#' @export
scale_alpha <- function(
  plot,
  range = NULL,
  limits = NULL,
  breaks = NULL,
  name = NULL,
  oob = NULL
) {
  if (.ggplot2_asks()) return(.ggplot2_own("scale_alpha"))
  .continuous_range_scale(
    plot,
    "alpha",
    range,
    limits,
    breaks,
    name,
    upper = 1,
    oob = oob
  )
}

#' @rdname scale_alpha
#' @export
scale_alpha_continuous <- scale_alpha

#' @rdname scale_alpha
#'
#' @description
#' `scale_alpha_manual()` sets the opacity of each level of a discrete
#' variable directly via `values` (named values match levels by name).
#' `scale_alpha_discrete()` / `scale_alpha_ordinal()` spread the levels evenly
#' over `range`; the discrete form warns that alpha is not advised for an
#' unordered variable, the ordinal form does not.
#'
#' @param values For `scale_alpha_manual()`, opacities in `[0, 1]`, one per
#'   level (or named by level).
#' @param labels For the discrete forms, legend labels (one per key, a vector
#'   named by level, or a function), or `NULL`.
#' @param na.value For the discrete forms, the opacity drawn for missing
#'   values, or `NULL` to drop them.
#' @param drop For the discrete forms, drop factor levels that no row uses
#'   (default `TRUE`, as in ggplot2): they get no legend key and no opacity, so
#'   the range is spread over the levels that remain. `FALSE` keeps every level.
#'   `limits` overrides it.
#' @examples
#' vplot(mtcars) |>
#'   mark_point(x = wt, y = mpg, alpha = factor(cyl)) |>
#'   scale_alpha_manual(values = c(0.2, 0.5, 1))
#' @export
scale_alpha_manual <- function(
  plot,
  values,
  breaks = NULL,
  labels = NULL,
  name = NULL,
  limits = NULL,
  na.value = NULL,
  drop = TRUE
) {
  .check_manual_present(values)
  plot <- .discrete_range_scale(
    plot,
    "alpha",
    "discrete",
    values = .check_manual_numbers(values, lower = 0, upper = 1),
    breaks = breaks,
    labels = labels,
    name = name,
    limits = limits,
    na_value = na.value,
    upper = 1
  )
  .with_drop(plot, "alpha", drop)
}

#' @rdname scale_alpha
#' @export
scale_alpha_discrete <- function(
  plot,
  range = NULL,
  breaks = NULL,
  labels = NULL,
  name = NULL,
  limits = NULL,
  na.value = NULL,
  drop = TRUE
) {
  if (.ggplot2_asks()) return(.ggplot2_own("scale_alpha_discrete"))
  plot <- .discrete_range_scale(
    plot,
    "alpha",
    "discrete",
    range = range,
    breaks = breaks,
    labels = labels,
    name = name,
    limits = limits,
    na_value = na.value,
    upper = 1
  )
  .with_drop(plot, "alpha", drop)
}

#' @rdname scale_alpha
#' @export
scale_alpha_ordinal <- function(
  plot,
  range = NULL,
  breaks = NULL,
  labels = NULL,
  name = NULL,
  limits = NULL,
  na.value = NULL,
  drop = TRUE
) {
  if (.ggplot2_asks()) return(.ggplot2_own("scale_alpha_ordinal"))
  plot <- .discrete_range_scale(
    plot,
    "alpha",
    "ordinal",
    range = range,
    breaks = breaks,
    labels = labels,
    name = name,
    limits = limits,
    na_value = na.value,
    upper = 1
  )
  .with_drop(plot, "alpha", drop)
}

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
#'   For `scale_linetype_manual()` `values` is required, and may also be
#'   ggplot2's numeric line types (`1` solid, `2` dashed, ... `6` twodash).
#'   Named values are matched to levels by name.
#' @param name Legend title, or `NULL` to derive from the encoding.
#' @param breaks,labels,limits As for [scale_shape()].
#' @param na.value The line type drawn for missing values, or `NULL`.
#' @param drop For the discrete scales, drop factor levels that no row uses
#'   (default `TRUE`, as in ggplot2): they get no legend key and no palette
#'   entry, so the colours go to the levels that remain. `FALSE` keeps every
#'   level. `limits` overrides it.
#' @return The modified [PlotSpec].
#' @examples
#' df <- data.frame(x = rep(1:10, 2), y = rnorm(20), g = rep(c("a", "b"), each = 10))
#' vplot(df) |> mark_line(x = x, y = y, linetype = g)
#' vplot(df) |>
#'   mark_line(x = x, y = y, linetype = g) |>
#'   scale_linetype_manual(values = c(a = "dotted", b = "solid"))
#' @export
scale_linetype <- function(
  plot,
  values = NULL,
  name = NULL,
  breaks = NULL,
  labels = NULL,
  limits = NULL,
  na.value = NULL,
  drop = TRUE
) {
  if (.ggplot2_asks()) return(.ggplot2_own("scale_linetype"))
  plot <- .discrete_palette_scale(
    plot,
    "linetype",
    .lty_to_linetype(values),
    .LINETYPE_PALETTE,
    "line type",
    name,
    breaks = breaks,
    labels = labels,
    limits = limits,
    na_value = .lty_to_linetype(na.value)
  )
  .with_drop(plot, "linetype", drop)
}

#' @rdname scale_linetype
#' @export
scale_linetype_manual <- function(
  plot,
  values,
  breaks = NULL,
  labels = NULL,
  name = NULL,
  limits = NULL,
  na.value = NULL,
  drop = TRUE
) {
  .check_manual_present(values)
  plot <- scale_linetype(
    plot,
    values = values,
    name = name,
    breaks = breaks,
    labels = labels,
    limits = limits,
    na.value = na.value
  )
  .with_drop(plot, "linetype", drop)
}

#' @rdname scale_linetype
#' @export
scale_linetype_discrete <- scale_linetype
