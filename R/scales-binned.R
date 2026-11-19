#' @include classes.R scales.R
NULL

# --- binned (classed) colour scales -----------------------------------------
#
# A binned scale cuts a continuous variable into classes and maps each class to
# one colour of a sequential palette -- the cartographic norm for choropleths
# (classed maps read back to a value range far more reliably than an unclassed
# ramp). Breaks come from `classInt::classIntervals()` when available (it offers
# jenks/fisher/etc.); a base-R fallback covers the common styles without the
# dependency. `classInt` lives in Suggests.

# Compute `n`-class breaks for `v` under a classification `style`. `quantile`,
# `equal`, and `pretty` are handled in base R so no dependency is needed; every
# other style (jenks, fisher, kmeans, headtails, sd, ...) routes through
# classInt::classIntervals(). Returns a sorted numeric vector of length n+1.
# Validate the user-facing `n` (bin count) and `style` up front, so a bad value
# gives a clear error instead of aborting deep in quantile()/seq()/classInt or on
# a length > 1 `style %in% ...` comparison.
.check_binned_args <- function(n, style, call = rlang::caller_env()) {
  if (
    !is.null(n) &&
      (!is.numeric(n) || length(n) != 1L || !is.finite(n) || n < 1)
  ) {
    cli::cli_abort("{.arg n} must be a single positive integer.", call = call)
  }
  if (!is.null(style) && (!is.character(style) || length(style) != 1L)) {
    cli::cli_abort("{.arg style} must be a single string.", call = call)
  }
}

.binned_breaks <- function(v, n, style) {
  v <- v[is.finite(v)]
  if (!length(v)) {
    cli::cli_abort("A binned scale needs at least one finite value.")
  }
  base_styles <- c("quantile", "equal", "pretty")
  if (style %in% base_styles) {
    brks <- switch(
      style,
      quantile = stats::quantile(
        v,
        probs = seq(0, 1, length.out = n + 1),
        names = FALSE,
        type = 7
      ),
      equal = seq(min(v), max(v), length.out = n + 1),
      pretty = {
        p <- sort(unique(pretty(v, n = n)))
        # Keep the cuts that *bracket* the data: the largest cut <= min and the
        # smallest cut >= max, plus everything between. Trimming to strictly
        # inside the range (the old behaviour) dropped the outer cuts and left
        # values below the first / above the last cut unclassified.
        lo <- which(p <= min(v))
        hi <- which(p >= max(v))
        i0 <- if (length(lo)) max(lo) else 1L
        i1 <- if (length(hi)) min(hi) else length(p)
        p[i0:i1]
      }
    )
    brks <- sort(unique(as.numeric(brks)))
    if (length(brks) < 2L) {
      brks <- range(v) + c(-0.5, 0.5)
    }
    return(brks)
  }
  .need_pkg("classInt", sprintf("scale_*_binned(style = \"%s\")", style))
  ci <- classInt::classIntervals(v, n = n, style = style)
  sort(unique(as.numeric(ci$brks)))
}

# ggplot2's binned-scale breaks (`style = "extended"`, used by the
# ggplot2-named binned scales: scale_*_steps2(), _stepsn(), _fermenter(),
# _viridis_b(), scale_size_binned()): about `n` (default 5) "nice" boundaries
# inside the limits. Without user limits the outer classes widen to a whole bin
# past the outermost boundary, as ggplot2's `nice.breaks` does; with limits the
# limits are the outer boundaries. Returns every class boundary, sorted.
.extended_bin_breaks <- function(v, n = NULL, limits = NULL) {
  lim <- if (!is.null(limits)) sort(as.numeric(limits)) else range(v)
  brks <- scales::breaks_extended(n = n %||% 5)(lim)
  brks <- brks[is.finite(brks) & brks >= lim[1] & brks <= lim[2]]
  if (is.null(limits)) {
    brks <- brks[!brks %in% lim]
    nb <- length(brks)
    if (nb >= 2L) {
      lim <- c(
        brks[1] - (brks[2] - brks[1]),
        brks[nb] + (brks[nb] - brks[nb - 1L])
      )
    } else if (nb == 1L) {
      bin <- max(brks - lim[1], lim[2] - brks)
      lim <- brks + c(-bin, bin)
    } else if (diff(lim) == 0) {
      lim <- lim + c(-0.05, 0.05)
    }
  }
  sort(unique(c(lim[1], brks, lim[2])))
}

# The class boundaries of a binned colour / size scale. Explicit `breaks` win:
# as given for the classic binned scales, and -- for the ggplot2-named ones
# (`style = "extended"`) -- framed by the limits (or the data range), as
# ggplot2 adds its limits as the outermost boundaries. Otherwise the scale's
# classification `style`/`n` over the data, restricted to any `limits`.
.binned_class_breaks <- function(v, scalespec, user_breaks = scalespec@breaks) {
  dom <- if (!is.null(scalespec@domain)) as.numeric(scalespec@domain)
  style <- scalespec@style %||% "quantile"
  if (identical(style, "extended")) {
    if (!is.null(user_breaks)) {
      lim <- if (!is.null(dom)) {
        sort(dom)
      } else if (length(v)) {
        range(v)
      }
      ub <- sort(as.numeric(user_breaks))
      ub <- ub[is.null(lim) | (ub >= min(lim) & ub <= max(lim))]
      return(sort(unique(c(lim, ub))))
    }
    if (!length(v) && is.null(dom)) {
      cli::cli_abort("A binned scale needs at least one finite value.")
    }
    return(.extended_bin_breaks(v, scalespec@n, dom))
  }
  if (!is.null(user_breaks)) {
    return(sort(as.numeric(user_breaks)))
  }
  # A user-set `limits` (domain) restricts the data the scale bins over, so the
  # classes cover the requested range rather than the observed data extent.
  if (!is.null(dom)) {
    v <- v[v >= min(dom) & v <= max(dom)]
  }
  .binned_breaks(v, as.integer(scalespec@n %||% 5L), style)
}

# The class index of each value under boundaries `brks`: classes are closed on
# the right ("(a, b]", the first "[a, b]") under ggplot2's `style =
# "extended"` and on the left otherwise. Values past the outer boundaries go to
# the outer classes (the caller handles out-of-bounds values first).
.class_index <- function(x, brks, right = FALSE) {
  findInterval(
    x,
    brks,
    left.open = right,
    rightmost.closed = TRUE,
    all.inside = TRUE
  )
}

# One colour per class of a binned colour scale. A ColorBrewer palette (from
# scale_*_fermenter()) gives its exact k-class colours. A colour ramp on a
# ggplot2-named binned scale (`style = "extended"`: steps2/stepsn) is sampled
# at each class's centre, rescaled over the boundaries -- about `midpoint` for
# a diverging scale -- as ggplot2 does. Anything else (named palettes, and the
# classic binned scales) spreads k colours evenly across the palette.
.binned_colours <- function(pal, brks, midpoint = NULL, style = NULL) {
  k <- length(brks) - 1L
  if (is.character(pal) && length(pal) == 1L && grepl(.BREWER_TOKEN, pal)) {
    return(.brewer_colours(pal, k))
  }
  # A reversed hcl palette (scale_*_viridis_b(direction = -1)).
  if (is.character(pal) && length(pal) == 1L && grepl("^hclrev:", pal)) {
    full <- .hcl_full(sub("^hclrev:", "", pal))
    return(.ramp_pal(rev(grDevices::hcl.colors(256, full)), k))
  }
  stops <- .continuous_stops(pal)
  if (identical(style, "extended") && !.is_hcl_pal(pal) && !is.null(pal)) {
    mids <- brks[-1L] - diff(brks) / 2
    rng <- range(brks)
    at <- if (is.null(midpoint)) {
      scales::rescale(mids, from = rng)
    } else {
      scales::rescale_mid(mids, from = rng, mid = midpoint)
    }
    ramp <- .ramp_pal(stops, 1001L)
    return(ramp[round(pmin(pmax(at, 0), 1) * 1000) + 1L])
  }
  .ramp_pal(stops, k)
}

# Train a binned size / alpha scale (scale_size_binned()): one output per
# class, the class centre rescaled over the boundaries and mapped on an area
# scale for size (ggplot2's `pal_area()`), linearly otherwise. The legend keys
# the classes. Values past the outer boundaries squish onto the outer classes.
.train_binned_aes <- function(
  scalespec,
  values,
  aes,
  out_range,
  legend_field,
  titles
) {
  v <- as.numeric(unlist(values, use.names = FALSE))
  has_na <- anyNA(v)
  v <- v[is.finite(v)]
  brks <- .binned_class_breaks(v, scalespec)
  k <- length(brks) - 1L
  if (k < 1L) {
    cli::cli_abort(c(
      "A binned {.field {aes}} scale needs at least 2 breaks (1 class).",
      i = "{.arg breaks} has {length(brks)} value{?s}."
    ))
  }
  right <- identical(scalespec@style, "extended")
  at <- scales::rescale(brks[-1L] - diff(brks) / 2, from = range(brks))
  outs <- if (identical(aes, "size")) {
    scales::rescale(sqrt(at), to = out_range, from = c(0, 1))
  } else {
    scales::rescale(at, to = out_range, from = c(0, 1))
  }
  oob <- .oob_fun(scalespec) %||% scales::oob_squish
  map <- function(x) {
    x <- oob(as.numeric(x), range = range(brks))
    out <- outs[.class_index(x, brks, right)]
    out[!is.finite(x)] <- NA_real_
    out
  }
  labels <- .match_labels(scalespec@labels, seq_len(k), aes) %||%
    .interval_labels(brks, right = right)
  rlang::exec(
    trained_scale,
    aes,
    "discrete",
    map = map,
    name = titles$title[[aes]],
    levels = labels,
    na = has_na,
    legend_breaks = labels,
    legend_labels = labels,
    !!!rlang::set_names(list(outs), legend_field)
  )
}

# Human-readable interval labels "[a, b)" from a break vector; the last class is
# right-closed "[y, z]". With `right = TRUE` the classes close on the right,
# "(a, b]", the first left-closed "[a, b]". Numbers are formatted compactly.
.interval_labels <- function(brks, right = FALSE) {
  fmt <- function(x) trimws(formatC(x, format = "g", digits = 4))
  k <- length(brks) - 1L
  if (k < 1L) {
    return(character(0))
  }
  lo <- fmt(brks[-length(brks)])
  hi <- fmt(brks[-1])
  if (right) {
    return(paste0(c("[", rep("(", k - 1L)), lo, ", ", hi, "]"))
  }
  close <- c(rep(")", k - 1L), "]")
  paste0("[", lo, ", ", hi, close)
}

#' Binned (classed) colour scales
#'
#' `scale_fill_binned()` / `scale_color_binned()` cut a continuous aesthetic into
#' classes and colour each class with one step of a sequential palette — the
#' standard choropleth scale. Class breaks are chosen by a classification
#' `style`. `quantile`, `equal`, and `pretty` need no extra packages; other
#' styles (e.g. `"jenks"`, `"fisher"`, `"kmeans"`, `"headtails"`) delegate to the
#' optional `classInt` package (in `Suggests`) and error if it is absent.
#'
#' @param plot A [PlotSpec].
#' @param style Classification style: one of `"quantile"`, `"equal"`, `"pretty"`
#'   (base R), or any `classInt::classIntervals()` style. Default `"quantile"`.
#' @param n Number of classes (default `5`). Ignored when `breaks` is supplied.
#' @param breaks Explicit class boundaries (length = number of classes + 1),
#'   overriding `style`/`n`.
#' @param palette A sequential palette: `NULL` (batlow), an `grDevices::hcl.pals()`
#'   name, or a vector of colours interpolated across the classes.
#' @param labels Optional class labels (one per class); defaults to interval
#'   ranges like `"[0, 10)"`.
#' @param na_value Fill colour for `NA` values (shown as a distinct legend
#'   swatch). Default `"grey80"`.
#' @param name Legend title, or `NULL` to derive from the encoding.
#' @return The modified [PlotSpec].
#' @seealso [mark_sf()], [scale_fill_continuous()]
#' @examples
#' \dontrun{
#' nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
#' vplot(nc) |>
#'   mark_sf(fill = SID74) |>
#'   scale_fill_binned(style = "quantile", n = 5) |>
#'   coord_sf()
#' }
#' @export
scale_fill_binned <- function(
  plot,
  style = "quantile",
  n = 5,
  breaks = NULL,
  palette = NULL,
  labels = NULL,
  na_value = "grey80",
  name = NULL
) {
  .binned_scale(plot, "fill", style, n, breaks, palette, labels, na_value, name)
}

#' @rdname scale_fill_binned
#' @export
scale_color_binned <- function(
  plot,
  style = "quantile",
  n = 5,
  breaks = NULL,
  palette = NULL,
  labels = NULL,
  na_value = "grey80",
  name = NULL
) {
  .binned_scale(
    plot,
    "color",
    style,
    n,
    breaks,
    palette,
    labels,
    na_value,
    name
  )
}

#' @rdname scale_fill_binned
#' @details
#' `scale_color_steps()` / `scale_fill_steps()` are aliases of the binned colour
#' scales, for ggplot2 users who reach for the `_steps` name — a continuous
#' variable cut into steps with an interval-key legend.
#' @export
scale_color_steps <- scale_color_binned

#' @rdname scale_fill_binned
#' @export
scale_colour_steps <- scale_color_binned

#' @rdname scale_fill_binned
#' @export
scale_fill_steps <- scale_fill_binned

#' @rdname scale_fill_binned
#' @description
#' The ggplot2-named binned colour scales follow ggplot2's binning: without
#' `breaks`, about `n.breaks` "nice" class boundaries inside the data range,
#' the outer classes widened to whole bins (with `limits`, the limits are the
#' outer boundaries); classes close on the right, `(a, b]`.
#'
#' * `scale_*_steps2()` --- a diverging `low`--`mid`--`high` ramp centred on
#'   `midpoint`, each class taking the ramp's colour at its centre.
#' * `scale_*_stepsn()` --- an n-colour ramp `colours`, sampled at each class
#'   centre.
#' * `scale_*_fermenter()` --- a ColorBrewer palette's k-class colours (as
#'   [scale_color_distiller()], but binned).
#' * `scale_*_viridis_b()` --- k evenly spaced viridis colours.
#'
#' @param low,mid,high,midpoint For `scale_*_steps2()`, the end and midpoint
#'   colours and the data value at `mid`.
#' @param colours For `scale_*_stepsn()`, the colours of the ramp.
#' @param type,direction For `scale_*_fermenter()`, the ColorBrewer family a
#'   numeric `palette` indexes and its order (see [scale_color_brewer()]); for
#'   `scale_*_viridis_b()`, `direction = -1` reverses the map.
#' @param option For `scale_*_viridis_b()`, the viridis map (see
#'   [scale_color_viridis_c()]).
#' @param limits For the ggplot2-named scales, a length-2 numeric
#'   `c(min, max)`: the outer class boundaries.
#' @param n.breaks For the ggplot2-named scales, the approximate number of
#'   class boundaries when `breaks` is `NULL` (default 5).
#' @param na.value For the ggplot2-named scales, the colour of missing values.
#' @param oob For the ggplot2-named scales, what to do with values outside the
#'   outer boundaries: `"squish"` (the default, as in ggplot2) colours them as
#'   the outer class, `"censor"` with `na.value`; or a `function(x, range)`.
#' @examples
#' vplot(faithful) |>
#'   mark_point(x = eruptions, y = waiting, color = waiting - 70) |>
#'   scale_color_steps2()
#' @export
scale_color_steps2 <- function(
  plot,
  low = "#832424",
  mid = "#FFFFFF",
  high = "#3A3A98",
  midpoint = 0,
  name = NULL,
  breaks = NULL,
  labels = NULL,
  limits = NULL,
  n.breaks = NULL,
  na.value = "grey50",
  oob = NULL
) {
  .gg_binned_scale(
    plot,
    "color",
    c(low, mid, high),
    name = name,
    breaks = breaks,
    labels = labels,
    limits = limits,
    n.breaks = n.breaks,
    na.value = na.value,
    oob = oob,
    midpoint = midpoint
  )
}

#' @rdname scale_fill_binned
#' @export
scale_color_stepsn <- function(
  plot,
  colours,
  name = NULL,
  breaks = NULL,
  labels = NULL,
  limits = NULL,
  n.breaks = NULL,
  na.value = "grey50",
  oob = NULL
) {
  .check_manual_values(colours, "colours")
  .gg_binned_scale(
    plot,
    "color",
    colours,
    name = name,
    breaks = breaks,
    labels = labels,
    limits = limits,
    n.breaks = n.breaks,
    na.value = na.value,
    oob = oob
  )
}

#' @rdname scale_fill_binned
#' @export
scale_color_fermenter <- function(
  plot,
  palette = 1,
  type = "seq",
  direction = -1,
  name = NULL,
  breaks = NULL,
  labels = NULL,
  limits = NULL,
  n.breaks = NULL,
  na.value = "grey50",
  oob = NULL
) {
  .gg_binned_scale(
    plot,
    "color",
    .brewer_scale_palette(palette, type, direction, "discrete"),
    name = name,
    breaks = breaks,
    labels = labels,
    limits = limits,
    n.breaks = n.breaks,
    na.value = na.value,
    oob = oob
  )
}

#' @rdname scale_fill_binned
#' @export
scale_color_viridis_b <- function(
  plot,
  option = "viridis",
  direction = 1,
  name = NULL,
  breaks = NULL,
  labels = NULL,
  limits = NULL,
  n.breaks = NULL,
  na.value = "grey50",
  oob = NULL
) {
  .gg_binned_scale(
    plot,
    "color",
    .viridis_binned_pal(option, direction),
    name = name,
    breaks = breaks,
    labels = labels,
    limits = limits,
    n.breaks = n.breaks,
    na.value = na.value,
    oob = oob
  )
}

#' @rdname scale_fill_binned
#' @export
scale_fill_steps2 <- function(
  plot,
  low = "#832424",
  mid = "#FFFFFF",
  high = "#3A3A98",
  midpoint = 0,
  name = NULL,
  breaks = NULL,
  labels = NULL,
  limits = NULL,
  n.breaks = NULL,
  na.value = "grey50",
  oob = NULL
) {
  .gg_binned_scale(
    plot,
    "fill",
    c(low, mid, high),
    name = name,
    breaks = breaks,
    labels = labels,
    limits = limits,
    n.breaks = n.breaks,
    na.value = na.value,
    oob = oob,
    midpoint = midpoint
  )
}

#' @rdname scale_fill_binned
#' @export
scale_fill_stepsn <- function(
  plot,
  colours,
  name = NULL,
  breaks = NULL,
  labels = NULL,
  limits = NULL,
  n.breaks = NULL,
  na.value = "grey50",
  oob = NULL
) {
  .check_manual_values(colours, "colours")
  .gg_binned_scale(
    plot,
    "fill",
    colours,
    name = name,
    breaks = breaks,
    labels = labels,
    limits = limits,
    n.breaks = n.breaks,
    na.value = na.value,
    oob = oob
  )
}

#' @rdname scale_fill_binned
#' @export
scale_fill_fermenter <- function(
  plot,
  palette = 1,
  type = "seq",
  direction = -1,
  name = NULL,
  breaks = NULL,
  labels = NULL,
  limits = NULL,
  n.breaks = NULL,
  na.value = "grey50",
  oob = NULL
) {
  .gg_binned_scale(
    plot,
    "fill",
    .brewer_scale_palette(palette, type, direction, "discrete"),
    name = name,
    breaks = breaks,
    labels = labels,
    limits = limits,
    n.breaks = n.breaks,
    na.value = na.value,
    oob = oob
  )
}

#' @rdname scale_fill_binned
#' @export
scale_fill_viridis_b <- function(
  plot,
  option = "viridis",
  direction = 1,
  name = NULL,
  breaks = NULL,
  labels = NULL,
  limits = NULL,
  n.breaks = NULL,
  na.value = "grey50",
  oob = NULL
) {
  .gg_binned_scale(
    plot,
    "fill",
    .viridis_binned_pal(option, direction),
    name = name,
    breaks = breaks,
    labels = labels,
    limits = limits,
    n.breaks = n.breaks,
    na.value = na.value,
    oob = oob
  )
}

#' @rdname scale_fill_binned
#' @export
scale_colour_steps2 <- scale_color_steps2

#' @rdname scale_fill_binned
#' @export
scale_colour_stepsn <- scale_color_stepsn

#' @rdname scale_fill_binned
#' @export
scale_colour_fermenter <- scale_color_fermenter

#' @rdname scale_fill_binned
#' @export
scale_colour_viridis_b <- scale_color_viridis_b

# The shared engine behind the ggplot2-named binned colour scales: a binned
# scale with ggplot2's breaks (`style = "extended"`).
.gg_binned_scale <- function(
  plot,
  aesthetic,
  palette,
  name,
  breaks,
  labels,
  limits,
  n.breaks,
  na.value,
  oob,
  midpoint = NULL,
  call = rlang::caller_env()
) {
  .check_plot(plot)
  .check_continuous_limits(limits, "limits", numeric = TRUE, call = call)
  .check_binned_args(n.breaks, "extended", call = call)
  .add_scale(
    plot,
    ScaleSpec(
      aesthetic = aesthetic,
      type = "binned",
      palette = palette,
      breaks = breaks,
      labels = labels,
      domain = limits,
      style = "extended",
      n = n.breaks,
      na_value = na.value,
      midpoint = midpoint,
      name = name,
      oob = .check_oob(oob, call = call)
    )
  )
}

# The palette of scale_*_viridis_b(): the hcl.colors() viridis map, or (for
# `direction = -1`) the brewer-style reversed ramp as a colour vector.
.viridis_binned_pal <- function(option, direction, call = rlang::caller_env()) {
  if (
    !identical(as.numeric(direction), 1) &&
      !identical(as.numeric(direction), -1)
  ) {
    cli::cli_abort(
      "{.arg direction} must be {.val {1}} or {.val {-1}}.",
      call = call
    )
  }
  pal <- .viridis_pal(option)
  if (direction == 1) {
    return(pal)
  }
  paste0("hclrev:", pal)
}

#' Binned position scales
#'
#' `scale_x_binned()` / `scale_y_binned()` cut a continuous position variable into
#' bins: axis ticks fall at the bin **boundaries** and each datum is drawn at its
#' bin **centre**. Breaks use the same classification as the binned colour scales
#' (`style`/`n`; `classInt` for jenks/fisher/… — a Suggest). Handy to summarise a
#' dense continuous axis, or to place `mark_bar()` on a binned continuous `x`.
#'
#' @param plot A [PlotSpec].
#' @param style Binning style: `"pretty"` (default), `"equal"`, `"quantile"`, or a
#'   `classInt` style.
#' @param n Target number of bins.
#' @param breaks Explicit bin boundaries (overrides `style`/`n`), or `NULL`.
#' @param labels Explicit boundary labels, or `NULL` to format the boundaries.
#' @param name Axis title, or `NULL` to derive from the encoding.
#' @param guide The axis guide: [guide_axis()] to stagger, rotate or thin the
#'   labels, `"none"` for no labels or ticks, or `NULL` (default) for the plain
#'   axis. Same as `guides(x = )` / `guides(y = )`.
#' @return The modified [PlotSpec].
#' @examples
#' vplot(mtcars) |> mark_point(x = wt, y = mpg) |> scale_x_binned(n = 6)
#' @export
scale_x_binned <- function(
  plot,
  style = "pretty",
  n = 10,
  breaks = NULL,
  labels = NULL,
  name = NULL,
  guide = NULL
) {
  .binned_position_scale(plot, "x", style, n, breaks, labels, name, guide)
}

#' @rdname scale_x_binned
#' @export
scale_y_binned <- function(
  plot,
  style = "pretty",
  n = 10,
  breaks = NULL,
  labels = NULL,
  name = NULL,
  guide = NULL
) {
  .binned_position_scale(plot, "y", style, n, breaks, labels, name, guide)
}

.binned_position_scale <- function(
  plot,
  aesthetic,
  style,
  n,
  breaks,
  labels,
  name,
  guide = NULL
) {
  .check_plot(plot)
  .check_binned_args(n, style)
  plot <- .add_scale(
    plot,
    ScaleSpec(
      aesthetic = aesthetic,
      type = "binned",
      breaks = breaks,
      labels = labels,
      style = style,
      n = n,
      name = name
    )
  )
  .with_axis_guide(plot, aesthetic, guide)
}

.binned_scale <- function(
  plot,
  aesthetic,
  style,
  n,
  breaks,
  palette,
  labels,
  na_value,
  name
) {
  .check_plot(plot)
  .check_binned_args(n, style)
  .add_scale(
    plot,
    ScaleSpec(
      aesthetic = aesthetic,
      type = "binned",
      palette = palette,
      breaks = breaks,
      labels = labels,
      style = style,
      n = n,
      na_value = na_value,
      name = name
    )
  )
}
