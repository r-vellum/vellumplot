#' @include classes.R coord.R compile-resolve.R
NULL

# Reconcile user-supplied labels against the breaks they annotate. Labels and
# breaks must correspond one-to-one; silently recycling a short `labels` vector
# against an auto-computed break count mislabels the axis/legend, so we require
# matching lengths (ggplot2 semantics). Returns NULL when no labels were given
# (the caller then formats the breaks itself).
.match_labels <- function(
  user_labels,
  breaks,
  aes,
  call = rlang::caller_env()
) {
  if (is.null(user_labels)) {
    return(NULL)
  }
  if (length(user_labels) != length(breaks)) {
    cli::cli_abort(
      c(
        "{.arg labels} for the {.field {aes}} scale must have one entry per break.",
        i = "Got {length(user_labels)} label{?s} for {length(breaks)} break{?s}.",
        i = "Supply matching {.arg breaks}, or one label per break."
      ),
      call = call
    )
  }
  as.character(user_labels)
}

# Keep only the legend breaks (and their paired labels) that are finite and
# inside `rng`. Shared by every continuous trainer's break/label filter.
.filter_breaks_labels <- function(breaks, labels, rng) {
  keep <- is.finite(breaks) & breaks >= min(rng) & breaks <= max(rng)
  list(
    breaks = breaks[keep],
    labels = if (!is.null(labels)) labels[keep] else NULL
  )
}

# An identity scale: values pass through verbatim (coerced), with no guide.
# Shared by the size/alpha/edge-width, shape, linetype and colour trainers.
.identity_scale <- function(kind, coerce, name) {
  list(kind = kind, map = coerce, name = name, no_guide = TRUE)
}

# "Not enough <noun>" abort for a discrete palette that ran short of levels.
# Shared by the shape / pattern / linetype trainers.
.not_enough_abort <- function(noun, field, arg, n, avail) {
  cli::cli_abort(c(
    "Not enough {noun} for {n} {.field {field}} level{?s} ({avail} available).",
    i = "Supply at least {n} {noun} via {.arg {arg}}."
  ))
}

# Default perceptual ramp for continuous colour and qualitative palette for
# discrete colour. Batlow (Crameri's scientific colour maps) is perceptually
# uniform, colour-vision-deficient safe, and grayscale-safe like viridis, but
# its muted ink-and-sand tones suit vellum's aesthetic and are less ubiquitous.
.batlow <- function(n = 256) grDevices::hcl.colors(n, "Batlow")

# vellum's signature qualitative palette: muted, dark "ink" tones. More
# distinctive than hcl's "Dark 3", ordered so that progressively smaller k stay
# well separated (the most distinct hues come first).
.VELLUM_QUAL <- c(
  "#2F4858",
  "#0072B2",
  "#009E73",
  "#E69F00",
  "#D55E00",
  "#CC79A7"
)

# Return k qualitative colours. Up to length(.VELLUM_QUAL) we take the leading
# colours as-is; beyond that we interpolate in Lab space to fill k slots
# (distinctiveness necessarily degrades past the hand-picked set).
.qual_palette <- function(k) {
  n <- length(.VELLUM_QUAL)
  if (k <= n) {
    return(.VELLUM_QUAL[seq_len(k)])
  }
  grDevices::colorRampPalette(.VELLUM_QUAL, space = "Lab")(k)
}

# Point-size range (mm) a mapped `size` aesthetic spans.
.SIZE_RANGE <- c(1, 4)

# Default continuous label formatter: no thousands grouping, so 4-digit values
# such as years or IDs stay "2010" rather than "2 010" (scales' default inserts
# a grouping separator). See #27.
.label_number_default <- function(b) scales::label_number(big.mark = "")(b)

# Derive a secondary-axis companion for a continuous position scale. A secondary
# axis places secondary-space breaks at the primary's NATIVE coordinates, with
# its own labels and title. `tfun` is the primary data->native map, `rng` the
# primary data range, `domain` the primary native domain (shared by both axes),
# and `primary_breaks`/`primary_labels` the trained primary axis (for `dup_axis`).
# Returns a list `{domain, breaks, labels, name}` (the shape the axis drawers
# read) or NULL when the range is degenerate.
.train_sec_axis <- function(
  sec,
  tfun,
  rng,
  primary_breaks,
  primary_labels,
  domain,
  aesthetic
) {
  # A plain duplicate with no overrides reuses the primary breaks/labels exactly.
  if (isTRUE(sec@dup) && is.null(sec@breaks) && is.null(sec@labels)) {
    return(list(
      domain = domain,
      breaks = primary_breaks,
      labels = primary_labels,
      name = sec@name
    ))
  }
  lo <- min(rng)
  hi <- max(rng)
  if (!is.finite(lo) || !is.finite(hi) || lo == hi) {
    return(NULL)
  }
  pg <- seq(lo, hi, length.out = 1000L)
  native_g <- tfun(pg)
  sec_g <- suppressWarnings(as.numeric(sec@transform(pg)))
  ok <- is.finite(native_g) & is.finite(sec_g)
  native_g <- native_g[ok]
  sec_g <- sec_g[ok]
  if (length(sec_g) < 2L) {
    return(NULL)
  }
  dsec <- diff(sec_g)
  if (!(all(dsec > 0) || all(dsec < 0))) {
    cli::cli_abort(c(
      "The {.field {aesthetic}} secondary axis {.arg transform} must be monotonic.",
      i = "It is not strictly increasing or decreasing over the range {.val {c(lo, hi)}}."
    ))
  }
  srng <- range(sec_g)
  sbreaks <- if (!is.null(sec@breaks)) {
    as.numeric(sec@breaks)
  } else {
    scales::breaks_extended()(srng)
  }
  # A vector of labels aligns to the unfiltered breaks and is filtered alongside;
  # a labelling function is applied to the surviving breaks (mirrors the primary).
  lab_vec <- if (is.function(sec@labels)) {
    NULL
  } else {
    .match_labels(sec@labels, sbreaks, aesthetic)
  }
  fb <- .filter_breaks_labels(sbreaks, lab_vec, srng)
  sbreaks <- fb$breaks
  lab_vec <- fb$labels
  # `approx` needs an increasing x; flip both together for a decreasing transform.
  if (dsec[1] < 0) {
    sec_g <- rev(sec_g)
    native_g <- rev(native_g)
  }
  pos <- stats::approx(x = sec_g, y = native_g, xout = sbreaks)$y
  labels <- if (is.function(sec@labels)) {
    as.character(sec@labels(sbreaks))
  } else {
    lab_vec %||% .label_number_default(sbreaks)
  }
  fin <- is.finite(pos)
  list(
    domain = domain,
    breaks = pos[fin],
    labels = labels[fin],
    name = sec@name
  )
}

# Breaks for a symlog axis: zero plus signed powers of ten (1, 10, 100, ...) that
# fall inside the data range. Falls back to a small symmetric set for a degenerate
# range. Breaks are in DATA units (the registry transforms them to positions).
.symlog_breaks <- function(rng) {
  hi <- max(abs(rng))
  if (!is.finite(hi) || hi <= 0) {
    brks <- c(-1, 0, 1)
  } else {
    p <- floor(log10(hi))
    pos <- 10^(0:max(p, 0))
    brks <- sort(unique(c(0, pos, -pos)))
  }
  brks[brks >= rng[1] & brks <= rng[2]]
}

# Continuous position transforms. Each is `(transform, inverse, breaks, format)`:
# `transform` maps data -> native (viewport) units; `breaks` generates breaks in
# DATA units (then transformed to positions); `format` labels them. Kept as a
# local registry (rather than scales::transform_*) so log10 label formatting
# stays under our control rather than tracking scales' defaults.
.TRANSFORMS <- list(
  identity = list(
    transform = function(x) x,
    breaks = function(rng) scales::breaks_extended()(rng),
    format = function(b) .label_number_default(b)
  ),
  log10 = list(
    transform = function(x) log10(x),
    breaks = function(rng) scales::breaks_log()(rng),
    format = function(b) format(b, trim = TRUE, scientific = FALSE)
  ),
  sqrt = list(
    transform = function(x) sqrt(x),
    breaks = function(rng) scales::breaks_extended()(rng),
    format = function(b) .label_number_default(b)
  ),
  # Symmetric log ("symlog"): linear through zero, logarithmic in the tails, so
  # signed data spanning several orders of magnitude (and zero/negatives, which
  # log10 cannot show) reads on one axis. `sign(x) * log10(1 + |x|)` -- near zero
  # this is ~ x/ln(10) (linear), and for large |x| it is ~ sign(x)*log10(|x|).
  # Breaks sit at 0 and signed powers of ten within range.
  symlog = list(
    transform = function(x) sign(x) * log10(1 + abs(x)),
    breaks = function(rng) .symlog_breaks(rng),
    format = function(b) .label_number_default(b)
  ),
  # Reverse keeps the data mapping as identity and instead flips the trained
  # domain (a decreasing native domain is what vellum reads as a reversed axis).
  # Negating the data here as well would double-flip and cancel out.
  reverse = list(
    transform = function(x) x,
    breaks = function(rng) scales::breaks_extended()(rng),
    format = function(b) .label_number_default(b),
    flip = TRUE
  )
)

# Resolve a scalespec's transform to a `(transform, breaks, format)` object. A
# character name indexes the registry; a `scales::transform_*()` object is
# adapted directly (its `$transform`/`$breaks`/`$format`). Back-compat: the old
# `scale_*_continuous(trans = "log10")` stored `type = "log10"`.
.resolve_trans <- function(scalespec) {
  tr <- if (!is.null(scalespec)) scalespec@trans else NULL
  if (
    is.null(tr) && !is.null(scalespec) && identical(scalespec@type, "log10")
  ) {
    tr <- "log10"
  }
  tr <- tr %||% "identity"
  if (is.character(tr)) {
    out <- .TRANSFORMS[[tr]]
    if (is.null(out)) {
      cli::cli_abort(c(
        "Unknown transform {.val {tr}}.",
        i = "Use one of {.val {names(.TRANSFORMS)}} or a {.fn scales::transform_*} object."
      ))
    }
    return(out)
  }
  # a scales transform object. A reverse transform is handled by our flip path
  # (identity map + flipped domain), so route it through the registry entry
  # rather than using its data-negating `$transform` (which would double-flip).
  if (identical(tr$name, "reverse")) {
    return(.TRANSFORMS$reverse)
  }
  list(
    transform = tr$transform,
    breaks = tr$breaks %||% function(rng) scales::breaks_extended()(rng),
    format = tr$format %||% function(b) .label_number_default(b),
    flip = FALSE
  )
}

# The transform's name (identity/log10/sqrt/reverse, or a scales object's name),
# mirroring `.resolve_trans`'s resolution. Recorded on the trained scale so a host
# knows how to invert native -> data (e.g. `10^native` for log10). `reverse` maps
# data identically and flips the (native) domain, so a host treats it as identity.
.trans_name <- function(scalespec) {
  tr <- if (!is.null(scalespec)) scalespec@trans else NULL
  if (
    is.null(tr) && !is.null(scalespec) && identical(scalespec@type, "log10")
  ) {
    return("log10")
  }
  tr <- tr %||% "identity"
  if (is.character(tr)) {
    return(if (tr %in% names(.TRANSFORMS)) tr else "identity")
  }
  if (identical(tr$name, "reverse")) {
    return("reverse")
  }
  tr$name %||% "identity"
}

# Normalise a palette name for case/space/punctuation-insensitive matching
# against grDevices::hcl.pals().
.norm_pal <- function(s) tolower(gsub("[ ._-]", "", s))

# grDevices::hcl.pals() and its normalised form, computed once per session
# (the palette list is constant) so name matching does not re-scan it each call.
.pal_cache <- new.env(parent = emptyenv())
.hcl_pal_table <- function() {
  if (is.null(.pal_cache$full)) {
    full <- grDevices::hcl.pals()
    .pal_cache$full <- full
    .pal_cache$norm <- .norm_pal(full)
  }
  .pal_cache
}
.is_hcl_pal <- function(x) {
  is.character(x) &&
    length(x) == 1L &&
    .norm_pal(x) %in% .hcl_pal_table()$norm
}
.hcl_full <- function(x) {
  tab <- .hcl_pal_table()
  tab$full[match(.norm_pal(x), tab$norm)]
}
.are_colours <- function(x) {
  tryCatch(
    {
      grDevices::col2rgb(x)
      TRUE
    },
    error = function(e) FALSE
  )
}

# Continuous-colour ramp stops: NULL -> batlow default; an hcl.pals() name ->
# that palette; else a user colour vector.
.continuous_stops <- function(palette) {
  if (is.null(palette)) {
    return(.batlow())
  }
  if (.is_hcl_pal(palette)) {
    return(grDevices::hcl.colors(256, .hcl_full(palette)))
  }
  if (!.are_colours(palette)) {
    cli::cli_abort(
      "{.arg palette} is not a known palette name or colour vector."
    )
  }
  palette
}

# Interpolate evenly-spaced colour `stops` into `n` colours, blending in `space`.
# The default is the perceptually-uniform Oklab, which removes the muddy,
# over-dark midtones and hue drift of sRGB interpolation (e.g. a blue->yellow
# ramp no longer dips through grey) -- so a continuous or binned colour scale
# reads evenly. `grDevices::colorRampPalette()` only offers "rgb"/"Lab", so Oklab
# is done via farver (a `scales` dependency, always present). Set
# `options(vellumplot.color.interpolation = "srgb")` for the pre-0.4 behaviour.
.ramp_pal <- function(
  stops,
  n,
  space = getOption("vellumplot.color.interpolation", "oklab")
) {
  space <- match.arg(as.character(space), c("oklab", "srgb", "lab"))
  if (identical(space, "srgb")) {
    return(grDevices::colorRampPalette(stops)(n))
  }
  if (identical(space, "lab")) {
    return(grDevices::colorRampPalette(stops, space = "Lab")(n))
  }
  m <- length(stops)
  if (m == 1L || n == 1L) {
    return(grDevices::colorRampPalette(stops)(n))
  }
  lab <- farver::convert_colour(
    t(grDevices::col2rgb(stops)),
    from = "rgb",
    to = "oklab"
  )
  pos <- seq(0, 1, length.out = m)
  at <- seq(0, 1, length.out = n)
  interp <- vapply(
    1:3,
    function(ch) stats::approx(pos, lab[, ch], xout = at)$y,
    numeric(n)
  )
  rgb <- pmin(
    pmax(round(farver::convert_colour(interp, from = "oklab", to = "rgb")), 0),
    255
  )
  grDevices::rgb(rgb[, 1], rgb[, 2], rgb[, 3], maxColorValue = 255)
}

# Discrete colours for `levels`: NULL -> vellum qualitative; an hcl.pals() name
# -> that palette at k colours; a named colour vector -> mapped by name (manual,
# missing levels grey50); else a colour vector recycled to k.
.discrete_colours <- function(palette, levels) {
  k <- length(levels)
  if (is.null(palette)) {
    return(.qual_palette(k))
  }
  if (.is_hcl_pal(palette)) {
    return(grDevices::hcl.colors(k, .hcl_full(palette)))
  }
  if (!.are_colours(palette)) {
    cli::cli_abort(
      "{.arg palette}/{.arg values} are not colours or a palette name."
    )
  }
  if (!is.null(names(palette))) {
    cols <- unname(palette[levels])
    cols[is.na(cols)] <- "grey50"
    return(cols)
  }
  rep_len(palette, k)
}

# Default shapes a mapped `shape` aesthetic cycles through (the vellum marker
# vocabulary). The first six are stable; triangle_down/star extend the cycle.
.SHAPE_PALETTE <- c(
  "circle",
  "square",
  "triangle",
  "diamond",
  "plus",
  "cross",
  "triangle_down",
  "star"
)
# Shape drawn for NA-valued data (and the NA legend key): a neutral circle.
.SHAPE_NA <- "circle"

# Train a position scale. Dispatches to a continuous (numeric/temporal) or a
# discrete (factor/character/logical) scale. Returns a trained-scale list: the
# native domain (for the viewport scale), break positions in native units, their
# labels, a data->native mapping function, and `discrete`/`band_width` flags the
# mark compiler uses (e.g. for bar widths).
.train_position <- function(
  aesthetic,
  values,
  scalespec,
  title,
  include_zero = FALSE,
  lim = NULL
) {
  name <- .scale_title(scalespec, title)
  raw <- do.call(c, values) # combine across layers, preserving class
  # A binned position scale bins the continuous variable (ticks at boundaries,
  # data drawn at bin centres). Checked before the numeric branch since the data
  # is numeric.
  if (!is.null(scalespec) && identical(scalespec@type, "binned")) {
    return(.train_position_binned(aesthetic, raw, scalespec, name))
  }
  is_time_type <- !is.null(scalespec) &&
    scalespec@type %in% c("date", "datetime", "time")
  if (
    is.numeric(raw) ||
      inherits(raw, c("Date", "POSIXct", "difftime")) ||
      is_time_type
  ) {
    .train_position_continuous(
      aesthetic,
      raw,
      scalespec,
      name,
      include_zero,
      lim
    )
  } else {
    .train_position_discrete(aesthetic, values, scalespec, name)
  }
}

# Coerce a numeric vector back to the class of a Date/POSIXct prototype, so
# scales::breaks_width() (which is class-aware) sees the right type.
.as_like_time <- function(num, proto) {
  if (inherits(proto, "Date")) {
    structure(num, class = "Date")
  } else if (inherits(proto, "POSIXct")) {
    .POSIXct(num, tz = attr(proto, "tzone") %||% "")
  } else {
    num
  }
}

.train_position_continuous <- function(
  aesthetic,
  raw,
  scalespec,
  name,
  include_zero,
  lim = NULL
) {
  is_time <- inherits(raw, c("Date", "POSIXct")) ||
    (!is.null(scalespec) && scalespec@type %in% c("date", "datetime", "time"))
  user_lim <- lim %||% (if (!is.null(scalespec)) scalespec@domain else NULL)
  tr <- if (is_time) NULL else .resolve_trans(scalespec)

  # The data range. Explicit user limits are taken verbatim (so a descending
  # `c(hi, lo)` reverses the axis) and short-circuit the data scan entirely --
  # important for very large layers (e.g. datashade), where materialising and
  # filtering the full coordinate vector would cost gigabytes. Otherwise derive
  # the range with an allocation-free single pass: `range(finite = TRUE)` drops
  # NA/Inf without building a filtered copy. Only when the transform cannot
  # represent the endpoints (e.g. non-positive values on a log/sqrt scale) do we
  # pay for a filtered pass to exclude the values it can't map (ggplot2 does the
  # same -- the mark is still drawn, clipped; only the trained range excludes them).
  rng <- if (!is.null(user_lim)) {
    ul <- as.numeric(user_lim)
    # Partial limits (`ylim(0, NA)` / `scale_*(limits = c(NA, hi))`) pin the
    # supplied end and train the NA end from the data, rather than collapsing the
    # whole axis. Only the NA endpoint(s) are filled from the data range.
    if (anyNA(ul)) {
      data_r <- suppressWarnings(range(as.numeric(raw), finite = TRUE))
      ul[is.na(ul)] <- data_r[is.na(ul)]
    }
    ul
  } else if (is_time) {
    suppressWarnings(range(as.numeric(raw), finite = TRUE))
  } else {
    num <- as.numeric(raw)
    r <- suppressWarnings(range(num, finite = TRUE))
    if (all(is.finite(r)) && !all(is.finite(tr$transform(r)))) {
      r <- suppressWarnings(range(
        num[is.finite(tr$transform(num))],
        finite = TRUE
      ))
    }
    r
  }
  # Force a zero baseline for bars/areas, but only when the transform keeps it
  # finite (a log/sqrt scale cannot include 0) and the user has not set explicit
  # limits: `range(c(rng, 0))` sorts ascending, so zero-forcing an explicit
  # descending `c(hi, lo)` would silently un-reverse the axis and override the
  # user's chosen bound. Explicit limits are taken verbatim.
  if (include_zero && is.null(user_lim) && all(is.finite(rng))) {
    cand <- range(c(rng, 0))
    if (is_time || all(is.finite(tr$transform(cand)))) {
      rng <- cand
    }
  }
  if (!all(is.finite(rng)) || diff(rng) == 0) {
    centre <- if (all(is.finite(rng))) rng[1] else 0
    rng <- centre + c(-0.5, 0.5)
  }

  user_breaks <- if (!is.null(scalespec)) scalespec@breaks else NULL
  user_labels <- if (!is.null(scalespec)) scalespec@labels else NULL
  sec_spec <- if (!is.null(scalespec)) scalespec@sec_axis else NULL
  sec_list <- NULL

  if (is_time) {
    # Dates/times keep their dedicated pretty()/format() path, with optional
    # `date_breaks` (interval string) and `date_labels` (strftime format).
    dbreaks <- if (!is.null(scalespec)) scalespec@date_breaks else NULL
    dlabels <- if (!is.null(scalespec)) scalespec@date_labels else NULL
    is_classed <- inherits(raw, c("Date", "POSIXct"))
    tfun <- function(v) as.numeric(v)
    domain <- scales::expand_range(rng, mul = 0.05)
    braw <- if (!is.null(user_breaks)) {
      user_breaks
    } else if (!is.null(dbreaks) && is_classed) {
      scales::breaks_width(dbreaks)(.as_like_time(rng, raw))
    } else {
      pretty(raw)
    }
    lab <- .match_labels(user_labels, braw, aesthetic)
    keep <- as.numeric(braw) >= min(rng) & as.numeric(braw) <= max(rng)
    braw <- braw[keep]
    if (!is.null(lab)) {
      lab <- lab[keep]
    }
    breaks <- as.numeric(braw)
    labels <- lab %||%
      (if (!is.null(dlabels) && inherits(braw, c("Date", "POSIXct"))) {
        format(braw, dlabels)
      } else {
        format(braw)
      })
    # `type` stays "continuous" so downstream (guides/coord) is unchanged; the
    # date/datetime nature is reported separately via `time_unit` (below), which a
    # host uses to interpret the numeric values (Date -> days, POSIXct -> seconds
    # since 1970). `transform` is identity (values are `as.numeric()` epochs).
    type <- "continuous"
    trans_name <- "identity"
    time_unit <- if (
      inherits(raw, "POSIXct") ||
        (!is.null(scalespec) && scalespec@type %in% c("datetime", "time"))
    ) {
      "second"
    } else {
      "day"
    }
  } else {
    trans_name <- .trans_name(scalespec)
    time_unit <- NULL
    tfun <- function(v) tr$transform(as.numeric(v))
    tdom <- tr$transform(rng)
    if (!all(is.finite(tdom))) {
      cli::cli_abort(c(
        "The {.field {aesthetic}} data range falls outside the scale transform's domain.",
        i = "Range {.val {rng}} is not valid for this transform (e.g. a log scale needs positive values)."
      ))
    }
    # Order-preserving so a reversed `rng` keeps the axis reversed.
    domain <- scales::expand_range(tdom, mul = 0.05)
    # A reverse transform flips the axis direction by decreasing the domain.
    if (isTRUE(tr$flip)) {
      domain <- rev(domain)
    }
    braw <- if (!is.null(user_breaks)) {
      as.numeric(user_breaks)
    } else {
      tr$breaks(rng)
    }
    lab <- .match_labels(user_labels, braw, aesthetic)
    fb <- .filter_breaks_labels(braw, lab, rng)
    braw <- fb$breaks
    lab <- fb$labels
    breaks <- tr$transform(braw)
    labels <- lab %||% tr$format(braw)
    type <- if (!is.null(scalespec) && identical(scalespec@type, "log10")) {
      "log10"
    } else {
      "continuous"
    }
    if (!is.null(sec_spec)) {
      sec_list <- .train_sec_axis(
        sec_spec,
        tfun,
        rng,
        breaks,
        labels,
        domain,
        aesthetic
      )
    }
  }

  list(
    aesthetic = aesthetic,
    type = type,
    discrete = FALSE,
    band_width = NULL,
    data_range = rng,
    domain = domain,
    breaks = breaks,
    labels = labels,
    map = tfun,
    name = name,
    sec = sec_list,
    transform = trans_name,
    time_unit = time_unit
  )
}

# A discrete (band) position scale: levels map to integer positions 1..k, each
# occupying a unit-width band; the domain pads half a band each side. A user
# `limits` (in `scalespec@domain`) sets/reorders/subsets the levels.
.train_position_discrete <- function(aesthetic, values, scalespec, name) {
  levs <- if (!is.null(scalespec) && !is.null(scalespec@domain)) {
    as.character(scalespec@domain)
  } else {
    .cat_levels(values)
  }
  k <- length(levs)
  list(
    aesthetic = aesthetic,
    type = "discrete",
    discrete = TRUE,
    band_width = 1,
    data_range = c(1, k),
    domain = c(0.5, k + 0.5),
    breaks = seq_len(k),
    labels = levs,
    map = function(x) match(as.character(x), levs),
    name = name,
    transform = "identity",
    time_unit = NULL
  )
}

# A binned position scale: bin the continuous data, tick at bin boundaries, and
# map each datum to its bin centre. `band_width` (the bin width) lets mark_bar
# size bars to a bin; unequal bins use the median width.
.train_position_binned <- function(aesthetic, raw, scalespec, name) {
  v <- as.numeric(raw)
  v <- v[is.finite(v)]
  if (!length(v)) {
    cli::cli_abort(
      "A binned {.field {aesthetic}} scale needs at least one finite value."
    )
  }
  brks <- if (!is.null(scalespec) && !is.null(scalespec@breaks)) {
    sort(unique(as.numeric(scalespec@breaks)))
  } else {
    n <- if (!is.null(scalespec@n)) as.integer(scalespec@n) else 10L
    style <- if (!is.null(scalespec@style)) scalespec@style else "pretty"
    .binned_breaks(v, n, style)
  }
  if (length(brks) < 2L) {
    brks <- range(v) + c(-0.5, 0.5)
  }
  lo <- min(brks)
  hi <- max(brks)
  centers <- (brks[-length(brks)] + brks[-1L]) / 2
  widths <- diff(brks)
  bw <- if (isTRUE(all.equal(max(widths), min(widths)))) {
    widths[1]
  } else {
    stats::median(widths)
  }
  ulab <- if (!is.null(scalespec)) scalespec@labels else NULL
  labs <- .match_labels(ulab, brks, aesthetic) %||% format(brks, trim = TRUE)
  list(
    aesthetic = aesthetic,
    type = "binned",
    discrete = FALSE,
    band_width = bw,
    data_range = c(lo, hi),
    domain = scales::expand_range(c(lo, hi), mul = 0.05),
    breaks = brks,
    labels = labs,
    map = function(x) {
      i <- findInterval(
        as.numeric(x),
        brks,
        rightmost.closed = TRUE,
        all.inside = TRUE
      )
      centers[i]
    },
    name = name,
    transform = "identity",
    time_unit = NULL
  )
}

# Train the colour scale (if any layer maps `color`/`fill`). Returns NULL when
# colour is not mapped, else a trained-scale list carrying a values->colour
# mapping plus the data the legend needs.
.train_colour <- function(
  spec,
  resolved,
  channel = "color",
  fill_channel = "fill",
  aes = "color"
) {
  values <- .pool_values(resolved, channel)
  if (is.null(values) && !is.null(fill_channel)) {
    values <- .pool_values(resolved, fill_channel)
  }
  if (is.null(values)) {
    return(NULL)
  }

  scalespec <- .scale_for(spec, aes)
  # channel type from the first layer mapping colour
  chan_type <- NULL
  for (L in resolved) {
    chan_type <- L$types[[channel]] %||%
      (if (!is.null(fill_channel)) L$types[[fill_channel]] else NULL)
    if (!is.null(chan_type)) break
  }
  kind <- if (
    !is.null(scalespec) && length(scalespec@type) && nzchar(scalespec@type)
  ) {
    scalespec@type
  } else if (identical(chan_type, "quantitative")) {
    "continuous"
  } else {
    "discrete"
  }
  title <- .scale_title(scalespec, .default_title(spec, aes))

  pal <- if (!is.null(scalespec)) scalespec@palette else NULL
  # No explicit palette? A theme may supply a default (e.g. theme_cyberpunk()'s
  # neon set): `palette` for discrete, `palette.continuous` for continuous/binned.
  if (is.null(pal)) {
    pal <- .theme_palette(spec, kind)
  }
  user_breaks <- if (!is.null(scalespec)) scalespec@breaks else NULL
  user_labels <- if (!is.null(scalespec)) scalespec@labels else NULL
  # Diverging continuous scale: the data value that sits at the ramp's midpoint
  # colour (scale_*_gradient2()). NULL for an ordinary single-ended ramp.
  midpoint <- if (!is.null(scalespec)) scalespec@midpoint else NULL

  # Colour for NA-valued features (choropleth "no data"), and whether any pooled
  # value was actually NA (so the guide shows a distinct NA swatch).
  na_value <- .colour_na_value(spec, scalespec, resolved)
  has_na <- any(is.na(unlist(values, use.names = FALSE)))
  key_glyph <- .key_glyph_for_aes(resolved, channel)

  # An identity scale uses the data values verbatim as colours (they must already
  # be valid R/CSS colours) and draws no legend.
  if (identical(kind, "identity")) {
    return(c(
      .identity_scale("identity", as.character, title),
      list(na = FALSE, na_value = na_value, key_glyph = key_glyph)
    ))
  }

  if (identical(kind, "binned")) {
    v <- as.numeric(unlist(values, use.names = FALSE))
    v <- v[is.finite(v)]
    # A user-set `limits` (domain) restricts the data the scale bins over, so the
    # classes cover the requested range rather than the observed data extent.
    if (!is.null(scalespec) && !is.null(scalespec@domain)) {
      dom <- as.numeric(scalespec@domain)
      v <- v[v >= min(dom) & v <= max(dom)]
    }
    nclass <- as.integer(scalespec@n %||% 5L)
    style <- scalespec@style %||% "quantile"
    brks <- if (!is.null(user_breaks)) {
      sort(as.numeric(user_breaks))
    } else {
      .binned_breaks(v, nclass, style)
    }
    k <- length(brks) - 1L
    # A binned scale needs >= 1 class, i.e. >= 2 breaks. User breaks skip
    # `.binned_breaks` (which self-guards), so a length-1 breaks would reach
    # colorRampPalette(...)(0) and abort cryptically. (Position-binned scales
    # recover by widening; a colour scale has no sensible one-class recovery.)
    if (k < 1L) {
      cli::cli_abort(c(
        "A binned {.field color} scale needs at least 2 breaks (1 class).",
        i = "{.arg breaks} has {length(brks)} value{?s}."
      ))
    }
    cols <- .ramp_pal(.continuous_stops(pal), k)
    map <- function(x) {
      i <- findInterval(x, brks, rightmost.closed = TRUE, all.inside = TRUE)
      out <- cols[i]
      out[!is.finite(x)] <- na_value
      out
    }
    labels <- .match_labels(user_labels, seq_len(k), aes) %||%
      .interval_labels(brks)
    list(
      kind = "binned",
      map = map,
      name = title,
      breaks = brks,
      levels = labels,
      labels = labels,
      colors = cols,
      na = has_na,
      na_value = na_value,
      key_glyph = key_glyph
    )
  } else if (identical(kind, "continuous")) {
    v <- as.numeric(unlist(values, use.names = FALSE))
    v <- v[is.finite(v)]
    # A user-set `limits` (domain) fixes the colour range, matching the size /
    # alpha trainer; without it the range is the observed data extent.
    rng <- if (!is.null(scalespec) && !is.null(scalespec@domain)) {
      as.numeric(scalespec@domain)
    } else if (length(v)) {
      range(v)
    } else {
      c(0, 1)
    }
    if (diff(rng) == 0) {
      rng <- rng + c(-0.5, 0.5)
    }
    pal256 <- .ramp_pal(.continuous_stops(pal), 256)
    # Quantize to <=256 bins so the mark style-grouping stays bounded. A diverging
    # scale (midpoint set) rescales *about the midpoint* so the neutral `mid`
    # colour (bin ~128) lands on `midpoint`, and each side spans as far as the
    # data reaches on that side -- `scales::rescale_mid`, matching ggplot2's
    # scale_*_gradient2().
    bin <- if (is.null(midpoint)) {
      function(x) {
        i <- floor(scales::rescale(x, to = c(0, 255), from = rng)) + 1L
        pmin(pmax(i, 1L), 256L)
      }
    } else {
      function(x) {
        i <- floor(
          scales::rescale_mid(x, to = c(0, 255), from = rng, mid = midpoint)
        ) +
          1L
        pmin(pmax(i, 1L), 256L)
      }
    }
    map <- function(x) {
      out <- pal256[bin(x)]
      out[!is.finite(x)] <- na_value
      out
    }
    lbrk <- if (!is.null(user_breaks)) {
      as.numeric(user_breaks)
    } else {
      scales::breaks_extended()(rng)
    }
    llab <- .match_labels(user_labels, lbrk, aes)
    fb <- .filter_breaks_labels(lbrk, llab, rng)
    lbrk <- fb$breaks
    llab <- fb$labels
    list(
      kind = "continuous",
      map = map,
      name = title,
      range = rng,
      pal256 = pal256,
      midpoint = midpoint,
      legend_breaks = lbrk,
      legend_labels = llab %||% .label_number_default(lbrk),
      na = has_na,
      na_value = na_value,
      key_glyph = key_glyph
    )
  } else {
    all_levels <- .cat_levels(values)
    cols <- .discrete_colours(pal, all_levels)
    names(cols) <- all_levels
    map <- function(x) {
      out <- unname(cols[as.character(x)])
      # NA inputs *and* values outside the trained levels (lookup misses) both
      # fall back to na_value rather than rendering an invalid/transparent colour.
      out[is.na(out)] <- na_value
      out
    }
    # `breaks` selects which levels appear in the legend (and their order); the
    # colour mapping itself always covers every data level. Unknown breaks are
    # dropped. Labels (if given) must match the shown levels one-to-one.
    levels <- if (!is.null(user_breaks)) {
      ub <- as.character(user_breaks)
      ub[ub %in% all_levels]
    } else {
      all_levels
    }
    labels <- .match_labels(user_labels, levels, aes) %||% levels
    list(
      kind = "discrete",
      map = map,
      name = title,
      levels = levels,
      labels = labels,
      colors = unname(cols[levels]),
      na = has_na,
      na_value = na_value,
      key_glyph = key_glyph
    )
  }
}

# Colour for NA-valued features: a scale's explicit `na_value`, else an sf
# layer's `na_value` (from mark_sf(na_value=)), else a neutral grey. Only used
# when the data actually contains NA.
.colour_na_value <- function(spec, scalespec, resolved) {
  if (!is.null(scalespec) && !is.null(scalespec@na_value)) {
    return(scalespec@na_value)
  }
  for (L in resolved) {
    if (identical(L$mark, "sf") && !is.null(L$stat_params$na_value)) {
      return(L$stat_params$na_value)
    }
  }
  "grey50"
}

# Train a continuous, rescaled-to-a-range aesthetic (size / edge width / alpha):
# rescale the data range to an output range and carry representative breaks for a
# legend. The three differ only in constants, so they share this body:
#   channel      -- pool key + default-title key ("size"/"linewidth"/"alpha")
#   aes          -- scale_for key ("size"/"edge_width"/"alpha")
#   out_default  -- the output range when the user sets none
#   legend_field -- the mapped-break field a guide reads ("legend_sizes"/...)
# `na`, the identity short-circuit, and `.match_labels` are applied uniformly.
# (An identity edge-width scale is not currently constructible, and neither
# alpha nor edge width exposes `labels`, so those two paths are dormant for them
# -- they exist so the three trainers stay consistent, not to change behaviour.)
.train_continuous_aes <- function(
  spec,
  resolved,
  kind,
  channel,
  aes,
  out_default,
  legend_field
) {
  values <- .pool_values(resolved, channel)
  if (is.null(values)) {
    return(NULL)
  }
  scalespec <- .scale_for(spec, aes)
  # An identity scale uses the data values verbatim (mm for size, opacity for
  # alpha), with no guide.
  if (!is.null(scalespec) && identical(scalespec@type, "identity")) {
    return(.identity_scale(kind, as.numeric, .default_title(spec, channel)))
  }
  v <- as.numeric(unlist(values, use.names = FALSE))
  has_na <- anyNA(v) # detect before dropping non-finite, for the NA legend key
  v <- v[is.finite(v)]
  rng <- if (!is.null(scalespec) && !is.null(scalespec@domain)) {
    as.numeric(scalespec@domain)
  } else if (length(v)) {
    range(v)
  } else {
    c(0, 1)
  }
  if (diff(rng) == 0) {
    rng <- rng + c(-0.5, 0.5)
  }
  out_range <- if (!is.null(scalespec) && !is.null(scalespec@range)) {
    as.numeric(scalespec@range)
  } else {
    out_default
  }
  # `scale_size_area()` (type "area") maps value -> marker *area*: size 0 at
  # value 0, and size = max_size * sqrt(value / data_max), so area is
  # proportional to the value. Otherwise the aesthetic rescales linearly.
  area_mode <- !is.null(scalespec) && identical(scalespec@type, "area")
  map <- if (area_mode) {
    max_size <- out_range[2]
    denom <- max(rng[2], .Machine$double.eps)
    function(x) max_size * sqrt(pmax(as.numeric(x), 0) / denom)
  } else {
    function(x) scales::rescale(x, to = out_range, from = rng)
  }
  name <- .scale_title(scalespec, .default_title(spec, channel))
  lbrk <- if (!is.null(scalespec) && !is.null(scalespec@breaks)) {
    as.numeric(scalespec@breaks)
  } else {
    scales::breaks_extended()(rng)
  }
  llab <- .match_labels(
    if (!is.null(scalespec)) scalespec@labels else NULL,
    lbrk,
    aes
  )
  fb <- .filter_breaks_labels(lbrk, llab, rng)
  lbrk <- fb$breaks
  llab <- fb$labels
  out <- list(
    kind = kind,
    map = map,
    name = name,
    range = rng,
    na = has_na,
    legend_breaks = lbrk,
    legend_labels = llab %||% .label_number_default(lbrk)
  )
  out[[legend_field]] <- map(lbrk)
  out
}

# Train the size scale (if any layer maps `size` to data). Maps values to a
# point-size range in mm and carries representative breaks for a size legend.
.train_size <- function(spec, resolved) {
  .train_continuous_aes(
    spec,
    resolved,
    kind = "size",
    channel = "size",
    aes = "size",
    out_default = .SIZE_RANGE,
    legend_field = "legend_sizes"
  )
}

# Edge-width output range (lwd multiples) a mapped edge `linewidth` spans.
.EDGE_WIDTH_RANGE <- c(0.3, 3)

# Train the edge-width scale (if any edge layer maps `linewidth` to data). Mirrors
# the size scale: rescale the data range to an lwd range; emits its own legend.
.train_edge_width <- function(spec, resolved) {
  .train_continuous_aes(
    spec,
    resolved,
    kind = "edge_width",
    channel = "linewidth",
    aes = "edge_width",
    out_default = .EDGE_WIDTH_RANGE,
    legend_field = "legend_widths"
  )
}

# Train the shape scale (if any layer maps `shape` to data). Shape is always
# discrete: levels cycle through `.SHAPE_PALETTE` (or a user `scale_shape(values)`).
.train_shape <- function(spec, resolved) {
  values <- .pool_values(resolved, "shape")
  if (is.null(values)) {
    return(NULL)
  }
  scalespec <- .scale_for(spec, "shape")
  # An identity shape scale uses the data values verbatim as shape names.
  if (!is.null(scalespec) && identical(scalespec@type, "identity")) {
    return(.identity_scale(
      "shape",
      as.character,
      .default_title(spec, "shape")
    ))
  }
  levels <- .cat_levels(values)
  has_na <- anyNA(unlist(values, use.names = FALSE))
  pal <- if (!is.null(scalespec) && !is.null(scalespec@palette)) {
    scalespec@palette
  } else {
    .SHAPE_PALETTE
  }
  if (length(levels) > length(pal)) {
    .not_enough_abort(
      "shapes",
      "shape",
      "scale_shape(values=)",
      length(levels),
      length(pal)
    )
  }
  shapes <- rep_len(pal, length(levels))
  names(shapes) <- levels
  name <- .scale_title(scalespec, .default_title(spec, "shape"))
  list(
    kind = "shape",
    # NA values map to the NA shape (a neutral circle) rather than to NA, which
    # would otherwise reach points_grob and error; the legend shows an NA key.
    map = function(x) {
      s <- unname(shapes[as.character(x)])
      s[is.na(s)] <- .SHAPE_NA
      s
    },
    name = name,
    levels = levels,
    na = has_na,
    shapes = unname(shapes)
  )
}

# Distinct textures a mapped (discrete) `pattern` aesthetic cycles through, in a
# legible order (orientation then motif). Each is a `vellum_pattern`.
.pattern_palette <- function() {
  list(
    pattern_stripe(angle = 45),
    pattern_crosshatch(),
    pattern_dot(),
    pattern_grid(),
    pattern_stripe(angle = 0),
    pattern_stripe(angle = 90),
    pattern_checker(),
    pattern_stripe(angle = 135)
  )
}

# A `scale_pattern(values =)` palette to a list of fill paints: either a list of
# pattern/hatch objects already (raster `vellum_pattern` from `pattern_stripe()`
# etc., or crisp vector `vellum_hatch` from `pattern_hatch()`), or a character
# vector of builder names.
.resolve_pattern_values <- function(values) {
  if (
    is.list(values) &&
      all(vapply(
        values,
        function(v) inherits(v, c("vellum_pattern", "vellum_hatch")),
        logical(1)
      ))
  ) {
    return(values)
  }
  builders <- list(
    stripe = pattern_stripe,
    crosshatch = pattern_crosshatch,
    grid = pattern_grid,
    dot = pattern_dot,
    checker = pattern_checker
  )
  lapply(as.character(values), function(nm) {
    b <- builders[[nm]]
    if (is.null(b)) {
      cli::cli_abort(c(
        "Unknown pattern {.val {nm}} in {.arg scale_pattern(values=)}.",
        i = "Use {.or {.val {names(builders)}}}, or pass {.fn pattern_stripe}-style objects."
      ))
    }
    b()
  })
}

# Train the pattern scale (if any layer maps `pattern`). Always discrete: levels
# cycle through `.pattern_palette()` (or a user `scale_pattern(values)`). `map`
# returns the per-row level (a grouping key); `objs` looks the level up to its
# `vellum_pattern`, used as the fill paint at emit and the legend swatch.
.train_pattern <- function(spec, resolved) {
  values <- .pool_values(resolved, "pattern")
  if (is.null(values)) {
    return(NULL)
  }
  scalespec <- .scale_for(spec, "pattern")
  levels <- .cat_levels(values)
  pal <- if (!is.null(scalespec) && !is.null(scalespec@palette)) {
    .resolve_pattern_values(scalespec@palette)
  } else {
    .pattern_palette()
  }
  if (length(levels) > length(pal)) {
    .not_enough_abort(
      "patterns",
      "pattern",
      "scale_pattern(values=)",
      length(levels),
      length(pal)
    )
  }
  objs <- pal[seq_along(levels)]
  names(objs) <- levels
  list(
    kind = "pattern",
    map = function(x) as.character(x),
    objs = objs,
    name = .scale_title(scalespec, .default_title(spec, "pattern")),
    levels = levels,
    patterns = unname(objs)
  )
}

# Output opacity range a mapped `alpha` aesthetic spans (kept off 0 so the
# faintest mark is still visible).
.ALPHA_RANGE <- c(0.1, 1)

# Line types a mapped (discrete) `linetype` aesthetic cycles through.
.LINETYPE_PALETTE <- c(
  "solid",
  "dashed",
  "dotted",
  "dotdash",
  "longdash",
  "twodash"
)

# Train the alpha scale (if any layer maps `alpha` to data). Continuous: rescale
# the data range to an opacity range; carries breaks for an alpha legend.
.train_alpha <- function(spec, resolved) {
  .train_continuous_aes(
    spec,
    resolved,
    kind = "alpha",
    channel = "alpha",
    aes = "alpha",
    out_default = .ALPHA_RANGE,
    legend_field = "legend_alphas"
  )
}

# Train the linetype scale (if any layer maps `linetype` to data). Always
# discrete: levels cycle through `.LINETYPE_PALETTE` (or a user palette).
.train_linetype <- function(
  spec,
  resolved,
  channel = "linetype",
  aes = "linetype"
) {
  values <- .pool_values(resolved, channel)
  if (is.null(values)) {
    return(NULL)
  }
  scalespec <- .scale_for(spec, aes)
  if (!is.null(scalespec) && identical(scalespec@type, "identity")) {
    return(.identity_scale("linetype", as.character, .default_title(spec, aes)))
  }
  levels <- .cat_levels(values)
  pal <- if (!is.null(scalespec) && !is.null(scalespec@palette)) {
    scalespec@palette
  } else {
    .LINETYPE_PALETTE
  }
  if (length(levels) > length(pal)) {
    .not_enough_abort(
      "line types",
      "linetype",
      "scale_linetype(values=)",
      length(levels),
      length(pal)
    )
  }
  ltys <- rep_len(pal, length(levels))
  names(ltys) <- levels
  name <- .scale_title(scalespec, .default_title(spec, aes))
  list(
    kind = "linetype",
    map = function(x) unname(ltys[as.character(x)]),
    name = name,
    levels = levels,
    linetypes = unname(ltys)
  )
}

# --- edge-scoped colour / alpha / linetype ---------------------------------
# Edge marks carry their colour / alpha / linetype on dedicated channels
# (`edge_color` / `edge_alpha` / `edge_linetype`), so an edge aesthetic never
# pools into -- and never collides with -- the node colour / alpha / linetype
# scale. Each edge trainer reuses the matching node trainer's body, only pointing
# it at the edge channel and the `scale_edge_*()` scale key. (`edge_width`, the
# one edge scale that shipped first, already works this way via the `linewidth`
# channel; these three complete the set.)

.train_edge_colour <- function(spec, resolved) {
  .train_colour(
    spec,
    resolved,
    channel = "edge_color",
    fill_channel = NULL,
    aes = "edge_color"
  )
}

.train_edge_alpha <- function(spec, resolved) {
  .train_continuous_aes(
    spec,
    resolved,
    kind = "alpha",
    channel = "edge_alpha",
    aes = "edge_alpha",
    out_default = .ALPHA_RANGE,
    legend_field = "legend_alphas"
  )
}

.train_edge_linetype <- function(spec, resolved) {
  .train_linetype(
    spec,
    resolved,
    channel = "edge_linetype",
    aes = "edge_linetype"
  )
}

# Pool numeric values feeding a position axis: the channel itself plus, for
# rule layers, the matching intercept (which may be a constant in `params`), the
# segment endpoint (`<channel>end`), and any `<channel>min`/`<channel>max`
# extent (e.g. a smooth's confidence ribbon).
.axis_pool <- function(resolved, channel, intercept) {
  vs <- list()
  for (L in resolved) {
    v <- L$values[[channel]] %||%
      L$values[[intercept]] %||%
      L$params[[intercept]]
    if (!is.null(v)) {
      vs <- c(vs, list(v))
    }
    # Segment/edge endpoints extend the axis just like the start coordinate, so
    # a segment-only plot derives its domain from both ends (not just `x`/`y`).
    end <- L$values[[paste0(channel, "end")]]
    if (!is.null(end)) {
      vs <- c(vs, list(end))
    }
    lo <- L$values[[paste0(channel, "min")]]
    hi <- L$values[[paste0(channel, "max")]]
    if (!is.null(lo)) vs <- c(vs, list(lo, hi))
  }
  if (length(vs)) vs else NULL
}

# Native-coordinate bounding box a density-shape mark (violin/ridgeline) will
# draw, so scale training can widen the panel to fit it. These marks generate
# their geometry at emit time from `stats::density()`, which extends past the raw
# data (the density support is padded ~3 bandwidths), and a ridge additionally
# rises `scale * band` above its category baseline -- none of which the raw-data
# axis pool sees. Returns `list(x = c(lo, hi), y = c(lo, hi))`, with `NULL` for an
# axis it does not extend. The density math mirrors the emitters exactly (shared
# `.density_by_cat`), so the trained domain matches what is drawn.
.mark_footprint <- function(L, scales) {
  adjust <- L$stat_params$adjust %||% 1
  if (identical(L$mark, "violin")) {
    xv <- L$values$x
    yv <- as.numeric(L$values$y)
    levs <- .cat_levels(xv)
    xc <- scales$x$map(levs)
    band <- scales$x$band_width %||% .resolution(scales$x$map(xv))
    hw <- .VIOLIN_HALFWIDTH * band
    dens <- .density_by_cat(yv, xv, levs, adjust)
    supp <- unlist(lapply(dens, function(d) if (!is.null(d)) range(d$x)))
    list(
      x = c(min(xc) - hw, max(xc) + hw),
      y = if (length(supp)) range(supp) else NULL
    )
  } else if (identical(L$mark, "ridgeline")) {
    xv <- as.numeric(L$values$x)
    yv <- L$values$y
    levs <- .cat_levels(yv)
    ypos <- scales$y$map(levs)
    band <- scales$y$band_width %||% 1
    scale_h <- (L$stat_params$height %||% .RIDGE_HEIGHT) * band
    dens <- .density_by_cat(xv, yv, levs, adjust)
    supp <- unlist(lapply(dens, function(d) if (!is.null(d)) range(d$x)))
    list(
      x = if (length(supp)) range(supp) else NULL,
      y = c(min(ypos), max(ypos) + scale_h)
    )
  } else if (identical(L$mark, "halfeye")) {
    # one-sided density slab: extends x to the right of each tick by the slab
    # width, and y over the density support (tails past the sample range).
    xv <- L$values$x
    yv <- as.numeric(L$values$y)
    levs <- .cat_levels(xv)
    xc <- scales$x$map(levs)
    band <- scales$x$band_width %||% .resolution(scales$x$map(xv))
    hw <- (L$stat_params$scale %||% .SLAB_WIDTH) * band
    dens <- .density_by_cat(yv, xv, levs, adjust)
    supp <- unlist(lapply(dens, function(d) if (!is.null(d)) range(d$x)))
    list(
      x = c(min(xc), max(xc) + hw),
      y = if (length(supp)) range(supp) else NULL
    )
  } else {
    list(x = NULL, y = NULL)
  }
}

# Widen the position domains so a violin/ridgeline layer's drawn footprint is not
# clipped by the panel. Only ever grows a domain (union), and skips an axis whose
# limits the user set explicitly (`coord_*(xlim=/ylim=)` or a scale `limits=`),
# since explicit limits are intentional cropping that must win.
.expand_position_for_marks <- function(resolved, scales, expand_x, expand_y) {
  for (L in resolved) {
    if (!L$mark %in% c("violin", "ridgeline", "halfeye")) {
      next
    }
    fp <- .mark_footprint(L, scales)
    if (expand_x && !is.null(fp$x) && all(is.finite(fp$x))) {
      scales$x$domain <- range(c(scales$x$domain, fp$x))
    }
    if (expand_y && !is.null(fp$y) && all(is.finite(fp$y))) {
      scales$y$domain <- range(c(scales$y$domain, fp$y))
    }
  }
  scales
}

# Train all scales the plot needs: x, y (position), colour, and size. Bars force
# the y axis to include the zero baseline.
.train_scales <- function(spec, resolved) {
  zero_base <- .needs_zero(resolved)
  co <- .coord_of(spec)
  xs <- .axis_pool(resolved, "x", "xintercept")
  ys <- .axis_pool(resolved, "y", "yintercept")
  if (is.null(xs) || is.null(ys)) {
    cli::cli_abort(
      "Every layer needs an {.field x} and {.field y} encoding."
    )
  }
  scales <- list(
    x = .train_position(
      "x",
      xs,
      .scale_for(spec, "x"),
      .default_title(spec, "x"),
      lim = co@xlim
    ),
    y = .train_position(
      "y",
      ys,
      .scale_for(spec, "y"),
      .y_axis_title(spec, resolved),
      include_zero = zero_base,
      lim = co@ylim
    ),
    color = .train_colour(spec, resolved),
    size = .train_size(spec, resolved),
    shape = .train_shape(spec, resolved),
    pattern = .train_pattern(spec, resolved),
    edge_width = .train_edge_width(spec, resolved),
    alpha = .train_alpha(spec, resolved),
    linetype = .train_linetype(spec, resolved),
    edge_color = .train_edge_colour(spec, resolved),
    edge_alpha = .train_edge_alpha(spec, resolved),
    edge_linetype = .train_edge_linetype(spec, resolved)
  )
  # Apply per-scale guide overrides (guides(): guide = "none" / guide_legend()).
  for (aes in c(
    "color",
    "size",
    "shape",
    "pattern",
    "edge_width",
    "alpha",
    "linetype",
    "edge_color",
    "edge_alpha",
    "edge_linetype"
  )) {
    ss <- .scale_for(spec, aes)
    g <- if (!is.null(ss)) ss@guide else NULL
    if (!is.null(scales[[aes]]) && !is.null(g)) {
      scales[[aes]] <- .apply_guide(scales[[aes]], g)
    }
  }
  # Grow the panel to fit any density-shape mark's drawn footprint, unless the
  # user pinned that axis's limits.
  sx <- .scale_for(spec, "x")
  sy <- .scale_for(spec, "y")
  x_explicit <- !is.null(co@xlim) || (!is.null(sx) && !is.null(sx@domain))
  y_explicit <- !is.null(co@ylim) || (!is.null(sy) && !is.null(sy@domain))
  scales <- .expand_position_for_marks(
    resolved,
    scales,
    expand_x = !x_explicit,
    expand_y = !y_explicit
  )
  scales
}
