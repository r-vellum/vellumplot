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
# `...` carries any extra fields (the colour scale's NA colour and key glyph).
.identity_scale <- function(aesthetic, coerce, name, ...) {
  trained_scale(
    aesthetic,
    "identity",
    map = coerce,
    name = name,
    identity = TRUE,
    no_guide = TRUE,
    ...
  )
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
    inverse = function(x) x,
    breaks = function(rng) scales::breaks_extended()(rng),
    format = function(b) .label_number_default(b)
  ),
  log10 = list(
    transform = function(x) log10(x),
    inverse = function(x) 10^x,
    breaks = function(rng) scales::breaks_log()(rng),
    format = function(b) format(b, trim = TRUE, scientific = FALSE)
  ),
  sqrt = list(
    transform = function(x) sqrt(x),
    inverse = function(x) x^2,
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
    inverse = function(x) sign(x) * (10^abs(x) - 1),
    breaks = function(rng) .symlog_breaks(rng),
    format = function(b) .label_number_default(b)
  ),
  # Reverse keeps the data mapping as identity and instead flips the trained
  # domain (a decreasing native domain is what vellum reads as a reversed axis).
  # Negating the data here as well would double-flip and cancel out.
  reverse = list(
    transform = function(x) x,
    inverse = function(x) x,
    breaks = function(rng) scales::breaks_extended()(rng),
    format = function(b) .label_number_default(b),
    flip = TRUE
  )
)

# Resolve a scalespec's transform to a `(transform, inverse, breaks, format)` object. A
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
    inverse = tr$inverse,
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
  if (.is_brewer_pal(palette)) {
    return(.brewer_stops(palette))
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
  tok <- .token_colours(palette, k)
  if (!is.null(tok)) {
    return(tok)
  }
  if (.is_hcl_pal(palette)) {
    return(grDevices::hcl.colors(k, .hcl_full(palette)))
  }
  if (.is_brewer_pal(palette)) {
    return(.brewer_colours(palette, k))
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
# discrete (factor/character/logical) scale. Returns a `trained_scale()`: the
# native domain (for the viewport scale), break positions in native units, their
# labels, a data->native mapping function, and `discrete`/`band_width` flags the
# mark compiler uses (e.g. for bar widths).
.train_position <- function(
  aesthetic,
  values,
  scalespec,
  title,
  include_zero = FALSE,
  lim = NULL,
  fit = NULL
) {
  name <- title # resolved by the titles record (.plot_titles)
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
      lim,
      fit = fit
    )
  } else {
    .train_position_discrete(aesthetic, values, scalespec, name, lim)
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

# Default breaks for a date/time axis: `pretty()` at decreasing densities until
# the labels fit the axis (`fit`, from `.axis_label_fit()`), else the sparsest
# tried (VP-92). Labels are pretty()'s own ("2000", "Jan", "Jan 08", "06:00"),
# as ggplot2 shows them, or the scale's `date_labels` format. Breaks outside the
# trained range `rng` are dropped before the fit is judged. When nothing fits,
# the axis keeps the first and last break of the sparsest set that still has
# two in range, rather than thinning to a single label that gives no sense of
# the scale (VP-102). Returns the breaks (classed like the data) and their
# labels.
.pretty_time_breaks <- function(rng, raw, domain, dlabels = NULL, fit = NULL) {
  x <- .as_like_time(rng, raw)
  best <- NULL
  multi <- NULL # sparsest candidate with at least two breaks in range
  ends <- function(cand) {
    k <- unique(c(1L, length(cand$breaks)))
    list(breaks = cand$breaks[k], labels = cand$labels[k])
  }
  for (n in c(5L, 4L, 3L, 2L, 1L)) {
    b <- pretty(x, n)
    lab <- if (!is.null(dlabels) && inherits(b, c("Date", "POSIXct"))) {
      format(b, dlabels)
    } else {
      attr(b, "labels") %||% format(b)
    }
    keep <- as.numeric(b) >= min(rng) & as.numeric(b) <= max(rng)
    best <- list(breaks = b[keep], labels = as.character(lab[keep]))
    if (is.null(fit)) {
      return(best)
    }
    if (sum(keep) < 2L) {
      return(if (is.null(multi)) best else ends(multi))
    }
    if (fit(as.numeric(best$breaks), best$labels, domain)) {
      return(best)
    }
    multi <- best
  }
  ends(multi)
}

# Clear space (mm) wanted between neighbouring tick labels. 1 mm, about a third
# of the default axis text's line height: VP-92's 2 mm was set against a rough
# 75%-of-the-plot axis length, which overestimated small plots; with the panel
# measured as the layout does (VP-101), 2 mm thinned six month labels on a
# 3-inch plot that sit 1.5 mm apart.
.LABEL_GAP_MM <- 1

# A test of whether a set of tick labels fits along position axis `aes`, for
# choosing date breaks by the available length (VP-92). The labels are measured
# with vellum's text metrics at the theme's axis text size. They are drawn at
# the axis guide's `angle` (`guide_axis(angle =)`), else the theme element's,
# and `guide_axis(n.dodge =)` staggers them over rows, so neighbours in one row
# are `n.dodge` breaks apart (VP-101). The axis length is estimated by
# `.axis_fit_len_mm()`, since the panel is laid out only after training, from
# `ctx` (`.axis_fit_ctx()`, or a function returning it, called only if the fit
# is used). NULL
# (no thinning) for polar and other non-cartesian coordinates, blank axis text,
# or an axis without labels (`guides(x = "none")`).
.axis_label_fit <- function(spec, aes, ctx = NULL) {
  co <- .coord_of(spec)
  if (!co@kind %in% c("cartesian", "flip", "fixed")) {
    return(NULL)
  }
  horiz <- xor(identical(aes, "x"), identical(co@kind, "flip"))
  rt <- .resolve_theme(.theme_of(spec))
  el <- rt[[if (horiz) "axis.text.x" else "axis.text.y"]]
  if (is.null(el) || .is_blank(el)) {
    return(NULL)
  }
  ss <- .scale_for(spec, aes)
  g <- if (!is.null(ss)) ss@guide
  if (identical(g, "none")) {
    return(NULL)
  }
  g <- if (is.list(g) && identical(g$kind, "axis")) g else list()
  if (identical(g$kind, "none")) {
    return(NULL)
  }
  rot <- g$angle %||% .el_rot(el)
  n_dodge <- max(1L, g$n.dodge %||% 1L)
  # measured on first use: only a date axis without set breaks calls the fit
  len_mm <- NULL
  function(breaks, labels, domain) {
    if (is.null(len_mm)) {
      len_mm <<- .axis_fit_len_mm(
        spec,
        rt,
        horiz,
        if (is.function(ctx)) ctx() else ctx
      )
    }
    .labels_fit(breaks, labels, domain, len_mm, el@size, rot, horiz, n_dodge)
  }
}

# Do tick `labels` at `breaks` (on an axis spanning `domain` over `len_mm`)
# clear each other? Each label is measured on its own, and each pair of
# neighbours in one dodge row (`n_dodge` breaks apart) needs the clearance of
# their mean width -- half of each label reaches towards the other -- so a
# narrow label beside a wide one is not judged by the widest label of the axis
# (VP-110).
.labels_fit <- function(
  breaks,
  labels,
  domain,
  len_mm,
  size,
  rot,
  horiz,
  n_dodge = 1L
) {
  n <- length(breaks)
  if (n - n_dodge < 1L) {
    return(TRUE)
  }
  o <- order(breaks)
  breaks <- breaks[o]
  labels <- labels[o]
  wh <- vapply(
    labels,
    function(l) {
      tg <- .txt(l, size)
      c(
        vellum::vl_convert(vellum::grobwidth(tg), "mm"),
        vellum::vl_convert(vellum::grobheight(tg), "mm")
      )
    },
    numeric(2),
    USE.NAMES = FALSE
  )
  i <- seq_len(n - n_dodge)
  j <- i + n_dodge
  step <- (breaks[j] - breaks[i]) / abs(diff(domain)) * len_mm
  need <- vapply(
    seq_along(i),
    function(k) {
      .label_clearance_mm(
        (wh[1L, i[k]] + wh[1L, j[k]]) / 2,
        max(wh[2L, i[k]], wh[2L, j[k]]),
        rot,
        horiz
      )
    },
    numeric(1)
  )
  all(step >= need)
}

# The spacing along an axis two neighbouring tick labels (each `w` x `h` mm
# unrotated, drawn at `rot` degrees) need so they do not touch, with
# `.LABEL_GAP_MM` clear between them. Two equal rectangles rotated alike and
# offset by `d` along the axis are apart when `d` covers either their length
# along the text direction or their height across it -- so labels at 45 degrees
# pack as densely as their line height allows, not their width. Horizontal
# text on a horizontal axis needs `w` plus the gap, as before.
.label_clearance_mm <- function(w, h, rot, horiz) {
  th <- rot * pi / 180
  along <- abs(if (horiz) cos(th) else sin(th)) # text direction . axis
  across <- abs(if (horiz) sin(th) else cos(th)) # text normal . axis
  need <- function(size, f) if (f > 1e-6) (size + .LABEL_GAP_MM) / f else Inf
  min(need(w, along), need(h, across))
}

# Estimated length (mm) of one panel along the axis being fitted, before the
# layout runs: the plot's width (height, for a vertical axis such as x under
# coord_flip()) less the plot margins, the legends beside (above / below) the
# panels, the other axis's title and label gutter -- or the title, subtitle and
# caption bands and this axis's own gutter, for a vertical axis -- and the
# panel spacing, shared among the facet columns (rows). `ctx`
# (`.axis_fit_ctx()`) carries the trained legends' extents and the vertical
# axis's labels; without it they count as absent / a short number. The layout
# measures the same tracks, so this tracks the real panel closely; facet strips
# and secondary axes are not counted (VP-101).
.axis_fit_len_mm <- function(spec, rt, horiz, ctx = NULL) {
  grid <- .mark_ctx$panel_grid %||% c(R = 1L, C = 1L)
  pm <- rep_len(rt[["plot.margin"]] %||% 0, 4L) # (t, r, b, l) mm
  leg <- ctx$legend %||% c(top = 0, right = 0, bottom = 0, left = 0)
  mm <- function(u) vellum::vl_convert(u, "mm")
  gap <- rt[["panel.spacing"]] %||% 0
  tick <- rt[["axis.ticks.length"]] %||% 0
  if (horiz) {
    v <- ctx$vertical
    yl <- if (isFALSE(v$shows_labels)) {
      0
    } else {
      mm(.track_w_axis(
        rt[["axis.text.y"]],
        .longest(v$labels %||% "00.0"),
        tick + .PAD_MM,
        rot = v$rot %||% .el_rot(rt[["axis.text.y"]]),
        n_dodge = v$n_dodge %||% 1L
      ))
    }
    yt <- mm(.track_w(rt[["axis.title.y"]], v$name, .PAD_MM, rot = 90))
    total <- spec@width * 25.4 - pm[2] - pm[4] - leg[["left"]] -
      leg[["right"]] - yl - yt - (grid[["C"]] - 1) * gap
    n <- grid[["C"]]
  } else {
    labs <- spec@labels
    band <- function(el, label, pad) {
      if (is.null(label) || is.null(rt[[el]])) 0 else mm(.track_h(rt[[el]], label, pad))
    }
    xl <- mm(.track_h(rt[["axis.text.x"]], "0", tick + .PAD_MM))
    xt <- band("axis.title.x", ctx$horizontal_name, .PAD_MM)
    total <- spec@height * 25.4 - pm[1] - pm[3] - leg[["top"]] -
      leg[["bottom"]] - xl - xt -
      band("plot.title", labs$title, 2 * .PAD_MM) -
      band("plot.subtitle", labs$subtitle, .PAD_MM) -
      band("plot.caption", labs$caption, .PAD_MM) -
      (grid[["R"]] - 1) * gap
    n <- grid[["R"]]
  }
  # never below a sliver, so a cramped plot still thins rather than erroring
  max(total, 10) / n
}

# The context the date-break fit (`.axis_label_fit()`) estimates the panel
# from (VP-101): the extent (mm) of the legends on each side, measured from the
# trained legend scales exactly as the layout sizes them, and the vertical
# axis's labels, name and guide (its label gutter narrows the panels).
# `scales` holds the trained legend scales, and the vertical position scale
# when it is trained; `titles` the axis titles.
.axis_fit_ctx <- function(spec, scales, titles, flip = FALSE) {
  rt <- .resolve_theme(.theme_of(spec))
  leg <- c(top = 0, right = 0, bottom = 0, left = 0)
  guides <- tryCatch(.legend_guides(scales), error = function(e) list())
  if (length(guides)) {
    groups <- .legend_groups(guides, rt)
    avail_h <- .legend_avail_h(spec@height, rt)
    for (sd in c("left", "right")) {
      if (length(groups[[sd]])) {
        leg[[sd]] <- vellum::vl_convert(
          .legend_width(groups[[sd]], rt, avail_h),
          "mm"
        )
      }
    }
    for (sd in c("top", "bottom")) {
      if (length(groups[[sd]])) {
        leg[[sd]] <- vellum::vl_convert(.legend_height(groups[[sd]], rt), "mm")
      }
    }
  }
  va <- if (flip) "x" else "y"
  ha <- if (flip) "y" else "x"
  vsc <- scales[[va]]
  vertical <- list(name = titles$title[[va]])
  if (!is.null(vsc)) {
    vertical <- list(
      name = vsc$name,
      labels = as.character(vsc$labels),
      rot = .axis_rot(vsc, rt[["axis.text.y"]]),
      n_dodge = .axis_guide_of(vsc)$n.dodge %||% 1L,
      shows_labels = .axis_shows_labels(vsc)
    )
  }
  list(
    legend = leg,
    vertical = vertical,
    horizontal_name = titles$title[[ha]]
  )
}

.train_position_continuous <- function(
  aesthetic,
  raw,
  scalespec,
  name,
  include_zero,
  lim = NULL,
  fit = NULL
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
    tfun <- .oob_wrap(function(v) as.numeric(v), scalespec, user_lim, rng)
    domain <- .expand_domain(rng, scalespec)
    # Breaks span the trained range `rng` (which honours explicit limits), not
    # the raw data: a month of data under 2019-2022 limits gets yearly ticks,
    # not weekly ones crammed into that month. An axis with no finite data and
    # no limits has nothing to tick (ggplot2 draws it bare); `rng` is then
    # only a placeholder window.
    no_data <- is.null(user_lim) &&
      !any(is.finite(suppressWarnings(as.numeric(raw))))
    auto <- NULL
    braw <- if (!is.null(user_breaks)) {
      user_breaks
    } else if (no_data) {
      .as_like_time(numeric(0), raw)
    } else if (!is.null(dbreaks) && is_classed) {
      scales::breaks_width(dbreaks)(.as_like_time(rng, raw))
    } else {
      auto <- .pretty_time_breaks(rng, raw, domain, dlabels, fit)
      auto$breaks
    }
    lab <- .match_labels(user_labels, braw, aesthetic) %||% auto$labels
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
    tfun <- .oob_wrap(
      function(v) tr$transform(as.numeric(v)),
      scalespec,
      user_lim,
      rng
    )
    tdom <- tr$transform(rng)
    if (!all(is.finite(tdom))) {
      cli::cli_abort(c(
        "The {.field {aesthetic}} data range falls outside the scale transform's domain.",
        i = "Range {.val {rng}} is not valid for this transform (e.g. a log scale needs positive values)."
      ))
    }
    # Order-preserving so a reversed `rng` keeps the axis reversed.
    domain <- .expand_domain(tdom, scalespec)
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

  trained_scale(
    aesthetic,
    type,
    map = tfun,
    name = name,
    discrete = FALSE,
    band_width = NULL,
    data_range = rng,
    domain = domain,
    breaks = breaks,
    labels = labels,
    sec = sec_list,
    transform = trans_name,
    time_unit = time_unit
  )
}

# A discrete (band) position scale: levels map to integer positions 1..k, each
# occupying a unit-width band; the domain pads half a band each side. A user
# `limits` (in `scalespec@domain`) sets/reorders/subsets the levels. Missing
# values get a trailing "NA" band (ggplot2's `na.translate = TRUE`) unless the
# user set the limits, which then decide exactly what is shown.
#
# `lim` (coord_cartesian(xlim =) and friends) zooms the view as on a continuous
# axis: it is in the axis's native positions (level i sits at i), as in ggplot2,
# and pads half a band each side. Levels stay; ticks outside the view drop.
.train_position_discrete <- function(
  aesthetic,
  values,
  scalespec,
  name,
  lim = NULL
) {
  user_limits <- !is.null(scalespec) && !is.null(scalespec@domain)
  levs <- if (user_limits) {
    as.character(scalespec@domain)
  } else {
    .cat_levels(values)
  }
  # scale_*_discrete(drop = TRUE), the default as in ggplot2: a factor level no
  # row uses gets no band. Explicit limits decide the levels themselves.
  drop <- is.null(scalespec) || !isFALSE(scalespec@drop)
  if (!user_limits && drop) {
    seen <- unique(as.character(unlist(
      lapply(values, as.character),
      use.names = FALSE
    )))
    levs <- levs[levs %in% seen]
  }
  na_pos <- NULL
  if (!user_limits && anyNA(unlist(values, use.names = FALSE))) {
    levs <- c(levs, "NA")
    na_pos <- length(levs)
  }
  k <- length(levs)
  domain <- c(0.5, k + 0.5)
  breaks <- seq_len(k)
  labels <- .discrete_axis_labels(levs, scalespec, aesthetic)
  # scale_*_discrete(breaks =): only these levels get a tick (in level order).
  ub <- if (!is.null(scalespec)) scalespec@breaks else NULL
  if (!is.null(ub)) {
    shown <- levs %in% as.character(ub)
    breaks <- breaks[shown]
    labels <- labels[shown]
  }
  if (!is.null(lim)) {
    lim <- as.numeric(lim)
    lim[is.na(lim)] <- c(1, k)[is.na(lim)]
    pad <- if (lim[1] > lim[2]) c(0.5, -0.5) else c(-0.5, 0.5)
    domain <- lim + pad
    shown <- breaks >= min(domain) & breaks <= max(domain)
    breaks <- breaks[shown]
    labels <- labels[shown]
  }
  trained_scale(
    aesthetic,
    "discrete",
    map = function(x) {
      i <- match(as.character(x), levs)
      if (!is.null(na_pos)) {
        i[is.na(x)] <- na_pos
      }
      i
    },
    name = name,
    discrete = TRUE,
    band_width = 1,
    data_range = c(1, k),
    domain = domain,
    breaks = breaks,
    labels = labels,
    levels = levs, # stays the data levels even if `labels` is relabelled later
    transform = "identity",
    time_unit = NULL
  )
}

# The tick labels of a discrete axis, one per level: the levels themselves, or
# scale_*_discrete(labels =) -- a function of the levels, a vector named by
# level (unnamed levels keep their own text), or an unnamed vector with one
# label per level that gets a tick (`breaks`, else every level).
.discrete_axis_labels <- function(levs, scalespec, aesthetic) {
  ul <- if (!is.null(scalespec)) scalespec@labels else NULL
  if (is.null(ul)) {
    return(levs)
  }
  if (is.function(ul)) {
    return(as.character(ul(levs)))
  }
  if (!is.null(names(ul))) {
    out <- levs
    hit <- levs %in% names(ul)
    out[hit] <- as.character(ul[levs[hit]])
    return(out)
  }
  ub <- if (!is.null(scalespec)) scalespec@breaks else NULL
  ticked <- if (!is.null(ub)) {
    levs %in% as.character(ub)
  } else {
    rep(TRUE, length(levs))
  }
  lab <- .match_labels(ul, levs[ticked], aesthetic)
  out <- levs
  out[ticked] <- lab
  out
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
  trained_scale(
    aesthetic,
    "binned",
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
    discrete = FALSE,
    band_width = bw,
    data_range = c(lo, hi),
    domain = .expand_domain(c(lo, hi), scalespec),
    breaks = brks,
    labels = labs,
    transform = "identity",
    time_unit = NULL
  )
}

# Train a colour scale -- `color`, `fill` or `edge_color`, each from every
# layer mapping its own channel (as ggplot2, a fill scale trains on all the
# fill values even when another layer maps a subset to `color`). Returns NULL
# when the channel is not mapped, else a trained-scale list carrying a
# values->colour mapping plus the data the legend needs.
.train_colour <- function(
  spec,
  resolved,
  channel = "color",
  aes = channel,
  titles = .plot_titles(spec, resolved)
) {
  values <- .pool_values(resolved, channel)
  if (!length(values)) {
    return(NULL)
  }

  scalespec <- .scale_for(spec, aes)
  explicit_type <- !is.null(scalespec) &&
    length(scalespec@type) &&
    nzchar(scalespec@type)
  chan_type <- .colour_channel_type(resolved, channel, explicit_type)
  type <- if (explicit_type) {
    scalespec@type
  } else if (identical(chan_type, "quantitative")) {
    "continuous"
  } else {
    "discrete"
  }
  # A discrete scale (scale_*_manual(), _discrete(), _brewer(), ...) on a
  # numeric variable: ggplot2 refuses it, rather than giving every distinct
  # number its own colour.
  if (explicit_type && identical(type, "discrete")) {
    .check_discrete_colour(resolved, channel, aes)
  }
  title <- titles$title[[aes]]

  pal <- if (!is.null(scalespec)) scalespec@palette else NULL
  # No explicit palette? A theme may supply a default (e.g. theme_cyberpunk()'s
  # neon set): `palette` for discrete, `palette.continuous` for continuous/binned.
  if (is.null(pal)) {
    pal <- .theme_palette(spec, type)
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
  key_style <- .key_style_for_aes(resolved, channel)
  key_alpha <- .key_alpha_for_aes(resolved, channel)

  # A bivariate scale (scale_fill_bivariate()), or -- with no explicit type -- a
  # fill mapped to a bi_class() column or given guide_bivariate(): a discrete
  # scale over the "x-y" classes with a 2-D legend. See R/bivariate.R.
  bi_info <- if (aes %in% c("color", "fill")) .bivariate_info(spec, aes)
  wants_bi <- identical(type, "bivariate") ||
    (!explicit_type &&
      (!is.null(bi_info) ||
        (!is.null(scalespec) &&
          is.list(scalespec@guide) &&
          identical(scalespec@guide$kind, "bivariate"))))
  if (wants_bi) {
    return(.train_bivariate(
      titles$named[[aes]],
      scalespec,
      values,
      bi_info,
      na_value,
      has_na,
      key_glyph,
      aes
    ))
  }

  # An identity scale uses the data values verbatim as colours (they must already
  # be valid R/CSS colours) and draws no legend.
  if (identical(type, "identity")) {
    return(.identity_scale(
      aes,
      as.character,
      name = title,
      na = FALSE,
      na_value = na_value,
      key_glyph = key_glyph
    ))
  }

  if (identical(type, "binned")) {
    v <- as.numeric(unlist(values, use.names = FALSE))
    v <- v[is.finite(v)]
    brks <- .binned_class_breaks(v, scalespec, user_breaks)
    # The ggplot2-named binned scales close classes on the right, as ggplot2.
    right <- identical(scalespec@style, "extended")
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
    cols <- .binned_colours(pal, brks, midpoint, scalespec@style)
    # `oob` acts on values past the outer boundaries; by default they squish
    # onto the outer classes.
    oob <- .oob_fun(scalespec) %||% scales::oob_squish
    map <- function(x) {
      x <- oob(as.numeric(x), range = range(brks))
      out <- cols[.class_index(x, brks, right)]
      out[!is.finite(x)] <- na_value
      out
    }
    labels <- .match_labels(user_labels, seq_len(k), aes) %||%
      .interval_labels(brks, right = right)
    trained_scale(
      aes,
      "binned",
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
  } else if (identical(type, "continuous")) {
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
    # `oob` (scale_*_continuous(oob =)) acts on values outside the range; the
    # default squishes them onto the ramp's ends (`bin()` clamps).
    oob <- .oob_fun(scalespec) %||% scales::oob_keep
    map <- function(x) {
      x <- oob(as.numeric(x), range = sort(rng))
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
    trained_scale(
      aes,
      "continuous",
      map = map,
      name = title,
      range = rng,
      pal256 = pal256,
      midpoint = midpoint,
      legend_breaks = lbrk,
      legend_labels = llab %||% .label_number_default(lbrk),
      # Whether the breaks/labels above were *asked for* rather than derived.
      # `guide_colourbar(n.breaks =)` re-derives the tick positions, so it needs
      # to know when not to: an explicit `breaks =`/`labels =` on the scale is a
      # statement about which values get a tick, and outranks a count.
      breaks_asked = !is.null(user_breaks),
      labels_asked = !is.null(user_labels),
      na = has_na,
      # Only missing values (and no limits): no colourbar, as in ggplot2.
      no_guide = if (.all_missing(v, scalespec)) TRUE,
      na_value = na_value,
      key_glyph = key_glyph
    )
  } else {
    all_levels <- .discrete_scale_levels(scalespec, values)
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
    keys <- .discrete_legend_keys(scalespec, all_levels, aes)
    levels <- keys$levels
    labels <- keys$labels
    trained_scale(
      aes,
      "discrete",
      map = map,
      name = title,
      levels = levels,
      labels = labels,
      colors = unname(cols[levels]),
      na = has_na,
      na_value = na_value,
      key_glyph = key_glyph,
      key_style = key_style,
      key_alpha = key_alpha
    )
  }
}

# Abort when every layer that maps a colour channel maps a quantitative
# variable to a discrete colour scale.
.check_discrete_colour <- function(resolved, channels, aes) {
  votes <- .colour_channel_votes(resolved, channels)$votes
  if (!length(votes)) {
    return(invisible())
  }
  quant <- vapply(
    votes,
    function(f) identical(f$type, "quantitative"),
    logical(1)
  )
  if (all(quant)) {
    ch <- votes[[1]]$channel
    cli::cli_abort(
      c(
        "Continuous values supplied to a discrete {.field {aes}} scale.",
        x = "{.field {ch}} is mapped to a numeric variable.",
        i = "Map a factor (e.g. {.code {ch} = factor(...)}), or use a continuous
          scale such as {.fn scale_{aes}_continuous}."
      ),
      call = NULL
    )
  }
  invisible()
}

# The encoding type driving a colour scale, from every layer mapping one of
# `channels`. Layers that disagree on whether the variable is quantitative
# cannot share one scale; unless the user fixed the scale type explicitly, that
# is an error rather than a silent squish/grey.
.colour_channel_type <- function(resolved, channels, explicit_type = FALSE) {
  found <- .colour_channel_votes(resolved, channels)
  if (!length(found$votes)) {
    return(found$fallback)
  }
  clash <- .colour_type_clash(found$votes)
  if (!explicit_type && !is.null(clash)) {
    q <- clash$quantitative
    d <- clash$discrete
    cli::cli_abort(c(
      "Layers map {.field {q$channel}} to incompatible variable types.",
      "x" = "One layer maps it to a quantitative variable, another to a
        {d$type} one.",
      "i" = "Layers share one {.field {q$channel}} scale, so every layer
        mapping it must use the same kind of variable.",
      "i" = "Convert one of them (e.g. with {.fn factor} or
        {.fn as.numeric}), or set the scale type explicitly."
    ))
  }
  found$votes[[1]]$type
}

# Each layer's encoding type for the colour `channels`, as a list of
# `list(layer, channel, type)` votes, plus the first type seen at all (the
# fallback when no layer votes). Shared by `.colour_channel_type()` and the
# pre-compile check in `spec_diagnose()`, so the two agree on what clashes.
.colour_channel_votes <- function(resolved, channels) {
  votes <- list()
  fallback <- NULL
  for (i in seq_along(resolved)) {
    L <- resolved[[i]]
    for (ch in channels) {
      t <- L$types[[ch]]
      fallback <- fallback %||% t
      v <- L$values[[ch]]
      # An empty (e.g. an empty facet cell) or all-NA layer carries no type
      # information -- its inferred type is an artefact -- so it never votes.
      if (!is.null(t) && length(v) && !all(is.na(v))) {
        votes[[length(votes) + 1L]] <- list(layer = i, channel = ch, type = t)
      }
    }
  }
  list(votes = votes, fallback = fallback)
}

# The first quantitative and first non-quantitative vote when the colour votes
# disagree, or NULL when they agree.
.colour_type_clash <- function(votes) {
  quant <- vapply(
    votes,
    function(f) identical(f$type, "quantitative"),
    logical(1)
  )
  if (!any(quant) || all(quant)) {
    return(NULL)
  }
  list(
    quantitative = votes[[which(quant)[1]]],
    discrete = votes[[which(!quant)[1]]]
  )
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
  channel,
  aes,
  out_default,
  legend_field,
  titles = .plot_titles(spec, resolved)
) {
  values <- .pool_values(resolved, channel)
  if (is.null(values)) {
    return(NULL)
  }
  scalespec <- .scale_for(spec, aes)
  # An identity scale uses the data values verbatim (mm for size, opacity for
  # alpha), with no guide.
  if (!is.null(scalespec) && identical(scalespec@type, "identity")) {
    return(.identity_scale(aes, as.numeric, titles$default[[aes]]))
  }
  out_range <- if (!is.null(scalespec) && !is.null(scalespec@range)) {
    as.numeric(scalespec@range)
  } else {
    out_default
  }
  stype <- if (!is.null(scalespec)) scalespec@type else ""
  # scale_size_binned(): classes of the data, one size per class.
  if (identical(stype, "binned")) {
    return(.train_binned_aes(
      scalespec,
      values,
      aes,
      out_range,
      legend_field,
      titles
    ))
  }
  # A factor, character or logical mapped to size or alpha trains a discrete
  # scale (ggplot2's scale_size_discrete() / scale_alpha_discrete()), so its
  # legend keys are the levels, not the factor's integer codes. So does an
  # explicit discrete scale (scale_size_manual(), scale_alpha_ordinal(), ...).
  if (
    aes %in% .DISCRETE_SIZED_AES &&
      (.discrete_values(values) || stype %in% c("discrete", "ordinal"))
  ) {
    # Manual values on a numeric variable: ggplot2 refuses it, rather than
    # giving every distinct number its own size.
    manual <- !is.null(scalespec) && !is.null(scalespec@palette)
    if (manual && !.discrete_values(values)) {
      cli::cli_abort(
        c(
          "Continuous values supplied to a discrete {.field {aes}} scale.",
          x = "{.field {channel}} is mapped to a numeric variable.",
          i = "Map a factor (e.g. {.code {channel} = factor(...)}), or use a
            continuous scale such as {.fn scale_{aes}_continuous}."
        ),
        call = NULL
      )
    }
    return(.train_discrete_aes(
      scalespec,
      values,
      aes,
      out_range,
      legend_field,
      titles
    ))
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
  # `scale_size_area()` (type "area") maps value -> marker *area*: size 0 at
  # value 0, and size = max_size * sqrt(value / data_max), so area is
  # proportional to the value. Otherwise the aesthetic rescales linearly.
  area_mode <- !is.null(scalespec) && identical(scalespec@type, "area")
  # `oob` (scale_size(oob =) and friends) acts on values outside the range
  # before they are mapped; by default they extrapolate (oob_keep).
  oob <- .oob_fun(scalespec) %||% scales::oob_keep
  map <- if (area_mode) {
    max_size <- out_range[2]
    denom <- max(rng[2], .Machine$double.eps)
    function(x) {
      x <- oob(as.numeric(x), range = sort(rng))
      max_size * sqrt(pmax(x, 0) / denom)
    }
  } else {
    function(x) {
      scales::rescale(
        oob(as.numeric(x), range = sort(rng)),
        to = out_range,
        from = rng
      )
    }
  }
  name <- titles$title[[aes]]
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
  rlang::exec(
    trained_scale,
    aes,
    "continuous",
    map = map,
    name = name,
    range = rng,
    na = has_na,
    # Only missing values (and no limits): nothing to key, so no legend, as
    # in ggplot2 -- not a 0-1 legend for a range the data never had.
    no_guide = if (.all_missing(v, scalespec)) TRUE,
    legend_breaks = lbrk,
    legend_labels = llab %||% .label_number_default(lbrk),
    !!!rlang::set_names(list(map(lbrk)), legend_field)
  )
}

# The sized aesthetics that train a discrete scale for categorical data. The
# widths stay continuous: a categorical line width is a linetype's job.
.DISCRETE_SIZED_AES <- c("size", "alpha")

# Is a pooled channel categorical (any layer maps a factor, character or
# logical)? An empty or all-NA layer (an empty facet cell) carries no type
# information -- a bare NA is logical -- so it never votes.
.discrete_values <- function(values) {
  any(vapply(
    values,
    function(v) {
      length(v) > 0L &&
        !all(is.na(v)) &&
        (is.factor(v) || is.character(v) || is.logical(v))
    },
    logical(1)
  ))
}

# Train a discrete size / alpha scale: one key per level, the levels spread
# evenly over the output range -- in area for size (so the marks grow
# perceptually evenly), linearly for alpha -- as ggplot2's
# scale_size_ordinal() / scale_alpha_ordinal() do. Like ggplot2 it warns for an
# unordered variable, where size or alpha implies an order the data lacks.
# `breaks` picks the levels the legend shows; `labels` relabels them.
.train_discrete_aes <- function(
  scalespec,
  values,
  aes,
  out_range,
  legend_field,
  titles
) {
  all_levels <- .discrete_scale_levels(scalespec, values)
  # Manual values (scale_size_manual(), scale_alpha_manual()) set each level's
  # output directly, so they imply no order and never warn.
  pal <- if (!is.null(scalespec)) scalespec@palette else NULL
  voting <- Filter(function(v) length(v) > 0L && !all(is.na(v)), values)
  ordered <- all(vapply(voting, is.ordered, logical(1))) ||
    (!is.null(scalespec) && identical(scalespec@type, "ordinal"))
  if (!ordered && is.null(pal)) {
    cli::cli_warn(
      "Using {.field {aes}} for a discrete variable is not advised.",
      class = "vellumplot_discrete_sized_aes"
    )
  }
  n <- length(all_levels)
  out <- if (!is.null(pal)) {
    if (is.null(names(pal)) && n > length(pal)) {
      .not_enough_abort(
        "values",
        aes,
        paste0("scale_", aes, "_manual(values=)"),
        n,
        length(pal)
      )
    }
    as.numeric(.palette_for_levels(pal, all_levels))
  } else if (identical(aes, "size")) {
    sqrt(seq(out_range[1]^2, out_range[2]^2, length.out = n))
  } else {
    seq(out_range[1], out_range[2], length.out = n)
  }
  names(out) <- all_levels
  na_out <- if (!is.null(scalespec)) scalespec@na_value else NULL
  map <- function(x) {
    o <- unname(out[as.character(x)])
    if (!is.null(na_out)) {
      o[is.na(o)] <- na_out
    }
    o
  }
  keys <- .discrete_legend_keys(scalespec, all_levels, aes)
  levels <- keys$levels
  labels <- keys$labels
  rlang::exec(
    trained_scale,
    aes,
    "discrete",
    map = map,
    name = titles$title[[aes]],
    levels = levels,
    na = anyNA(unlist(values, use.names = FALSE)),
    legend_breaks = levels,
    legend_labels = labels,
    !!!rlang::set_names(list(map(levels)), legend_field)
  )
}

# Train the size scale (if any layer maps `size` to data). Maps values to a
# point-size range in mm and carries representative breaks for a size legend.
.train_size <- function(spec, resolved, titles = .plot_titles(spec, resolved)) {
  .train_continuous_aes(
    spec,
    resolved,
    channel = "size",
    aes = "size",
    out_default = .SIZE_RANGE,
    legend_field = "legend_sizes",
    titles = titles
  )
}

# Edge-width output range (lwd multiples) a mapped edge `linewidth` spans.
.EDGE_WIDTH_RANGE <- c(0.3, 3)

# Line-width output range (lwd units) a mapped `linewidth` spans.
.LINEWIDTH_RANGE <- c(0.5, 4)

# `.LINEWIDTH_MARKS` (the marks that read the line-width scale rather than the
# edge-width one) lives with the aesthetic registry, R/aes-registry.R.

# The layers of `resolved` whose mark is (or, with `invert`, is not) in `marks`.
.layers_with_mark <- function(resolved, marks, invert = FALSE) {
  hit <- vapply(resolved, function(L) L$mark %in% marks, logical(1))
  resolved[if (invert) !hit else hit]
}

# Train the edge-width scale (if any edge layer maps `linewidth` to data). Mirrors
# the size scale: rescale the data range to an lwd range; emits its own legend.
.train_edge_width <- function(
  spec,
  resolved,
  titles = .plot_titles(spec, resolved)
) {
  .train_continuous_aes(
    spec,
    .layers_with_mark(resolved, .LINEWIDTH_MARKS, invert = TRUE),
    channel = "linewidth",
    aes = "edge_width",
    out_default = .EDGE_WIDTH_RANGE,
    legend_field = "legend_widths",
    titles = titles
  )
}

# Train the line-width scale (if a line/step/segment layer maps `linewidth`).
# Same shape as the edge-width scale, on the marks whose emitter can honour it.
.train_linewidth <- function(
  spec,
  resolved,
  titles = .plot_titles(spec, resolved)
) {
  .train_continuous_aes(
    spec,
    .layers_with_mark(resolved, .LINEWIDTH_MARKS),
    channel = "linewidth",
    aes = "linewidth",
    out_default = .LINEWIDTH_RANGE,
    legend_field = "legend_widths",
    titles = titles
  )
}

# Does `x` read as SVG path data (a `d` attribute: a moveto with its two
# coordinates, then numbers and path commands) or name a `.svg` file?
.looks_like_svg_shape <- function(x) {
  x <- as.character(x)
  num <- "[-+]?(?:\\d+\\.?\\d*|\\.\\d+)(?:[eE][-+]?\\d+)?"
  !is.na(x) &
    (grepl(
      paste0(
        "^\\s*[Mm]\\s*",
        num,
        "[\\s,]*",
        num,
        "[-+.,0-9eE\\sMmLlHhVvCcSsQqTtAaZz]*$"
      ),
      x,
      perl = TRUE
    ) |
      grepl("\\.svg$", x, ignore.case = TRUE))
}

# A mapped `shape` whose values are SVG paths is a mapping, not icons: the
# levels cycle through the shape palette (VP-112). Say so, once per plot, and
# point to the forms that draw the paths.
.warn_shape_paths <- function(levels) {
  if (!any(.looks_like_svg_shape(levels))) {
    return(invisible())
  }
  cli::cli_warn(
    c(
      "The mapped {.field shape} values look like SVG paths, but a mapped \\
       {.field shape} is a variable: its levels cycle through the shape \\
       palette.",
      i = "To draw one path for every point, give it as a literal string \\
           ({.code shape = \"M0 0 L1 1 ...\"}) or inject the variable \\
           ({.code shape = !!path}).",
      i = "To draw each row's own path, add {.fn scale_shape_identity}."
    ),
    class = "vellumplot_shape_svg_mapped"
  )
  invisible()
}

# Train the shape scale (if any layer maps `shape` to data). Shape is always
# discrete: levels cycle through `.SHAPE_PALETTE` (or a user `scale_shape(values)`).
.train_shape <- function(
  spec,
  resolved,
  titles = .plot_titles(spec, resolved)
) {
  values <- .pool_values(resolved, "shape")
  if (is.null(values)) {
    return(NULL)
  }
  scalespec <- .scale_for(spec, "shape")
  # An identity shape scale uses the data values verbatim as shape names.
  if (!is.null(scalespec) && identical(scalespec@type, "identity")) {
    return(.identity_scale(
      "shape",
      function(x) as.character(.pch_to_shape(x)),
      titles$default[["shape"]]
    ))
  }
  levels <- .discrete_scale_levels(scalespec, values)
  .warn_shape_paths(levels)
  has_na <- anyNA(unlist(values, use.names = FALSE))
  pal <- if (!is.null(scalespec) && !is.null(scalespec@palette)) {
    scalespec@palette
  } else {
    .SHAPE_PALETTE
  }
  if (is.null(names(pal)) && length(levels) > length(pal)) {
    .not_enough_abort(
      "shapes",
      "shape",
      "scale_shape(values=)",
      length(levels),
      length(pal)
    )
  }
  shapes <- .palette_for_levels(pal, levels)
  names(shapes) <- levels
  na_shape <- (if (!is.null(scalespec)) scalespec@na_value) %||% .SHAPE_NA
  # NA values (and levels outside `limits`) map to the NA shape (a neutral
  # circle) rather than to NA, which would otherwise reach points_grob and
  # error; the legend shows an NA key.
  map <- function(x) {
    s <- unname(shapes[as.character(x)])
    s[is.na(s)] <- na_shape
    s
  }
  keys <- .discrete_legend_keys(scalespec, levels, "shape")
  trained_scale(
    "shape",
    "discrete",
    map = map,
    name = titles$title[["shape"]],
    levels = keys$levels,
    labels = keys$labels,
    na = has_na,
    shapes = map(keys$levels)
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
.train_pattern <- function(
  spec,
  resolved,
  titles = .plot_titles(spec, resolved)
) {
  values <- .pool_values(resolved, "pattern")
  if (is.null(values)) {
    return(NULL)
  }
  scalespec <- .scale_for(spec, "pattern")
  levels <- .discrete_scale_levels(scalespec, values)
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
  trained_scale(
    "pattern",
    "discrete",
    map = function(x) as.character(x),
    name = titles$title[["pattern"]],
    objs = objs,
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
.train_alpha <- function(
  spec,
  resolved,
  titles = .plot_titles(spec, resolved)
) {
  .train_continuous_aes(
    spec,
    resolved,
    channel = "alpha",
    aes = "alpha",
    out_default = .ALPHA_RANGE,
    legend_field = "legend_alphas",
    titles = titles
  )
}

# Train the linetype scale (if any layer maps `linetype` to data). Always
# discrete: levels cycle through `.LINETYPE_PALETTE` (or a user palette).
.train_linetype <- function(
  spec,
  resolved,
  channel = "linetype",
  aes = "linetype",
  titles = .plot_titles(spec, resolved)
) {
  values <- .pool_values(resolved, channel)
  if (is.null(values)) {
    return(NULL)
  }
  scalespec <- .scale_for(spec, aes)
  if (!is.null(scalespec) && identical(scalespec@type, "identity")) {
    return(.identity_scale(
      aes,
      as.character,
      titles$default[[aes]]
    ))
  }
  levels <- .discrete_scale_levels(scalespec, values)
  pal <- if (!is.null(scalespec) && !is.null(scalespec@palette)) {
    scalespec@palette
  } else {
    .LINETYPE_PALETTE
  }
  if (is.null(names(pal)) && length(levels) > length(pal)) {
    .not_enough_abort(
      "line types",
      "linetype",
      "scale_linetype(values=)",
      length(levels),
      length(pal)
    )
  }
  ltys <- .palette_for_levels(pal, levels)
  names(ltys) <- levels
  na_lty <- if (!is.null(scalespec)) scalespec@na_value else NULL
  map <- function(x) {
    out <- unname(ltys[as.character(x)])
    if (!is.null(na_lty)) {
      out[is.na(out)] <- na_lty
    }
    out
  }
  keys <- .discrete_legend_keys(scalespec, levels, aes)
  trained_scale(
    aes,
    "discrete",
    map = map,
    name = titles$title[[aes]],
    levels = keys$levels,
    labels = keys$labels,
    linetypes = map(keys$levels)
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

.train_edge_colour <- function(
  spec,
  resolved,
  titles = .plot_titles(spec, resolved)
) {
  .train_colour(spec, resolved, channel = "edge_color", titles = titles)
}

.train_fill <- function(spec, resolved, titles = .plot_titles(spec, resolved)) {
  .train_colour(spec, resolved, channel = "fill", titles = titles)
}

.train_edge_alpha <- function(
  spec,
  resolved,
  titles = .plot_titles(spec, resolved)
) {
  .train_continuous_aes(
    spec,
    resolved,
    channel = "edge_alpha",
    aes = "edge_alpha",
    out_default = .ALPHA_RANGE,
    legend_field = "legend_alphas",
    titles = titles
  )
}

.train_edge_linetype <- function(
  spec,
  resolved,
  titles = .plot_titles(spec, resolved)
) {
  .train_linetype(
    spec,
    resolved,
    channel = "edge_linetype",
    aes = "edge_linetype",
    titles = titles
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

# Native-coordinate extent a density-shape mark (violin/ridgeline/halfeye) will
# draw along one axis, so scale training can widen the panel to fit it. These
# marks generate their geometry at emit time from `stats::density()`, which
# extends past the raw data (the density support is padded ~3 bandwidths), and a
# ridge additionally rises `scale * band` above its category baseline -- none of
# which the raw-data axis pool sees. `sc` is the trained scale of axis `aes`;
# the result is `c(lo, hi)` in that scale's *native* (transformed) coordinates,
# or `NULL` when the mark does not extend the axis. The density math mirrors the
# emitters exactly (shared `.density_by_cat`, estimated on the native values from
# `.native_values()`), so the trained domain matches what is drawn -- also on a
# log/sqrt axis.
.mark_footprint <- function(L, aes, sc) {
  adjust <- L$stat_params$adjust %||% 1
  # the density support of `vals` per category of `cats`, mapped to native
  support <- function(vals, cats) {
    dens <- .density_by_cat(
      .native_values(sc, vals),
      cats,
      .cat_levels(cats),
      adjust,
      .group_key(L),
      trim = isTRUE(L$stat_params$trim)
    )
    .finite_range(unlist(lapply(dens, function(d) if (!is.null(d)) d$x)))
  }
  # category positions on a discrete axis, widened by `lo`/`hi` bands
  cats_extent <- function(cats, lo, hi) {
    pos <- sc$map(.cat_levels(cats))
    band <- sc$band_width %||% .resolution(sc$map(cats))
    c(min(pos) - lo * band, max(pos) + hi * band)
  }
  if (identical(L$mark, "violin")) {
    # a violin's value axis is y, or x when it is horizontal
    ax <- .dist_axes(.dist_orient(L))
    if (identical(aes, ax[["val"]])) {
      return(support(L$values[[ax[["val"]]]], L$values[[ax[["cat"]]]]))
    }
    cats_extent(
      L$values[[ax[["cat"]]]],
      .VIOLIN_HALFWIDTH,
      .VIOLIN_HALFWIDTH
    )
  } else if (identical(L$mark, "halfeye")) {
    ax <- .dist_axes(.dist_orient(L))
    if (identical(aes, ax[["val"]])) {
      return(support(L$values[[ax[["val"]]]], L$values[[ax[["cat"]]]]))
    }
    # one-sided slab: extends to the right of (or above) each tick by its width
    cats_extent(
      L$values[[ax[["cat"]]]],
      0,
      L$stat_params$scale %||% .SLAB_WIDTH
    )
  } else if (identical(L$mark, "ridgeline")) {
    if (identical(aes, "x")) {
      return(support(L$values$x, L$values$y))
    }
    # a ridge rises `height * band` above its category baseline
    pos <- sc$map(.cat_levels(L$values$y))
    band <- sc$band_width %||% 1
    c(min(pos), max(pos) + (L$stat_params$height %||% .RIDGE_HEIGHT) * band)
  } else {
    NULL
  }
}

# The range of the finite values of `x`, or NULL when there are none.
.finite_range <- function(x) {
  x <- x[is.finite(x)]
  if (length(x)) range(x) else NULL
}

# Grow a native domain to cover `ext`, keeping its orientation: a descending
# domain (a reversed axis, or explicit `c(hi, lo)` limits) stays descending.
.union_domain <- function(domain, ext) {
  out <- range(c(domain, ext))
  if (length(domain) == 2L && domain[1] > domain[2]) rev(out) else out
}

# Train one position axis: the data/intercept/extent pool through
# `.train_position()`, then widen the domain so any violin/ridgeline/halfeye
# layer's drawn footprint is not clipped. The widening only ever grows a domain,
# and is skipped when the user pinned this axis's limits (`coord_*(xlim=/ylim=)`
# or a scale `limits=`), since explicit limits are intentional cropping that must
# win. Breaks were picked for the raw data range, so a widened axis is re-broken
# over the drawn extent (`.rebreak_widened()`) -- else the density tails run past
# the last labelled tick. The single entry point for both the shared scales and
# free facet scales.
.train_axis <- function(
  spec,
  resolved,
  aes,
  title,
  include_zero = FALSE,
  lim = NULL,
  values = .axis_pool(resolved, aes, paste0(aes, "intercept")),
  fit_ctx = NULL
) {
  scalespec <- .scale_for(spec, aes)
  fit <- .axis_label_fit(spec, aes, fit_ctx)
  sc <- .train_position(aes, values, scalespec, title, include_zero, lim, fit)
  # Bars on a continuous x, and tiles / rasters on either axis, reach half a
  # cell past their centres: train on those edges too, as ggplot2 does, so the
  # end bars and tiles are not clipped (VP-90, VP-99).
  ext <- .mark_extent_data(sc, resolved, aes, scalespec, lim)
  if (!is.null(ext)) {
    values <- c(values, list(ext))
    sc <- .train_position(aes, values, scalespec, title, include_zero, lim, fit)
  }
  sc <- .widen_for_footprints(
    sc,
    spec,
    resolved,
    aes,
    values,
    scalespec,
    title,
    lim
  )
  # Infinite positions land on the (final) panel edge.
  if (!isTRUE(sc$discrete)) {
    sc <- .scale_set(sc, map = .inf_to_edge(sc$map, sc$domain))
  }
  .attach_axis_guide(sc, scalespec)
}

# The extent along position axis `aes` of the marks drawn wider than their
# centres, in data units: bars on x (`mark_bar()`, `mark_histogram()`, ...: each
# bar's centre +/- half its width, computed in native units exactly as
# `.emit_bar()` places and sizes it via `.bar_geometry()`, after any dodge), and
# tiles and rasters on x and y (each cell's centre +/- half its width / height,
# as `.emit_tile()` / `.emit_raster()` size them, VP-99), and error bars' caps
# and cross-bars' boxes along their category axis (`.errorbar_geometry()`,
# VP-104, VP-105) -- ggplot2 trains on
# these xmin/xmax/ymin/ymax edges too. The native extent is taken back through
# the transform's inverse (a date axis keeps its class). NULL when there are no
# such marks, the axis is discrete or binned (cells fill their bands), or the
# user pinned the limits, which must win.
.mark_extent_data <- function(sc, resolved, aes, scalespec, lim) {
  if (
    !aes %in% c("x", "y") ||
      isTRUE(sc$discrete) ||
      !is.null(sc$band_width) ||
      !is.null(lim) ||
      (!is.null(scalespec) && !is.null(scalespec@domain))
  ) {
    return(NULL)
  }
  ext <- NULL
  proto <- NULL
  for (L in resolved) {
    v <- L$values[[aes]]
    if (
      is.null(v) ||
        !L$mark %in% c("bar", "tile", "raster", "errorbar", "crossbar") ||
        (identical(L$mark, "bar") && !identical(aes, "x")) ||
        (L$mark %in%
          c("errorbar", "crossbar") &&
          !identical(aes, .dodge_axis(L))) ||
        (!is.numeric(v) && !inherits(v, c("Date", "POSIXct")))
    ) {
      next
    }
    vp <- sc$map(v)
    ok <- is.finite(vp)
    if (!any(ok)) {
      next
    }
    proto <- proto %||% v
    edges <- switch(
      L$mark,
      bar = {
        g <- .bar_geometry(L, vp, .resolution(vp), length(vp))
        c(g$x[ok] - g$width[ok] / 2, g$x[ok] + g$width[ok] / 2)
      },
      errorbar = ,
      crossbar = {
        g <- .errorbar_geometry(L, vp, .resolution(vp), length(vp))
        c(g$x[ok] - g$width[ok] / 2, g$x[ok] + g$width[ok] / 2)
      },
      tile = {
        w <- .tile_size(L, aes, vp, length(vp))
        c(vp[ok] - w[ok] / 2, vp[ok] + w[ok] / 2)
      },
      raster = sc$map(.raster_extent(v))
    )
    ext <- range(c(ext, edges), finite = TRUE)
  }
  if (is.null(ext)) {
    return(NULL)
  }
  if (!is.null(sc$time_unit)) {
    return(.as_like_time(ext, proto))
  }
  inv <- .resolve_trans(scalespec)$inverse
  out <- suppressWarnings(inv(ext))
  if (all(is.finite(out))) out
}

# The footprint widening of `.train_axis()` (see there).
.widen_for_footprints <- function(
  sc,
  spec,
  resolved,
  aes,
  values,
  scalespec,
  title,
  lim
) {
  explicit <- !is.null(lim) ||
    (!is.null(scalespec) && !is.null(scalespec@domain))
  if (explicit || !is.numeric(sc$domain)) {
    return(sc)
  }
  drawn <- NULL # native extent of every footprint
  for (L in resolved) {
    if (!L$mark %in% c("violin", "ridgeline", "halfeye")) {
      next
    }
    ext <- .mark_footprint(L, aes, sc)
    if (!is.null(ext) && all(is.finite(ext))) {
      sc <- .scale_set(sc, domain = .union_domain(sc$domain, ext))
      drawn <- range(c(drawn, ext))
    }
  }
  if (!is.null(drawn)) {
    sc <- .rebreak_widened(sc, drawn, aes, values, scalespec, title)
  }
  sc
}

# Re-pick a continuous axis's breaks (labels, secondary axis) for the extent a
# density footprint widened it to. `drawn` is that footprint in native units; it
# goes back to data units through the transform's inverse and is unioned with the
# data range, then the axis is re-trained over that range as fixed limits, and
# only its breaks/labels/secondary axis are kept -- the (already widened) domain
# is left alone. A no-op for user-set `breaks` (taken as given), date/time axes,
# discrete axes, and a transform without an inverse.
.rebreak_widened <- function(sc, drawn, aes, values, scalespec, title) {
  if (
    isTRUE(sc$discrete) ||
      !is.null(sc$time_unit) ||
      (!is.null(scalespec) && !is.null(scalespec@breaks))
  ) {
    return(sc)
  }
  inv <- .resolve_trans(scalespec)$inverse
  if (is.null(inv)) {
    return(sc)
  }
  rng <- range(c(sc$data_range, suppressWarnings(inv(drawn))))
  if (!all(is.finite(rng))) {
    return(sc)
  }
  wide <- .train_position(aes, values, scalespec, title, lim = rng)
  .scale_set(sc, breaks = wide$breaks, labels = wide$labels, sec = wide$sec)
}

# Train all scales the plot needs: x, y (position), colour, and size. Bars force
# the y axis to include the zero baseline.
.train_scales <- function(
  spec,
  resolved,
  titles = .plot_titles(spec, resolved)
) {
  zero_base <- .needs_zero(resolved)
  co <- .coord_of(spec)
  xs <- .axis_pool(resolved, "x", "xintercept")
  ys <- .axis_pool(resolved, "y", "yintercept")
  # A rug draws on whichever axes it has, so `mark_rug(x = )` alone trains an
  # empty y axis (as ggplot2 does) instead of asking for a y encoding.
  has_rug <- any(vapply(resolved, function(L) identical(L$mark, "rug"), NA))
  blank <- NULL
  if (has_rug && xor(is.null(xs), is.null(ys))) {
    blank <- if (is.null(xs)) "x" else "y"
    if (is.null(xs)) xs <- list(numeric(0)) else ys <- list(numeric(0))
  }
  if (is.null(xs) || is.null(ys)) {
    miss <- c("x", "y")[c(is.null(xs), is.null(ys))]
    cli::cli_abort(
      "No layer maps {.field {miss}}; every plot needs an {.field x} and a {.field y} position."
    )
  }
  # The legend scales first: they never read the position scales, and the
  # date-break fit of the axes takes the legends' width off the panel (VP-101).
  # Then the vertical axis, whose label gutter the horizontal axis's fit also
  # subtracts.
  # Each registered scale by its trainer, then its guides() override
  # (guide = "none" / guide_legend()).
  aes_sc <- list()
  for (e in .AES_REGISTRY) {
    if (is.null(e$trainer)) {
      next
    }
    sc <- .aes_trainer(e)(spec, resolved, titles = titles)
    ss <- .scale_for(spec, e$name)
    g <- if (!is.null(ss)) ss@guide else NULL
    if (!is.null(sc) && !is.null(g)) {
      sc <- .apply_guide(sc, g)
    }
    if (!is.null(sc) && e$guide %in% .KEY_ALPHA_GUIDES && is.null(sc$key_alpha)) {
      sc <- .with_key_alpha(sc, e, resolved)
    }
    if (!is.null(sc) && e$guide %in% c("color", "size", "shape")) {
      kp <- .key_point_for_aes(
        Filter(function(L) .aes_serves_mark(e, L$mark), resolved),
        e$aliases[[1]]
      )
      if (!is.null(kp)) {
        sc <- .scale_set(sc, key_point = kp)
      }
    }
    aes_sc[e$name] <- list(sc)
  }
  flip <- identical(co@kind, "flip")
  train <- function(aes, scales_so_far) {
    .train_axis(
      spec,
      resolved,
      aes,
      titles$title[[aes]],
      include_zero = identical(aes, "y") && zero_base,
      lim = if (identical(aes, "x")) co@xlim else co@ylim,
      values = if (identical(aes, "x")) xs else ys,
      fit_ctx = function() .axis_fit_ctx(spec, scales_so_far, titles, flip)
    )
  }
  va <- if (flip) "x" else "y"
  ha <- if (flip) "y" else "x"
  pos <- list()
  pos[[va]] <- train(va, aes_sc)
  pos[[ha]] <- train(ha, c(aes_sc, pos))
  scales <- list(x = pos$x, y = pos$y)
  if (!is.null(blank)) {
    scales[[blank]] <- .scale_set(
      scales[[blank]],
      breaks = numeric(0),
      labels = character(0),
      name = NULL
    )
  }
  # The registered scales follow x and y, in registry (legend) order; an
  # untrained one stays as a NULL entry so every key is always present.
  scales <- c(scales, aes_sc)
  if (identical(.fill_series_aes(scales), "fill")) {
    scales$fill$series_aes <- "fill"
  }
  scales
}

# The legends whose point / line keys take a layer's constant alpha (VP-70);
# a discrete colour scale reads its own in `.train_colour()`.
.KEY_ALPHA_GUIDES <- c(
  "color",
  "size",
  "shape",
  "linewidth",
  "edge_width",
  "linetype"
)

# `sc` with the `key_alpha` (`.key_alpha_for_aes()`) of the layers registry row
# `e` serves, read on the scale's channel (`linewidth` for the edge widths).
.with_key_alpha <- function(sc, e, resolved) {
  served <- Filter(function(L) .aes_serves_mark(e, L$mark), resolved)
  ka <- .key_alpha_for_aes(served, e$aliases[[1]])
  if (is.null(ka)) sc else .scale_set(sc, key_alpha = ka)
}

# The namespace of a discrete fill legend's interactive series keys
# ("<aes>:<level>", see `.legend_series_key()`): "color", as when colour and
# fill shared one scale, unless a separate discrete colour legend -- one that
# does not merge with it (another title or other keys) -- would claim the same
# keys, when the fill keys read "fill:<level>".
.fill_series_aes <- function(scales) {
  cs <- scales$color
  fs <- scales$fill
  apart <- !is.null(cs) &&
    !is.null(fs) &&
    identical(cs$type, "discrete") &&
    identical(fs$type, "discrete") &&
    !(identical(cs$name, fs$name) &&
      identical(cs$levels, fs$levels) &&
      identical(cs$labels, fs$labels))
  if (apart) "fill" else "color"
}

# Did a continuous legend scale see no finite value, and no limits to show
# instead? `v` is the pooled values with the non-finite ones dropped.
.all_missing <- function(v, scalespec) {
  !length(v) && (is.null(scalespec) || is.null(scalespec@domain))
}

# A scale's `oob` (out-of-bounds) handler as a `function(x, range)`, or NULL
# when the scale keeps its default. The strings name scales' handlers:
# "censor" (outside values become NA), "squish" (clamped to the limits),
# "keep" (left alone). Infinite values pass all three, as in scales.
.oob_fun <- function(scalespec) {
  oob <- if (!is.null(scalespec)) scalespec@oob else NULL
  if (is.null(oob) || is.function(oob)) {
    return(oob)
  }
  switch(
    oob,
    censor = scales::oob_censor,
    squish = scales::oob_squish,
    keep = scales::oob_keep
  )
}

# Wrap a position scale's data -> native map with its `oob` handler, applied
# in data space against the user's limits. Without limits nothing is out of
# bounds, and without `oob` an axis keeps its default: values past the limits
# are drawn where they fall and clipped by the panel (oob_keep).
.oob_wrap <- function(map, scalespec, user_lim, rng) {
  force(map)
  oob <- .oob_fun(scalespec)
  if (is.null(oob) || is.null(user_lim)) {
    return(map)
  }
  lims <- sort(as.numeric(rng))
  function(v) map(oob(as.numeric(v), range = lims))
}

# A continuous position map that puts an infinite data position on the panel
# edge, as ggplot2 does: `x = Inf` is the right edge, `-Inf` the left (under a
# reverse transform, mirrored). Only infinite *data* moves: a finite value a
# transform cannot represent (0 on a log axis) stays non-finite, so the marks
# and density estimates that drop it still do. `domain` is the final native
# domain.
.inf_to_edge <- function(map, domain) {
  force(map)
  lo <- min(domain)
  hi <- max(domain)
  function(x) {
    out <- map(x)
    inf <- is.numeric(x) & !is.na(x) & is.infinite(x)
    if (any(inf) && is.numeric(out)) {
      out[inf & out > 0] <- hi
      out[inf & out < 0] <- lo
    }
    out
  }
}
