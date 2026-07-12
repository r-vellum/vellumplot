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

# Continuous position transforms. Each is `(transform, inverse, breaks, format)`:
# `transform` maps data -> native (viewport) units; `breaks` generates breaks in
# DATA units (then transformed to positions); `format` labels them. Kept as a
# local registry (rather than scales::transform_*) so log10 label formatting
# stays under our control rather than tracking scales' defaults.
.TRANSFORMS <- list(
  identity = list(
    transform = function(x) x,
    breaks = function(rng) scales::breaks_extended()(rng),
    format = function(b) scales::label_number()(b)
  ),
  log10 = list(
    transform = function(x) log10(x),
    breaks = function(rng) scales::breaks_log()(rng),
    format = function(b) format(b, trim = TRUE, scientific = FALSE)
  ),
  sqrt = list(
    transform = function(x) sqrt(x),
    breaks = function(rng) scales::breaks_extended()(rng),
    format = function(b) scales::label_number()(b)
  ),
  # Reverse keeps the data mapping as identity and instead flips the trained
  # domain (a decreasing native domain is what vellum reads as a reversed axis).
  # Negating the data here as well would double-flip and cancel out.
  reverse = list(
    transform = function(x) x,
    breaks = function(rng) scales::breaks_extended()(rng),
    format = function(b) scales::label_number()(b),
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
    format = tr$format %||% function(b) scales::label_number()(b),
    flip = FALSE
  )
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

# Default shapes a mapped `shape` aesthetic cycles through.
.SHAPE_PALETTE <- c("circle", "square", "triangle", "diamond", "plus", "cross")
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
    as.numeric(user_lim)
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
    type <- "continuous"
  } else {
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
    keep <- is.finite(braw) & braw >= min(rng) & braw <= max(rng)
    braw <- braw[keep]
    if (!is.null(lab)) {
      lab <- lab[keep]
    }
    breaks <- tr$transform(braw)
    labels <- lab %||% tr$format(braw)
    type <- if (!is.null(scalespec) && identical(scalespec@type, "log10")) {
      "log10"
    } else {
      "continuous"
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
    name = name
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
    name = name
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
    name = name
  )
}

# Train the colour scale (if any layer maps `color`/`fill`). Returns NULL when
# colour is not mapped, else a trained-scale list carrying a values->colour
# mapping plus the data the legend needs.
.train_colour <- function(spec, resolved) {
  values <- .pool_values(resolved, "color")
  if (is.null(values)) {
    values <- .pool_values(resolved, "fill")
  }
  if (is.null(values)) {
    return(NULL)
  }

  scalespec <- .scale_for(spec, "color")
  # channel type from the first layer mapping colour
  chan_type <- NULL
  for (L in resolved) {
    chan_type <- L$types[["color"]] %||% L$types[["fill"]]
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
  title <- .scale_title(scalespec, .default_title(spec, "color"))

  pal <- if (!is.null(scalespec)) scalespec@palette else NULL
  # No explicit palette? A theme may supply a default (e.g. theme_cyberpunk()'s
  # neon set): `palette` for discrete, `palette.continuous` for continuous/binned.
  if (is.null(pal)) {
    pal <- .theme_palette(spec, kind)
  }
  user_breaks <- if (!is.null(scalespec)) scalespec@breaks else NULL
  user_labels <- if (!is.null(scalespec)) scalespec@labels else NULL

  # Colour for NA-valued features (choropleth "no data"), and whether any pooled
  # value was actually NA (so the guide shows a distinct NA swatch).
  na_value <- .colour_na_value(spec, scalespec, resolved)
  has_na <- any(is.na(unlist(values, use.names = FALSE)))
  key_glyph <- .key_glyph_for_aes(resolved, "color")

  # An identity scale uses the data values verbatim as colours (they must already
  # be valid R/CSS colours) and draws no legend.
  if (identical(kind, "identity")) {
    return(list(
      kind = "identity",
      map = function(x) as.character(x),
      name = title,
      no_guide = TRUE,
      na = FALSE,
      na_value = na_value,
      key_glyph = key_glyph
    ))
  }

  if (identical(kind, "binned")) {
    v <- as.numeric(unlist(values, use.names = FALSE))
    v <- v[is.finite(v)]
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
    cols <- grDevices::colorRampPalette(.continuous_stops(pal))(k)
    map <- function(x) {
      i <- findInterval(x, brks, rightmost.closed = TRUE, all.inside = TRUE)
      out <- cols[i]
      out[!is.finite(x)] <- na_value
      out
    }
    labels <- .match_labels(user_labels, seq_len(k), "color") %||%
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
    rng <- if (length(v)) range(v) else c(0, 1)
    if (diff(rng) == 0) {
      rng <- rng + c(-0.5, 0.5)
    }
    pal256 <- grDevices::colorRampPalette(.continuous_stops(pal))(256)
    # Quantize to <=256 bins so the mark style-grouping stays bounded.
    bin <- function(x) {
      i <- floor(scales::rescale(x, to = c(0, 255), from = rng)) + 1L
      pmin(pmax(i, 1L), 256L)
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
    llab <- .match_labels(user_labels, lbrk, "color")
    keep <- is.finite(lbrk) & lbrk >= rng[1] & lbrk <= rng[2]
    lbrk <- lbrk[keep]
    if (!is.null(llab)) {
      llab <- llab[keep]
    }
    list(
      kind = "continuous",
      map = map,
      name = title,
      range = rng,
      pal256 = pal256,
      legend_breaks = lbrk,
      legend_labels = llab %||% scales::label_number()(lbrk),
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
    labels <- .match_labels(user_labels, levels, "color") %||% levels
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

# Train the size scale (if any layer maps `size` to data). Maps values to a
# point-size range in mm and carries representative breaks for a size legend.
.train_size <- function(spec, resolved) {
  values <- .pool_values(resolved, "size")
  if (is.null(values)) {
    return(NULL)
  }
  scalespec <- .scale_for(spec, "size")
  # An identity size scale uses the data values verbatim as point sizes (mm).
  if (!is.null(scalespec) && identical(scalespec@type, "identity")) {
    return(list(
      kind = "size",
      map = function(x) as.numeric(x),
      name = .default_title(spec, "size"),
      no_guide = TRUE
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
  out_range <- if (!is.null(scalespec) && !is.null(scalespec@range)) {
    as.numeric(scalespec@range)
  } else {
    .SIZE_RANGE
  }
  map <- function(x) scales::rescale(x, to = out_range, from = rng)
  name <- .scale_title(scalespec, .default_title(spec, "size"))
  lbrk <- if (!is.null(scalespec) && !is.null(scalespec@breaks)) {
    as.numeric(scalespec@breaks)
  } else {
    scales::breaks_extended()(rng)
  }
  llab <- .match_labels(
    if (!is.null(scalespec)) scalespec@labels else NULL,
    lbrk,
    "size"
  )
  keep <- is.finite(lbrk) & lbrk >= rng[1] & lbrk <= rng[2]
  lbrk <- lbrk[keep]
  if (!is.null(llab)) {
    llab <- llab[keep]
  }
  list(
    kind = "size",
    map = map,
    name = name,
    range = rng,
    na = has_na,
    legend_breaks = lbrk,
    legend_sizes = map(lbrk),
    legend_labels = llab %||% scales::label_number()(lbrk)
  )
}

# Edge-width output range (lwd multiples) a mapped edge `linewidth` spans.
.EDGE_WIDTH_RANGE <- c(0.3, 3)

# Train the edge-width scale (if any edge layer maps `linewidth` to data). Mirrors
# the size scale: rescale the data range to an lwd range; emits its own legend.
.train_edge_width <- function(spec, resolved) {
  values <- .pool_values(resolved, "linewidth")
  if (is.null(values)) {
    return(NULL)
  }
  scalespec <- .scale_for(spec, "edge_width")
  v <- as.numeric(unlist(values, use.names = FALSE))
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
    .EDGE_WIDTH_RANGE
  }
  map <- function(x) scales::rescale(x, to = out_range, from = rng)
  name <- .scale_title(scalespec, .default_title(spec, "linewidth"))
  lbrk <- if (!is.null(scalespec) && !is.null(scalespec@breaks)) {
    as.numeric(scalespec@breaks)
  } else {
    scales::breaks_extended()(rng)
  }
  llab <- .match_labels(
    if (!is.null(scalespec)) scalespec@labels else NULL,
    lbrk,
    "edge_width"
  )
  keep <- is.finite(lbrk) & lbrk >= rng[1] & lbrk <= rng[2]
  lbrk <- lbrk[keep]
  if (!is.null(llab)) {
    llab <- llab[keep]
  }
  list(
    kind = "edge_width",
    map = map,
    name = name,
    range = rng,
    legend_breaks = lbrk,
    legend_widths = map(lbrk),
    legend_labels = llab %||% scales::label_number()(lbrk)
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
    return(list(
      kind = "shape",
      map = function(x) as.character(x),
      name = .default_title(spec, "shape"),
      no_guide = TRUE
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
    cli::cli_abort(c(
      "Not enough shapes for {length(levels)} {.field shape} level{?s} ({length(pal)} available).",
      i = "Supply at least {length(levels)} shapes via {.arg scale_shape(values=)}."
    ))
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
  values <- .pool_values(resolved, "alpha")
  if (is.null(values)) {
    return(NULL)
  }
  scalespec <- .scale_for(spec, "alpha")
  if (!is.null(scalespec) && identical(scalespec@type, "identity")) {
    return(list(
      kind = "alpha",
      map = function(x) as.numeric(x),
      name = .default_title(spec, "alpha"),
      no_guide = TRUE
    ))
  }
  v <- as.numeric(unlist(values, use.names = FALSE))
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
    .ALPHA_RANGE
  }
  map <- function(x) scales::rescale(x, to = out_range, from = rng)
  name <- .scale_title(scalespec, .default_title(spec, "alpha"))
  lbrk <- if (!is.null(scalespec) && !is.null(scalespec@breaks)) {
    as.numeric(scalespec@breaks)
  } else {
    scales::breaks_extended()(rng)
  }
  keep <- is.finite(lbrk) & lbrk >= rng[1] & lbrk <= rng[2]
  lbrk <- lbrk[keep]
  list(
    kind = "alpha",
    map = map,
    name = name,
    range = rng,
    legend_breaks = lbrk,
    legend_alphas = map(lbrk),
    legend_labels = scales::label_number()(lbrk)
  )
}

# Train the linetype scale (if any layer maps `linetype` to data). Always
# discrete: levels cycle through `.LINETYPE_PALETTE` (or a user palette).
.train_linetype <- function(spec, resolved) {
  values <- .pool_values(resolved, "linetype")
  if (is.null(values)) {
    return(NULL)
  }
  scalespec <- .scale_for(spec, "linetype")
  if (!is.null(scalespec) && identical(scalespec@type, "identity")) {
    return(list(
      kind = "linetype",
      map = function(x) as.character(x),
      name = .default_title(spec, "linetype"),
      no_guide = TRUE
    ))
  }
  levels <- .cat_levels(values)
  pal <- if (!is.null(scalespec) && !is.null(scalespec@palette)) {
    scalespec@palette
  } else {
    .LINETYPE_PALETTE
  }
  if (length(levels) > length(pal)) {
    cli::cli_abort(c(
      "Not enough line types for {length(levels)} {.field linetype} level{?s} ({length(pal)} available).",
      i = "Supply at least {length(levels)} line types via {.arg scale_linetype(values=)}."
    ))
  }
  ltys <- rep_len(pal, length(levels))
  names(ltys) <- levels
  name <- .scale_title(scalespec, .default_title(spec, "linetype"))
  list(
    kind = "linetype",
    map = function(x) unname(ltys[as.character(x)]),
    name = name,
    levels = levels,
    linetypes = unname(ltys)
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
    hw <- 0.4 * band
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
    scale_h <- (L$stat_params$scale %||% 1.4) * band
    dens <- .density_by_cat(xv, yv, levs, adjust)
    supp <- unlist(lapply(dens, function(d) if (!is.null(d)) range(d$x)))
    list(
      x = if (length(supp)) range(supp) else NULL,
      y = c(min(ypos), max(ypos) + scale_h)
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
    if (!L$mark %in% c("violin", "ridgeline")) {
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
    edge_width = .train_edge_width(spec, resolved),
    alpha = .train_alpha(spec, resolved),
    linetype = .train_linetype(spec, resolved)
  )
  # Apply per-scale guide overrides (guides(): guide = "none" / guide_legend()).
  for (aes in c("color", "size", "shape", "edge_width", "alpha", "linetype")) {
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
