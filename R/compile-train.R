#' @include classes.R coord.R compile-resolve.R
NULL

# Default perceptual ramp for continuous colour and qualitative palette for
# discrete colour.
.viridis <- function(n = 256) grDevices::hcl.colors(n, "viridis")

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
# stays byte-identical to v1.
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
  reverse = list(
    transform = function(x) -x,
    breaks = function(rng) scales::breaks_extended()(rng),
    format = function(b) scales::label_number()(b)
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
  # a scales transform object
  list(
    transform = tr$transform,
    breaks = tr$breaks %||% function(rng) scales::breaks_extended()(rng),
    format = tr$format %||% function(b) scales::label_number()(b)
  )
}

# Normalise a palette name for case/space/punctuation-insensitive matching
# against grDevices::hcl.pals().
.norm_pal <- function(s) tolower(gsub("[ ._-]", "", s))
.is_hcl_pal <- function(x) {
  is.character(x) &&
    length(x) == 1L &&
    .norm_pal(x) %in% .norm_pal(grDevices::hcl.pals())
}
.hcl_full <- function(x) {
  grDevices::hcl.pals()[match(.norm_pal(x), .norm_pal(grDevices::hcl.pals()))]
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

# Continuous-colour ramp stops: NULL -> viridis default; an hcl.pals() name ->
# that palette; else a user colour vector.
.continuous_stops <- function(palette) {
  if (is.null(palette)) {
    return(.viridis())
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
  name <- if (!is.null(scalespec) && !is.null(scalespec@name)) {
    scalespec@name
  } else {
    title
  }
  raw <- do.call(c, values) # combine across layers, preserving class
  if (is.numeric(raw) || inherits(raw, c("Date", "POSIXct"))) {
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

.train_position_continuous <- function(
  aesthetic,
  raw,
  scalespec,
  name,
  include_zero,
  lim = NULL
) {
  is_time <- inherits(raw, c("Date", "POSIXct"))

  num <- as.numeric(raw)
  num <- num[is.finite(num)]
  user_lim <- lim %||% (if (!is.null(scalespec)) scalespec@domain else NULL)
  rng <- if (!is.null(user_lim)) as.numeric(user_lim) else range(num)
  if (include_zero) {
    rng <- range(c(rng, 0))
  }
  if (!all(is.finite(rng)) || diff(rng) == 0) {
    centre <- if (all(is.finite(rng))) rng[1] else 0
    rng <- centre + c(-0.5, 0.5)
  }

  user_breaks <- if (!is.null(scalespec)) scalespec@breaks else NULL
  user_labels <- if (!is.null(scalespec)) scalespec@labels else NULL

  if (is_time) {
    # Dates/times keep their dedicated pretty()/format() path.
    tfun <- function(v) as.numeric(v)
    domain <- scales::expand_range(rng, mul = 0.05)
    braw <- if (!is.null(user_breaks)) user_breaks else pretty(raw)
    keep <- as.numeric(braw) >= rng[1] & as.numeric(braw) <= rng[2]
    braw <- braw[keep]
    breaks <- as.numeric(braw)
    labels <- if (!is.null(user_labels)) {
      rep_len(user_labels, length(braw))
    } else {
      format(braw)
    }
    type <- "continuous"
  } else {
    tr <- .resolve_trans(scalespec)
    tfun <- function(v) tr$transform(as.numeric(v))
    domain <- scales::expand_range(range(tr$transform(rng)), mul = 0.05)
    braw <- if (!is.null(user_breaks)) {
      as.numeric(user_breaks)
    } else {
      tr$breaks(rng)
    }
    keep <- is.finite(braw) & braw >= min(rng) & braw <= max(rng)
    lab <- if (!is.null(user_labels)) {
      rep_len(user_labels, length(braw))
    } else {
      NULL
    }
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
  kind <- if (!is.null(scalespec)) {
    scalespec@type
  } else if (identical(chan_type, "quantitative")) {
    "continuous"
  } else {
    "discrete"
  }
  title <- if (!is.null(scalespec) && !is.null(scalespec@name)) {
    scalespec@name
  } else {
    .default_title(spec, "color")
  }

  pal <- if (!is.null(scalespec)) scalespec@palette else NULL
  user_breaks <- if (!is.null(scalespec)) scalespec@breaks else NULL
  user_labels <- if (!is.null(scalespec)) scalespec@labels else NULL

  if (identical(kind, "continuous")) {
    v <- as.numeric(unlist(values, use.names = FALSE))
    rng <- range(v[is.finite(v)])
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
      out[!is.finite(x)] <- NA
      out
    }
    lbrk <- if (!is.null(user_breaks)) {
      as.numeric(user_breaks)
    } else {
      scales::breaks_extended()(rng)
    }
    keep <- is.finite(lbrk) & lbrk >= rng[1] & lbrk <= rng[2]
    llab <- if (!is.null(user_labels)) {
      rep_len(user_labels, length(lbrk))
    } else {
      NULL
    }
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
      legend_labels = llab %||% scales::label_number()(lbrk)
    )
  } else {
    levels <- .cat_levels(values)
    cols <- .discrete_colours(pal, levels)
    names(cols) <- levels
    map <- function(x) unname(cols[as.character(x)])
    list(
      kind = "discrete",
      map = map,
      name = title,
      levels = levels,
      labels = user_labels %||% levels,
      colors = unname(cols)
    )
  }
}

# Train the size scale (if any layer maps `size` to data). Maps values to a
# point-size range in mm and carries representative breaks for a size legend.
.train_size <- function(spec, resolved) {
  values <- .pool_values(resolved, "size")
  if (is.null(values)) {
    return(NULL)
  }
  scalespec <- .scale_for(spec, "size")
  v <- as.numeric(unlist(values, use.names = FALSE))
  rng <- if (!is.null(scalespec) && !is.null(scalespec@domain)) {
    as.numeric(scalespec@domain)
  } else {
    range(v[is.finite(v)])
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
  name <- if (!is.null(scalespec) && !is.null(scalespec@name)) {
    scalespec@name
  } else {
    .default_title(spec, "size")
  }
  lbrk <- if (!is.null(scalespec) && !is.null(scalespec@breaks)) {
    as.numeric(scalespec@breaks)
  } else {
    scales::breaks_extended()(rng)
  }
  keep <- is.finite(lbrk) & lbrk >= rng[1] & lbrk <= rng[2]
  llab <- if (!is.null(scalespec) && !is.null(scalespec@labels)) {
    rep_len(scalespec@labels, length(lbrk))
  } else {
    NULL
  }
  lbrk <- lbrk[keep]
  if (!is.null(llab)) {
    llab <- llab[keep]
  }
  list(
    kind = "size",
    map = map,
    name = name,
    range = rng,
    legend_breaks = lbrk,
    legend_sizes = map(lbrk),
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
  levels <- .cat_levels(values)
  pal <- if (!is.null(scalespec) && !is.null(scalespec@palette)) {
    scalespec@palette
  } else {
    .SHAPE_PALETTE
  }
  shapes <- rep_len(pal, length(levels))
  names(shapes) <- levels
  name <- if (!is.null(scalespec) && !is.null(scalespec@name)) {
    scalespec@name
  } else {
    .default_title(spec, "shape")
  }
  list(
    kind = "shape",
    map = function(x) unname(shapes[as.character(x)]),
    name = name,
    levels = levels,
    shapes = unname(shapes)
  )
}

# Pool numeric values feeding a position axis: the channel itself plus, for
# rule layers, the matching intercept (which may be a constant in `params`), and
# any `<channel>min`/`<channel>max` extent (e.g. a smooth's confidence ribbon).
.axis_pool <- function(resolved, channel, intercept) {
  vs <- list()
  for (L in resolved) {
    v <- L$values[[channel]] %||%
      L$values[[intercept]] %||%
      L$params[[intercept]]
    if (!is.null(v)) {
      vs <- c(vs, list(v))
    }
    lo <- L$values[[paste0(channel, "min")]]
    hi <- L$values[[paste0(channel, "max")]]
    if (!is.null(lo)) vs <- c(vs, list(lo, hi))
  }
  if (length(vs)) vs else NULL
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
      "Every layer needs an {.field x} and {.field y} encoding (v1)."
    )
  }
  list(
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
    shape = .train_shape(spec, resolved)
  )
}
