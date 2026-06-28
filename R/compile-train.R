#' @include classes.R compile-resolve.R
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
  include_zero = FALSE
) {
  name <- if (!is.null(scalespec) && !is.null(scalespec@name)) {
    scalespec@name
  } else {
    title
  }
  raw <- do.call(c, values) # combine across layers, preserving class
  if (is.numeric(raw) || inherits(raw, c("Date", "POSIXct"))) {
    .train_position_continuous(aesthetic, raw, scalespec, name, include_zero)
  } else {
    .train_position_discrete(aesthetic, values, name)
  }
}

.train_position_continuous <- function(
  aesthetic,
  raw,
  scalespec,
  name,
  include_zero
) {
  is_log <- !is.null(scalespec) && identical(scalespec@type, "log10")
  is_time <- inherits(raw, c("Date", "POSIXct"))
  tfun <- if (is_log) {
    function(v) log10(as.numeric(v))
  } else {
    function(v) as.numeric(v)
  }

  num <- as.numeric(raw)
  num <- num[is.finite(num)]
  user_lim <- if (!is.null(scalespec)) scalespec@domain else NULL
  rng <- if (!is.null(user_lim)) as.numeric(user_lim) else range(num)
  if (include_zero) {
    rng <- range(c(rng, 0))
  }
  if (!all(is.finite(rng)) || diff(rng) == 0) {
    centre <- if (all(is.finite(rng))) rng[1] else 0
    rng <- centre + c(-0.5, 0.5)
  }

  trng <- if (is_log) log10(rng) else rng
  domain <- scales::expand_range(trng, mul = 0.05)

  if (is_time) {
    braw <- pretty(raw)
    braw <- braw[as.numeric(braw) >= rng[1] & as.numeric(braw) <= rng[2]]
    breaks <- as.numeric(braw)
    labels <- format(braw)
  } else if (is_log) {
    braw <- scales::breaks_log()(rng)
    braw <- braw[is.finite(braw) & braw >= rng[1] & braw <= rng[2]]
    breaks <- log10(braw)
    labels <- format(braw, trim = TRUE, scientific = FALSE)
  } else {
    braw <- scales::breaks_extended()(rng)
    braw <- braw[is.finite(braw) & braw >= rng[1] & braw <= rng[2]]
    breaks <- braw
    labels <- scales::label_number()(braw)
  }

  list(
    aesthetic = aesthetic,
    type = if (is_log) "log10" else "continuous",
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
# occupying a unit-width band; the domain pads half a band each side.
.train_position_discrete <- function(aesthetic, values, name) {
  levs <- .cat_levels(values)
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

  if (identical(kind, "continuous")) {
    v <- as.numeric(unlist(values, use.names = FALSE))
    rng <- range(v[is.finite(v)])
    if (diff(rng) == 0) {
      rng <- rng + c(-0.5, 0.5)
    }
    pal <- if (!is.null(scalespec) && !is.null(scalespec@palette)) {
      scalespec@palette
    } else {
      .viridis()
    }
    pal256 <- grDevices::colorRampPalette(pal)(256)
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
    lbrk <- scales::breaks_extended()(rng)
    lbrk <- lbrk[is.finite(lbrk) & lbrk >= rng[1] & lbrk <= rng[2]]
    list(
      kind = "continuous",
      map = map,
      name = title,
      range = rng,
      pal256 = pal256,
      legend_breaks = lbrk,
      legend_labels = scales::label_number()(lbrk)
    )
  } else {
    levels <- .cat_levels(values)
    cols <- if (!is.null(scalespec) && !is.null(scalespec@palette)) {
      rep_len(scalespec@palette, length(levels))
    } else {
      .qual_palette(length(levels))
    }
    names(cols) <- levels
    map <- function(x) unname(cols[as.character(x)])
    list(
      kind = "discrete",
      map = map,
      name = title,
      levels = levels,
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
  v <- as.numeric(unlist(values, use.names = FALSE))
  rng <- range(v[is.finite(v)])
  if (diff(rng) == 0) {
    rng <- rng + c(-0.5, 0.5)
  }
  map <- function(x) scales::rescale(x, to = .SIZE_RANGE, from = rng)
  lbrk <- scales::breaks_extended()(rng)
  lbrk <- lbrk[is.finite(lbrk) & lbrk >= rng[1] & lbrk <= rng[2]]
  list(
    kind = "size",
    map = map,
    name = .default_title(spec, "size"),
    range = rng,
    legend_breaks = lbrk,
    legend_sizes = map(lbrk),
    legend_labels = scales::label_number()(lbrk)
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
  has_bar <- .has_bar(resolved)
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
      .default_title(spec, "x")
    ),
    y = .train_position(
      "y",
      ys,
      .scale_for(spec, "y"),
      .y_axis_title(spec, resolved),
      include_zero = has_bar
    ),
    color = .train_colour(spec, resolved),
    size = .train_size(spec, resolved)
  )
}
