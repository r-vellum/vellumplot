#' @include classes.R compile-resolve.R
NULL

# Default perceptual ramp for continuous colour and qualitative palette for
# discrete colour.
.viridis <- function(n = 256) grDevices::hcl.colors(n, "viridis")
.qual_palette <- function(k) grDevices::hcl.colors(k, "Dark 3")

# Train a continuous position scale. `values` is a list of vectors (one per
# layer), which may be numeric or temporal (Date/POSIXct). Returns a trained-
# scale list: the expanded native domain (for the viewport scale), break
# positions in native (transformed) units, their labels, and a data->native
# mapping function. Temporal axes map to numeric and get date-aware breaks.
.train_position <- function(aesthetic, values, scalespec, title) {
  is_log <- !is.null(scalespec) && identical(scalespec@type, "log10")
  raw <- do.call(c, values) # combine across layers, preserving class
  is_time <- inherits(raw, c("Date", "POSIXct"))
  tfun <- if (is_log) function(v) log10(as.numeric(v)) else function(v) as.numeric(v)

  num <- as.numeric(raw)
  num <- num[is.finite(num)]
  user_lim <- if (!is.null(scalespec)) scalespec@domain else NULL
  rng <- if (!is.null(user_lim)) as.numeric(user_lim) else range(num)
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

  name <- if (!is.null(scalespec) && !is.null(scalespec@name)) scalespec@name else title
  list(
    aesthetic = aesthetic, type = if (is_log) "log10" else "continuous",
    data_range = rng, domain = domain,
    breaks = breaks, labels = labels, map = tfun, name = name
  )
}

# Train the colour scale (if any layer maps `color`/`fill`). Returns NULL when
# colour is not mapped, else a trained-scale list carrying a values->colour
# mapping plus the data the legend needs.
.train_colour <- function(spec, resolved) {
  values <- .pool_values(resolved, "color")
  if (is.null(values)) values <- .pool_values(resolved, "fill")
  if (is.null(values)) return(NULL)

  scalespec <- .scale_for(spec, "color")
  # channel type from the first layer mapping colour
  chan_type <- NULL
  for (L in resolved) {
    chan_type <- L$types[["color"]] %||% L$types[["fill"]]
    if (!is.null(chan_type)) break
  }
  kind <- if (!is.null(scalespec)) scalespec@type else
    if (identical(chan_type, "quantitative")) "continuous" else "discrete"
  title <- if (!is.null(scalespec) && !is.null(scalespec@name)) scalespec@name else
    .default_title(spec, "color")

  if (identical(kind, "continuous")) {
    v <- as.numeric(unlist(values, use.names = FALSE))
    rng <- range(v[is.finite(v)])
    if (diff(rng) == 0) rng <- rng + c(-0.5, 0.5)
    pal <- if (!is.null(scalespec) && !is.null(scalespec@palette)) scalespec@palette else .viridis()
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
    list(kind = "continuous", map = map, name = title, range = rng,
         pal256 = pal256, legend_breaks = lbrk,
         legend_labels = scales::label_number()(lbrk))
  } else {
    f <- unlist(lapply(values, as.character), use.names = FALSE)
    levels <- if (is.factor(values[[1]])) levels(values[[1]]) else sort(unique(f))
    cols <- if (!is.null(scalespec) && !is.null(scalespec@palette)) {
      rep_len(scalespec@palette, length(levels))
    } else {
      .qual_palette(length(levels))
    }
    names(cols) <- levels
    map <- function(x) unname(cols[as.character(x)])
    list(kind = "discrete", map = map, name = title,
         levels = levels, colors = unname(cols))
  }
}

# Pool numeric values feeding a position axis: the channel itself plus, for
# rule layers, the matching intercept (which may be a constant in `params`).
.axis_pool <- function(resolved, channel, intercept) {
  vs <- list()
  for (L in resolved) {
    v <- L$values[[channel]] %||% L$values[[intercept]] %||% L$params[[intercept]]
    if (!is.null(v)) vs <- c(vs, list(v))
  }
  if (length(vs)) vs else NULL
}

# Train all scales the plot needs: x, y (position) and colour.
.train_scales <- function(spec, resolved) {
  xs <- .axis_pool(resolved, "x", "xintercept")
  ys <- .axis_pool(resolved, "y", "yintercept")
  if (is.null(xs) || is.null(ys)) {
    cli::cli_abort("Every layer needs an {.field x} and {.field y} encoding (v1).")
  }
  list(
    x = .train_position("x", xs, .scale_for(spec, "x"), .default_title(spec, "x")),
    y = .train_position("y", ys, .scale_for(spec, "y"), .default_title(spec, "y")),
    color = .train_colour(spec, resolved)
  )
}
