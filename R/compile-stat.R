#' @include classes.R compile-resolve.R
NULL

# The optional grouping value for a layer (a mapped colour/fill), or NULL.
.layer_group <- function(L) L$values$color %||% L$values$fill

# Apply a layer's statistical transform, producing new `values` (and evaluating
# any `after_stat()` channels against the stat output). The identity stat is a
# no-op; a bar with no `y` defaults to the count stat.
.apply_stat <- function(L) {
  stat <- L$stat
  if (identical(L$mark, "bar") && stat == "identity" && is.null(L$values$y)) {
    stat <- "count"
  }
  if (stat == "identity") {
    if (length(L$after)) {
      cli::cli_abort(
        "{.fn after_stat} needs a statistical mark (e.g. {.fn mark_histogram})."
      )
    }
    return(L)
  }
  sdf <- switch(
    stat,
    count = .stat_count(L),
    bin = .stat_bin(L),
    smooth = .stat_smooth(L),
    cli::cli_abort("Unknown stat {.val {stat}}.")
  )
  .merge_stat(L, sdf)
}

# Fold a stat's output data frame back into the layer. After a stat, the layer's
# data IS the stat output: x (and y) come from it, raw stage-1 inputs are
# replaced. `after_stat()` channels (evaluated against the stat output) take
# precedence over the stat's default y.
.merge_stat <- function(L, sdf) {
  after_names <- names(L$after)
  for (nm in after_names) {
    L$values[[nm]] <- rlang::eval_tidy(L$after[[nm]], data = sdf)
    L$types[[nm]] <- .infer_type(L$values[[nm]])
  }
  L$values$x <- sdf$x
  if (!is.null(sdf$group)) {
    if (!is.null(L$values$color)) {
      L$values$color <- sdf$group
    } else {
      L$values$fill <- sdf$group
    }
  }
  if (!("y" %in% after_names) && !is.null(sdf$y)) {
    L$values$y <- sdf$y
    L$types$y <- "quantitative"
  }
  if (!is.null(sdf$ymin)) {
    L$values$ymin <- sdf$ymin
    L$values$ymax <- sdf$ymax
  }
  if (!is.null(sdf$width)) {
    L$values$width <- sdf$width
  }
  L$n <- nrow(sdf)
  L
}

# Count rows per x (per x and group, if a colour/fill is mapped). x (and the
# group) are kept as factors so a custom factor level order survives into the
# trained position/colour scales rather than being re-sorted alphabetically.
.stat_count <- function(L) {
  xlevs <- .cat_levels(L$values$x)
  x <- factor(as.character(L$values$x), levels = xlevs)
  grp <- .layer_group(L)
  if (is.null(grp)) {
    t <- table(x)
    t <- t[t > 0]
    n <- as.numeric(t)
    data.frame(
      x = factor(names(t), levels = xlevs),
      count = n,
      y = n,
      density = n / sum(n),
      stringsAsFactors = FALSE
    )
  } else {
    glevs <- .cat_levels(grp)
    g <- factor(as.character(grp), levels = glevs)
    agg <- as.data.frame(table(x = x, group = g), stringsAsFactors = FALSE)
    agg <- agg[agg$Freq > 0, , drop = FALSE]
    data.frame(
      x = factor(agg$x, levels = xlevs),
      group = factor(agg$group, levels = glevs),
      count = agg$Freq,
      y = agg$Freq,
      density = agg$Freq / sum(agg$Freq),
      stringsAsFactors = FALSE
    )
  }
}

# Bin a continuous x into `bins` equal-width bins; count per bin (per group).
.stat_bin <- function(L) {
  x <- as.numeric(L$values$x)
  ok <- is.finite(x)
  x <- x[ok]
  bins <- L$stat_params$bins %||% 30L
  rng <- range(x)
  if (diff(rng) == 0) {
    rng <- rng + c(-0.5, 0.5)
  }
  breaks <- seq(rng[1], rng[2], length.out = bins + 1L)
  width <- diff(breaks)[1]
  centers <- (utils::head(breaks, -1L) + breaks[-1L]) / 2
  idx <- findInterval(x, breaks, rightmost.closed = TRUE, all.inside = TRUE)
  grp <- .layer_group(L)
  if (is.null(grp)) {
    count <- tabulate(idx, nbins = bins)
    total <- sum(count)
    data.frame(
      x = centers,
      count = count,
      y = count,
      width = width,
      density = count / (total * width)
    )
  } else {
    glevs <- .cat_levels(grp)
    g <- factor(as.character(grp)[ok], levels = glevs)
    tab <- table(factor(idx, levels = seq_len(bins)), g)
    out <- expand.grid(
      bin = seq_len(bins),
      group = colnames(tab),
      KEEP.OUT.ATTRS = FALSE,
      stringsAsFactors = FALSE
    )
    out$count <- as.numeric(tab[cbind(
      out$bin,
      match(out$group, colnames(tab))
    )])
    total <- sum(out$count)
    data.frame(
      x = centers[out$bin],
      group = factor(out$group, levels = glevs),
      count = out$count,
      y = out$count,
      width = width,
      density = out$count / (total * width)
    )
  }
}

# Fit y ~ x (per group) and predict on a dense grid, with a confidence ribbon.
.stat_smooth <- function(L) {
  method <- L$stat_params$method %||% "lm"
  if (!identical(method, "lm")) {
    cli::cli_abort(
      "Only {.val lm} smoothing is available; got {.val {method}}."
    )
  }
  se <- isTRUE(L$stat_params$se %||% TRUE)
  level <- L$stat_params$level %||% 0.95
  x <- as.numeric(L$values$x)
  y <- as.numeric(L$values$y)
  grp <- .layer_group(L)
  groups <- if (is.null(grp)) {
    list(all = seq_along(x))
  } else {
    split(seq_along(x), as.character(grp))
  }

  parts <- lapply(names(groups), function(gn) {
    i <- groups[[gn]]
    fit <- stats::lm(y ~ x, data = data.frame(x = x[i], y = y[i]))
    xg <- seq(min(x[i]), max(x[i]), length.out = 80)
    pr <- stats::predict(fit, newdata = data.frame(x = xg), se.fit = se)
    fitv <- if (se) pr$fit else pr
    df <- data.frame(x = xg, y = fitv)
    if (se) {
      t <- stats::qt(1 - (1 - level) / 2, fit$df.residual)
      df$ymin <- fitv - t * pr$se.fit
      df$ymax <- fitv + t * pr$se.fit
    }
    if (!is.null(grp)) {
      df$group <- gn
    }
    df
  })
  do.call(rbind, parts)
}
