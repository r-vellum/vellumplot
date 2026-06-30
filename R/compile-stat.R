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
    bin2d = .stat_bin2d(L),
    hexbin = .stat_hexbin(L),
    density = .stat_density(L),
    aggregate = .stat_aggregate(L),
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
  if (!is.null(sdf$height)) {
    L$values$height <- sdf$height
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

# Bin a continuous (x, y) into a `bins x bins` grid; one row per non-empty cell
# with its centre, count, and cell width/height (for tile rendering).
.stat_bin2d <- function(L) {
  x <- as.numeric(L$values$x)
  y <- as.numeric(L$values$y)
  ok <- is.finite(x) & is.finite(y)
  x <- x[ok]
  y <- y[ok]
  bins <- L$stat_params$bins %||% 30L
  edges <- function(v) {
    rng <- range(v)
    if (diff(rng) == 0) {
      rng <- rng + c(-0.5, 0.5)
    }
    seq(rng[1], rng[2], length.out = bins + 1L)
  }
  xb <- edges(x)
  yb <- edges(y)
  xc <- (utils::head(xb, -1L) + xb[-1L]) / 2
  yc <- (utils::head(yb, -1L) + yb[-1L]) / 2
  xi <- findInterval(x, xb, rightmost.closed = TRUE, all.inside = TRUE)
  yi <- findInterval(y, yb, rightmost.closed = TRUE, all.inside = TRUE)
  tab <- table(
    factor(xi, levels = seq_len(bins)),
    factor(yi, levels = seq_len(bins))
  )
  cells <- which(tab > 0, arr.ind = TRUE)
  data.frame(
    x = xc[cells[, 1]],
    y = yc[cells[, 2]],
    count = as.numeric(tab[cells]),
    width = diff(xb)[1],
    height = diff(yb)[1]
  )
}

# Round axial hex coordinates (q, r) to the nearest hex centre via cube rounding
# (Red Blob Games). Vectorised; returns the integer axial coordinates.
.hex_round <- function(q, rax) {
  xc <- q
  zc <- rax
  yc <- -xc - zc
  rx <- round(xc)
  ry <- round(yc)
  rz <- round(zc)
  dx <- abs(rx - xc)
  dy <- abs(ry - yc)
  dz <- abs(rz - zc)
  cond_x <- dx > dy & dx > dz
  cond_z <- !cond_x & (dz > dy)
  rx[cond_x] <- -ry[cond_x] - rz[cond_x]
  rz[cond_z] <- -rx[cond_z] - ry[cond_z]
  list(q = rx, r = rz)
}

# Hexagonal 2-D binning (flat-top). Bins (x, y) into a hex lattice with ~`bins`
# columns across x; returns one row per occupied hex with its data-space centre,
# count, and circumradius `width` (in x-data units) used to size the hexagon.
# Binning is done in an isotropic space (y scaled to the x range) so hexes are
# regular there; they render geometrically exact under coord_fixed().
.stat_hexbin <- function(L) {
  x <- as.numeric(L$values$x)
  y <- as.numeric(L$values$y)
  ok <- is.finite(x) & is.finite(y)
  x <- x[ok]
  y <- y[ok]
  bins <- L$stat_params$bins %||% 30L
  xr <- range(x)
  yr <- range(y)
  if (diff(xr) == 0) {
    xr <- xr + c(-0.5, 0.5)
  }
  if (diff(yr) == 0) {
    yr <- yr + c(-0.5, 0.5)
  }
  r <- diff(xr) / (bins * 1.5) # circumradius in x-units (flat-top columns at 1.5r)
  asp <- diff(xr) / diff(yr)
  px <- x - xr[1]
  py <- (y - yr[1]) * asp # isotropic y in x-units
  ax <- .hex_round((2 / 3 * px) / r, (-1 / 3 * px + sqrt(3) / 3 * py) / r)
  key <- paste(ax$q, ax$r, sep = ",")
  tab <- table(key)
  qr <- do.call(rbind, lapply(strsplit(names(tab), ","), as.numeric))
  qi <- qr[, 1]
  ri <- qr[, 2]
  cx <- r * 1.5 * qi + xr[1]
  cy_iso <- r * sqrt(3) * (ri + qi / 2)
  cy <- cy_iso / asp + yr[1]
  # `width` is the x-data circumradius; `height` the full y-data extent of a hex
  # (the isotropic r*sqrt(3) un-scaled back to y-data units). The emitter draws
  # each hex from these so they tile data space at any panel aspect.
  data.frame(
    x = cx,
    y = cy,
    count = as.numeric(tab),
    width = r,
    height = r * sqrt(3) / asp
  )
}

# Split `n` row indices into groups, preserving factor level order (so a custom
# factor order survives into the trained position/colour scales). Returns a named
# list keyed by level, or a single "all" group when ungrouped, plus the levels.
.stat_groups <- function(grp, n) {
  if (is.null(grp)) {
    return(list(groups = list(all = seq_len(n)), levels = NULL))
  }
  glevs <- .cat_levels(grp)
  gc <- as.character(grp)
  groups <- lapply(glevs, function(g) which(gc == g))
  names(groups) <- glevs
  list(groups = groups, levels = glevs)
}

# A 1-D kernel density of x (per group): a dense (x, density) curve. Groups with
# fewer than 2 finite points are skipped with a warning (a density needs >= 2).
.stat_density <- function(L) {
  x <- as.numeric(L$values$x)
  grp <- .layer_group(L)
  g <- .stat_groups(grp, length(x))
  glevs <- g$levels
  adjust <- L$stat_params$adjust %||% 1
  parts <- lapply(names(g$groups), function(gn) {
    xi <- x[g$groups[[gn]]]
    xi <- xi[is.finite(xi)]
    if (length(xi) < 2) {
      if (!is.null(grp)) {
        cli::cli_warn(
          "Skipping {.field {gn}}: {.fn mark_density} needs at least 2 points."
        )
      }
      return(NULL)
    }
    d <- stats::density(xi, adjust = adjust)
    df <- data.frame(x = d$x, y = d$y, density = d$y)
    if (!is.null(grp)) {
      df$group <- factor(gn, levels = glevs)
    }
    df
  })
  parts <- parts[!vapply(parts, is.null, logical(1))]
  if (!length(parts)) {
    cli::cli_abort("{.fn mark_density} needs at least 2 finite points.")
  }
  do.call(rbind, parts)
}

# Summarise y per x category (per group) with `fun` (default mean).
.stat_aggregate <- function(L) {
  x <- L$values$x
  y <- as.numeric(L$values$y)
  fun <- match.fun(L$stat_params$fun %||% "mean")
  xlevs <- .cat_levels(x)
  xf <- factor(as.character(x), levels = xlevs)
  grp <- .layer_group(L)
  if (is.null(grp)) {
    yv <- tapply(y, xf, fun)
    keep <- !is.na(yv)
    data.frame(
      x = factor(xlevs[keep], levels = xlevs),
      y = as.numeric(yv[keep])
    )
  } else {
    glevs <- .cat_levels(grp)
    gf <- factor(as.character(grp), levels = glevs)
    agg <- stats::aggregate(y ~ xf + gf, FUN = fun)
    data.frame(
      x = factor(as.character(agg$xf), levels = xlevs),
      group = factor(as.character(agg$gf), levels = glevs),
      y = agg$y
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
  g <- .stat_groups(grp, length(x))
  glevs <- g$levels

  parts <- lapply(names(g$groups), function(gn) {
    i <- g$groups[[gn]]
    xi <- x[i]
    yi <- y[i]
    ok <- is.finite(xi) & is.finite(yi)
    xi <- xi[ok]
    yi <- yi[ok]
    if (length(unique(xi)) < 2) {
      if (!is.null(grp)) {
        cli::cli_warn(
          "Skipping {.field {gn}}: {.fn mark_smooth} needs at least 2 distinct x values."
        )
      }
      return(NULL)
    }
    fit <- stats::lm(y ~ x, data = data.frame(x = xi, y = yi))
    xg <- seq(min(xi), max(xi), length.out = 80)
    pr <- stats::predict(fit, newdata = data.frame(x = xg), se.fit = se)
    fitv <- if (se) pr$fit else pr
    df <- data.frame(x = xg, y = fitv)
    if (se) {
      t <- stats::qt(1 - (1 - level) / 2, fit$df.residual)
      df$ymin <- fitv - t * pr$se.fit
      df$ymax <- fitv + t * pr$se.fit
    }
    if (!is.null(grp)) {
      df$group <- factor(gn, levels = glevs)
    }
    df
  })
  parts <- parts[!vapply(parts, is.null, logical(1))]
  if (!length(parts)) {
    cli::cli_abort(
      "{.fn mark_smooth} needs at least 2 distinct x values to fit."
    )
  }
  do.call(rbind, parts)
}
