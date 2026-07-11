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
    density_2d = .stat_density_2d(L),
    hexbin = .stat_hexbin(L),
    density = .stat_density(L),
    aggregate = .stat_aggregate(L),
    smooth = .stat_smooth(L),
    ecdf = .stat_ecdf(L),
    qq = .stat_qq(L),
    qq_line = .stat_qq_line(L),
    dotplot = .stat_dotplot(L),
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
  # Contour stats emit ordered vertices grouped into pieces (one polyline/band per
  # piece) and, for filled bands, sub-path rings; the contour emitters read these.
  if (!is.null(sdf$.piece)) {
    L$values$.piece <- sdf$.piece
    L$values$.ring <- sdf$.ring
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

# The 2-D scalar field to contour: a kernel density estimate of (x, y) (the
# default), or a supplied `z` surface reshaped from a regular (x, y) grid.
# Returns the grid coords `gx`/`gy` and a matrix `gz` with rows = y, cols = x
# (the layout isoband expects).
.contour_field <- function(L) {
  x <- as.numeric(L$values$x)
  y <- as.numeric(L$values$y)
  z <- L$values$z
  if (!is.null(z)) {
    ux <- sort(unique(x))
    uy <- sort(unique(y))
    if (length(ux) * length(uy) != length(z)) {
      cli::cli_abort(c(
        "Contouring a {.arg z} surface needs {.arg x}/{.arg y} on a complete regular grid.",
        i = "Got {length(x)} rows, but a {length(ux)} x {length(uy)} grid needs {length(ux) * length(uy)}."
      ))
    }
    m <- matrix(NA_real_, nrow = length(uy), ncol = length(ux))
    m[cbind(match(y, uy), match(x, ux))] <- as.numeric(z)
    list(gx = ux, gy = uy, gz = m)
  } else {
    .need_pkg("MASS", "2-D density contours (mark_contour())")
    ok <- is.finite(x) & is.finite(y)
    x <- x[ok]
    y <- y[ok]
    if (length(x) < 3L) {
      cli::cli_abort("A 2-D density contour needs at least 3 finite points.")
    }
    ng <- L$stat_params$n %||% 100L
    kd <- MASS::kde2d(x, y, n = ng)
    # kde2d's z is [x, y]; isoband wants [y, x], so transpose.
    list(gx = kd$x, gy = kd$y, gz = t(kd$z))
  }
}

# Contour break levels: explicit `breaks`, else a `binwidth` grid, else ~`bins`
# pretty cuts. Only levels strictly inside the field range are kept.
.contour_breaks <- function(gz, sp) {
  rng <- range(gz, na.rm = TRUE)
  br <- if (!is.null(sp$breaks)) {
    sort(unique(as.numeric(sp$breaks)))
  } else if (!is.null(sp$binwidth)) {
    seq(floor(rng[1] / sp$binwidth) * sp$binwidth, rng[2], by = sp$binwidth)
  } else {
    pretty(rng, n = sp$bins %||% 10L)
  }
  br[br > rng[1] & br < rng[2]]
}

# Flatten isoband::isolines() output (a per-level list of list(x, y, id)) into an
# ordered vertex table x/y/level/.piece, one global piece per traced line.
.isoline_df <- function(lines) {
  parts <- list()
  base <- 0L
  for (lv in names(lines)) {
    e <- lines[[lv]]
    if (!length(e$x)) {
      next
    }
    pid <- base + match(e$id, unique(e$id))
    base <- max(pid)
    parts[[length(parts) + 1L]] <- data.frame(
      x = e$x,
      y = e$y,
      level = as.numeric(lv),
      .piece = pid,
      .ring = NA_integer_
    )
  }
  if (!length(parts)) {
    return(data.frame(
      x = numeric(),
      y = numeric(),
      level = numeric(),
      .piece = integer(),
      .ring = integer()
    ))
  }
  do.call(rbind, parts)
}

# Flatten isoband::isobands() output into x/y/level/.piece/.ring: one piece per
# band (drawn as a single evenodd path so holes are cut), `.ring` the sub-path id
# within the band, `level` the band's representative value (for the fill scale).
.isoband_df <- function(bands, levels) {
  parts <- list()
  for (i in seq_along(bands)) {
    e <- bands[[i]]
    if (!length(e$x)) {
      next
    }
    parts[[length(parts) + 1L]] <- data.frame(
      x = e$x,
      y = e$y,
      level = levels[i],
      .piece = i,
      .ring = as.integer(e$id)
    )
  }
  if (!length(parts)) {
    return(data.frame(
      x = numeric(),
      y = numeric(),
      level = numeric(),
      .piece = integer(),
      .ring = integer()
    ))
  }
  do.call(rbind, parts)
}

# stat_density_2d / stat_contour: trace iso-lines (or, when `filled`, iso-bands)
# of the 2-D field with isoband. Output is an ordered vertex table (see the
# flatteners) consumed by .emit_contour / .emit_contour_filled.
.stat_density_2d <- function(L) {
  .need_pkg("isoband", "2-D contours (mark_contour())")
  fld <- .contour_field(L)
  brks <- .contour_breaks(fld$gz, L$stat_params)
  if (!length(brks)) {
    cli::cli_abort(c(
      "No contour levels fall inside the data range.",
      i = "Set {.arg breaks} or {.arg binwidth} explicitly."
    ))
  }
  if (isTRUE(L$stat_params$filled)) {
    lo <- c(-Inf, brks)
    hi <- c(brks, Inf)
    # Representative numeric level per band, increasing, for the fill scale.
    step <- if (length(brks) > 1L) stats::median(diff(brks)) else 1
    levels <- vapply(
      seq_along(lo),
      function(i) {
        if (is.finite(lo[i]) && is.finite(hi[i])) {
          (lo[i] + hi[i]) / 2
        } else if (!is.finite(lo[i])) {
          hi[i] - step / 2
        } else {
          lo[i] + step / 2
        }
      },
      numeric(1)
    )
    bands <- isoband::isobands(
      fld$gx,
      fld$gy,
      fld$gz,
      levels_low = lo,
      levels_high = hi
    )
    .isoband_df(bands, levels)
  } else {
    lines <- isoband::isolines(fld$gx, fld$gy, fld$gz, levels = brks)
    .isoline_df(lines)
  }
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

# The empirical cumulative distribution of x (per group): sorted x against the
# cumulative proportion, drawn as a right-continuous step.
.stat_ecdf <- function(L) {
  x <- as.numeric(L$values$x)
  grp <- .layer_group(L)
  g <- .stat_groups(grp, length(x))
  glevs <- g$levels
  parts <- lapply(names(g$groups), function(gn) {
    xi <- sort(x[g$groups[[gn]]])
    xi <- xi[is.finite(xi)]
    if (!length(xi)) {
      return(NULL)
    }
    n <- length(xi)
    y <- seq_len(n) / n
    df <- data.frame(x = xi, y = y, ecdf = y)
    if (!is.null(grp)) {
      df$group <- factor(gn, levels = glevs)
    }
    df
  })
  parts <- parts[!vapply(parts, is.null, logical(1))]
  if (!length(parts)) {
    cli::cli_abort("{.fn mark_ecdf} needs at least one finite value.")
  }
  do.call(rbind, parts)
}

# Sorted sample values against the theoretical quantiles of a reference
# distribution (per group): x = theoretical, y = sample.
.stat_qq <- function(L) {
  s0 <- L$values$sample %||% L$values$y %||% L$values$x
  s0 <- as.numeric(s0)
  dist <- match.fun(L$stat_params$distribution %||% "qnorm")
  grp <- .layer_group(L)
  g <- .stat_groups(grp, length(s0))
  glevs <- g$levels
  parts <- lapply(names(g$groups), function(gn) {
    s <- sort(s0[g$groups[[gn]]])
    s <- s[is.finite(s)]
    if (!length(s)) {
      return(NULL)
    }
    theo <- dist(stats::ppoints(length(s)))
    df <- data.frame(x = theo, y = s, sample = s, theoretical = theo)
    if (!is.null(grp)) {
      df$group <- factor(gn, levels = glevs)
    }
    df
  })
  parts <- parts[!vapply(parts, is.null, logical(1))]
  if (!length(parts)) {
    cli::cli_abort("{.fn mark_qq} needs at least one finite sample value.")
  }
  do.call(rbind, parts)
}

# The Q-Q reference line (per group): the line through the 1st and 3rd sample
# quartiles vs the matching theoretical quartiles, drawn across the theoretical
# range.
.stat_qq_line <- function(L) {
  s0 <- L$values$sample %||% L$values$y %||% L$values$x
  s0 <- as.numeric(s0)
  dist <- match.fun(L$stat_params$distribution %||% "qnorm")
  grp <- .layer_group(L)
  g <- .stat_groups(grp, length(s0))
  glevs <- g$levels
  parts <- lapply(names(g$groups), function(gn) {
    s <- sort(s0[g$groups[[gn]]])
    s <- s[is.finite(s)]
    if (length(s) < 2) {
      return(NULL)
    }
    theo <- dist(stats::ppoints(length(s)))
    sq <- stats::quantile(s, c(0.25, 0.75), names = FALSE)
    tq <- dist(c(0.25, 0.75))
    slope <- diff(sq) / diff(tq)
    intercept <- sq[1] - slope * tq[1]
    xr <- range(theo)
    df <- data.frame(x = xr, y = intercept + slope * xr)
    if (!is.null(grp)) {
      df$group <- factor(gn, levels = glevs)
    }
    df
  })
  parts <- parts[!vapply(parts, is.null, logical(1))]
  if (!length(parts)) {
    cli::cli_abort("{.fn mark_qq_line} needs at least 2 finite sample values.")
  }
  do.call(rbind, parts)
}

# A dot plot: bin x into equal-width bins and stack one dot per observation
# within its bin. Returns one row per observation with x = bin centre and
# y = stack height (0.5, 1.5, ...), for the point emitter.
.stat_dotplot <- function(L) {
  x <- as.numeric(L$values$x)
  x <- sort(x[is.finite(x)])
  if (!length(x)) {
    cli::cli_abort("{.fn mark_dotplot} needs at least one finite value.")
  }
  span <- diff(range(x))
  bw <- L$stat_params$binwidth %||% (if (span > 0) span / 30 else 1)
  if (!is.finite(bw) || bw <= 0) {
    bw <- 1
  }
  bin <- floor((x - min(x)) / bw)
  stack <- stats::ave(seq_along(bin), bin, FUN = seq_along)
  data.frame(
    x = min(x) + (bin + 0.5) * bw,
    y = stack - 0.5,
    count = 1
  )
}
