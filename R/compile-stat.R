#' @include classes.R compile-resolve.R
NULL

# The optional grouping value for a layer (a mapped colour/fill), or NULL.
# The grouping vector for a stat (colour, else fill). Recycled to the layer's row
# count so a grouping aesthetic that resolved shorter than the data (e.g. a
# constant of length 1) still aligns 1:1 when masked by `is.finite(x)` etc.,
# matching the defensive `rep_len()` in `.position_stack`.
.layer_group <- function(L) {
  g <- L$values$color %||% L$values$fill
  n <- L$n %||% length(g)
  if (!is.null(g) && length(g) >= 1L && length(g) != n) {
    g <- rep_len(g, n)
  }
  g
}

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
    ellipse = .stat_ellipse(L),
    hull = .stat_hull(L),
    window = .stat_window(L),
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
  # An `after_stat()` bound to x takes precedence over the stat's default x, just
  # as it does for y below; only fall back to `sdf$x` when x was not overridden.
  if (!("x" %in% after_names) && !is.null(sdf$x)) {
    L$values$x <- sdf$x
  }
  if (!is.null(sdf$group)) {
    # Realign *both* colour and fill to the aggregated groups. Grouping is taken
    # from colour %||% fill, so at least one matches the group; setting both keeps
    # the other aesthetic length-consistent with the reduced row count (a fill
    # left at its pre-stat, now-wrong-length vector would misalign downstream).
    if (!is.null(L$values$color)) {
      L$values$color <- sdf$group
    }
    if (!is.null(L$values$fill)) {
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
      # proportion within each group (each group sums to 1), matching ggplot2
      density = agg$Freq / stats::ave(agg$Freq, agg$group, FUN = sum),
      stringsAsFactors = FALSE
    )
  }
}

# Bin a continuous x into `bins` equal-width bins; count per bin (per group).
.stat_bin <- function(L) {
  x <- as.numeric(L$values$x)
  ok <- is.finite(x)
  x <- x[ok]
  if (!length(x)) {
    cli::cli_abort("{.fn mark_histogram} needs at least one finite value.")
  }
  bins <- L$stat_params$bins %||% 30L
  rng <- .pad_degenerate_range(range(x))
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
    # Normalise density per group (each group integrates to 1), matching
    # ggplot2 -- not by the grand total across all groups.
    gtot <- stats::ave(out$count, out$group, FUN = sum)
    data.frame(
      x = centers[out$bin],
      group = factor(out$group, levels = glevs),
      count = out$count,
      y = out$count,
      width = width,
      density = out$count / (gtot * width)
    )
  }
}

# Bin a continuous (x, y) into a `bins x bins` grid; one row per non-empty cell
# with its centre, count, and cell width/height (for tile rendering).
.stat_bin2d <- function(L) {
  xy <- .finite_xy(as.numeric(L$values$x), as.numeric(L$values$y))
  x <- xy$x
  y <- xy$y
  if (!length(x)) {
    cli::cli_abort("{.fn mark_bin2d} needs at least one finite (x, y) pair.")
  }
  bins <- L$stat_params$bins %||% 30L
  edges <- function(v) {
    rng <- .pad_degenerate_range(range(v))
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
# Returns the grid coords `gx`/`gy` and a matrix `gz` with **rows = x, cols = y**
# -- the base-R convention `vl_contour()` uses (`dim(z) == c(length(x),
# length(y))`, as `image()`/`contour()`/`outer(xs, ys, f)` do).
.contour_field <- function(L) {
  x <- as.numeric(L$values$x)
  y <- as.numeric(L$values$y)
  z <- L$values$z
  if (!is.null(z)) {
    ux <- sort(unique(x))
    uy <- sort(unique(y))
    # A complete regular grid needs the right row count AND one z per cell; a
    # count-only check passes a duplicated cell paired with a missing one, which
    # would leave a silent NA hole in the surface.
    if (
      length(ux) * length(uy) != length(z) ||
        anyDuplicated(cbind(match(y, uy), match(x, ux)))
    ) {
      cli::cli_abort(c(
        "Contouring a {.arg z} surface needs {.arg x}/{.arg y} on a complete regular grid.",
        i = "Got {length(x)} rows for a {length(ux)} x {length(uy)} grid; every (x, y) cell must appear exactly once."
      ))
    }
    m <- matrix(NA_real_, nrow = length(ux), ncol = length(uy))
    m[cbind(match(x, ux), match(y, uy))] <- as.numeric(z)
    list(gx = ux, gy = uy, gz = m)
  } else {
    .need_pkg("MASS", "2-D density contours (mark_contour())")
    xy <- .finite_xy(x, y)
    x <- xy$x
    y <- xy$y
    if (length(x) < 3L) {
      cli::cli_abort("A 2-D density contour needs at least 3 finite points.")
    }
    ng <- L$stat_params$n %||% 100L
    kd <- MASS::kde2d(x, y, n = ng)
    # kde2d's z is already [x, y] (rows = x) -- exactly what vl_contour() wants.
    list(gx = kd$x, gy = kd$y, gz = kd$z)
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

# Empty contour vertex table (the shape .emit_contour / .emit_contour_filled read).
.empty_contour_df <- function() {
  data.frame(
    x = numeric(),
    y = numeric(),
    level = numeric(),
    .piece = integer(),
    .ring = integer()
  )
}

# `vl_contour()` output -> the ordered vertex table `.emit_contour()` reads: one
# `.piece` per traced contour (chained polyline). A closed contour comes back as
# an open sequence, so its first point is repeated to close the ring.
.vl_contour_lines_df <- function(cc) {
  if (!nrow(cc)) {
    return(.empty_contour_df())
  }
  ids <- unique(cc$id)
  parts <- lapply(seq_along(ids), function(k) {
    e <- cc[cc$id == ids[k], , drop = FALSE]
    if (isTRUE(e$closed[1])) {
      e <- rbind(e, e[1L, , drop = FALSE])
    }
    data.frame(
      x = e$x,
      y = e$y,
      level = e$level[1],
      .piece = k,
      .ring = NA_integer_
    )
  })
  do.call(rbind, parts)
}

# Close an *open* contour -- one that leaves the grid, so its endpoints sit on
# the domain boundary -- into a filled ring by walking the domain rectangle
# clockwise from the contour's end point back to its start, inserting the corners
# passed along the way. `vl_contour()` orients every traced line consistently
# (the field-above-level side on the right), so the clockwise return keeps that
# region enclosed instead of cutting a straight chord across the panel (which
# leaves triangular wedges). Endpoints not on the boundary -- which should not
# occur for an open marching-squares contour -- fall back to a straight close.
.close_contour_ring <- function(ex, ey, xlim, ylim) {
  x0 <- xlim[1]
  x1 <- xlim[2]
  y0 <- ylim[1]
  y1 <- ylim[2]
  eps <- 1e-9 * max(x1 - x0, y1 - y0, 1)
  # Clockwise perimeter parameter t in [0, 4): left edge up [0, 1), top edge
  # right [1, 2), right edge down [2, 3), bottom edge left [3, 4). Corners fall
  # at the integers t = 0, 1, 2, 3.
  perim_t <- function(px, py) {
    if (abs(px - x0) <= eps) {
      return((py - y0) / (y1 - y0))
    }
    if (abs(py - y1) <= eps) {
      return(1 + (px - x0) / (x1 - x0))
    }
    if (abs(px - x1) <= eps) {
      return(2 + (y1 - py) / (y1 - y0))
    }
    if (abs(py - y0) <= eps) {
      return(3 + (x1 - px) / (x1 - x0))
    }
    NA_real_
  }
  n <- length(ex)
  t_end <- perim_t(ex[n], ey[n])
  t_start <- perim_t(ex[1], ey[1])
  if (is.na(t_end) || is.na(t_start)) {
    return(list(x = ex, y = ey))
  }
  corner_x <- c(x0, x0, x1, x1) # t = 0, 1, 2, 3
  corner_y <- c(y0, y1, y1, y0)
  span <- (t_start - t_end) %% 4 # clockwise arc length from end back to start
  off <- (c(0, 1, 2, 3) - t_end) %% 4 # each corner's clockwise offset from end
  keep <- which(off > eps & off < span - eps)
  keep <- keep[order(off[keep])]
  list(x = c(ex, corner_x[keep]), y = c(ey, corner_y[keep]))
}

# `vl_contour()` output -> filled bands. Each contour becomes a filled ring, and
# the rings are ordered by level ascending so an inner (higher) level paints over
# the outer (lower) one -- painter's order, which for the nested closed loops of
# a density gives the layered filled look without an even-odd band construction.
# An open contour (one that exits the `xlim`/`ylim` grid) is closed along the
# domain boundary so it fills the region on the field-above-level side rather
# than closing with a chord that would slice a wedge across the panel.
.vl_contour_bands_df <- function(cc, xlim, ylim) {
  if (!nrow(cc)) {
    return(.empty_contour_df())
  }
  ids <- unique(cc$id)
  pieces <- lapply(ids, function(i) cc[cc$id == i, , drop = FALSE])
  pieces <- pieces[order(vapply(pieces, function(e) e$level[1], numeric(1)))]
  parts <- lapply(seq_along(pieces), function(k) {
    e <- pieces[[k]]
    ring <- if (isTRUE(e$closed[1])) {
      list(x = e$x, y = e$y)
    } else {
      .close_contour_ring(e$x, e$y, xlim, ylim)
    }
    data.frame(
      x = ring$x,
      y = ring$y,
      level = e$level[1],
      .piece = k,
      .ring = 1L
    )
  })
  do.call(rbind, parts)
}

# stat_density_2d / stat_contour: trace iso-lines (or, when `filled`, filled
# iso-rings) of the 2-D field with the engine's marching-squares
# `vellum::vl_contour()`. Output is an ordered vertex table consumed by
# .emit_contour / .emit_contour_filled. Needs no `isoband`.
.stat_density_2d <- function(L) {
  fld <- .contour_field(L)
  brks <- .contour_breaks(fld$gz, L$stat_params)
  if (!length(brks)) {
    cli::cli_abort(c(
      "No contour levels fall inside the data range.",
      i = "Set {.arg breaks} or {.arg binwidth} explicitly."
    ))
  }
  xlim <- range(fld$gx)
  ylim <- range(fld$gy)
  cc <- vellum::vl_contour(
    fld$gz,
    levels = brks,
    xlim = xlim,
    ylim = ylim
  )
  if (isTRUE(L$stat_params$filled)) {
    .vl_contour_bands_df(cc, xlim, ylim)
  } else {
    .vl_contour_lines_df(cc)
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
  xy <- .finite_xy(as.numeric(L$values$x), as.numeric(L$values$y))
  x <- xy$x
  y <- xy$y
  if (!length(x)) {
    cli::cli_abort("{.fn mark_hex} needs at least one finite (x, y) pair.")
  }
  bins <- L$stat_params$bins %||% 30L
  xr <- .pad_degenerate_range(range(x))
  yr <- .pad_degenerate_range(range(y))
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
  # One pass in level order, instead of a which() scan per level (O(n * groups)).
  groups <- split(seq_len(n), factor(gc, levels = glevs))
  list(groups = groups, levels = glevs)
}

# The shared per-group scaffold for the grouped stats (density/ecdf/qq/qq_line/
# smooth/ellipse/hull). `compute(idx, gn, grouped, k)` gets a group's row indices,
# its level name, whether the layer is grouped, and the group's 1-based index
# (for a `.piece`); it returns that group's stat rows (without the `group`
# column) or NULL to drop the group. The scaffold splits by group, drops the
# NULLs, attaches the `group` factor, aborts with `empty_msg` when every group is
# dropped, and rbinds the survivors.
.per_group <- function(L, n, compute, empty_msg) {
  grp <- .layer_group(L)
  g <- .stat_groups(grp, n)
  glevs <- g$levels
  gnames <- names(g$groups)
  parts <- lapply(seq_along(gnames), function(k) {
    gn <- gnames[[k]]
    df <- compute(g$groups[[gn]], gn, !is.null(grp), k)
    if (is.null(df)) {
      return(NULL)
    }
    if (!is.null(grp)) {
      df$group <- factor(gn, levels = glevs)
    }
    df
  })
  parts <- parts[!vapply(parts, is.null, logical(1))]
  if (!length(parts)) {
    cli::cli_abort(empty_msg)
  }
  do.call(rbind, parts)
}

# Keep only the positions where both x and y are finite (a common pre-filter for
# the 2-D stats).
.finite_xy <- function(x, y) {
  ok <- is.finite(x) & is.finite(y)
  list(x = x[ok], y = y[ok])
}

# Expand a degenerate (zero-width) range so downstream binning has a non-zero
# span to divide.
.pad_degenerate_range <- function(rng) {
  if (diff(rng) == 0) rng + c(-0.5, 0.5) else rng
}

# A 1-D kernel density of x (per group): a dense (x, density) curve. Groups with
# fewer than 2 finite points are skipped with a warning (a density needs >= 2).
.stat_density <- function(L) {
  x <- as.numeric(L$values$x)
  adjust <- L$stat_params$adjust %||% 1
  .per_group(
    L,
    length(x),
    function(idx, gn, grouped, k) {
      xi <- x[idx]
      xi <- xi[is.finite(xi)]
      if (length(xi) < 2) {
        if (grouped) {
          cli::cli_warn(
            "Skipping {.field {gn}}: {.fn mark_density} needs at least 2 points."
          )
        }
        return(NULL)
      }
      d <- stats::density(xi, adjust = adjust)
      data.frame(x = d$x, y = d$y, density = d$y)
    },
    "{.fn mark_density} needs at least 2 finite points."
  )
}

# Warn about aggregated categories whose summary is `NA` (an empty group, or a
# summary of data containing `NA` when `fun` does not remove it -- e.g. the
# default `mean` without `na.rm = TRUE`), and return the keep mask. Both branches
# of `.stat_aggregate` use this so the grouped and ungrouped paths drop `NA`
# summaries on identical terms.
.drop_na_summary <- function(is_na) {
  if (any(is_na)) {
    cli::cli_warn(
      "Dropped {sum(is_na)} categor{?y/ies} with a missing summary value ({.code NA})."
    )
  }
  !is_na
}

# Summarise y per x category (per group) with `fun` (default mean). `fun` sees
# every value in a category (including NAs); a category whose summary is `NA` is
# dropped with a warning. The grouped path uses `na.action = na.pass` so it feeds
# `fun` the same NA-inclusive values the ungrouped `tapply` path does.
.stat_aggregate <- function(L) {
  x <- L$values$x
  y <- as.numeric(L$values$y)
  fun <- match.fun(L$stat_params$fun %||% "mean")
  xlevs <- .cat_levels(x)
  xf <- factor(as.character(x), levels = xlevs)
  grp <- .layer_group(L)
  if (is.null(grp)) {
    yv <- tapply(y, xf, fun)
    keep <- .drop_na_summary(is.na(yv))
    data.frame(
      x = factor(xlevs[keep], levels = xlevs),
      y = as.numeric(yv[keep])
    )
  } else {
    glevs <- .cat_levels(grp)
    gf <- factor(as.character(grp), levels = glevs)
    agg <- stats::aggregate(y ~ xf + gf, FUN = fun, na.action = stats::na.pass)
    keep <- .drop_na_summary(is.na(agg$y))
    data.frame(
      x = factor(as.character(agg$xf[keep]), levels = xlevs),
      group = factor(as.character(agg$gf[keep]), levels = glevs),
      y = agg$y[keep]
    )
  }
}

# Supported smoothing methods. "auto" resolves per group by point count
# (loess for small n, gam for large) -- ggplot2's rule.
.SMOOTH_METHODS <- c("auto", "lm", "loess", "glm", "gam", "rq")

# Resolve "auto" to a concrete method given a group's finite-point count.
.smooth_auto <- function(n) {
  if (n < 1000) "loess" else "gam"
}

# Fit one group with `method` and predict on the dense grid `xg`, returning a
# data frame of `(x, y[, ymin, ymax])`. The confidence band is a t-interval for
# lm/loess (residual df) and a normal interval for glm/gam (on the link scale,
# back-transformed); quantile regression (`rq`) draws the fitted line only.
.smooth_fit <- function(
  method,
  xi,
  yi,
  xg,
  se,
  level,
  span,
  formula,
  method.args
) {
  d <- data.frame(x = xi, y = yi)
  nd <- data.frame(x = xg)
  z <- stats::qnorm(1 - (1 - level) / 2)

  if (method == "lm") {
    fo <- formula %||% (y ~ x)
    fit <- do.call(stats::lm, c(list(formula = fo, data = d), method.args))
    pr <- stats::predict(fit, newdata = nd, se.fit = se)
    fitv <- if (se) pr$fit else pr
    out <- data.frame(x = xg, y = fitv)
    if (se) {
      t <- stats::qt(1 - (1 - level) / 2, fit$df.residual)
      out$ymin <- fitv - t * pr$se.fit
      out$ymax <- fitv + t * pr$se.fit
    }
    return(out)
  }

  if (method == "loess") {
    fo <- formula %||% (y ~ x)
    fit <- do.call(
      stats::loess,
      c(list(formula = fo, data = d, span = span), method.args)
    )
    pr <- stats::predict(fit, newdata = nd, se = se)
    fitv <- if (se) pr$fit else pr
    out <- data.frame(x = xg, y = fitv)
    if (se) {
      t <- stats::qt(1 - (1 - level) / 2, pr$df)
      out$ymin <- fitv - t * pr$se.fit
      out$ymax <- fitv + t * pr$se.fit
    }
    return(out)
  }

  if (method == "glm") {
    fo <- formula %||% (y ~ x)
    args <- method.args
    if (is.null(args$family)) {
      args$family <- stats::gaussian()
    }
    fit <- do.call(stats::glm, c(list(formula = fo, data = d), args))
    pr <- stats::predict(fit, newdata = nd, se.fit = se, type = "link")
    linkinv <- fit$family$linkinv
    fitv <- if (se) pr$fit else pr
    out <- data.frame(x = xg, y = linkinv(fitv))
    if (se) {
      out$ymin <- linkinv(fitv - z * pr$se.fit)
      out$ymax <- linkinv(fitv + z * pr$se.fit)
    }
    return(out)
  }

  if (method == "gam") {
    .need_pkg("mgcv", "{.code mark_smooth(method = \"gam\")}")
    fo <- formula %||% (y ~ s(x))
    # gam looks up smooth constructors (`s`, `te`, ...) in the formula's
    # environment; point it at mgcv's namespace so the default (and a bare
    # `y ~ s(x)` from the caller) resolves without mgcv being attached.
    environment(fo) <- asNamespace("mgcv")
    args <- method.args
    if (is.null(args$family)) {
      args$family <- stats::gaussian()
    }
    fit <- do.call(mgcv::gam, c(list(formula = fo, data = d), args))
    pr <- mgcv::predict.gam(fit, newdata = nd, se.fit = se, type = "link")
    linkinv <- fit$family$linkinv
    fitv <- if (se) pr$fit else pr
    out <- data.frame(x = xg, y = linkinv(fitv))
    if (se) {
      out$ymin <- linkinv(fitv - z * pr$se.fit)
      out$ymax <- linkinv(fitv + z * pr$se.fit)
    }
    return(out)
  }

  if (method == "rq") {
    .need_pkg("quantreg", "{.code mark_smooth(method = \"rq\")}")
    tau <- method.args$tau %||% 0.5
    if (length(tau) != 1) {
      cli::cli_abort(c(
        "{.fn mark_smooth} with {.val rq} takes a single {.arg tau}.",
        i = "For several quantiles, add one {.fn mark_smooth} layer per {.arg tau}."
      ))
    }
    fo <- formula %||% (y ~ x)
    fit <- quantreg::rq(fo, tau = tau, data = d)
    fitv <- as.numeric(stats::predict(fit, newdata = nd))
    return(data.frame(x = xg, y = fitv))
  }

  cli::cli_abort("Unknown smoothing method {.val {method}}.")
}

# Fit y ~ x (per group) with the requested method and predict on a dense grid,
# with a confidence ribbon where the method supports one.
.stat_smooth <- function(L) {
  method <- L$stat_params$method %||% "auto"
  ok_methods <- .SMOOTH_METHODS
  if (!method %in% ok_methods) {
    cli::cli_abort(c(
      "Unknown {.arg method} {.val {method}} for {.fn mark_smooth}.",
      i = "Use one of {.or {.val {ok_methods}}}."
    ))
  }
  se <- isTRUE(L$stat_params$se %||% TRUE)
  # Quantile regression has no symmetric confidence band; never draw a ribbon.
  if (identical(method, "rq")) {
    se <- FALSE
  }
  level <- L$stat_params$level %||% 0.95
  span <- L$stat_params$span %||% 0.75
  formula <- L$stat_params$formula
  method.args <- L$stat_params$method.args %||% list()
  x <- as.numeric(L$values$x)
  y <- as.numeric(L$values$y)
  .per_group(
    L,
    length(x),
    function(idx, gn, grouped, k) {
      f <- .finite_xy(x[idx], y[idx])
      xi <- f$x
      yi <- f$y
      if (length(unique(xi)) < 2) {
        if (grouped) {
          cli::cli_warn(
            "Skipping {.field {gn}}: {.fn mark_smooth} needs at least 2 distinct x values."
          )
        }
        return(NULL)
      }
      m <- if (identical(method, "auto")) .smooth_auto(length(xi)) else method
      xg <- seq(min(xi), max(xi), length.out = 80)
      .smooth_fit(m, xi, yi, xg, se, level, span, formula, method.args)
    },
    "{.fn mark_smooth} needs at least 2 distinct x values to fit."
  )
}

# The empirical cumulative distribution of x (per group): sorted x against the
# cumulative proportion, drawn as a right-continuous step.
.stat_ecdf <- function(L) {
  x <- as.numeric(L$values$x)
  .per_group(
    L,
    length(x),
    function(idx, gn, grouped, k) {
      xi <- sort(x[idx])
      xi <- xi[is.finite(xi)]
      if (!length(xi)) {
        return(NULL)
      }
      n <- length(xi)
      y <- seq_len(n) / n
      data.frame(x = xi, y = y, ecdf = y)
    },
    "{.fn mark_ecdf} needs at least one finite value."
  )
}

# Sorted sample values against the theoretical quantiles of a reference
# distribution (per group): x = theoretical, y = sample.
.stat_qq <- function(L) {
  s0 <- L$values$sample %||% L$values$y %||% L$values$x
  s0 <- as.numeric(s0)
  dist <- match.fun(L$stat_params$distribution %||% "qnorm")
  .per_group(
    L,
    length(s0),
    function(idx, gn, grouped, k) {
      s <- sort(s0[idx])
      s <- s[is.finite(s)]
      if (!length(s)) {
        return(NULL)
      }
      theo <- dist(stats::ppoints(length(s)))
      data.frame(x = theo, y = s, sample = s, theoretical = theo)
    },
    "{.fn mark_qq} needs at least one finite sample value."
  )
}

# The Q-Q reference line (per group): the line through the 1st and 3rd sample
# quartiles vs the matching theoretical quartiles, drawn across the theoretical
# range.
.stat_qq_line <- function(L) {
  s0 <- L$values$sample %||% L$values$y %||% L$values$x
  s0 <- as.numeric(s0)
  dist <- match.fun(L$stat_params$distribution %||% "qnorm")
  .per_group(
    L,
    length(s0),
    function(idx, gn, grouped, k) {
      s <- sort(s0[idx])
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
      data.frame(x = xr, y = intercept + slope * xr)
    },
    "{.fn mark_qq_line} needs at least 2 finite sample values."
  )
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

# Window (rolling / cumulative / offset) operations on an x-ordered y series.
.WINDOW_OPS <- c(
  "mean",
  "sum",
  "median",
  "min",
  "max",
  "cumsum",
  "cummean",
  "cummax",
  "cummin",
  "lag",
  "lead",
  "rank"
)

# A sliding-window reduction of `y` with window size `k` and alignment
# ("right" = trailing, "left" = leading, "center"). `partial = TRUE` computes at
# the edges from the available (shorter) window; `FALSE` leaves them NA.
.roll <- function(y, k, fun, align, partial) {
  n <- length(y)
  out <- rep(NA_real_, n)
  lead_n <- switch(
    align,
    right = 0L,
    left = k - 1L,
    center = ceiling((k - 1) / 2)
  )
  lag_n <- k - 1L - lead_n
  for (i in seq_len(n)) {
    lo <- max(i - lag_n, 1L)
    hi <- min(i + lead_n, n)
    if (!partial && (hi - lo + 1L) < k) {
      next
    }
    out[i] <- fun(y[lo:hi])
  }
  out
}

# Apply one window op to an x-ordered `y`.
.window_apply <- function(y, op, k, align, partial) {
  n <- length(y)
  switch(
    op,
    cumsum = cumsum(y),
    cummean = cumsum(y) / seq_len(n),
    cummax = cummax(y),
    cummin = cummin(y),
    rank = as.numeric(rank(y, ties.method = "average")),
    lag = c(rep(NA_real_, min(k, n)), utils::head(y, -k)),
    lead = c(utils::tail(y, -k), rep(NA_real_, min(k, n))),
    mean = .roll(y, k, mean, align, partial),
    sum = .roll(y, k, sum, align, partial),
    median = .roll(y, k, stats::median, align, partial),
    min = .roll(y, k, min, align, partial),
    max = .roll(y, k, max, align, partial)
  )
}

# Window transform: a rolling / cumulative / offset statistic of `y` computed
# per group, over rows ordered by `x`. Output is the standard `(x, y[, group])`
# stat shape, so any line/point emitter draws it unchanged. Rows where the
# statistic is undefined (non-partial edges, lag/lead shifts) are dropped, so a
# line does not dip to a gap.
.stat_window <- function(L) {
  op <- L$stat_params$op
  ok_ops <- .WINDOW_OPS
  if (is.null(op) || !op %in% ok_ops) {
    cli::cli_abort(c(
      "{.fn mark_line} {.arg window} needs an {.field op}.",
      i = "Use one of {.or {.val {ok_ops}}}."
    ))
  }
  # Rolling windows default to 7; lag/lead shift by 1 unless told otherwise.
  k <- L$stat_params$k %||% (if (op %in% c("lag", "lead")) 1L else 7L)
  if (!is.numeric(k) || length(k) != 1L || is.na(k) || k < 1 || k != round(k)) {
    cli::cli_abort(c(
      "{.fn mark_line} {.arg window} needs a positive integer {.field k}.",
      i = "Got {.obj_type_friendly {k}}."
    ))
  }
  k <- as.integer(k)
  align <- L$stat_params$align %||% "right"
  partial <- L$stat_params$partial %||% TRUE
  x <- as.numeric(L$values$x)
  y <- as.numeric(L$values$y)
  grp <- .layer_group(L)
  g <- .stat_groups(grp, length(x))
  glevs <- g$levels
  gnames <- names(g$groups)
  parts <- lapply(gnames, function(gn) {
    i <- g$groups[[gn]]
    o <- order(x[i])
    xi <- x[i][o]
    yi <- y[i][o]
    yo <- .window_apply(yi, op, k, align, partial)
    df <- data.frame(x = xi, y = yo)
    if (!is.null(grp)) {
      df$group <- factor(gn, levels = glevs)
    }
    df[is.finite(df$y), , drop = FALSE]
  })
  parts <- parts[vapply(parts, nrow, integer(1)) > 0]
  if (!length(parts)) {
    cli::cli_abort("{.fn mark_line} {.arg window} produced no finite values.")
  }
  do.call(rbind, parts)
}

# Require x and y for the region stats (ellipse/hull), with a clear message.
.need_xy <- function(L, what) {
  if (is.null(L$values$x) || is.null(L$values$y)) {
    cli::cli_abort("{what} needs both {.arg x} and {.arg y}.")
  }
}

# Boundary vertices of a data-space confidence ellipse for one group, following
# ggplot2's `stat_ellipse`: a unit circle transformed by the Cholesky factor of
# the (robust `t`, normal, or Euclidean) covariance, scaled by an F-quantile
# radius, and translated to the centre. Returns a `segments+1 x 2` matrix.
.ellipse_points <- function(x, y, type, level, segments) {
  n <- length(x)
  dfn <- 2
  dfd <- n - 1
  angles <- (0:segments) * 2 * pi / segments
  unit <- cbind(cos(angles), sin(angles))
  if (type == "euclid") {
    # A circle of radius `level` (data units) about the mean -- no covariance.
    centre <- c(mean(x), mean(y))
    pts <- sweep(level * unit, 2, centre, "+")
    return(pts)
  }
  if (type == "t") {
    .need_pkg("MASS", "{.code mark_ellipse(type = \"t\")}")
    fit <- MASS::cov.trob(cbind(x, y))
    cov <- fit$cov
    centre <- fit$center
  } else {
    cov <- stats::cov(cbind(x, y))
    centre <- c(mean(x), mean(y))
  }
  radius <- sqrt(dfn * stats::qf(level, dfn, dfd))
  chol_decomp <- chol(cov)
  t(centre + radius * t(unit %*% chol_decomp))
}

# Per-group confidence/data ellipse. Emits ordered boundary vertices with a
# per-group `.piece` (so each ellipse is a distinct closed polygon) and the
# `group` column so colour/fill flows back through `.merge_stat`.
.stat_ellipse <- function(L) {
  .need_xy(L, "{.fn mark_ellipse}")
  type <- L$stat_params$type %||% "t"
  if (!type %in% c("t", "norm", "euclid")) {
    cli::cli_abort(c(
      "Unknown ellipse {.arg type} {.val {type}}.",
      i = "Use one of {.or {.val {c('t', 'norm', 'euclid')}}}."
    ))
  }
  level <- L$stat_params$level %||% 0.95
  segments <- L$stat_params$segments %||% 51L
  x <- as.numeric(L$values$x)
  y <- as.numeric(L$values$y)
  .per_group(
    L,
    length(x),
    function(idx, gn, grouped, k) {
      f <- .finite_xy(x[idx], y[idx])
      xi <- f$x
      yi <- f$y
      if (length(xi) < 3) {
        cli::cli_warn(
          "Skipping {.field {gn}}: {.fn mark_ellipse} needs at least 3 points."
        )
        return(NULL)
      }
      pts <- .ellipse_points(xi, yi, type, level, segments)
      data.frame(x = pts[, 1], y = pts[, 2], .piece = k)
    },
    "{.fn mark_ellipse} needs at least 3 points to fit."
  )
}

# Per-group convex hull. Emits the hull vertices (counter-clockwise) with a
# per-group `.piece`; `polygon_grob` closes the ring.
.stat_hull <- function(L) {
  .need_xy(L, "{.fn mark_hull}")
  x <- as.numeric(L$values$x)
  y <- as.numeric(L$values$y)
  expand <- L$stat_params$expand %||% 0
  .per_group(
    L,
    length(x),
    function(idx, gn, grouped, k) {
      f <- .finite_xy(x[idx], y[idx])
      xi <- f$x
      yi <- f$y
      if (length(xi) < 3) {
        cli::cli_warn(
          "Skipping {.field {gn}}: {.fn mark_hull} needs at least 3 points."
        )
        return(NULL)
      }
      h <- grDevices::chull(xi, yi)
      hx <- xi[h]
      hy <- yi[h]
      # `expand` grows the hull outward from its centroid so it encloses the node
      # markers (whose centres the raw hull passes through). 0 = the tight hull.
      if (expand != 0) {
        cx <- mean(hx)
        cy <- mean(hy)
        hx <- cx + (hx - cx) * (1 + expand)
        hy <- cy + (hy - cy) * (1 + expand)
      }
      data.frame(x = hx, y = hy, .piece = k)
    },
    "{.fn mark_hull} needs at least 3 points."
  )
}
