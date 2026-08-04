#' @include classes.R
NULL

# Mark emitters: basic marks (point, line, rule, bar, area/step, smooth).

.emit_point <- function(scene, L, scales) {
  n <- L$n
  if (
    isTRUE(L$stat_params$auto) && n > .DATASHADE_AUTO && .can_datashade(scales)
  ) {
    return(.emit_datashade(scene, L, scales))
  }
  sk <- .mark_sketch(L, scales)
  xn <- rep_len(scales$x$map(L$values$x), n)
  yn <- rep_len(scales$y$map(L$values$y), n)
  band_x <- scales$x$band_width %||% .resolution(xn)
  # jitterdodge first offsets each element into its group's dodged slot, then
  # jitters within the (narrower) slot.
  slot <- band_x
  if (identical(L$position, "jitterdodge")) {
    grp <- L$values$color %||% L$values$fill
    dw <- L$stat_params$dodge_width %||% 0.75
    if (!is.null(grp)) {
      levs <- .cat_levels(grp)
      G <- length(levs)
      rank <- match(as.character(rep_len(grp, n)), levs)
      xn <- xn + (rank - (G + 1) / 2) / G * dw * band_x
      slot <- dw * band_x / G
    }
  }
  if (L$position %in% c("jitter", "jitterdodge")) {
    ax <- L$stat_params$jitter_width %||%
      (0.4 * if (identical(L$position, "jitterdodge")) slot else band_x)
    ay <- L$stat_params$jitter_height %||% (0.4 * .resolution(yn))
    jit <- .with_seed(
      L$stat_params$seed,
      list(
        x = stats::runif(n, -ax, ax),
        y = stats::runif(n, -ay, ay)
      )
    )
    xn <- xn + jit$x
    yn <- yn + jit$y
  }
  col <- rep_len(.aes_colour(L, scales, "black"), n)
  size <- rep_len(.aes_size(L, scales, 1), n)
  shape <- rep_len(.aes_shape(L, scales, "circle"), n)
  alpha <- rep_len(.aes_alpha(L, scales, NA_real_), n)

  # A `shape` that is not a built-in marker is an SVG icon (a `d` path or a
  # `.svg` file); those points are drawn as crisp vector `svg_grob()`s, the rest
  # as the usual batched `points_grob()`.
  dsvg <- .shape_svg_d(shape)
  svg_i <- which(!is.na(dsvg))
  pt_ok <- is.na(dsvg)

  for (idx0 in .style_groups(n, list(col = col, alpha = alpha))) {
    idx <- idx0[pt_ok[idx0]] # built-in markers in this style group
    if (!length(idx)) {
      next
    }
    a <- alpha[idx[1]]
    xy <- .xy_units(scales, xn[idx], yn[idx])
    # PROVENANCE: `idx` are the layer rows in this style group -- the row-key
    # refinement (DESIGN §4). This is the canonical example; other grouped
    # emitters below should pass `rows = idx` the same way.
    scene <- .draw(
      scene,
      vellum::points_grob(
        xy$x,
        xy$y,
        size = vellum::vl_unit(size[idx], "mm"),
        shape = shape[idx],
        sketch = sk,
        gp = vellum::vl_gpar(
          fill = col[idx[1]],
          col = col[idx[1]],
          alpha = gp_alpha(a)
        )
      ),
      rows = idx
    )
  }
  # SVG icons: one `svg_grob()` per point. `size` is the icon's longer-side
  # length in mm; the `size` aesthetic scales it, with a factor so a default-size
  # icon is legible rather than point-sized. Icons are drawn unkeyed: a keyed
  # multi-sub-path svg path miscounts between `scene_model()`'s grammar walk and
  # the backend (a vellum-side accounting gap), so per-icon interactivity is not
  # wired yet -- the icons still render on every backend.
  for (i in svg_i) {
    xy <- .xy_units(scales, xn[i], yn[i])
    scene <- .draw(
      scene,
      vellum::svg_grob(
        dsvg[i],
        xy$x,
        xy$y,
        size = vellum::vl_unit(size[i] * .SVG_MARKER_MM, "mm"),
        gp = vellum::vl_gpar(
          fill = col[i],
          col = col[i],
          alpha = gp_alpha(alpha[i])
        )
      )
    )
  }
  scene
}

# Longer-side length (mm) of a `size = 1` SVG icon marker; the `size` aesthetic
# scales from here. Chosen so a default icon reads as an icon, not a dot.
.SVG_MARKER_MM <- 5

.emit_line <- function(scene, L, scales) {
  n <- L$n
  if (
    isTRUE(L$stat_params$auto) && n > .DATASHADE_AUTO && .can_datashade(scales)
  ) {
    return(.emit_line_datashade(scene, L, scales))
  }
  xn <- rep_len(scales$x$map(L$values$x), n)
  yn <- rep_len(scales$y$map(L$values$y), n)
  col <- rep_len(.aes_colour(L, scales, "black"), n)
  alpha <- rep_len(.aes_alpha(L, scales, NA_real_), n)
  lty <- .resolve_lty(L, scales, n)
  lwd <- .aes_param(L, "linewidth", 1.5)
  sk <- .mark_sketch(L, scales)

  gi <- 0L
  for (idx in .style_groups(n, list(col = col, alpha = alpha, lty = lty))) {
    o <- idx[order(xn[idx])] # a line is drawn in x order
    xy <- .xy_path(scales, xn[o], yn[o])
    scene <- .draw(
      scene,
      vellum::lines_grob(
        xy$x,
        xy$y,
        sketch = .sketch_bump(sk, gi),
        gp = .gp_stroke(col, alpha, idx[1], lwd, lty)
      ),
      # PROVENANCE: `o` are the layer rows this polyline draws (x-ordered).
      rows = o
    )
    gi <- gi + 1L
  }
  scene
}

# A sparkline (vsparkline()): a compact, axis-free chart of one series drawn to
# FILL the panel box, directly in npc (like the layout marks) rather than through
# the trained scales -- so it fits tightly with no axis expansion. `type` picks
# the shape: a "line" trend (optional dots on its extremes / last point, kept off
# the edge by a small pad), a "bar" column micro-chart (bars from the box floor),
# or a "winloss" chart of equal up/down bars about the mid line.
.emit_sparkline <- function(scene, L, scales) {
  n <- L$n
  sp <- L$stat_params
  type <- sp$type %||% "line"
  rawx <- rep_len(L$values$x, n)
  rawy <- rep_len(L$values$y, n)
  o <- order(rawx)
  rawx <- rawx[o]
  rawy <- rawy[o]
  col <- sp$color %||% "grey30"
  npc <- function(u) vellum::vl_unit(u, "npc")

  xr <- range(rawx[is.finite(rawx)])
  xn <- if (diff(xr) > 0) (rawx - xr[1]) / diff(xr) else rep(0.5, n)
  yr <- range(rawy[is.finite(rawy)])
  yspan <- yr[2] - yr[1]
  bw <- 0.7 / n # bar slot as a fraction of the box width

  if (identical(type, "line")) {
    pad <- 0.14 # keep the trend (and its dots) off the top/bottom edge
    yn <- if (yspan > 0) {
      pad + (1 - 2 * pad) * (rawy - yr[1]) / yspan
    } else {
      rep(0.5, n)
    }
    scene <- .draw(
      scene,
      vellum::lines_grob(
        npc(xn),
        npc(yn),
        gp = vellum::vl_gpar(
          col = col,
          lwd = sp$linewidth %||% 1,
          lineend = "round",
          linejoin = "round"
        )
      ),
      rows = o
    )
    hi <- switch(
      sp$points %||% "extremes",
      extremes = unique(c(which.min(rawy), which.max(rawy))),
      last = n,
      integer(0)
    )
    if (length(hi)) {
      pc <- sp$point_color %||% "firebrick"
      scene <- .draw(
        scene,
        vellum::points_grob(
          npc(xn[hi]),
          npc(yn[hi]),
          # point_size is a diameter (mm); points_grob wants a radius.
          size = vellum::vl_unit((sp$point_size %||% 1.4) / 2, "mm"),
          shape = "circle",
          gp = vellum::vl_gpar(fill = pc, col = pc)
        ),
        rows = o[hi]
      )
    }
    return(scene)
  }

  # bar centres inset by half a slot so the edge bars sit inside the box.
  bx <- bw / 2 + xn * (1 - bw)

  if (identical(type, "bar")) {
    base <- min(rawy, sp$baseline %||% Inf, na.rm = TRUE) # box floor
    top <- max(rawy, na.rm = TRUE)
    frac <- if (top > base) (rawy - base) / (top - base) else rep(0.5, n)
    h <- 0.94 * frac
    return(.draw(
      scene,
      vellum::rect_grob(
        npc(bx),
        npc(h / 2),
        width = npc(rep(bw, n)),
        height = npc(h),
        gp = vellum::vl_gpar(fill = col, col = NA)
      ),
      rows = o
    ))
  }

  # winloss: equal up/down bars about the mid line.
  up <- rawy >= (sp$baseline %||% 0)
  h <- 0.4
  draw_side <- function(scene, sel, yc, fill) {
    if (!length(sel)) {
      return(scene)
    }
    .draw(
      scene,
      vellum::rect_grob(
        npc(bx[sel]),
        npc(yc),
        width = npc(rep(bw, length(sel))),
        height = npc(h),
        gp = vellum::vl_gpar(fill = fill, col = NA)
      ),
      rows = o[sel]
    )
  }
  scene <- draw_side(scene, which(up), 0.5 + h / 2, sp$win_color %||% "#2c7fb8")
  scene <- draw_side(
    scene,
    which(!up),
    0.5 - h / 2,
    sp$loss_color %||% "#d7301f"
  )
  scene
}

# A grob / sub-plot annotation (annotate("grob"/"sparkline")): place a supplied
# vellum grob or a PlotSpec (e.g. a vsparkline) at each data coordinate, in a box
# of physical size `width` x `height`, aligned by `halign`/`valign`. The box is
# anchored with a compound native+mm coordinate so it tracks the data point at any
# size/aspect. A PlotSpec is drawn via the same `.draw_plot` seam `inset()` uses.
.emit_grob <- function(scene, L, scales) {
  n <- L$n
  sp <- L$stat_params
  g <- sp$grob
  w_mm <- .mm(sp$width, sp$units)
  h_mm <- .mm(sp$height, sp$units)
  hoff <- switch(sp$halign %||% "centre", left = 0.5, right = -0.5, 0) * w_mm
  voff <- switch(sp$valign %||% "centre", bottom = 0.5, top = -0.5, 0) * h_mm
  xn <- rep_len(scales$x$map(L$values$x), n)
  yn <- rep_len(scales$y$map(L$values$y), n)
  is_plot <- S7::S7_inherits(g, PlotSpec)
  for (i in seq_len(n)) {
    pos <- .xy_units(scales, xn[i], yn[i])
    scene <- vellum::push(
      scene,
      vellum::vl_viewport(
        x = pos$x + vellum::vl_unit(hoff, "mm"),
        y = pos$y + vellum::vl_unit(voff, "mm"),
        width = vellum::vl_unit(w_mm, "mm"),
        height = vellum::vl_unit(h_mm, "mm")
      )
    )
    scene <- if (is_plot) .draw_plot(scene, g) else vellum::draw(scene, g)
    scene <- vellum::pop(scene)
  }
  scene
}

# Contour lines: one polyline per traced piece, vertices kept in trace order (not
# x-sorted — a contour is not a function of x), coloured by the mapped level.
.emit_contour <- function(scene, L, scales) {
  n <- L$n
  if (!n) {
    return(scene)
  }
  xn <- rep_len(scales$x$map(L$values$x), n)
  yn <- rep_len(scales$y$map(L$values$y), n)
  col <- rep_len(.aes_colour(L, scales, "#3366bb"), n)
  alpha <- rep_len(.aes_alpha(L, scales, NA_real_), n)
  lwd <- .aes_param(L, "linewidth", 0.6)
  lty <- .resolve_lty(L, scales, n)
  piece <- L$values$.piece
  sk <- .mark_sketch(L, scales)
  gi <- 0L
  # split once (first-appearance order) instead of a which() rescan per piece.
  for (idx in split(seq_along(piece), factor(piece, levels = unique(piece)))) {
    xy <- .xy_path(scales, xn[idx], yn[idx])
    scene <- .draw(
      scene,
      vellum::lines_grob(
        xy$x,
        xy$y,
        sketch = .sketch_bump(sk, gi),
        gp = .gp_stroke(col, alpha, idx[1], lwd, lty)
      ),
      rows = idx
    )
    gi <- gi + 1L
  }
  scene
}

# Filled contour bands: one even-odd path per band (rings via `id`, so holes are
# cut out), filled by the band's level.
.emit_contour_filled <- function(scene, L, scales) {
  n <- L$n
  if (!n) {
    return(scene)
  }
  xn <- rep_len(scales$x$map(L$values$x), n)
  yn <- rep_len(scales$y$map(L$values$y), n)
  fill <- rep_len(.aes_colour(L, scales, "#3366bb"), n)
  alpha <- rep_len(.aes_alpha(L, scales, NA_real_), n)
  piece <- L$values$.piece
  ring <- L$values$.ring
  sk <- .mark_sketch(L, scales)
  gi <- 0L
  # split once (first-appearance order) instead of a which() rescan per piece.
  for (idx in split(seq_along(piece), factor(piece, levels = unique(piece)))) {
    a <- alpha[idx[1]]
    xy <- .xy_path(scales, xn[idx], yn[idx])
    scene <- .draw(
      scene,
      vellum::path_grob(
        xy$x,
        xy$y,
        id = ring[idx],
        rule = "evenodd",
        sketch = .sketch_bump(sk, gi),
        gp = vellum::vl_gpar(
          fill = fill[idx[1]],
          col = NA,
          alpha = gp_alpha(a)
        )
      ),
      rows = idx
    )
    gi <- gi + 1L
  }
  scene
}

.emit_rule <- function(scene, L, scales) {
  col <- .aes_colour(L, scales, "grey40")[1]
  lwd <- .aes_param(L, "linewidth", 1)
  lty <- .aes_linetype(L, scales, NULL)
  alpha <- .aes_alpha(L, scales, NA_real_)[1]
  gp <- vellum::vl_gpar(
    col = col,
    lwd = lwd,
    lty = lty,
    alpha = gp_alpha(alpha)
  )
  sk <- .mark_sketch(L, scales)
  yi <- .intercept(L, "yintercept")
  xi <- .intercept(L, "xintercept")
  if (!is.null(yi)) {
    vy <- scales$y$map(yi)
    if (length(vy)) {
      k <- length(vy)
      s <- .seg_units(
        scales,
        vellum::vl_unit(rep(0, k), "npc"),
        vellum::vl_unit(vy, "native"),
        vellum::vl_unit(rep(1, k), "npc"),
        vellum::vl_unit(vy, "native")
      )
      scene <- .draw(
        scene,
        vellum::segments_grob(s$x0, s$y0, s$x1, s$y1, sketch = sk, gp = gp)
      )
    }
  }
  if (!is.null(xi)) {
    vx <- scales$x$map(xi)
    if (length(vx)) {
      k <- length(vx)
      s <- .seg_units(
        scales,
        vellum::vl_unit(vx, "native"),
        vellum::vl_unit(rep(0, k), "npc"),
        vellum::vl_unit(vx, "native"),
        vellum::vl_unit(rep(1, k), "npc")
      )
      scene <- .draw(
        scene,
        vellum::segments_grob(
          s$x0,
          s$y0,
          s$x1,
          s$y1,
          sketch = .sketch_bump(sk, 1L),
          gp = gp
        )
      )
    }
  }
  scene
}

# Smallest positive gap between sorted unique positions (the bar width unit on a
# continuous x); 1 if there is only one bar.
.resolution <- function(x) {
  u <- sort(unique(x[is.finite(x)]))
  if (length(u) < 2) {
    return(1)
  }
  min(diff(u))
}

# Bars in polar space become annular sectors (wedges). With theta = "x" (rose /
# coxcomb) the categorical x band sets the angular span and y the radius; with
# theta = "y" (pie / bullseye) the stacked [ymin, ymax] sets the angular span —
# normalized per x-group so a slice set always closes the full circle — and the
# x band sets the radial ring. Wedges tile the full circle (no inter-bar gap).
.emit_bar_polar <- function(scene, L, scales) {
  ctx <- scales$polar
  n <- L$n
  xp <- rep_len(scales$x$map(L$values$x), n)
  fill <- rep_len(.aes_colour(L, scales, "grey35"), n)
  alpha <- rep_len(.aes_alpha(L, scales, NA_real_), n)
  band <- scales$x$band_width %||% .resolution(xp)
  w <- rep_len(L$values$width %||% band, n)

  ymin_d <- if (!is.null(L$values$ymin)) L$values$ymin else rep(0, n)
  ymax_d <- if (!is.null(L$values$ymax)) L$values$ymax else L$values$y
  ymin <- rep_len(as.numeric(ymin_d), n)
  ymax <- rep_len(as.numeric(ymax_d), n)

  if (identical(ctx$theta_aes, "x")) {
    a0 <- ctx$theta_map(xp - w / 2)
    a1 <- ctx$theta_map(xp + w / 2)
    r0 <- ctx$r_map(ymin)
    r1 <- ctx$r_map(ymax)
  } else {
    # Normalize by the sum of the per-slice spans within an x-group. For the
    # stacked input this path expects (ymin/ymax cumulative from 0) this equals
    # max(ymax); summing the spans keeps the total correct if the slices are not
    # pre-stacked.
    total <- stats::ave(
      ymax - ymin,
      as.character(rep_len(L$values$x, n)),
      FUN = sum
    )
    total[total == 0] <- 1
    a0 <- ctx$ang_frac(ymin / total)
    a1 <- ctx$ang_frac(ymax / total)
    r0 <- ctx$r_map(xp - w / 2)
    r1 <- ctx$r_map(xp + w / 2)
  }
  theta0 <- pmin(a0, a1)
  theta1 <- pmax(a0, a1)
  sk <- .mark_sketch(L, scales)

  for (idx in .style_groups(n, list(fill = fill, alpha = alpha))) {
    a <- alpha[idx[1]]
    scene <- .draw(
      scene,
      vellum::sector_grob(
        x = vellum::vl_unit(rep(0, length(idx)), "native"),
        y = vellum::vl_unit(rep(0, length(idx)), "native"),
        r0 = vellum::vl_unit(r0[idx], "native"),
        r1 = vellum::vl_unit(r1[idx], "native"),
        theta0 = theta0[idx],
        theta1 = theta1[idx],
        fill = fill[idx],
        sketch = sk,
        gp = vellum::vl_gpar(col = NA, alpha = gp_alpha(a))
      ),
      rows = idx
    )
  }
  scene
}

# dodge2: at each x, place only the groups actually present, filling the total
# width `tw` with equal slots separated by `padding` (a fraction of the slot). So
# a category with fewer groups gets wider bars centred on the tick, rather than
# leaving gaps for absent groups (plain dodge). Returns per-element centre `xc`
# and width `w` (native px), aligned to the input row order.
.dodge2_bars <- function(xp, grp, n, tw, padding) {
  xc <- rep_len(xp, n)
  w <- rep_len(tw, n)
  if (is.null(grp)) {
    return(list(xc = xc, w = w))
  }
  levs <- .cat_levels(grp)
  g <- as.character(rep_len(grp, n))
  for (xi in unique(xc)) {
    rows <- which(xc == xi)
    rows <- rows[order(match(g[rows], levs))]
    k <- length(rows)
    slot <- tw[rows][1] / k
    for (j in seq_len(k)) {
      i <- rows[j]
      w[i] <- slot * (1 - padding)
      xc[i] <- xi + (j - (k + 1) / 2) * slot
    }
  }
  list(xc = xc, w = w)
}

# The shared per-style-group rect draw for the filled rectangular marks (bar,
# tile, rect): group by fill/alpha/pattern, then draw one `rect_grob` per group
# from the precomputed centre (`xc`, `yc`) and size (`w`, `h`) vectors. `paint` is
# a constant fill (e.g. a gradient) that overrides the per-group pattern object.
.emit_rect_groups <- function(
  scene,
  L,
  scales,
  xc,
  yc,
  w,
  h,
  fill,
  alpha,
  sk,
  paint = NULL
) {
  n <- L$n
  gi <- 0L
  pkey <- .aes_pattern_key(L, scales, n)
  for (idx in .style_groups(
    n,
    c(list(fill = fill, alpha = alpha), if (!is.null(pkey)) list(pat = pkey))
  )) {
    r <- .rect_units(scales, xc[idx], yc[idx], w[idx], h[idx])
    scene <- .draw(
      scene,
      vellum::rect_grob(
        x = r$x,
        y = r$y,
        width = r$width,
        height = r$height,
        sketch = .sketch_bump(sk, gi),
        gp = .gp_fill(
          fill,
          alpha,
          idx[1],
          paint %||% .pattern_obj(scales, pkey, idx[1])
        )
      ),
      rows = idx
    )
    gi <- gi + 1L
  }
  scene
}

.emit_bar <- function(scene, L, scales) {
  grad <- .paint_fill(L)
  if (!is.null(scales$polar)) {
    if (!is.null(grad)) {
      cli::cli_abort(
        "Paint fills (gradient/pattern) are not supported for polar bars / pies."
      )
    }
    return(.emit_bar_polar(scene, L, scales))
  }
  n <- L$n
  sk <- .mark_sketch(L, scales)
  xp <- rep_len(scales$x$map(L$values$x), n)
  fill <- if (is.null(grad)) rep_len(.aes_colour(L, scales, "grey35"), n)
  alpha <- rep_len(.aes_alpha(L, scales, NA_real_), n)
  band <- scales$x$band_width %||% .resolution(xp)

  # vertical span: a stacked [ymin, ymax], else 0 -> y. The baseline is clamped
  # into the finite domain so a log/sqrt y scale (where map(0) = -Inf) still
  # draws bars from the axis floor rather than a degenerate rect.
  ymin_d <- if (!is.null(L$values$ymin)) L$values$ymin else rep(0, n)
  ymax_d <- if (!is.null(L$values$ymax)) L$values$ymax else L$values$y
  y0 <- rep_len(.clamp_baseline(scales$y$map(ymin_d), scales$y$domain), n)
  y1 <- rep_len(scales$y$map(ymax_d), n)

  # Bar width: a stat-provided `width` (histogram bins) fills the bin so bars
  # touch; categorical / explicit bars leave a 10% gap around the band.
  w <- rep_len(L$values$width %||% (0.9 * band), n)
  xc <- xp
  if (identical(L$position, "dodge")) {
    grp <- L$values$color %||% L$values$fill
    if (!is.null(grp)) {
      levs <- .cat_levels(grp)
      G <- length(levs)
      rank <- match(as.character(rep_len(grp, n)), levs)
      # `dodge_width` (data units) overrides the band as the group's total span.
      span <- L$stat_params$dodge_width %||% band
      w <- w / G
      xc <- xp + (rank - (G + 1) / 2) / G * span
    }
  } else if (identical(L$position, "dodge2")) {
    d2 <- .dodge2_bars(
      xp,
      L$values$color %||% L$values$fill,
      n,
      L$values$width %||% (0.9 * band),
      L$stat_params$dodge2_padding %||% 0.1
    )
    xc <- d2$xc
    w <- d2$w
  }

  # A gradient fill paints every bar as one rect grob sharing the paint.
  if (!is.null(grad)) {
    r <- .rect_units(scales, xc, (y0 + y1) / 2, w, abs(y1 - y0))
    return(.draw(
      scene,
      vellum::rect_grob(
        x = r$x,
        y = r$y,
        width = r$width,
        height = r$height,
        sketch = sk,
        gp = vellum::vl_gpar(fill = grad, col = NA)
      ),
      rows = seq_len(n)
    ))
  }

  .emit_rect_groups(
    scene,
    L,
    scales,
    xc,
    (y0 + y1) / 2,
    w,
    abs(y1 - y0),
    fill,
    alpha,
    sk
  )
}

# A fitted smooth: a confidence ribbon (polygon) under a fitted line, one per
# colour group.
.emit_smooth <- function(scene, L, scales) {
  n <- L$n
  xn <- rep_len(scales$x$map(L$values$x), n)
  yn <- rep_len(scales$y$map(L$values$y), n)
  col <- rep_len(.aes_colour(L, scales, "#3366CC"), n)
  # a mapped/constant band alpha overrides the ribbon's native 0.25; the fitted
  # line honours a `linewidth` param (its own default weight is 1.5)
  alpha <- rep_len(.aes_alpha(L, scales, NA_real_), n)
  lwd <- .aes_param(L, "linewidth", 1.5)
  sk <- .mark_sketch(L, scales)
  has_se <- !is.null(L$values$ymin)
  ymin <- if (has_se) scales$y$map(L$values$ymin)
  ymax <- if (has_se) scales$y$map(L$values$ymax)

  gi <- 0L
  for (idx in .style_groups(n, list(col = col, alpha = alpha))) {
    o <- idx[order(xn[idx])]
    cc <- col[idx[1]]
    a <- alpha[idx[1]]
    if (has_se) {
      poly <- .xy_area(scales, xn[o], ymin[o], xn[o], ymax[o])
      scene <- .draw(
        scene,
        vellum::polygon_grob(
          poly$x,
          poly$y,
          sketch = .sketch_bump(sk, gi),
          gp = vellum::vl_gpar(
            fill = cc,
            col = NA,
            alpha = gp_alpha(if (is.na(a)) 0.25 else a)
          )
        )
      )
    }
    ln <- .xy_path(scales, xn[o], yn[o])
    scene <- .draw(
      scene,
      vellum::lines_grob(
        ln$x,
        ln$y,
        sketch = .sketch_bump(sk, gi + 50L),
        gp = vellum::vl_gpar(col = cc, lwd = lwd)
      )
    )
    gi <- gi + 1L
  }
  scene
}

# A filled band between two y boundaries `ya`/`yb` (already mapped to native y),
# drawn as one x-ordered polygon per style group -- the shared body of the ribbon
# and area marks. A gradient fill paints the whole band as one polygon.
.emit_band <- function(scene, L, scales, ya, yb) {
  n <- L$n
  xn <- rep_len(scales$x$map(L$values$x), n)
  sk <- .mark_sketch(L, scales)

  grad <- .paint_fill(L)
  if (!is.null(grad)) {
    o <- order(xn)
    poly <- .xy_area(scales, xn[o], ya[o], xn[o], yb[o])
    return(.draw(
      scene,
      vellum::polygon_grob(
        poly$x,
        poly$y,
        sketch = sk,
        gp = vellum::vl_gpar(fill = grad, col = NA)
      ),
      # PROVENANCE: one polygon over the whole layer, in x order.
      rows = o
    ))
  }

  fill <- rep_len(.aes_colour(L, scales, "grey50"), n)
  alpha <- rep_len(.aes_alpha(L, scales, NA_real_), n)

  gi <- 0L
  for (idx in .style_groups(n, list(fill = fill, alpha = alpha))) {
    o <- idx[order(xn[idx])]
    poly <- .xy_area(scales, xn[o], ya[o], xn[o], yb[o])
    scene <- .draw(
      scene,
      vellum::polygon_grob(
        poly$x,
        poly$y,
        sketch = .sketch_bump(sk, gi),
        gp = .gp_fill(fill, alpha, idx[1])
      ),
      # PROVENANCE: `o` are the layer rows this polygon draws (x-ordered).
      rows = o
    )
    gi <- gi + 1L
  }
  scene
}

# A filled ribbon between ymin and ymax, one polygon per style group.
.emit_ribbon <- function(scene, L, scales) {
  n <- L$n
  ymin <- rep_len(scales$y$map(L$values$ymin), n)
  ymax <- rep_len(scales$y$map(L$values$ymax), n)
  .emit_band(scene, L, scales, ymin, ymax)
}

# An area mark: the region between `y` and the zero baseline, or between a
# stacked `[ymin, ymax]` span when `position = "stack"`/`"fill"` set one (see
# `.position_stack`). Baseline resolution mirrors `.emit_bar`.
.emit_area <- function(scene, L, scales) {
  n <- L$n
  # vertical span: a stacked [ymin, ymax], else 0 -> y. The baseline is clamped
  # into the finite domain so a log/sqrt y scale (map(0) = -Inf) still draws from
  # the axis floor. `.emit_band` takes (near, far) = (y1, y0) to match the
  # historical polygon winding.
  ymin_d <- if (!is.null(L$values$ymin)) L$values$ymin else rep(0, n)
  ymax_d <- if (!is.null(L$values$ymax)) L$values$ymax else L$values$y
  y0 <- rep_len(.clamp_baseline(scales$y$map(ymin_d), scales$y$domain), n)
  y1 <- rep_len(scales$y$map(ymax_d), n)
  .emit_band(scene, L, scales, y1, y0)
}

# A staircase line: each segment is expanded into a horizontal-then-vertical
# ("hv") or vertical-then-horizontal ("vh") pair before drawing.
.emit_step <- function(scene, L, scales) {
  n <- L$n
  if (
    isTRUE(L$stat_params$auto) && n > .DATASHADE_AUTO && .can_datashade(scales)
  ) {
    return(.emit_line_datashade(scene, L, scales, step = TRUE))
  }
  xn <- rep_len(scales$x$map(L$values$x), n)
  yn <- rep_len(scales$y$map(L$values$y), n)
  col <- rep_len(.aes_colour(L, scales, "black"), n)
  alpha <- rep_len(.aes_alpha(L, scales, NA_real_), n)
  lty <- .resolve_lty(L, scales, n)
  lwd <- .aes_param(L, "linewidth", 1.5)
  dir <- L$stat_params$direction %||% "hv"
  sk <- .mark_sketch(L, scales)

  gi <- 0L
  for (idx in .style_groups(n, list(col = col, alpha = alpha, lty = lty))) {
    o <- idx[order(xn[idx])]
    sx <- xn[o]
    sy <- yn[o]
    m <- length(sx)
    if (m >= 2) {
      if (identical(dir, "vh")) {
        ex <- c(rep(sx[-m], each = 2), sx[m])
        ey <- c(sy[1], rep(sy[-1], each = 2))
      } else {
        ex <- c(sx[1], rep(sx[-1], each = 2))
        ey <- c(rep(sy[-m], each = 2), sy[m])
      }
    } else {
      ex <- sx
      ey <- sy
    }
    ln <- .xy_path(scales, ex, ey)
    scene <- .draw(
      scene,
      vellum::lines_grob(
        ln$x,
        ln$y,
        sketch = .sketch_bump(sk, gi),
        gp = .gp_stroke(col, alpha, idx[1], lwd, lty)
      ),
      # PROVENANCE: `o` are the layer rows this staircase draws (x-ordered).
      rows = o
    )
    gi <- gi + 1L
  }
  scene
}

# A group summary region (ellipse / convex hull): one closed polygon per group
# `.piece`. Stroked by the `color` aesthetic; filled only when a `fill` is
# mapped or set (so the ggplot2 default -- an unfilled boundary -- holds). The
# `.piece` grouping comes from the stat; each polygon draws its own vertices.
#
# PROVENANCE: `rows = idx` records the polygon's own vertices; when the layer
# maps a discrete colour/fill, each vertex also carries its series key
# ("<aes>:<value>") in `meta$legend` (via `.legend_membership`), which is the
# cross-layer hook a host uses to link a region to its group's points
# (GAP-ANALYSIS.md 12.1).
.emit_region <- function(scene, L, scales) {
  n <- L$n
  if (!n) {
    return(scene)
  }
  xn <- rep_len(scales$x$map(L$values$x), n)
  yn <- rep_len(scales$y$map(L$values$y), n)
  stroke <- rep_len(
    .aes_colour(L, scales, "#3366bb", fill_fallback = FALSE),
    n
  )
  # Fill only when the layer supplies one; otherwise draw an unfilled boundary.
  # A constant paint (gradient/pattern) fill is substituted at the gp below, not
  # carried in the per-row colour vector (which `rep_len` would corrupt).
  paint <- .paint_fill(L)
  fillv <- if (!is.null(L$values$fill)) {
    scales$color$map(L$values$fill)
  } else if (!is.null(L$params$fill) && is.null(paint)) {
    L$params$fill
  } else {
    NA
  }
  fillv <- rep_len(fillv, n)
  alpha <- rep_len(.aes_alpha(L, scales, NA_real_), n)
  lwd <- .aes_param(L, "linewidth", 1)
  piece <- L$values$.piece
  sk <- .mark_sketch(L, scales)
  gi <- 0L
  # split once (first-appearance order) instead of a which() rescan per piece.
  for (idx in split(seq_along(piece), factor(piece, levels = unique(piece)))) {
    i0 <- idx[1]
    xy <- .xy_path(scales, xn[idx], yn[idx])
    scene <- .draw(
      scene,
      vellum::polygon_grob(
        xy$x,
        xy$y,
        sketch = .sketch_bump(sk, gi),
        gp = vellum::vl_gpar(
          fill = .pattern_at(L, scales, i0) %||% .paint_or(L, fillv[i0]),
          col = stroke[i0],
          lwd = lwd,
          alpha = gp_alpha(alpha[i0])
        )
      ),
      rows = idx
    )
    gi <- gi + 1L
  }
  scene
}
