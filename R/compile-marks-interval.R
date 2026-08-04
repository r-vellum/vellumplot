#' @include classes.R
NULL

# Mark emitters: interval marks (errorbar, linerange, segment).

# Vertical error bars from ymin to ymax (with optional horizontal caps).
.emit_errorbar <- function(scene, L, scales, caps = TRUE) {
  n <- L$n
  xn <- rep_len(scales$x$map(L$values$x), n)
  ymin <- rep_len(scales$y$map(L$values$ymin), n)
  ymax <- rep_len(scales$y$map(L$values$ymax), n)
  col <- rep_len(.aes_colour(L, scales, "black"), n)
  alpha <- rep_len(.aes_alpha(L, scales, NA_real_), n)
  lwd <- .aes_param(L, "linewidth", 1)
  lty <- .resolve_lty(L, scales, n)
  band <- scales$x$band_width %||% .resolution(xn)
  # A mapped `width` lands in values (a constant one in params); read values first
  # so a per-row cap width is honoured, not silently ignored.
  width <- rep_len(L$values$width %||% .aes_param(L, "width", 0.5), n)
  half <- (width * band) / 2
  sk <- .mark_sketch(L, scales)

  gi <- 0L
  for (idx in .style_groups(n, list(col = col, alpha = alpha, lty = lty))) {
    x0 <- xn[idx]
    y0 <- ymin[idx]
    x1 <- xn[idx]
    y1 <- ymax[idx]
    if (caps) {
      x0 <- c(x0, xn[idx] - half[idx], xn[idx] - half[idx])
      x1 <- c(x1, xn[idx] + half[idx], xn[idx] + half[idx])
      y0 <- c(y0, ymin[idx], ymax[idx])
      y1 <- c(y1, ymin[idx], ymax[idx])
    }
    s <- .seg_units(
      scales,
      vellum::vl_unit(x0, "native"),
      vellum::vl_unit(y0, "native"),
      vellum::vl_unit(x1, "native"),
      vellum::vl_unit(y1, "native")
    )
    scene <- .draw(
      scene,
      vellum::segments_grob(
        s$x0,
        s$y0,
        s$x1,
        s$y1,
        sketch = .sketch_bump(sk, gi),
        gp = .gp_stroke(col, alpha, idx[1], lwd, lty)
      ),
      # PROVENANCE: each error bar is one datum (row-preserving). The segments are
      # emitted as [bars, lower caps, upper caps] (each `idx`-ordered), so a bar's
      # up-to-three segments all resolve to — and are keyed by — the same row.
      rows = if (caps) c(idx, idx, idx) else idx
    )
    gi <- gi + 1L
  }
  scene
}

.emit_linerange <- function(scene, L, scales) {
  .emit_errorbar(scene, L, scales, caps = FALSE)
}

# Straight segments from (x, y) to (xend, yend), batched per colour group.
.emit_segment <- function(scene, L, scales) {
  n <- L$n
  if (
    isTRUE(L$stat_params$auto) && n > .DATASHADE_AUTO && .can_datashade(scales)
  ) {
    return(.emit_segment_datashade(scene, L, scales))
  }
  x0 <- rep_len(scales$x$map(L$values$x), n)
  y0 <- rep_len(scales$y$map(L$values$y), n)
  x1 <- rep_len(scales$x$map(L$values$xend), n)
  y1 <- rep_len(scales$y$map(L$values$yend), n)
  col <- rep_len(.aes_colour(L, scales, "black"), n)
  alpha <- rep_len(.aes_alpha(L, scales, NA_real_), n)
  lwd <- .aes_param(L, "linewidth", 1)
  lty <- .resolve_lty(L, scales, n)
  sk <- .mark_sketch(L, scales)

  gi <- 0L
  for (idx in .style_groups(n, list(col = col, alpha = alpha, lty = lty))) {
    s <- .seg_units(
      scales,
      vellum::vl_unit(x0[idx], "native"),
      vellum::vl_unit(y0[idx], "native"),
      vellum::vl_unit(x1[idx], "native"),
      vellum::vl_unit(y1[idx], "native")
    )
    scene <- .draw(
      scene,
      vellum::segments_grob(
        s$x0,
        s$y0,
        s$x1,
        s$y1,
        sketch = .sketch_bump(sk, gi),
        gp = .gp_stroke(col, alpha, idx[1], lwd, lty)
      ),
      rows = idx
    )
    gi <- gi + 1L
  }
  scene
}
