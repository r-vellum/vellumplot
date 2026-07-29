#' @include classes.R compile-marks.R
NULL

# Label repulsion (ggrepel-style), resolved as an exact two-pass compile.
#
# The hard part of repel is that overlap must be judged in the *device* metric
# (a label's pixel box vs the panel's pixel extent), not in data units. vellum
# resolves the panel to device pixels only during its layout pass, but the plot
# size is fixed on the spec (`vplot(width=, height=, dpi=)`), so the panel
# geometry is deterministic at compile time and recoverable from `scene_model()`.
#
# So `as_vellum_scene()` on a spec with a repel layer compiles once (labels at
# their anchors), reads the panel's device-px rect + native ranges from
# `scene_model()`, runs a force-directed placement in that exact pixel space,
# maps the settled centres back to native, and recompiles with the labels moved
# (plus leader segments). No approximation, no vellum change. Scoped to a single
# cartesian panel for now (faceted / polar / warped plots error clearly).

# Iteration budget for the force relaxation. O(n^2) per step; labels number in
# the tens in practice, so this is cheap.
.REPEL_MAX_ITER <- 1500L

# A label may drift at most this fraction of the panel's shorter side from its
# anchor. See the leash note in `.repel_solve()`.
.REPEL_REACH_FRAC <- 0.22

# Do any of a spec's layers request repulsion?
.any_repel <- function(spec) {
  if (!S7::S7_inherits(spec, PlotSpec)) {
    return(FALSE)
  }
  for (L in spec@layers) {
    if (isTRUE(L@stat_params$repel$on)) {
      return(TRUE)
    }
  }
  FALSE
}

# native -> device-px affine for a panel row of `scene_model()$panels`. Returns
# `to_px(nx, ny)` and `to_native(px, py)`, exact inverses. Orientation of the y
# flip is irrelevant to overlap (symmetric in |dy|) as long as the pair round-
# trips, so we use a simple lo->px0 / hi->px1 convention.
.repel_affine <- function(panel) {
  xr <- panel$xscale_hi - panel$xscale_lo
  yr <- panel$yscale_hi - panel$yscale_lo
  sx <- (panel$px1 - panel$px0) / xr
  sy <- (panel$py1 - panel$py0) / yr
  list(
    to_px = function(nx, ny) {
      list(
        x = panel$px0 + (nx - panel$xscale_lo) * sx,
        y = panel$py0 + (ny - panel$yscale_lo) * sy
      )
    },
    to_native = function(px, py) {
      list(
        x = panel$xscale_lo + (px - panel$px0) / sx,
        y = panel$yscale_lo + (py - panel$py0) / sy
      )
    },
    px_lo = c(min(panel$px0, panel$px1), min(panel$py0, panel$py1)),
    px_hi = c(max(panel$px0, panel$px1), max(panel$py0, panel$py1))
  )
}

# Force-directed box placement. `cx,cy` are label-box centres (px), `w,h` their
# sizes (px, padded), `ax,ay` the anchor points (px). Boxes repel each other and
# the anchor points, spring back to their own anchor, and stay inside the panel
# `[lo, hi]` px rectangle. Deterministic given `seed`. Returns settled centres.
.repel_solve <- function(cx, cy, w, h, ax, ay, lo, hi, seed) {
  n <- length(cx)
  span <- max(hi - lo, 1)
  # A tiny seeded nudge breaks exact ties (co-located points) reproducibly.
  jit <- .with_seed(seed %||% 1L, stats::runif(2L * n, -1, 1)) * (span * 0.01)
  cx <- cx + jit[seq_len(n)]
  cy <- cy + jit[n + seq_len(n)]

  # A gentle spring and a small step keep the relaxation stable.
  spring <- 0.02
  step <- 0.45
  hw <- w / 2
  hh <- h / 2
  # A leash bounds how far a label may drift from its anchor. Without it, a
  # label pushed off a cluster of anchors keeps being repelled with nothing to
  # pull it back (the spring is deliberately weak), slides along a panel wall,
  # and ends up flung into a far corner with a panel-spanning leader. Bounding
  # the displacement to a fraction of the panel keeps every label local -- as
  # ggrepel does -- so over-dense cases degrade to nearby overlap, not flight.
  reach <- .REPEL_REACH_FRAC * min(hi - lo)

  for (iter in seq_len(.REPEL_MAX_ITER)) {
    fx <- numeric(n)
    fy <- numeric(n)
    for (i in seq_len(n)) {
      # box-box overlap: push apart along the centre difference
      dx <- cx[i] - cx
      dy <- cy[i] - cy
      ox <- (hw[i] + hw) - abs(dx)
      oy <- (hh[i] + hh) - abs(dy)
      hit <- ox > 0 & oy > 0
      hit[i] <- FALSE
      if (any(hit)) {
        sgnx <- ifelse(dx[hit] >= 0, 1, -1)
        sgny <- ifelse(dy[hit] >= 0, 1, -1)
        # push along whichever axis needs the smaller move (less disruptive)
        push_x <- ox[hit] <= oy[hit]
        fx[i] <- fx[i] + sum(ifelse(push_x, sgnx * ox[hit], 0))
        fy[i] <- fy[i] + sum(ifelse(push_x, 0, sgny * oy[hit]))
      }
      # box-point repulsion: keep the box off every anchor it covers
      pdx <- cx[i] - ax
      pdy <- cy[i] - ay
      pin <- abs(pdx) < hw[i] & abs(pdy) < hh[i]
      if (any(pin)) {
        ex <- hw[i] - abs(pdx[pin])
        ey <- hh[i] - abs(pdy[pin])
        px_move <- ex <= ey
        fx[i] <- fx[i] +
          sum(ifelse(px_move, ifelse(pdx[pin] >= 0, 1, -1) * ex, 0))
        fy[i] <- fy[i] +
          sum(ifelse(px_move, 0, ifelse(pdy[pin] >= 0, 1, -1) * ey))
      }
    }
    # spring each box back toward its own anchor
    fx <- fx + spring * (ax - cx)
    fy <- fy + spring * (ay - cy)
    cx <- cx + step * fx
    cy <- cy + step * fy
    # leash: no label may drift further than `reach` from its own anchor
    ddx <- cx - ax
    ddy <- cy - ay
    r <- sqrt(ddx^2 + ddy^2)
    far <- r > reach
    if (any(far)) {
      cx[far] <- ax[far] + ddx[far] / r[far] * reach
      cy[far] <- ay[far] + ddy[far] / r[far] * reach
    }
    # keep boxes within the panel
    cx <- pmin(pmax(cx, lo[1] + hw), hi[1] - hw)
    cy <- pmin(pmax(cy, lo[2] + hh), hi[2] - hh)
    if (max(abs(fx)) < 0.05 && max(abs(fy)) < 0.05) {
      break
    }
  }
  list(cx = cx, cy = cy)
}

# The point on a box's edge closest to the anchor, for a leader line's far end.
.repel_edge_point <- function(cx, cy, hw, hh, ax, ay) {
  dx <- ax - cx
  dy <- ay - cy
  if (dx == 0 && dy == 0) {
    return(c(cx, cy))
  }
  # scale the direction so it lands on the nearer box edge
  tx <- if (dx != 0) hw / abs(dx) else Inf
  ty <- if (dy != 0) hh / abs(dy) else Inf
  t <- min(tx, ty, 1)
  c(cx + dx * t, cy + dy * t)
}

# Compute one repel layer's settled label positions + leader segments (native),
# from its resolved anchors, the trained scales, and a panel's px geometry.
.repel_layer_solution <- function(L, scales, panel, dpi, p) {
  nx <- rep_len(scales$x$map(L$values$x), L$n)
  ny <- rep_len(scales$y$map(L$values$y), L$n)
  label <- rep_len(as.character(L$values$label), L$n)
  fs <- L$params$size %||% 8

  aff <- .repel_affine(panel)
  a <- aff$to_px(nx, ny)

  # measure each label in device px (inches * dpi), plus padding
  in2px <- dpi
  w_px <- vapply(
    label,
    function(s) vellum::vl_strwidth(s, fontsize = fs, unit = "in"),
    numeric(1)
  ) *
    in2px
  h_px <- vapply(
    label,
    function(s) vellum::vl_strheight(s, fontsize = fs, unit = "in"),
    numeric(1)
  ) *
    in2px
  box_pad <- (p$box_padding %||% 1) / 25.4 * dpi
  pt_pad <- (p$point_padding %||% 1) / 25.4 * dpi
  w <- w_px + 2 * box_pad
  h <- h_px + 2 * box_pad

  sol <- .repel_solve(
    a$x,
    a$y,
    w + 2 * pt_pad,
    h + 2 * pt_pad,
    a$x,
    a$y,
    aff$px_lo,
    aff$px_hi,
    p$seed
  )

  # leaders: from the anchor to the label box edge, when the label moved enough
  min_seg <- (p$min_segment_length %||% 2) / 25.4 * dpi
  moved <- sqrt((sol$cx - a$x)^2 + (sol$cy - a$y)^2)
  hw <- w / 2
  hh <- h / 2
  seg <- lapply(seq_len(L$n), function(i) {
    if (moved[i] < min_seg) {
      return(NULL)
    }
    edge <- .repel_edge_point(
      sol$cx[i],
      sol$cy[i],
      hw[i],
      hh[i],
      a$x[i],
      a$y[i]
    )
    far <- aff$to_native(edge[1], edge[2])
    list(x0 = nx[i], y0 = ny[i], x1 = far$x, y1 = far$y)
  })
  seg <- seg[!vapply(seg, is.null, logical(1))]

  disp <- aff$to_native(sol$cx, sol$cy)
  list(
    x = disp$x,
    y = disp$y,
    leaders = if (length(seg)) {
      data.frame(
        x0 = vapply(seg, `[[`, numeric(1), "x0"),
        y0 = vapply(seg, `[[`, numeric(1), "y0"),
        x1 = vapply(seg, `[[`, numeric(1), "x1"),
        y1 = vapply(seg, `[[`, numeric(1), "y1")
      )
    } else {
      NULL
    }
  )
}

# Attach a repel solution to each repel layer of `spec`, using the panel geometry
# from a provisional compile's `scene_model()`. Errors clearly where repel is not
# yet supported (facets / non-cartesian panels).
.attach_repel_solutions <- function(spec, sm) {
  # The data panels are named "panel-<row>-<col>"; other rows (e.g.
  # "plot-background") are structural. Repel needs exactly one data panel.
  data_panels <- sm$panels[grepl("^panel-", sm$panels$name), , drop = FALSE]
  if (nrow(data_panels) != 1L || is.na(data_panels$px0[1])) {
    cli::cli_abort(c(
      "{.arg repel} is only supported on a single cartesian panel for now.",
      i = "It cannot yet be combined with facets or a plot composition."
    ))
  }
  built <- .build_panels(spec)
  scales <- built$scales
  if (!is.null(scales$polar) || !is.null(scales$trans)) {
    cli::cli_abort(
      "{.arg repel} is not supported under {.fn coord_polar} / {.fn coord_trans}."
    )
  }
  panel <- as.list(data_panels[1, , drop = FALSE])
  resolved <- .resolve_layers(spec)
  layers <- spec@layers
  for (i in seq_along(layers)) {
    prm <- layers[[i]]@stat_params$repel
    if (!isTRUE(prm$on)) {
      next
    }
    L <- resolved[[i]]
    if (is.null(L$values$label)) {
      cli::cli_abort(
        "A repel {.fn mark_text} / {.fn mark_label} needs a {.arg label}."
      )
    }
    sol <- .repel_layer_solution(L, scales, panel, spec@dpi, prm)
    layers[[i]]@stat_params$repel$solution <- sol
  }
  spec@layers <- layers
  spec
}

# The repel params stored on a text/label layer, or NULL when repel is off.
.repel_params <- function(
  repel,
  box_padding,
  point_padding,
  min_segment_length,
  seed
) {
  if (!isTRUE(repel)) {
    return(NULL)
  }
  list(
    on = TRUE,
    box_padding = box_padding,
    point_padding = point_padding,
    min_segment_length = min_segment_length,
    seed = seed,
    solution = NULL
  )
}

# Draw a repel layer's leader lines (thin, in the label's ink colour). Called by
# the text/label emitters before the labels themselves.
.emit_repel_leaders <- function(scene, L, scales) {
  sol <- L$stat_params$repel$solution
  if (is.null(sol) || is.null(sol$leaders)) {
    return(scene)
  }
  seg <- sol$leaders
  col <- .text_colour(L, scales, "black")[1]
  a <- .xy_units(scales, seg$x0, seg$y0)
  b <- .xy_units(scales, seg$x1, seg$y1)
  .draw(
    scene,
    vellum::segments_grob(
      a$x,
      a$y,
      b$x,
      b$y,
      gp = vellum::vl_gpar(col = col, lwd = 0.4)
    )
  )
}
