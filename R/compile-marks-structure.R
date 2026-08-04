#' @include classes.R
NULL

# Mark emitters: structural diagrams (sankey, hierarchy, sunburst/treemap).

# A sankey flow diagram. The layout (`L$sankey`, native [0, 1] coords) is computed
# at resolve; here we draw filled Bézier ribbons (under) then node rects + labels.
.emit_sankey <- function(scene, L, scales) {
  lay <- L$sankey
  nodes <- lay$nodes
  rib <- lay$ribbons
  sk <- .mark_sketch(L, scales)

  # ribbons: a filled band between two horizontal cubic-Bézier edges. With
  # `flow_color = "gradient"` each ribbon fades (horizontally, in native coords)
  # from its source colour to its target colour; otherwise a flat source/target
  # fill (`rib$colour`).
  gradient <- identical(L$params$flow_color, "gradient")
  for (i in seq_len(nrow(rib))) {
    top <- .sankey_bezier(rib$xl[i], rib$sy1[i], rib$xr[i], rib$ty1[i])
    bot <- .sankey_bezier(rib$xl[i], rib$sy0[i], rib$xr[i], rib$ty0[i])
    px <- scales$x$map(c(top$x, rev(bot$x)))
    py <- scales$y$map(c(top$y, rev(bot$y)))
    xy <- .xy_path(scales, px, py)
    fill <- if (gradient) {
      vellum::linear_gradient(
        c(rib$colour_src[i], rib$colour_tgt[i]),
        x1 = scales$x$map(rib$xl[i]),
        x2 = scales$x$map(rib$xr[i]),
        units = "native",
        interpolation = "oklab"
      )
    } else {
      rib$colour[i]
    }
    scene <- .draw(
      scene,
      vellum::polygon_grob(
        xy$x,
        xy$y,
        gp = vellum::vl_gpar(
          fill = fill,
          col = NA,
          alpha = .SANKEY_RIBBON_ALPHA
        )
      )
    )
  }

  # nodes: a thin filled rect per node
  for (i in seq_len(nrow(nodes))) {
    xc <- scales$x$map((nodes$x0[i] + nodes$x1[i]) / 2)
    yc <- scales$y$map((nodes$y0[i] + nodes$y1[i]) / 2)
    w <- scales$x$map(nodes$x1[i]) - scales$x$map(nodes$x0[i])
    h <- scales$y$map(nodes$y1[i]) - scales$y$map(nodes$y0[i])
    r <- .rect_units(scales, xc, yc, w, h)
    scene <- .draw(
      scene,
      vellum::rect_grob(
        x = r$x,
        y = r$y,
        width = r$width,
        height = r$height,
        sketch = sk,
        gp = vellum::vl_gpar(fill = nodes$colour[i], col = NA)
      )
    )
  }

  # labels: source column (x0 ~ 0) to the left, everything else to the right;
  # with `show_values` the node's value is appended, e.g. "Grid (60)".
  if (isTRUE(L$params$label)) {
    show_values <- isTRUE(L$params$show_values)
    for (i in seq_len(nrow(nodes))) {
      left <- nodes$x0[i] < 1e-6
      xlab <- if (left) nodes$x0[i] else nodes$x1[i]
      just <- c(if (left) "right" else "left", "centre")
      txt <- nodes$name[i]
      if (show_values) {
        txt <- paste0(txt, " (", .label_number_default(nodes$value[i]), ")")
      }
      xy <- .xy_units(
        scales,
        scales$x$map(xlab),
        scales$y$map((nodes$y0[i] + nodes$y1[i]) / 2)
      )
      pad <- vellum::vl_unit(if (left) -1 else 1, "mm")
      scene <- .draw(
        scene,
        vellum::text_grob(
          txt,
          x = xy$x + pad,
          y = xy$y,
          just = just,
          gp = vellum::vl_gpar(fontsize = .SANKEY_LABEL_FONTSIZE)
        )
      )
    }
  }
  scene
}

# A space-filling hierarchy (sunburst / icicle / treemap / circlepack). The
# layout (`L$hierarchy`, native geometry centred at the origin) is computed at
# resolve; drawn as one batched grob per type in the aspect-locked square panel
# (domain [-1, 1], so native 0 is the centre). Nodes are painted shallow-first,
# the order the layout returns (BFS-ish), so nested children overlay their parent.
.HIER_LABEL_FS <- 8 # label font size (pt)

.emit_hierarchy <- function(scene, L, scales) {
  lay <- L$hierarchy
  if (!nrow(lay)) {
    return(scene)
  }
  type <- L$params$type %||% "sunburst"
  # Node fill comes from the (branch or mapped) fill scale; in branch mode fade
  # each node toward white by depth so levels within a branch stay distinct.
  base <- rep_len(.aes_colour(L, scales, "#7f7f7f"), nrow(lay))
  if (identical(L$hier_fill_mode, "branch")) {
    D <- max(lay$depth)
    amt <- (lay$depth - 1L) / max(1L, D - 1L) * (L$params$lighten %||% 0.6)
    lay$colour <- .lighten(base, amt)
  } else {
    lay$colour <- base
  }
  scene <- switch(
    type,
    sunburst = .emit_sunburst_regions(scene, lay),
    icicle = ,
    treemap = .emit_rect_regions(scene, lay),
    circlepack = .emit_circle_regions(scene, lay)
  )
  if (isTRUE(L$params$label)) {
    scene <- switch(
      type,
      sunburst = .emit_sunburst_labels(scene, L, lay),
      icicle = .emit_rect_labels(scene, L, lay, leaf_only = FALSE),
      treemap = .emit_rect_labels(scene, L, lay, leaf_only = TRUE),
      circlepack = .emit_circle_labels(scene, L, lay)
    )
  }
  scene
}

.emit_sunburst_regions <- function(scene, lay) {
  n <- nrow(lay)
  .draw(
    scene,
    vellum::sector_grob(
      x = vellum::vl_unit(rep(0, n), "native"),
      y = vellum::vl_unit(rep(0, n), "native"),
      r0 = vellum::vl_unit(lay$r0, "native"),
      r1 = vellum::vl_unit(lay$r1, "native"),
      theta0 = lay$theta0,
      theta1 = lay$theta1,
      fill = lay$colour,
      gp = vellum::vl_gpar(col = "white", lwd = 0.5)
    )
  )
}

# Treemap / icicle rectangles: `rect_grob`/`circle_grob` take a single gpar fill,
# so batch one grob per distinct colour (siblings share a branch+depth hue). The
# layout is shallow-first, so children still overlay parents within a colour.
.emit_rect_regions <- function(scene, lay) {
  for (col in unique(lay$colour)) {
    s <- lay[lay$colour == col, , drop = FALSE]
    scene <- .draw(
      scene,
      vellum::rect_grob(
        x = vellum::vl_unit((s$x0 + s$x1) / 2, "native"),
        y = vellum::vl_unit((s$y0 + s$y1) / 2, "native"),
        width = vellum::vl_unit(s$x1 - s$x0, "native"),
        height = vellum::vl_unit(s$y1 - s$y0, "native"),
        gp = vellum::vl_gpar(fill = col, col = "white", lwd = 0.5)
      )
    )
  }
  scene
}

# Circle-pack circles: one grob per distinct colour (drop zero-radius nodes).
.emit_circle_regions <- function(scene, lay) {
  lay <- lay[lay$cr > 0, , drop = FALSE]
  for (col in unique(lay$colour)) {
    s <- lay[lay$colour == col, , drop = FALSE]
    scene <- .draw(
      scene,
      vellum::circle_grob(
        x = vellum::vl_unit(s$cx, "native"),
        y = vellum::vl_unit(s$cy, "native"),
        r = vellum::vl_unit(s$cr, "native"),
        gp = vellum::vl_gpar(fill = col, col = "white", lwd = 0.5)
      )
    )
  }
  scene
}

# Draw the segment labels (and the optional centre/root label). Each label is
# placed at its segment's centroid, oriented to fit the wedge, kept upright, and
# inked for contrast; a label that fits in no allowed orientation is dropped.
.emit_sunburst_labels <- function(scene, L, lay) {
  fs <- .HIER_LABEL_FS
  # native -> mm: the aspect-locked panel spans the [-1, 1] square, so radius 1
  # (native) is ~half the shorter page side. Approximate (ignores gutters), which
  # is fine for a fit/hide heuristic.
  pg <- .mark_ctx$page
  page_mm <- if (!is.null(pg)) min(pg) * 25.4 else 6 * 25.4
  nat2mm <- page_mm / 2

  labels <- lay$id
  if (isTRUE(L$params$show_values)) {
    labels <- paste0(labels, " (", .label_number_default(lay$value), ")")
  }
  lw <- vapply(labels, function(s) .mm_tw(s, fs), numeric(1)) # label width, mm
  lh <- fs * 25.4 / 72 # a line's height, mm

  thetamid <- (lay$theta0 + lay$theta1) / 2
  rmid <- (lay$r0 + lay$r1) / 2
  arc_mm <- rmid * abs(lay$theta1 - lay$theta0) * nat2mm # tangential capacity
  rad_mm <- (lay$r1 - lay$r0) * nat2mm # radial capacity

  want <- L$params$orientation %||% "auto"
  # Per segment: choose an orientation whose capacity holds the label, else NA
  # (dropped). "auto" tries tangential -> radial (horizontal is modelled with the
  # same wedge-box capacity as tangential, so it adds nothing as an auto fallback
  # and is offered only on explicit `orientation = "horizontal"`).
  fit_orient <- function(i) {
    tang <- arc_mm[i] >= lw[i] && rad_mm[i] >= lh
    radial <- rad_mm[i] >= lw[i] && arc_mm[i] >= lh
    horiz <- arc_mm[i] >= lw[i] && rad_mm[i] >= lh # same box as tangential
    if (want == "tangential") {
      return(if (tang) "tangential" else NA_character_)
    }
    if (want == "radial") {
      return(if (radial) "radial" else NA_character_)
    }
    if (want == "horizontal") {
      return(if (horiz) "horizontal" else NA_character_)
    }
    if (tang) {
      "tangential"
    } else if (radial) {
      "radial"
    } else {
      NA_character_
    }
  }
  orient <- vapply(seq_len(nrow(lay)), fit_orient, character(1))
  ink <- .contrast_ink(lay$colour)

  keep <- which(!is.na(orient))
  for (i in keep) {
    rot <- switch(
      orient[i],
      tangential = .upright_rot(thetamid[i], "tangent"),
      radial = .upright_rot(thetamid[i], "radius"),
      0
    )
    scene <- .draw(
      scene,
      vellum::text_grob(
        labels[i],
        vellum::vl_unit(rmid[i] * cos(thetamid[i]), "native"),
        vellum::vl_unit(rmid[i] * sin(thetamid[i]), "native"),
        just = c("centre", "centre"),
        rot = rot,
        gp = vellum::vl_gpar(fontsize = fs, col = ink[i])
      )
    )
  }

  # Optional centre / root label.
  root <- attr(lay, "root")
  if (isTRUE(L$params$root_label) && !is.null(root)) {
    txt <- root$id
    if (isTRUE(L$params$show_values)) {
      txt <- paste0(txt, "\n", .label_number_default(root$value))
    }
    scene <- .draw(
      scene,
      vellum::text_grob(
        txt,
        vellum::vl_unit(0, "native"),
        vellum::vl_unit(0, "native"),
        just = c("centre", "centre"),
        gp = vellum::vl_gpar(fontsize = fs + 1, col = "black")
      )
    )
  }
  scene
}

# native -> mm for the aspect-locked [-1, 1] square panel: radius 1 is ~half the
# shorter page side. Approximate (ignores gutters), fine for a fit/hide heuristic.
.hier_nat2mm <- function() {
  pg <- .mark_ctx$page
  page_mm <- if (!is.null(pg)) min(pg) * 25.4 else 6 * 25.4
  page_mm / 2
}

# Treemap / icicle labels: centred horizontally in each node's rectangle, drawn
# where the text fits (width and a line's height both inside the rect). Treemap
# labels only leaves (parents sit under their children); icicle labels every
# node (bands do not overlap).
.emit_rect_labels <- function(scene, L, lay, leaf_only) {
  fs <- .HIER_LABEL_FS
  nat2mm <- .hier_nat2mm()
  labels <- lay$id
  if (isTRUE(L$params$show_values)) {
    labels <- paste0(labels, " (", .label_number_default(lay$value), ")")
  }
  lh <- fs * 25.4 / 72
  cand <- if (leaf_only) which(lay$leaf) else seq_len(nrow(lay))
  ink <- .contrast_ink(lay$colour)
  for (i in cand) {
    w_mm <- (lay$x1[i] - lay$x0[i]) * nat2mm
    h_mm <- (lay$y1[i] - lay$y0[i]) * nat2mm
    if (.mm_tw(labels[i], fs) > w_mm || lh > h_mm) {
      next
    }
    scene <- .draw(
      scene,
      vellum::text_grob(
        labels[i],
        vellum::vl_unit((lay$x0[i] + lay$x1[i]) / 2, "native"),
        vellum::vl_unit((lay$y0[i] + lay$y1[i]) / 2, "native"),
        just = c("centre", "centre"),
        gp = vellum::vl_gpar(fontsize = fs, col = ink[i])
      )
    )
  }
  scene
}

# Circle-pack labels: centred in each leaf's circle, drawn where the text chord
# fits (internal nodes are covered by their children, so only leaves are named).
.emit_circle_labels <- function(scene, L, lay) {
  fs <- .HIER_LABEL_FS
  nat2mm <- .hier_nat2mm()
  labels <- lay$id
  if (isTRUE(L$params$show_values)) {
    labels <- paste0(labels, " (", .label_number_default(lay$value), ")")
  }
  lh <- fs * 25.4 / 72
  ink <- .contrast_ink(lay$colour)
  for (i in which(lay$leaf & lay$cr > 0)) {
    r_mm <- lay$cr[i] * nat2mm
    half <- (r_mm * r_mm) - (lh / 2)^2 # widest chord at the text's height
    if (half <= 0) {
      next
    }
    if (.mm_tw(labels[i], fs) > 2 * sqrt(half)) {
      next
    }
    scene <- .draw(
      scene,
      vellum::text_grob(
        labels[i],
        vellum::vl_unit(lay$cx[i], "native"),
        vellum::vl_unit(lay$cy[i], "native"),
        just = c("centre", "centre"),
        gp = vellum::vl_gpar(fontsize = fs, col = ink[i])
      )
    )
  }
  scene
}
