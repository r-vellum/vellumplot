#' @include classes.R
NULL

# Mark emitters: graph marks (edges, flows, node pies, chord).

# A chord diagram (`L$chord`, sectors + ribbons in native circle coords centred
# at the origin). Ribbons are drawn first (filled Bezier polygons, under), then
# the sector band (one batched sector grob), then the node labels. `sector_grob`
# and the ribbon points share vellum's y-up frame, so arcs and ribbons align.
.CHORD_RIBBON_ALPHA <- 0.6
.CHORD_LABEL_FS <- 8
.CHORD_GAP <- 0.06 # radial inset of a ribbon's target end for `direction = "gap"`

# Append an alpha byte to a `#RRGGBB` colour, for per-stop gradient opacity.
.chord_rgba <- function(hex, a) {
  paste0(hex, sprintf("%02X", pmin(255L, pmax(0L, as.integer(round(a * 255))))))
}

.emit_chord <- function(scene, L, scales) {
  ch <- L$chord
  sec <- ch$sectors
  rib <- ch$ribbons
  if (!nrow(sec)) {
    return(scene)
  }
  r_in <- .CHORD_R_IN
  dir <- L$params$direction %||% "gradient"
  grad <- dir %in% c("gradient", "both")
  # "gap" pulls the target end short of the ring, leaving a directional gap.
  r_tgt <- if (dir %in% c("gap", "both")) r_in - .CHORD_GAP else r_in

  # Ribbons: a closed polygon bounded by the source sub-arc, a Bezier through the
  # centre to the target sub-arc, the target sub-arc, and a Bezier back. The
  # source end sits on the ring; the target end may be inset (gap) and/or faded
  # (gradient), so direction reads as solid-source -> faint/short-target.
  for (i in seq_len(nrow(rib))) {
    s <- .chord_arc(r_in, rib$sa0[i], rib$sa1[i])
    t <- .chord_arc(r_tgt, rib$ta0[i], rib$ta1[i])
    ns <- length(s$x)
    nt <- length(t$x)
    b1 <- .chord_bezier(s$x[ns], s$y[ns], t$x[1], t$y[1])
    b2 <- .chord_bezier(t$x[nt], t$y[nt], s$x[1], s$y[1])
    px <- c(s$x, b1$x, t$x, b2$x)
    py <- c(s$y, b1$y, t$y, b2$y)
    xy <- .xy_path(scales, px, py)
    if (grad) {
      # Alpha fade along the source->target axis (opaque source, faint target).
      sm <- (rib$sa0[i] + rib$sa1[i]) / 2
      tm <- (rib$ta0[i] + rib$ta1[i]) / 2
      fill <- vellum::linear_gradient(
        c(
          .chord_rgba(rib$colour[i], 0.75),
          .chord_rgba(rib$colour[i], 0.08)
        ),
        x1 = scales$x$map(r_in * cos(sm)),
        y1 = scales$y$map(r_in * sin(sm)),
        x2 = scales$x$map(r_tgt * cos(tm)),
        y2 = scales$y$map(r_tgt * sin(tm)),
        units = "native"
      )
      alpha <- 1
    } else {
      fill <- rib$colour[i]
      alpha <- .CHORD_RIBBON_ALPHA
    }
    scene <- .draw(
      scene,
      vellum::polygon_grob(
        xy$x,
        xy$y,
        gp = vellum::vl_gpar(fill = fill, col = NA, alpha = alpha)
      )
    )
  }

  # Sector band: one batched sector grob (centre at native origin).
  n <- nrow(sec)
  scene <- .draw(
    scene,
    vellum::sector_grob(
      x = vellum::vl_unit(rep(0, n), "native"),
      y = vellum::vl_unit(rep(0, n), "native"),
      r0 = vellum::vl_unit(rep(r_in, n), "native"),
      r1 = vellum::vl_unit(rep(.CHORD_R_OUT, n), "native"),
      theta0 = sec$theta0,
      theta1 = sec$theta1,
      fill = sec$colour,
      gp = vellum::vl_gpar(col = "white", lwd = 0.5)
    )
  )

  # Node labels radial, kept upright, and anchored at their *inner* end just
  # outside the ring so the text runs outward and never overlaps the sectors.
  # Upright-flipping reverses the reading direction on the left half, so the
  # justification that means "outward" flips with it.
  if (isTRUE(L$params$label)) {
    fs <- .CHORD_LABEL_FS
    rl <- .CHORD_R_OUT + 0.04
    mid <- (sec$theta0 + sec$theta1) / 2
    for (i in seq_len(n)) {
      deg <- ((mid[i] * 180 / pi + 180) %% 360) - 180
      flip <- deg > 90 || deg <= -90 # left half: label is flipped upright
      scene <- .draw(
        scene,
        vellum::text_grob(
          sec$node[i],
          vellum::vl_unit(rl * cos(mid[i]), "native"),
          vellum::vl_unit(rl * sin(mid[i]), "native"),
          just = c(if (flip) "right" else "left", "centre"),
          rot = .upright_rot(mid[i], "radius"),
          gp = vellum::vl_gpar(fontsize = fs, col = "black")
        )
      )
    }
  }
  scene
}

# Network edges: straight lines from (x, y) to (xend, yend), batched by style
# (colour / alpha / width) into one segments_grob per group -- so a graph with a
# handful of edge styles emits a handful of grobs regardless of edge count.
# Parallel / reciprocal offsets are baked into the coordinates by vgraph(); a
# self-loop is an edge whose endpoints coincide, drawn here as a small loop
# (nested outward when a vertex has several). Edge width maps through the
# edge-width scale when present. Optional arrowheads mark directed edges.
.emit_edges <- function(scene, L, scales) {
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
  col <- rep_len(.aes_edge_colour(L, scales, "grey40"), n)
  alpha <- rep_len(.aes_edge_alpha(L, scales, NA_real_), n)
  lwd <- rep_len(.edge_width(L, scales, 0.5), n)
  lty <- .aes_edge_linetype(L, scales, NULL)
  lty <- if (is.null(lty)) NULL else rep_len(lty, n)
  # `arrow` is resolved to a vl_arrow spec (or NULL) at mark-build time, so a
  # bare `arrow = TRUE` and an explicit `arrow = vl_arrow(...)` share one path.
  arr <- L$stat_params$arrow
  # Straight edges can be sketched; self-loops (loop_grob) are always crisp.
  sk <- .mark_sketch(L, scales)

  # Per-edge endpoint node radii (mm), for exact node-boundary capping. vellum
  # resolves the mm caps in device space at render, so this is correct at any
  # size/dpi and for arbitrary per-vertex sizes -- no native-per-mm estimate.
  gh <- scales$graph
  # A small gap (mm) past the node radius so the edge/arrowhead clears the marker.
  gap <- 0.4

  loop <- x0 == x1 & y0 == y1
  si <- which(!loop)
  # Bundle the resolved per-edge arrays for the routing helpers below. Non-loop
  # edges are drawn straight (default), as an orthogonal elbow (hierarchies), or
  # as a source->target gradient -- each still made of straight segments (no
  # curvature, a design non-goal). Self-loops always take the loop_grob path.
  E <- list(
    x0 = x0,
    y0 = y0,
    x1 = x1,
    y1 = y1,
    col = col,
    alpha = alpha,
    lwd = lwd,
    lty = lty,
    arr = arr,
    sk = sk,
    gh = gh,
    gap = gap
  )
  routing <- L$stat_params$routing %||% "straight"
  E$elbow_at <- L$stat_params$elbow_at %||% "mid"
  E$elbow_axis <- L$stat_params$elbow_axis %||% "auto"
  if (length(si)) {
    scene <- if (identical(routing, "elbow")) {
      .emit_edges_elbow(scene, si, E, scales)
    } else if (isTRUE(L$stat_params$gradient)) {
      .emit_edges_gradient(scene, si, E, scales)
    } else {
      .emit_edges_straight(scene, si, E, scales)
    }
  }

  # Self-loops: vellum's teardrop loop_grob (igraph shape) anchored at the vertex,
  # sized in mm so it tracks the node marker, feet on the node boundary (`foot`).
  # Each loop points into the largest gap between the vertex's incident edges
  # (the igraph "flower-petal" placement, precomputed in .loop_geometry), so loops
  # land in empty space; several on a vertex spread across the gap. A directed
  # loop carries an arrowhead tangent at the returning foot.
  li <- which(loop)
  if (length(li)) {
    cx <- mean(c(x0, x1))
    cy <- mean(c(y0, y1))
    for (j in li) {
      node_r_mm <- if (!is.null(gh)) gh$end_cap[j] else 2
      size_mm <- node_r_mm * 5 # size/0.3 = device bulge; clears the marker
      ang <- gh$loop_angle[j] %||% NA_real_
      if (!is.finite(ang)) {
        ang <- atan2(y0[j] - cy, x0[j] - cx) # fallback: away from centre
        if (!is.finite(ang)) {
          ang <- pi / 2
        }
      }
      # narrow the petal when the incident-edge gap is tight (rigraph factor)
      narrow <- gh$loop_narrow[j] %||% 1
      xy <- .xy_units(scales, x0[j], y0[j])
      a <- alpha[j]
      scene <- .draw(
        scene,
        vellum::loop_grob(
          xy$x,
          xy$y,
          size = vellum::vl_unit(size_mm, "mm"),
          foot = vellum::vl_unit(node_r_mm, "mm"),
          angle = ang,
          width = narrow,
          arrow = arr,
          gp = vellum::vl_gpar(
            col = col[j],
            lwd = lwd[j],
            lty = if (is.null(lty)) NULL else lty[j],
            alpha = gp_alpha(a)
          )
        )
      )
    }
  }
  scene
}

# Straight edges: one batched segments_grob per (colour, alpha, width, linetype)
# style group, each end capped at its node boundary (mm) and parallel/reciprocal
# edges spread by the device-space offset. The default routing.
.emit_edges_straight <- function(scene, si, E, scales) {
  grp_lwd <- round(E$lwd, 2)
  for (idx in .style_groups(
    length(si),
    list(
      col = E$col[si],
      alpha = E$alpha[si],
      lwd = grp_lwd[si],
      lty = if (is.null(E$lty)) NULL else E$lty[si]
    )
  )) {
    g <- si[idx]
    a <- E$alpha[g[1]]
    s <- .seg_units(
      scales,
      vellum::vl_unit(E$x0[g], "native"),
      vellum::vl_unit(E$y0[g], "native"),
      vellum::vl_unit(E$x1[g], "native"),
      vellum::vl_unit(E$y1[g], "native")
    )
    start_cap <- if (!is.null(E$gh)) {
      vellum::vl_unit(E$gh$start_cap[g] + E$gap, "mm")
    }
    end_cap <- if (!is.null(E$gh)) {
      vellum::vl_unit(E$gh$end_cap[g] + E$gap, "mm")
    }
    offset <- if (!is.null(E$gh)) vellum::vl_unit(E$gh$offset[g], "mm")
    scene <- .draw(
      scene,
      vellum::segments_grob(
        s$x0,
        s$y0,
        s$x1,
        s$y1,
        arrow = E$arr,
        start_cap = start_cap,
        end_cap = end_cap,
        offset = offset,
        sketch = E$sk,
        gp = vellum::vl_gpar(
          col = E$col[g[1]],
          lwd = E$lwd[g[1]],
          lty = if (is.null(E$lty)) NULL else E$lty[g[1]],
          alpha = gp_alpha(a)
        )
      ),
      rows = g
    )
  }
  scene
}

# The orthogonal step for one edge: a right-angle polyline (no curvature). The
# step runs along whichever axis the endpoints are farther apart on, so a
# top-down tree bends vertically and a left-right one horizontally, keeping
# sibling edges consistent.
# `at` places the corner along the primary axis: `"mid"` (default; the S-bend
# used by tree/DAG layouts), or `"start"`/`"end"` for the corner at the source or
# target coordinate -- `"start"` gives the dendrogram *bracket* (siblings share a
# bar at the parent's level). `axis` forces the primary axis (`"v"` = corner
# moves in y, `"h"` = in x); `"auto"` picks the longer side (the historical
# behaviour). Defaults reproduce the original midpoint elbow exactly.
.elbow_points <- function(x0, y0, x1, y1, at = "mid", axis = "auto") {
  vertical <- switch(
    axis,
    v = TRUE,
    h = FALSE,
    abs(y1 - y0) >= abs(x1 - x0)
  )
  frac <- switch(at, start = 0, end = 1, 0.5)
  if (vertical) {
    ym <- y0 + (y1 - y0) * frac
    list(x = c(x0, x0, x1, x1), y = c(y0, ym, ym, y1))
  } else {
    xm <- x0 + (x1 - x0) * frac
    list(x = c(x0, xm, xm, x1), y = c(y0, y0, y1, y1))
  }
}

# Elbow (orthogonal) edges for tree / DAG / dendrogram layouts. Each edge is a
# right-angle polyline drawn as its own lines_grob -- `lines_grob` caps trim the
# whole path, so per-edge node-boundary caps (mm) and the target arrowhead need
# one grob per edge (fine: elbows are for hierarchies, not hairballs). No
# curvature, no parallel offset.
.emit_edges_elbow <- function(scene, si, E, scales) {
  for (e in si) {
    p <- .elbow_points(
      E$x0[e],
      E$y0[e],
      E$x1[e],
      E$y1[e],
      at = E$elbow_at,
      axis = E$elbow_axis
    )
    xy <- .xy_units(scales, p$x, p$y)
    start_cap <- if (!is.null(E$gh)) {
      vellum::vl_unit(E$gh$start_cap[e] + E$gap, "mm")
    }
    end_cap <- if (!is.null(E$gh)) {
      vellum::vl_unit(E$gh$end_cap[e] + E$gap, "mm")
    }
    scene <- .draw(
      scene,
      vellum::lines_grob(
        xy$x,
        xy$y,
        arrow = E$arr,
        start_cap = start_cap,
        end_cap = end_cap,
        sketch = E$sk,
        gp = vellum::vl_gpar(
          col = E$col[e],
          lwd = E$lwd[e],
          lty = if (is.null(E$lty)) NULL else E$lty[e],
          alpha = gp_alpha(E$alpha[e])
        )
      ),
      rows = e
    )
  }
  scene
}

# Number of straight sub-segments a gradient edge is split into.
.GRADIENT_SEGMENTS <- 20L

# Source->target gradient edges: each straight edge is cut into `.GRADIENT_SEGMENTS`
# pieces whose opacity ramps from faint (source) to full (target), so direction
# reads without an arrowhead (igraph `edge.gradient`). All edges' piece `j` share
# one opacity, so the whole layer emits `K x styles` batched segments_grob calls.
# Nodes overdraw the endpoints (z-order), so no caps/arrows are needed here.
.emit_edges_gradient <- function(scene, si, E, scales) {
  k <- .GRADIENT_SEGMENTS
  grp_lwd <- round(E$lwd, 2)
  groups <- .style_groups(
    length(si),
    list(
      col = E$col[si],
      alpha = E$alpha[si],
      lwd = grp_lwd[si],
      lty = if (is.null(E$lty)) NULL else E$lty[si]
    )
  )
  for (j in seq_len(k)) {
    t0 <- (j - 1L) / k
    t1 <- j / k
    ramp <- 0.12 + 0.88 * ((j - 0.5) / k) # faint at source -> opaque at target
    for (idx in groups) {
      g <- si[idx]
      base_a <- E$alpha[g[1]]
      a <- (if (is.na(base_a)) 1 else base_a) * ramp
      ax <- E$x0[g] + t0 * (E$x1[g] - E$x0[g])
      ay <- E$y0[g] + t0 * (E$y1[g] - E$y0[g])
      bx <- E$x0[g] + t1 * (E$x1[g] - E$x0[g])
      by <- E$y0[g] + t1 * (E$y1[g] - E$y0[g])
      s <- .seg_units(
        scales,
        vellum::vl_unit(ax, "native"),
        vellum::vl_unit(ay, "native"),
        vellum::vl_unit(bx, "native"),
        vellum::vl_unit(by, "native")
      )
      scene <- .draw(
        scene,
        vellum::segments_grob(
          s$x0,
          s$y0,
          s$x1,
          s$y1,
          sketch = E$sk,
          gp = vellum::vl_gpar(
            col = E$col[g[1]],
            lwd = E$lwd[g[1]],
            lty = if (is.null(E$lty)) NULL else E$lty[g[1]],
            alpha = gp_alpha(a)
          )
        ),
        rows = g
      )
    }
  }
  scene
}

# Reconstruct a graph over unique edge endpoints (dropping self-loops), for the
# edge-bundle and flow-map layouts. Returns the igraph `g`, the unique node
# coordinates `pts`, and `keep` (the non-loop edge indices, for mapping results
# back to original-edge aesthetics), or NULL when nothing survives.
.reconstruct_edge_graph <- function(x0, y0, x1, y1, directed) {
  loop <- x0 == x1 & y0 == y1
  keep <- which(!loop)
  if (!length(keep)) {
    return(NULL)
  }
  src <- cbind(x0[keep], y0[keep])
  tgt <- cbind(x1[keep], y1[keep])
  pts <- unique(rbind(src, tgt))
  key <- function(m) paste(m[, 1L], m[, 2L], sep = "\r")
  pk <- key(pts)
  g <- igraph::graph_from_edgelist(
    cbind(match(key(src), pk), match(key(tgt), pk)),
    directed = directed
  )
  list(g = g, pts = pts, keep = keep)
}

# Bundled edges: draw each edge as a curved trunk instead of a straight line.
# The geometry is delegated to edgebundle (the "delegate the algorithm, own the
# drawing" pattern shared with graphlayouts): reconstruct an igraph + node
# coordinates from the edge endpoints, hand them to edgebundle::edge_bundle() for
# the chosen algorithm, and draw the returned polylines with this layer's edge
# aesthetics. Bundling runs in the layout's own (native) coordinate space, so the
# returned paths come back in native coords and map through .xy_path once, exactly
# like a straight edge. Self-loops carry no geometry to bundle and are skipped.
.emit_edge_bundle <- function(scene, L, scales) {
  .need_pkg("edgebundle", "mark_edge_bundle()")
  .need_pkg("igraph", "mark_edge_bundle()")
  n <- L$n
  x0 <- rep_len(scales$x$map(L$values$x), n)
  y0 <- rep_len(scales$y$map(L$values$y), n)
  x1 <- rep_len(scales$x$map(L$values$xend), n)
  y1 <- rep_len(scales$y$map(L$values$yend), n)
  col <- rep_len(.aes_edge_colour(L, scales, "grey40"), n)
  # Bundled trunks overlap heavily, so a faint default alpha lets density read.
  alpha <- rep_len(.aes_edge_alpha(L, scales, 0.3), n)
  lwd <- rep_len(.edge_width(L, scales, 0.5), n)
  lty <- .aes_edge_linetype(L, scales, NULL)
  lty <- if (is.null(lty)) NULL else rep_len(lty, n)
  sk <- .mark_sketch(L, scales)

  # Reconstruct nodes (unique endpoints) and a directed igraph over them. `keep`
  # drops self-loops (degenerate for bundling); the group->edge index below maps
  # back through it to recover each trunk's original-edge aesthetics.
  rec <- .reconstruct_edge_graph(x0, y0, x1, y1, directed = TRUE)
  if (is.null(rec)) {
    return(scene)
  }
  g <- rec$g
  pts <- rec$pts
  keep <- rec$keep

  type <- L$stat_params$type %||% "force"
  args <- c(
    list(object = g, xy = pts, type = type),
    L$stat_params$params %||% list()
  )
  b <- do.call(edgebundle::edge_bundle, args)

  # `group` labels each returned path; it is the edge index for most algorithms
  # and "<edge>.<stub>" for type = "stub" (two half-paths per edge). Stripping the
  # decimal recovers the position within `keep`, hence the original edge row.
  edge_of <- keep[as.integer(sub("\\..*$", "", as.character(b$group)))]
  paths <- split(seq_len(nrow(b)), b$group)
  for (idx in paths) {
    e <- edge_of[idx[1L]]
    xy <- .xy_path(scales, b$x[idx], b$y[idx])
    scene <- .draw(
      scene,
      vellum::lines_grob(
        xy$x,
        xy$y,
        sketch = sk,
        gp = vellum::vl_gpar(
          col = col[e],
          lwd = lwd[e],
          lty = if (is.null(lty)) NULL else lty[e],
          alpha = gp_alpha(alpha[e])
        )
      ),
      rows = e
    )
  }
  scene
}

# Flow map: a one-to-many tree from `root` fanning out to every destination
# along smooth merging branches whose width tracks the flow volume (the Minard
# idiom). Like `.emit_edge_bundle`, the graph + node xy are reconstructed from the
# edge endpoints (native coords) and the algorithm is delegated to edgebundle; the
# root is located as the reconstructed point nearest the coordinate resolved at
# mark-build time. `flow_tree` (spiral) and the `tnss_*` pipeline (Steiner) both
# return per-branch paths with a `flow` column; branches are drawn as constant-flow
# runs, each a `lines_grob` whose width comes from `flow` (round caps join them).
.emit_flow_map <- function(scene, L, scales) {
  .need_pkg("edgebundle", "mark_flow_map()")
  .need_pkg("igraph", "mark_flow_map()")
  sp <- L$stat_params
  type <- sp$type %||% "spiral"
  n <- L$n
  x0 <- rep_len(scales$x$map(L$values$x), n)
  y0 <- rep_len(scales$y$map(L$values$y), n)
  x1 <- rep_len(scales$x$map(L$values$xend), n)
  y1 <- rep_len(scales$y$map(L$values$yend), n)

  rec <- .reconstruct_edge_graph(x0, y0, x1, y1, directed = FALSE)
  if (is.null(rec)) {
    return(scene)
  }
  g <- rec$g
  pts <- rec$pts
  keep <- rec$keep
  igraph::E(g)$weight <- rep_len(sp$weight %||% 1, n)[keep]
  # Root: the reconstructed point nearest the build-time root coordinate, mapped
  # into the same native space as the endpoints.
  rxy <- c(scales$x$map(sp$root_xy[1]), scales$y$map(sp$root_xy[2]))
  root_i <- which.min((pts[, 1] - rxy[1])^2 + (pts[, 2] - rxy[2])^2)

  fm <- .flow_paths(type, g, pts, root_i, sp$params %||% list())
  if (is.null(fm) || !nrow(fm)) {
    return(scene)
  }

  # Map the computed flow onto the drawn width range (linewidth units), bounded so
  # a 1000x flow range stays legible rather than swamping the panel.
  wr <- sp$width_range %||% c(0.3, 3)
  fr <- range(fm$flow)
  wmap <- if (diff(fr) < .Machine$double.eps) {
    function(f) mean(wr)
  } else {
    function(f) wr[1L] + (f - fr[1L]) / (fr[2L] - fr[1L]) * (wr[2L] - wr[1L])
  }

  col <- sp$color %||% "#B2182B"
  a <- sp$alpha %||% 1
  sk <- .mark_sketch(L, scales)

  # One grob per branch, with the width varying smoothly along it. Each branch
  # used to be split into runs of EQUAL flow and stroked separately, so every
  # width change landed on a join -- round caps hid the step, but the staircase
  # was there. `lwd_profile` carries one multiplier per vertex and the engine
  # builds the ribbon at render, inside the panel viewport, so the taper is
  # continuous and still tracks a panel measured after axes and legends.
  #
  # `lwd = 1` so the profile IS the width in lwd units, exactly what `wmap()`
  # returns. `sketch` is not passed: it cannot be combined with a profile (it
  # would jitter the ribbon's outline rather than the pen), so a sketched flow
  # map keeps the plain stroke below.
  for (idx in split(seq_len(nrow(fm)), fm$grp)) {
    wv <- wmap(fm$flow[idx])
    if (!is.null(sk) || !any(wv > 0)) {
      # Sketched, or a degenerate all-zero width range: stroke it uniformly.
      xy <- .xy_path(scales, fm$x[idx], fm$y[idx])
      scene <- .draw(
        scene,
        vellum::lines_grob(
          xy$x,
          xy$y,
          sketch = sk,
          gp = vellum::vl_gpar(
            col = col,
            lwd = mean(wv),
            alpha = gp_alpha(a),
            lineend = "round",
            linejoin = "round"
          )
        )
      )
      next
    }
    d <- .xy_path_w(scales, fm$x[idx], fm$y[idx], wv)
    scene <- .draw(
      scene,
      vellum::lines_grob(
        d$x,
        d$y,
        lwd_profile = d$w,
        gp = vellum::vl_gpar(
          col = col,
          lwd = 1,
          alpha = gp_alpha(a),
          lineend = "round",
          linejoin = "round"
        )
      )
    )
  }
  scene
}

# Dispatch the flow-tree builder for `.emit_flow_map`, normalising both backends
# to a data frame of x / y / flow / grp (one group per drawable branch).
.flow_paths <- function(type, g, xy, root, params) {
  if (identical(type, "steiner")) {
    .need_pkg("interp", "mark_flow_map(type = \"steiner\")")
    dummies <- edgebundle::tnss_dummies(xy, root)
    gt <- do.call(
      edgebundle::tnss_tree,
      c(list(g = g, xy = xy, xydummy = dummies, root = root), params)
    )
    sm <- edgebundle::tnss_smooth(gt)
    data.frame(x = sm$x, y = sm$y, flow = sm$flow, grp = sm$destination)
  } else {
    fm <- do.call(
      edgebundle::flow_tree,
      c(list(object = g, xy = xy, root = root), params)
    )
    data.frame(x = fm$x, y = fm$y, flow = fm$flow, grp = fm$edge)
  }
}

# Pie / donut node glyphs: each vertex is drawn as a pie whose wedges are sized by
# the row's `cols` values (a composition), in absolute mm at the native vertex
# anchor (a proper circle under the aspect-locked graph panel). Wedges for
# category `j` across all nodes batch into one sector_grob, so the layer emits one
# grob per category. Categories take the qualitative palette (or `fill`) in order.
.emit_node_pie <- function(scene, L, scales) {
  sp <- L$stat_params
  weights <- sp$weights
  n <- nrow(weights)
  k <- ncol(weights)
  if (!n || !k) {
    return(scene)
  }
  xn <- rep_len(scales$x$map(L$values$x), n)
  yn <- rep_len(scales$y$map(L$values$y), n)
  cols_fill <- sp$fill %||% .qual_palette(k)
  # per-node fractions -> cumulative angles (a zero-weight node draws nothing)
  rs <- rowSums(weights, na.rm = TRUE)
  rs[!is.finite(rs) | rs == 0] <- 1
  frac <- weights / rs
  frac[!is.finite(frac)] <- 0
  cum <- t(apply(frac, 1L, function(z) cumsum(c(0, z)))) # n x (k + 1)
  r0 <- vellum::vl_unit(sp$inner * sp$size, "mm")
  r1 <- vellum::vl_unit(sp$size, "mm")
  for (j in seq_len(k)) {
    th0 <- cum[, j] * 2 * pi
    th1 <- cum[, j + 1L] * 2 * pi
    keep <- (th1 - th0) > 1e-9
    if (!any(keep)) {
      next
    }
    xy <- .xy_units(scales, xn[keep], yn[keep])
    scene <- .draw(
      scene,
      vellum::sector_grob(
        x = xy$x,
        y = xy$y,
        r0 = r0,
        r1 = r1,
        theta0 = th0[keep],
        theta1 = th1[keep],
        fill = cols_fill[j],
        gp = vellum::vl_gpar(
          col = sp$color,
          lwd = sp$linewidth,
          alpha = gp_alpha(sp$alpha)
        )
      ),
      rows = which(keep)
    )
  }
  scene
}
