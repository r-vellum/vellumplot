#' @include classes.R compile-marks.R
NULL

# Label repulsion (ggrepel-style), resolved by the engine's placement solver.
#
# `vl_place()` / `vl_repel()` (vellum) solve label collisions over the scene's
# *resolved device geometry* and express the answer as an absolute millimetre
# offset added on top of each label's existing coordinate. Because that offset is
# a compound `<anchor> + <mm>` unit resolved at render, repel is
# coordinate-system agnostic: faceted, polar and warped panels are just more
# boxes to the solver, and every panel is solved together. So repel here is a
# single post-compile pass over the built scene -- no second compile, no
# per-panel affine, and none of the old "single cartesian panel only" limit.
#
# Flow (`.repel_scene`): the marks compile once with each repel label emitted as
# a NAMED node ("repel:<panel>:<layer>:<group>"; a mark_label's rounded
# background as "repelbg:<...>"). Then: solve (`vl_place`), shift each label node
# -- and its background -- by the solved offset, and draw a leader from each
# moved label back to its anchor. Every other mark is an obstacle by default (a
# point under the label is avoided); an obstacle that *contains* a label is
# background, not a collision (vellum handles that).

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

# The repel params stored on a text/label layer, or NULL when repel is off.
# `point_padding`/`seed` are retained for back-compatibility but no longer steer
# the solve: the engine solver pads every box uniformly and is deterministic, so
# there is nothing for a seed to vary.
.repel_params <- function(
  repel,
  box_padding,
  point_padding,
  min_segment_length,
  seed,
  avoid = "marks"
) {
  if (!isTRUE(repel)) {
    return(NULL)
  }
  list(
    on = TRUE,
    box_padding = box_padding,
    min_segment_length = min_segment_length,
    # What the solve treats as obstacles: "marks" (the default -- scatter text
    # dodges every mark) or "labels" (direct series labels belong AT their line
    # end, so they may only be pushed off each other, never off the marks).
    avoid = avoid
  )
}

# The node name a repel label (or its background) carries, unique per panel so a
# faceted layer's per-panel copies never collide. `.mark_ctx$panel` is the
# enclosing panel (`.mark_panel_id()`: "panel-r-c", "cell-<k>" or
# "cell-<k>-panel-r-c"), or absent for a lone panel.
.repel_name <- function(gi, bg = FALSE) {
  pn <- .mark_panel_id()
  if (is.null(pn) || is.na(pn)) {
    pn <- "p"
  }
  sprintf(
    "%s:%s:%s:%d",
    if (bg) "repelbg" else "repel",
    pn,
    .mark_ctx$layer %||% 0L,
    gi
  )
}

# The node glyph under each repelled node label (VP-129), keyed by the label
# node's name: per label element, the glyph radius (mm, 0 where no glyph sits at
# the label's vertex) and the label's nudge off its vertex (device-oriented mm),
# so the leader can stop at the glyph's rim instead of crossing it to the centre.
# Noted while the marks compile, read by `.repel_leaders()`.
.repel_glyph_ctx <- new.env(parent = emptyenv())

.repel_glyphs_reset <- function() {
  rm(list = ls(.repel_glyph_ctx, all.names = TRUE), envir = .repel_glyph_ctx)
}

# The node glyphs of a panel's resolved layers: their native x/y and radius
# (mm), from `mark_nodes()` (the marker size is its radius) and
# `mark_node_pie()` (the pie radius). NULL when the panel has no repelled node
# labels or no node glyphs.
.node_glyphs <- function(resolved, scales) {
  wants <- any(vapply(
    resolved,
    function(L) {
      identical(L$mark, "node_text") && isTRUE(L$stat_params$repel$on)
    },
    logical(1)
  ))
  if (!wants) {
    return(NULL)
  }
  out <- list()
  for (L in resolved) {
    r <- switch(
      L$mark,
      nodes = rep_len(.aes_size(L, scales, 1), L$n),
      node_pie = .node_pie_radius(L, scales, L$n),
      NULL
    )
    if (is.null(r) || !L$n) {
      next
    }
    out[[length(out) + 1L]] <- data.frame(
      x = rep_len(as.numeric(L$values$x), L$n),
      y = rep_len(as.numeric(L$values$y), L$n),
      r = as.numeric(r)
    )
  }
  if (!length(out)) {
    return(NULL)
  }
  do.call(rbind, out)
}

# Note the glyph radius and nudge of the label elements `idx` of a node-label
# layer, drawn as the repel node `name` at vertices `x`/`y` (raw values).
.repel_note_glyphs <- function(name, L, idx, x, y, scales) {
  g <- scales$node_glyphs
  r <- numeric(length(idx))
  if (!is.null(g) && nrow(g)) {
    key <- function(a, b) sprintf("%.15g %.15g", a, b)
    gk <- key(g$x, g$y)
    lk <- key(as.numeric(x), as.numeric(y))
    # the largest glyph at each label's vertex (a pie over a node marker)
    big <- tapply(g$r, gk, max, na.rm = TRUE)
    r <- unname(big[lk])
    r[!is.finite(r)] <- 0
  }
  nu <- .nudge_mm(L, idx, scales)
  assign(
    name,
    list(
      r = r,
      nx = rep_len(as.numeric(nu$x %||% 0), length(idx)),
      ny = rep_len(as.numeric(nu$y %||% 0), length(idx))
    ),
    envir = .repel_glyph_ctx
  )
  invisible()
}

# The tuning for the whole-scene solve. All repel labels are solved together, so
# a single set applies; take it from the first repel layer.
.repel_scene_params <- function(spec) {
  for (L in spec@layers) {
    pr <- L@stat_params$repel
    if (isTRUE(pr$on)) {
      return(list(
        padding = pr$box_padding %||% 1,
        max_shift = 10,
        min_seg = pr$min_segment_length %||% 2
      ))
    }
  }
  list(padding = 1, max_shift = 10, min_seg = 2)
}

# Resolve label repulsion over a fully compiled scene: solve, shift each label
# (and its background) by the solved mm offset, then draw the leaders. Returns
# the edited scene. A no-op when the scene carries no named repel labels.
.repel_scene <- function(scene, spec) {
  labs <- grep("^repel:", vellum::node_names(scene), value = TRUE)
  if (!length(labs)) {
    return(scene)
  }
  p <- .repel_scene_params(spec)
  # When every repel layer is a direct-label layer ("labels" mode), the solver
  # avoids only the labels themselves -- an end-of-line label must not be shoved
  # off its own (or a neighbour's) polyline, whose bounding box spans the whole
  # diagonal. Otherwise keep the default (avoid every mark), as scatter text wants.
  avoid <- if (.repel_avoid_labels_only(spec)) labs else NULL
  sol <- vellum::vl_place(
    scene,
    labels = labs,
    avoid = avoid,
    padding = p$padding,
    max_shift = p$max_shift
  )
  if (!nrow(sol)) {
    return(scene)
  }
  scene <- .repel_shift(scene, sol)
  # Re-solve the shifted scene: the labels no longer overlap, so this returns
  # each label's box at its *moved* position (the residual dx/dy is ~0). Those
  # boxes are where the leaders must land.
  moved <- vellum::vl_place(
    scene,
    labels = labs,
    avoid = avoid,
    padding = p$padding,
    max_shift = p$max_shift
  )
  # The leaders are drawn after the plot's draw, so point the default-colour
  # lookup at the plot's theme again (its grey follows a dark theme's ink).
  old_geom <- .mark_ctx$geom
  old_ground <- .mark_ctx$geom_ground
  old_font <- .mark_ctx$text_font
  on.exit(assign("geom", old_geom, envir = .mark_ctx), add = TRUE)
  on.exit(assign("geom_ground", old_ground, envir = .mark_ctx), add = TRUE)
  on.exit(assign("text_font", old_font, envir = .mark_ctx), add = TRUE)
  .set_geom_ctx(.theme_of(spec))
  .repel_leaders(scene, sol, moved, spec@dpi, spec@height, p$min_seg)
}

# TRUE when every repel-enabled layer asks to avoid only the other labels (the
# direct-series-label mode). A mixed plot (any scatter-text repel) falls back to
# avoiding all marks, so those labels still dodge the geometry.
.repel_avoid_labels_only <- function(spec) {
  modes <- character(0)
  for (L in spec@layers) {
    pr <- L@stat_params$repel
    if (isTRUE(pr$on)) {
      modes <- c(modes, pr$avoid %||% "marks")
    }
  }
  length(modes) > 0L && all(modes == "labels")
}

# Shift each solved label node -- and its paired "repelbg:" background box, if
# any -- by the solved per-index mm offset. Mirrors `vellum::vl_repel()`'s edit,
# but also carries a mark_label's rounded background along with its text so the
# two move as one.
.repel_shift <- function(scene, sol) {
  present <- vellum::node_names(scene)
  for (nm in unique(sol$name)) {
    s <- sol[sol$name == nm, , drop = FALSE]
    s <- s[order(s$index), , drop = FALSE]
    scene <- .repel_move_node(scene, nm, s$dx, s$dy, present, s$index)
    bg <- sub("^repel:", "repelbg:", nm)
    scene <- .repel_move_node(scene, bg, s$dx, s$dy, present, s$index)
  }
  scene
}

# Add an absolute (mm) offset to a named node's anchor. A text label has no
# background, so the "repelbg:" name is skipped when it is not in `present`
# (`get_node()` aborts on an unknown name rather than returning NULL).
#
# `vl_place()` reports only the elements it could place -- it skips a label
# whose box is not finite -- so `dx`/`dy` can be shorter than the node. They are
# scattered onto the node's elements by `index` (1-based); an element the solver
# skipped stays where it is.
.repel_move_node <- function(scene, name, dx, dy, present, index = NULL) {
  if (!name %in% present) {
    return(scene)
  }
  node <- vellum::get_node(scene, name)
  m <- max(length(node@x), length(node@y), index, length(dx))
  ox <- numeric(m)
  oy <- numeric(m)
  index <- index %||% seq_along(dx)
  ox[index] <- dx
  oy[index] <- dy
  nx <- vctrs::vec_recycle(node@x, m)
  ny <- vctrs::vec_recycle(node@y, m)
  vellum::edit_node(
    scene,
    name,
    x = nx + vellum::vl_unit(ox, "mm"),
    y = ny + vellum::vl_unit(oy, "mm")
  )
}

# The point on a box's edge nearest the anchor, for a leader's far end (so the
# line stops at the box rather than crossing the text). Vectorised.
.repel_edge <- function(cx, cy, hw, hh, ax, ay) {
  dx <- ax - cx
  dy <- ay - cy
  tx <- ifelse(dx != 0, hw / abs(dx), Inf)
  ty <- ifelse(dy != 0, hh / abs(dy), Inf)
  t <- pmin(tx, ty, 1)
  list(x = cx + dx * t, y = cy + dy * t)
}

# The noted glyph radius and nudge (mm) of each solved label element (rows of
# `sol`); zero where none was noted.
.repel_glyph_lookup <- function(sol) {
  n <- nrow(sol)
  out <- list(r = numeric(n), nx = numeric(n), ny = numeric(n))
  for (i in seq_len(n)) {
    g <- .repel_glyph_ctx[[sol$name[i]]]
    j <- sol$index[i]
    if (is.null(g) || j > length(g$r)) {
      next
    }
    out$r[i] <- g$r[j]
    out$nx[i] <- g$nx[j]
    out$ny[i] <- g$ny[j]
  }
  out
}

# Draw a thin leader from each moved label back to its anchor. Drawn in device
# space at the scene root (px -> mm, y flipped from device-down to root-up), so
# it is coordinate-agnostic exactly like the offsets: it needs no panel viewport
# and works under any coord. `sol` supplies each label's ORIGINAL box (its
# anchor); `moved` the box after shifting. A leader is drawn only when the label
# travelled at least `min_seg` mm, and it ends at the moved box's nearest edge.
.repel_leaders <- function(scene, sol, moved, dpi, height_in, min_seg) {
  key <- function(d) paste(d$name, d$index, sep = "\r")
  m <- moved[match(key(sol), key(moved)), , drop = FALSE]
  ax <- (sol$x0 + sol$x1) / 2
  ay <- (sol$y0 + sol$y1) / 2
  mx <- (m$x0 + m$x1) / 2
  my <- (m$y0 + m$y1) / 2
  hw <- (m$x1 - m$x0) / 2
  hh <- (m$y1 - m$y0) / 2
  min_px <- min_seg / 25.4 * dpi
  dist <- sqrt((mx - ax)^2 + (my - ay)^2)
  keep <- is.finite(dist) & is.finite(mx) & dist >= min_px
  if (!any(keep)) {
    return(scene)
  }
  # A node label's leader runs from its vertex (the anchor less the label's
  # nudge) and stops at the node glyph's rim, plus the small gap edges leave,
  # rather than crossing the glyph to its centre (VP-129).
  gl <- .repel_glyph_lookup(sol)
  px_mm <- dpi / 25.4
  cx <- ax - gl$nx * px_mm
  cy <- ay + gl$ny * px_mm # device y runs down
  glyph <- gl$r > 0
  ox <- ifelse(glyph, cx, ax)
  oy <- ifelse(glyph, cy, ay)
  edge <- .repel_edge(mx, my, hw, hh, ox, oy)
  rim <- ifelse(glyph, (gl$r + 0.4) * px_mm, 0)
  len <- sqrt((edge$x - ox)^2 + (edge$y - oy)^2)
  # a label still touching its glyph needs no leader
  keep <- keep & (!glyph | (is.finite(len) & len > rim))
  if (!any(keep)) {
    return(scene)
  }
  t <- ifelse(glyph & len > 0, rim / len, 0)
  ax <- ox + (edge$x - ox) * t
  ay <- oy + (edge$y - oy) * t
  edge <- list(x = edge$x[keep], y = edge$y[keep])
  h_px <- height_in * dpi
  x2mm <- function(px) px / dpi * 25.4
  y2mm <- function(py) (h_px - py) / dpi * 25.4
  vellum::draw(
    scene,
    vellum::segments_grob(
      vellum::vl_unit(x2mm(ax[keep]), "mm"),
      vellum::vl_unit(y2mm(ay[keep]), "mm"),
      vellum::vl_unit(x2mm(edge$x), "mm"),
      vellum::vl_unit(y2mm(edge$y), "mm"),
      gp = vellum::vl_gpar(col = .geom_paint("grey50", "neutral"), lwd = 0.4)
    )
  )
}
