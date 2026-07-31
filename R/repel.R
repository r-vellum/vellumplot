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
  seed
) {
  if (!isTRUE(repel)) {
    return(NULL)
  }
  list(
    on = TRUE,
    box_padding = box_padding,
    min_segment_length = min_segment_length
  )
}

# The node name a repel label (or its background) carries, unique per panel so a
# faceted layer's per-panel copies never collide. `.mark_ctx$panel` is the
# enclosing panel ("panel-r-c" / "subplot-N"), or absent for a lone panel.
.repel_name <- function(gi, bg = FALSE) {
  pn <- .mark_ctx$panel
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
  sol <- vellum::vl_place(
    scene,
    labels = labs,
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
    padding = p$padding,
    max_shift = p$max_shift
  )
  .repel_leaders(scene, sol, moved, spec@dpi, spec@height, p$min_seg)
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
    scene <- .repel_move_node(scene, nm, s$dx, s$dy, present)
    bg <- sub("^repel:", "repelbg:", nm)
    scene <- .repel_move_node(scene, bg, s$dx, s$dy, present)
  }
  scene
}

# Add an absolute (mm) offset to a named node's anchor. A text label has no
# background, so the "repelbg:" name is skipped when it is not in `present`
# (`get_node()` aborts on an unknown name rather than returning NULL).
.repel_move_node <- function(scene, name, dx, dy, present) {
  if (!name %in% present) {
    return(scene)
  }
  node <- vellum::get_node(scene, name)
  nx <- vctrs::vec_recycle(node@x, length(dx))
  ny <- vctrs::vec_recycle(node@y, length(dy))
  vellum::edit_node(
    scene,
    name,
    x = nx + vellum::vl_unit(dx, "mm"),
    y = ny + vellum::vl_unit(dy, "mm")
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
  edge <- .repel_edge(
    mx[keep],
    my[keep],
    hw[keep],
    hh[keep],
    ax[keep],
    ay[keep]
  )
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
      gp = vellum::vl_gpar(col = "grey50", lwd = 0.4)
    )
  )
}
