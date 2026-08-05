#' @include classes.R vplot.R theme.R vgraph.R sankey.R
NULL

# The tree layouts (hierarchy fraction assignment; dendrogram leaf DFS in
# vgraph.R) recurse once per level, so recursion depth == tree depth. A
# pathologically deep tree (a near-chain) would overflow R's evaluation stack
# with a cryptic error; refuse past this bound instead. Far above any legible
# hierarchy/dendrogram (a balanced tree of depth 20 already holds ~1e6 leaves).
.MAX_TREE_DEPTH <- 1000L

# --- vhierarchy: space-filling hierarchy diagrams ---------------------------
#
# One constructor draws a tree four ways, differing only in the layout geometry:
#   sunburst   concentric ring sectors (radial partition)
#   icicle     rectangular partition (adjacency along one axis)
#   treemap    squarified nested rectangles
#   circlepack circles enclosed in circles
# All take the same *parent list* (`id`, `parent` NA at the root, `value` for
# leaves; an internal node's value is the sum of its children) and render R-side
# into the axis-free, aspect-locked square panel. Like `vsankey()`/`vgraph()` the
# layout is computed here and drawn with batched vellum grobs.
#
# The circle-packing primitives (`.pack_siblings`/`.pack_enclose`) live in
# `hierarchy-pack.R`.

# Shared core: validate the parent list and compute, per node, its depth, subtree
# value, the [f0, f1) fraction it owns within [0, 1) (children split a parent's
# span by value, in input order), the depth-1 branch it belongs to, and a fill
# (branch hue lightened toward white with depth). Returns everything the per-type
# geometry helpers need. Errors identify the failing node.
.hierarchy_tree <- function(id, parent, value, call = rlang::caller_env()) {
  id <- as.character(id)
  parent <- as.character(parent)
  value <- as.numeric(value)
  n <- length(id)
  if (!n) {
    cli::cli_abort("{.fn vhierarchy} needs at least one node.", call = call)
  }
  if (anyDuplicated(id)) {
    cli::cli_abort("{.fn vhierarchy} {.arg id}s must be unique.", call = call)
  }

  is_root <- is.na(parent) | !nzchar(parent)
  if (sum(is_root) != 1L) {
    cli::cli_abort(
      "{.fn vhierarchy} needs exactly one root (a node with no {.arg parent}).",
      call = call
    )
  }
  root <- which(is_root)
  pidx <- match(parent, id)
  if (any(is.na(pidx) & !is_root)) {
    cli::cli_abort(
      "{.fn vhierarchy}: every non-root {.arg parent} must be an {.arg id}.",
      call = call
    )
  }

  children <- lapply(seq_len(n), function(i) which(pidx == i)) # input order

  # Depth by BFS; a revisit is a cycle, an unreached node a disconnected forest.
  depth <- rep(NA_integer_, n)
  depth[root] <- 0L
  # FIFO queue via head/tail pointers into a preallocated array (each node is
  # enqueued once), instead of an O(n) `queue[-1L]` pop + `c()` grow each step.
  queue <- integer(n)
  queue[1L] <- root
  qh <- 1L
  qt <- 2L
  while (qh < qt) {
    i <- queue[qh]
    qh <- qh + 1L
    for (kid in children[[i]]) {
      if (!is.na(depth[kid])) {
        cli::cli_abort(
          "{.fn vhierarchy} {.arg parent} relations must form a tree (no cycles).",
          call = call
        )
      }
      depth[kid] <- depth[i] + 1L
      queue[qt] <- kid
      qt <- qt + 1L
    }
  }
  if (anyNA(depth)) {
    cli::cli_abort(
      "{.fn vhierarchy}: every node must descend from the root.",
      call = call
    )
  }
  D <- max(depth)
  if (D < 1L) {
    cli::cli_abort(
      "{.fn vhierarchy} needs at least one level below the root.",
      call = call
    )
  }
  if (D > .MAX_TREE_DEPTH) {
    cli::cli_abort(
      c(
        "{.fn vhierarchy}: the tree is too deep ({D} levels) to lay out.",
        i = "The layout recurses per level; keep depth under {.val {(.MAX_TREE_DEPTH)}}."
      ),
      call = call
    )
  }

  # A leaf's own value drives its size, so it must be finite and non-negative
  # (internal nodes derive their value and may be NA).
  is_leaf <- lengths(children) == 0L
  bad <- is_leaf & (!is.finite(value) | value < 0)
  if (any(bad)) {
    cli::cli_abort(
      c(
        "{.fn vhierarchy} leaf {.arg value}s must be finite and non-negative.",
        i = "Node{?s} {.val {id[bad]}} {?has a/have} missing or negative value{?s}."
      ),
      call = call
    )
  }

  # Node value bottom-up: leaf uses its own value, internal node sums its children.
  val <- numeric(n)
  for (i in order(depth, decreasing = TRUE)) {
    ch <- children[[i]]
    val[i] <- if (length(ch)) sum(val[ch]) else value[i]
  }

  # Fractions: the root owns [0, 1); each node splits its span among its children
  # in proportion to value, in input order.
  f0 <- numeric(n)
  f1 <- numeric(n)
  f0[root] <- 0
  f1[root] <- 1
  assign_frac <- function(i) {
    ch <- children[[i]]
    if (!length(ch)) {
      return(invisible())
    }
    span <- f1[i] - f0[i]
    tot <- sum(val[ch])
    cum <- f0[i]
    for (kid in ch) {
      w <- if (tot > 0) val[kid] / tot * span else span / length(ch)
      f0[kid] <<- cum
      f1[kid] <<- cum + w
      cum <- cum + w
      assign_frac(kid)
    }
  }
  assign_frac(root)

  # Each node belongs to a depth-1 *branch* (its depth-1 ancestor). The branch is
  # what a fill scale colours by default; depth-lightening then separates levels
  # within a branch. `branch_levels` are the branch ids in input order, so the
  # default discrete fill scale assigns its palette in that order (preserving the
  # historical look) rather than alphabetically.
  # A node's branch is its first ancestor at depth <= 1. Resolve it in one pass by
  # visiting nodes parent-before-child (increasing depth), so each node inherits
  # its parent's already-resolved branch instead of re-walking to the root.
  branch <- integer(n)
  for (i in order(depth)) {
    branch[i] <- if (depth[i] <= 1L) i else branch[pidx[i]]
  }
  branch_levels <- id[which(depth == 1L)] # depth-1 ids, input order

  list(
    id = id,
    depth = depth,
    value = val,
    pidx = pidx,
    children = children,
    root = root,
    D = D,
    f0 = f0,
    f1 = f1,
    branch = id[branch],
    branch_levels = branch_levels
  )
}

# Assemble the per-node data frame the emitter consumes: the shared columns plus
# whatever geometry the type needs, for every non-root node, carrying the root
# (id + total) as an attribute for an optional centre label.
.hierarchy_frame <- function(tree, geom) {
  keep <- which(tree$depth >= 1L)
  lay <- data.frame(
    id = tree$id[keep],
    depth = tree$depth[keep],
    value = tree$value[keep],
    branch = tree$branch[keep], # depth-1 ancestor id (default fill variable)
    leaf = lengths(tree$children)[keep] == 0L,
    .node = keep, # original input index, to realign a mapped fill column
    stringsAsFactors = FALSE
  )
  lay <- cbind(lay, geom[keep, , drop = FALSE])
  # Shallow-first, so nested children overlay their parent (treemap/circlepack);
  # harmless for sunburst/icicle, whose regions never overlap.
  lay <- lay[order(lay$depth), , drop = FALSE]
  rownames(lay) <- NULL
  attr(lay, "root") <- list(
    id = tree$id[tree$root],
    value = tree$value[tree$root]
  )
  attr(lay, "branch_levels") <- tree$branch_levels
  lay
}

# --- per-type geometry (one row per node, root included; sliced by the frame) --

# Sunburst: depth -> ring, fraction -> angular span. Angles mirror the package's
# 12-o'clock/clockwise polar convention (pi/2 - 2*pi*frac).
.geom_sunburst <- function(tree, inner_radius) {
  w <- (1 - inner_radius) / tree$D
  data.frame(
    r0 = inner_radius + (tree$depth - 1L) * w,
    r1 = inner_radius + tree$depth * w,
    theta0 = .frac_to_angle(tree$f1),
    theta1 = .frac_to_angle(tree$f0)
  )
}

# Icicle: depth -> band along one axis, fraction -> position along the other.
# `flow` is the direction of increasing depth (root band on the opposite edge).
.geom_icicle <- function(tree, flow) {
  D <- tree$D
  d <- tree$depth
  # depth band and spread, each mapped from [0, 1] to the [-1, 1] panel
  d_lo <- 2 * (d - 1L) / D
  d_hi <- 2 * d / D
  sp0 <- -1 + 2 * tree$f0
  sp1 <- -1 + 2 * tree$f1
  switch(
    flow,
    down = data.frame(x0 = sp0, y0 = 1 - d_hi, x1 = sp1, y1 = 1 - d_lo),
    up = data.frame(x0 = sp0, y0 = -1 + d_lo, x1 = sp1, y1 = -1 + d_hi),
    right = data.frame(x0 = -1 + d_lo, y0 = sp0, x1 = -1 + d_hi, y1 = sp1),
    left = data.frame(x0 = 1 - d_hi, y0 = sp0, x1 = 1 - d_lo, y1 = sp1)
  )
}

# Worst (largest) aspect ratio in a squarified row of `areas` laid across a side
# of length `w` (Bruls, Huizing & van Wijk). `s` is sum(areas).
.squarify_worst <- function(areas, w) {
  s <- sum(areas)
  rmax <- max(areas)
  rmin <- min(areas)
  max(w * w * rmax / (s * s), s * s / (w * w * rmin))
}

# Squarified layout of `areas` (summing to the rectangle's area) inside the
# rectangle (x0, y0, x1, y1). Returns an n x 4 matrix of child rects, in order.
.squarify <- function(areas, x0, y0, x1, y1) {
  n <- length(areas)
  out <- matrix(0, n, 4L)
  i <- 1L
  while (i <= n) {
    w <- min(x1 - x0, y1 - y0)
    if (w <= 0) {
      # degenerate remaining strip: give the rest zero-area slivers, in order
      out[i:n, ] <- matrix(rep(c(x0, y0, x0, y0), each = n - i + 1L), ncol = 4L)
      break
    }
    j <- i
    best <- .squarify_worst(areas[i], w)
    while (j < n) {
      cand <- .squarify_worst(areas[i:(j + 1L)], w)
      if (cand > best) {
        break
      }
      best <- cand
      j <- j + 1L
    }
    row <- areas[i:j]
    s <- sum(row)
    thick <- s / w # row thickness along the longer side
    if ((x1 - x0) >= (y1 - y0)) {
      cy <- y0
      for (k in seq_along(row)) {
        ch <- row[k] / thick
        out[i + k - 1L, ] <- c(x0, cy, x0 + thick, cy + ch)
        cy <- cy + ch
      }
      x0 <- x0 + thick
    } else {
      cx <- x0
      for (k in seq_along(row)) {
        cw <- row[k] / thick
        out[i + k - 1L, ] <- c(cx, y0, cx + cw, y0 + thick)
        cx <- cx + cw
      }
      y0 <- y0 + thick
    }
    i <- j + 1L
  }
  out
}

# Treemap: recursively squarify each internal node's rectangle among its
# children, insetting the child region by `pad` so parents show as a frame.
.geom_treemap <- function(tree, pad = 0.012) {
  n <- length(tree$depth)
  rect <- matrix(NA_real_, n, 4L) # x0, y0, x1, y1 per node
  rect[tree$root, ] <- c(-1, -1, 1, 1)
  # top-down: place a node's children inside its (padded) rectangle
  order_td <- order(tree$depth)
  for (i in order_td) {
    ch <- tree$children[[i]]
    if (!length(ch)) {
      next
    }
    r <- rect[i, ]
    px <- min(pad, (r[3] - r[1]) / 3)
    py <- min(pad, (r[4] - r[2]) / 3)
    ix0 <- r[1] + px
    iy0 <- r[2] + py
    ix1 <- r[3] - px
    iy1 <- r[4] - py
    if (ix1 <= ix0 || iy1 <= iy0) {
      # too small to inset: pack flush
      ix0 <- r[1]
      iy0 <- r[2]
      ix1 <- r[3]
      iy1 <- r[4]
    }
    v <- tree$value[ch]
    if (sum(v) <= 0) {
      v <- rep(1, length(ch))
    }
    areas <- v / sum(v) * (ix1 - ix0) * (iy1 - iy0)
    rect[ch, ] <- .squarify(areas, ix0, iy0, ix1, iy1)
  }
  data.frame(x0 = rect[, 1], y0 = rect[, 2], x1 = rect[, 3], y1 = rect[, 4])
}

# Circle-pack: bottom-up, pack each node's children (leaf radius ~ sqrt(value))
# and take their enclosing circle as the node's radius; top-down, translate each
# node's children by its centre; finally scale the root to fill the panel.
.geom_circlepack <- function(tree) {
  n <- length(tree$depth)
  R <- rep(0, n)
  relx <- rep(0, n) # child centre relative to its parent's centre
  rely <- rep(0, n)
  is_leaf <- lengths(tree$children) == 0L
  R[is_leaf] <- sqrt(pmax(tree$value[is_leaf], 0))

  for (i in order(tree$depth, decreasing = TRUE)) {
    ch <- tree$children[[i]]
    if (!length(ch)) {
      next
    }
    pk <- .pack_siblings(R[ch])
    enc <- .pack_enclose(pk$x, pk$y, R[ch])
    # positions are enclose-centred already; keep the node's radius
    relx[ch] <- pk$x - enc$x
    rely[ch] <- pk$y - enc$y
    R[i] <- enc$r
  }

  cx <- rep(0, n)
  cy <- rep(0, n)
  for (i in order(tree$depth)) {
    ch <- tree$children[[i]]
    if (length(ch)) {
      cx[ch] <- cx[i] + relx[ch]
      cy[ch] <- cy[i] + rely[ch]
    }
  }

  s <- if (R[tree$root] > 0) 0.98 / R[tree$root] else 1
  data.frame(
    cx = (cx - cx[tree$root]) * s,
    cy = (cy - cy[tree$root]) * s,
    cr = R * s
  )
}

# Compute the layout for one type. Returns the per-node frame (see
# `.hierarchy_frame`) with the geometry columns that `.emit_hierarchy` expects.
.hierarchy_layout <- function(
  id,
  parent,
  value,
  type = "sunburst",
  inner_radius = 0,
  flow = "down",
  call = rlang::caller_env()
) {
  tree <- .hierarchy_tree(id, parent, value, call = call)
  geom <- switch(
    type,
    sunburst = .geom_sunburst(tree, inner_radius),
    icicle = .geom_icicle(tree, flow),
    treemap = .geom_treemap(tree),
    circlepack = .geom_circlepack(tree),
    cli::cli_abort("Unknown hierarchy {.arg type} {.val {type}}.", call = call)
  )
  lay <- .hierarchy_frame(tree, geom)
  attr(lay, "type") <- type
  lay
}

# --- shared colour / label helpers ------------------------------------------

# Blend colours toward white by `amount` in [0, 1] (0 = unchanged, 1 = white).
# Vectorised. Fades branch hues outward by depth.
.lighten <- function(col, amount) {
  m <- farver::decode_colour(col)
  amount <- pmin(pmax(amount, 0), 1)
  m <- m + (255 - m) * amount
  farver::encode_colour(m)
}

# A readable ink colour (black or white) for text over each fill, by perceived
# luminance (Rec. 601). Vectorised over `fill`.
.contrast_ink <- function(fill) {
  m <- farver::decode_colour(fill)
  lum <- (0.299 * m[, 1] + 0.587 * m[, 2] + 0.114 * m[, 3]) / 255
  ifelse(lum > 0.6, "black", "white")
}

# Rotation (degrees) for a label whose baseline runs along a sector's radius
# (`along = "radius"`) or tangent (`along = "tangent"`), given the mid-angle in
# radians. Kept upright: an angle pointing into the left half is flipped 180
# degrees so text never reads upside-down (`just = "centre"`, no justify swap).
.upright_rot <- function(theta_rad, along = c("radius", "tangent")) {
  along <- match.arg(along)
  deg <- theta_rad * 180 / pi + if (along == "tangent") 90 else 0
  deg <- ((deg + 180) %% 360) - 180 # normalise to (-180, 180]
  deg[deg > 90] <- deg[deg > 90] - 180
  deg[deg <= -90] <- deg[deg <= -90] + 180
  deg
}

#' Hierarchy diagrams: sunburst, icicle, treemap, circle-pack
#'
#' `vhierarchy()` draws a tree as a space-filling diagram, choosing the geometry
#' with `type`: `"sunburst"` (concentric ring sectors), `"icicle"` (rectangular
#' partition), `"treemap"` (squarified nested rectangles), or `"circlepack"`
#' (circles within circles). All four take the same *parent list* — `id`,
#' `parent` (`NA`/`""` at the root), and `value` (leaf values; an internal node's
#' value is the sum of its children) — so switching `type` re-encodes the same
#' data. Like [vgraph()] / [vsankey()] it returns a ready, axis-free,
#' aspect-locked [PlotSpec]; `mark_hierarchy()` is the layer it adds.
#'
#' The root is structural and never drawn (in a sunburst it is the centre). Each
#' node is labelled with its `id` where the label fits, and `show_values` appends
#' the value; small nodes are left unlabelled so a dense diagram stays legible.
#'
#' # Colour
#'
#' By default nodes are coloured by their depth-1 *branch* and lightened one step
#' per level, so sibling branches stay distinct and depth reads as shade. The
#' branch is an ordinary discrete fill scale, so `scale_fill_*()` recolours the
#' branches (e.g. `scale_fill_brewer()`), and `lighten` controls the depth fade
#' (`0` = flat colour per branch). Map `fill` to a node column instead to colour
#' every node by that variable — discrete or continuous, with the matching
#' `scale_fill_*()` — in which case the depth fade is not applied.
#'
#' @param data A data frame describing a hierarchy (a parent list).
#' @param id,parent,value Columns (tidy-eval): the node id, its parent id
#'   (`NA`/`""` for the root), and its value (used for leaves).
#' @param fill Optional column (tidy-eval) to colour nodes by. Unmapped
#'   (default), nodes are coloured by their depth-1 branch and lightened with
#'   depth; mapped, each node takes its `fill` value's colour with no depth fade.
#' @param lighten Branch mode only: how far the deepest level fades toward white,
#'   a fraction in `[0, 1]` (default `0.6`; `0` = flat colour per branch).
#'   Ignored when `fill` is mapped.
#' @param type Diagram geometry: `"sunburst"` (default), `"icicle"`, `"treemap"`,
#'   or `"circlepack"`.
#' @param inner_radius Sunburst only: central hole radius, a fraction in
#'   `[0, 1)`; `0` (default) fills to the centre.
#' @param flow Icicle only: the direction of increasing depth — `"down"`
#'   (default), `"up"`, `"right"`, or `"left"`.
#' @param label Label each node with its `id`? Default `TRUE`. Nodes too small
#'   for their label are left unlabelled.
#' @param show_values Append each node's value to its label, e.g. `"A1 (3)"`.
#'   Default `FALSE`.
#' @param orientation Sunburst labels only: `"auto"` (default) angles each label
#'   tangentially / radially to fit its wedge, or force `"radial"`,
#'   `"tangential"`, or `"horizontal"`. Labels are kept upright.
#' @param root_label Write the root's name (and, with `show_values`, its total)
#'   in the centre? Default `FALSE`. Most useful for `"sunburst"`.
#' @param width,height,dpi Page size (inches) and resolution.
#' @return A [PlotSpec] (`vhierarchy()`) or the modified plot (`mark_hierarchy()`).
#' @examples
#' h <- data.frame(
#'   id = c("root", "A", "B", "A1", "A2", "B1"),
#'   parent = c(NA, "root", "root", "A", "A", "B"),
#'   value = c(NA, NA, NA, 3, 2, 4)
#' )
#' vhierarchy(h, id, parent, value, show_values = TRUE)
#' vhierarchy(h, id, parent, value, type = "treemap")
#' @export
vhierarchy <- function(
  data,
  id,
  parent,
  value,
  fill = NULL,
  type = c("sunburst", "icicle", "treemap", "circlepack"),
  inner_radius = 0,
  flow = c("down", "up", "right", "left"),
  lighten = 0.6,
  label = TRUE,
  show_values = FALSE,
  orientation = c("auto", "radial", "tangential", "horizontal"),
  root_label = FALSE,
  width = 6,
  height = 6,
  dpi = 96
) {
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame (a parent list).")
  }
  .check_inner_radius(inner_radius)
  .check_dim(width, "width")
  .check_dim(height, "height")
  .check_dpi(dpi)
  p <- PlotSpec(
    data = data,
    coord = CoordSpec(kind = "fixed", ratio = 1), # square, aspect-locked
    theme = .theme_vgraph(),
    width = as.double(width),
    height = as.double(height),
    dpi = as.double(dpi)
  )
  mark_hierarchy(
    p,
    id = {{ id }},
    parent = {{ parent }},
    value = {{ value }},
    fill = {{ fill }},
    type = match.arg(type),
    inner_radius = inner_radius,
    flow = match.arg(flow),
    lighten = lighten,
    label = label,
    show_values = show_values,
    orientation = match.arg(orientation),
    root_label = root_label
  )
}

#' @rdname vhierarchy
#' @param plot A [PlotSpec].
#' @export
mark_hierarchy <- function(
  plot,
  id,
  parent,
  value,
  fill = NULL,
  type = c("sunburst", "icicle", "treemap", "circlepack"),
  inner_radius = 0,
  flow = c("down", "up", "right", "left"),
  lighten = 0.6,
  label = TRUE,
  show_values = FALSE,
  orientation = c("auto", "radial", "tangential", "horizontal"),
  root_label = FALSE
) {
  .check_plot(plot)
  .check_inner_radius(inner_radius)
  if (
    !is.numeric(lighten) || length(lighten) != 1L || lighten < 0 || lighten > 1
  ) {
    cli::cli_abort("{.arg lighten} must be a single number in {.val [0, 1]}.")
  }
  fill_q <- rlang::enquo(fill)
  channels <- rlang::enquos(id = id, parent = parent, value = value)
  if (!rlang::quo_is_null(fill_q)) {
    channels$fill <- fill_q
  } else if (is.null(plot@labels$color) && is.null(plot@labels$fill)) {
    # Default branch-fill legend title (the synthesised fill has no expression to
    # name it). Stored under the canonical `color` key; a later labs()/scale name
    # still wins.
    plot@labels$color <- "branch"
  }
  .add_layer(
    plot,
    "hierarchy",
    channels,
    const_params = list(
      type = match.arg(type),
      lighten = as.numeric(lighten),
      inner_radius = as.numeric(inner_radius),
      flow = match.arg(flow),
      label = isTRUE(label),
      show_values = isTRUE(show_values),
      orientation = match.arg(orientation),
      root_label = isTRUE(root_label)
    )
  )
}
