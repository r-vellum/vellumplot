#' @include classes.R coord.R theme.R
NULL

# --- network (igraph) support -----------------------------------------------
#
# A graph is two tables: a node table (one row per vertex, + layout x/y) and an
# edge table (one row per edge, + endpoint coords x/y -> xend/yend). `vgraph()`
# computes both once -- running the layout R-side, before scale training, exactly
# as the sf path reprojects before training -- and returns a PlotSpec whose data
# is the node table and whose `edge_data` slot holds the edge table (the default
# data for mark_edges()). Everything downstream is ordinary grammar: nodes are a
# point layer, edges a segment layer, in an aspect-locked cartesian panel.
#
# `igraph` and `graphlayouts` are Suggests: the package installs and runs without
# them; vgraph() errors clearly if they are absent. See _docs/DESIGN-igraph.md.

# Perpendicular spacing between parallel/reciprocal edges, as a fraction of the
# endpoint node radius (mm). Applied device-side via segments_grob(offset=), so it
# tracks the mm node markers at any figure size.
.EDGE_OFFSET_MM_FRAC <- 1.1

# Friendly layout name -> (package, function). graphlayouts names are tried
# first (its stress layout is the default); igraph supplies the classics.
.LAYOUT_REGISTRY <- list(
  stress = list(pkg = "graphlayouts", fn = "layout_with_stress"),
  sparse_stress = list(pkg = "graphlayouts", fn = "layout_with_sparse_stress"),
  backbone = list(pkg = "graphlayouts", fn = "layout_as_backbone"),
  pmds = list(pkg = "graphlayouts", fn = "layout_with_pmds"),
  pivotmds = list(pkg = "graphlayouts", fn = "layout_with_pmds"),
  focus = list(pkg = "graphlayouts", fn = "layout_with_focus"),
  centrality = list(pkg = "graphlayouts", fn = "layout_with_centrality"),
  fr = list(pkg = "igraph", fn = "layout_with_fr"),
  kk = list(pkg = "igraph", fn = "layout_with_kk"),
  drl = list(pkg = "igraph", fn = "layout_with_drl"),
  mds = list(pkg = "igraph", fn = "layout_with_mds"),
  circle = list(pkg = "igraph", fn = "layout_in_circle"),
  grid = list(pkg = "igraph", fn = "layout_on_grid"),
  star = list(pkg = "igraph", fn = "layout_as_star"),
  tree = list(pkg = "igraph", fn = "layout_as_tree"),
  sugiyama = list(pkg = "igraph", fn = "layout_with_sugiyama"),
  bipartite = list(pkg = "igraph", fn = "layout_as_bipartite"),
  # unrooted tree layout (equal-angle / equal-daylight / stress); `mode` and
  # `weights` pass through as layout args. Needs graphlayouts (>= 1.2.5).
  unrooted = list(pkg = "graphlayouts", fn = "layout_as_tree_unrooted")
)

# Auto-swap full stress for the pivot-based variant above this vertex count
# (full stress needs the whole N x N distance matrix).
.STRESS_SPARSE_THRESHOLD <- 1e4

# Coerce a user graph to igraph, or error. tbl_graph already inherits "igraph".
.as_igraph <- function(g, call = rlang::caller_env()) {
  if (inherits(g, "igraph")) {
    return(g)
  }
  if (inherits(g, "dendrogram")) {
    g <- stats::as.hclust(g)
  }
  if (inherits(g, "hclust")) {
    return(.hclust_to_igraph(g))
  }
  if (is.matrix(g) && ncol(g) == 2L) {
    return(igraph::graph_from_edgelist(g, directed = FALSE))
  }
  cli::cli_abort(
    c(
      "{.arg g} must be an {.cls igraph} graph, an {.cls hclust}/{.cls dendrogram}, or a 2-column edge-list matrix.",
      i = "Got {.obj_type_friendly {g}}."
    ),
    call = call
  )
}

# Coerce an `hclust` to a directed igraph tree for the dendrogram layout. Leaves
# are vertices 1..L (named by `hc$labels`, height 0); each of the L-1 merges is an
# internal vertex L+i at `hc$height[i]`, with a directed edge to each of its two
# children. `.leaf_pos` records each leaf's slot in `hc$order` so the layout
# spreads leaves without crossings; `cluster` (optional) is the `cutree(k)` group
# of every node whose subtree is monochromatic (NA above the cut), for colouring.
.hclust_to_igraph <- function(hc, k = NULL) {
  merge <- hc$merge
  nmerge <- nrow(merge)
  L <- nmerge + 1L
  n <- L + nmerge
  labels <- hc$labels %||% as.character(seq_len(L))
  # child node index for a merge-matrix entry: negative -> leaf, positive -> merge
  child_node <- function(e) if (e < 0) -e else L + e
  from <- integer(0)
  to <- integer(0)
  for (i in seq_len(nmerge)) {
    parent <- L + i
    from <- c(from, parent, parent)
    to <- c(to, child_node(merge[i, 1L]), child_node(merge[i, 2L]))
  }
  # `name` must be unique (it keys the node table); internal merges get synthetic
  # ids. `label` carries the display text -- leaf labels only, blank for merges.
  name <- c(as.character(labels), paste0(".merge", seq_len(nmerge)))
  label <- c(as.character(labels), rep("", nmerge))
  height <- c(rep(0, L), hc$height)
  leaf <- c(rep(TRUE, L), rep(FALSE, nmerge))
  leaf_pos <- rep(NA_real_, n)
  leaf_pos[hc$order] <- seq_len(L) # leaf i sits at its rank in hc$order
  g <- igraph::make_empty_graph(n = n, directed = TRUE)
  g <- igraph::add_edges(g, rbind(from, to))
  igraph::V(g)$name <- name
  igraph::V(g)$label <- label
  igraph::V(g)$height <- height
  igraph::V(g)$leaf <- leaf
  igraph::V(g)$.leaf_pos <- leaf_pos
  if (!is.null(k)) {
    node_cluster <- .hclust_clusters(hc, k, L, nmerge)
    igraph::V(g)$cluster <- node_cluster
    # an edge takes its child's cluster (NA above the cut -> neutral)
    igraph::E(g)$cluster <- node_cluster[to]
  }
  g
}

# `cutree(hc, k)` group per node, as a character vector length `L + nmerge`: each
# leaf's group, and each merge's group iff its whole subtree is one group (else
# NA -- the merge is at/above the cut and reads neutral).
.hclust_clusters <- function(hc, k, L, nmerge) {
  leaf_grp <- stats::cutree(hc, k = k)
  node_grp <- rep(NA_character_, L + nmerge)
  node_grp[seq_len(L)] <- as.character(leaf_grp)
  # bottom-up: a merge is monochromatic iff both children are the same group
  for (i in seq_len(nmerge)) {
    kids <- hc$merge[i, ]
    g1 <- if (kids[1] < 0) node_grp[-kids[1]] else node_grp[L + kids[1]]
    g2 <- if (kids[2] < 0) node_grp[-kids[2]] else node_grp[L + kids[2]]
    if (!is.na(g1) && !is.na(g2) && identical(g1, g2)) {
      node_grp[L + i] <- g1
    }
  }
  node_grp
}

# Normalise a layout function's return value to an N x 2 numeric matrix. Some
# layouts return a list (backbone -> $xy, sugiyama -> $layout).
.layout_matrix <- function(res, n, call = rlang::caller_env()) {
  if (is.list(res) && !is.data.frame(res)) {
    res <- res[["xy"]] %||% res[["layout"]] %||% NULL
    if (is.null(res)) {
      cli::cli_abort(
        "Layout function returned an unrecognised structure.",
        call = call
      )
    }
  }
  m <- as.matrix(res)
  if (!is.numeric(m) || nrow(m) != n || ncol(m) < 2L) {
    cli::cli_abort(
      "A layout must be an {.code N x 2} numeric matrix ({n} rows).",
      call = call
    )
  }
  m[, 1:2, drop = FALSE]
}

# Rooted dendrogram layout: leaves spread along one axis, each internal node
# centred over its children, the cross axis set to merge height (a `height`
# vertex attr, e.g. from an `hclust`) or to depth otherwise. `direction` orients
# it. Self-computed -- no igraph/graphlayouts call. Returns an N x 2 matrix.
.layout_dendrogram <- function(
  g,
  direction = c("down", "up", "left", "right"),
  ...
) {
  direction <- match.arg(direction)
  n <- igraph::vcount(g)
  if (!n) {
    return(matrix(numeric(0), 0L, 2L))
  }
  va <- igraph::vertex_attr_names(g)
  height <- if ("height" %in% va) as.numeric(igraph::V(g)$height) else NULL
  leaf_pos <- if (".leaf_pos" %in% va) {
    as.numeric(igraph::vertex_attr(g, ".leaf_pos"))
  } else {
    rep(NA_real_, n)
  }
  # Root: the tallest node (hclust), else the unique in-degree-0 vertex, else 1.
  root <- if (!is.null(height)) {
    which.max(height)
  } else {
    ind <- igraph::degree(g, mode = "in")
    if (igraph::is_directed(g) && sum(ind == 0) == 1L) which(ind == 0) else 1L
  }
  # Root the tree by BFS (undirected adjacency), recording depth + child lists.
  adj <- igraph::adjacent_vertices(g, igraph::V(g), mode = "all")
  parent <- rep(NA_integer_, n)
  depth <- rep(NA_integer_, n)
  children <- vector("list", n)
  depth[root] <- 0L
  queue <- root
  while (length(queue)) {
    v <- queue[1L]
    queue <- queue[-1L]
    kids <- setdiff(as.integer(adj[[v]]), parent[v])
    kids <- kids[is.na(depth[kids])]
    children[[v]] <- kids
    parent[kids] <- v
    depth[kids] <- depth[v] + 1L
    queue <- c(queue, kids)
  }
  is_leaf <- lengths(children) == 0L
  cross <- if (!is.null(height)) height else (max(depth) - depth)
  # Leaf spread: hclust ranks (crossing-free) when present, else in-order DFS.
  spread <- rep(NA_real_, n)
  if (all(!is.na(leaf_pos[is_leaf]))) {
    spread[is_leaf] <- leaf_pos[is_leaf]
  } else {
    counter <- 0L
    dfs <- function(v) {
      if (is_leaf[v]) {
        counter <<- counter + 1L
        spread[v] <<- counter
      } else {
        for (c in children[[v]]) {
          dfs(c)
        }
      }
    }
    dfs(root)
  }
  # Internal nodes centre over their children (bottom-up).
  for (v in order(depth, decreasing = TRUE)) {
    if (!is_leaf[v]) {
      spread[v] <- mean(spread[children[[v]]])
    }
  }
  xy <- switch(
    direction,
    down = cbind(spread, cross),
    up = cbind(spread, -cross),
    right = cbind(cross, spread),
    left = cbind(-cross, spread)
  )
  unname(xy)
}

# Compute vertex coordinates. `layout` is a name (dispatched via the registry), a
# supplied N x 2 matrix, or a function `f(g, ...)`.
.graph_layout <- function(
  g,
  layout = "stress",
  ...,
  call = rlang::caller_env()
) {
  n <- igraph::vcount(g)
  layout_args <- list(...)

  if (is.matrix(layout) || is.data.frame(layout)) {
    return(.layout_matrix(layout, n, call))
  }
  if (is.function(layout)) {
    return(.layout_matrix(do.call(layout, c(list(g), layout_args)), n, call))
  }
  # Dendrogram is self-computed (height-aware), not a registry/igraph layout.
  if (is.character(layout) && identical(tolower(layout), "dendrogram")) {
    return(.layout_matrix(
      do.call(.layout_dendrogram, c(list(g), layout_args)),
      n,
      call
    ))
  }
  if (!is.character(layout) || length(layout) != 1L) {
    cli::cli_abort(
      "{.arg layout} must be a layout name, an {.code N x 2} matrix, or a function.",
      call = call
    )
  }

  key <- tolower(layout)
  # Above the threshold, full stress is intractable -- fall back to sparse stress.
  if (identical(key, "stress") && n > .STRESS_SPARSE_THRESHOLD) {
    cli::cli_inform(c(
      i = "{n} nodes: using {.val sparse_stress} instead of full {.val stress}."
    ))
    key <- "sparse_stress"
  }
  reg <- .LAYOUT_REGISTRY[[key]]
  if (is.null(reg)) {
    cli::cli_abort(
      c(
        "Unknown layout {.val {layout}}.",
        i = "Choose one of {.val {names(.LAYOUT_REGISTRY)}}, or supply a matrix/function."
      ),
      call = call
    )
  }
  .need_pkg(reg$pkg, sprintf("layout = \"%s\"", layout), call = call)
  fn <- getExportedValue(reg$pkg, reg$fn)
  # sparse stress needs a pivot count; pick a sensible default if unset.
  if (
    identical(reg$fn, "layout_with_sparse_stress") &&
      is.null(layout_args$pivots)
  ) {
    layout_args$pivots <- min(n - 1L, 100L)
  }
  .layout_matrix(do.call(fn, c(list(g), layout_args)), n, call)
}

# Per-edge perpendicular offset multipliers. Parallel and reciprocal edges (same
# unordered vertex pair) spread across evenly-spaced offsets; a lone edge gets 0;
# self-loops get 0 (handled by the loop emitter). Offsets use a *canonical* pair
# frame (low->high vertex) so reciprocal edges land on opposite sides regardless
# of their own direction.
.edge_offsets <- function(ei) {
  m <- nrow(ei)
  loop <- if (m) ei[, 1] == ei[, 2] else logical(0)
  s <- numeric(m)
  if (m) {
    lo <- pmin(ei[, 1], ei[, 2])
    hi <- pmax(ei[, 1], ei[, 2])
    key <- paste(lo, hi, sep = "-")
    for (members in split(seq_len(m), key)) {
      members <- members[!loop[members]]
      k <- length(members)
      if (k > 1L) {
        s[members] <- (seq_len(k) - 1L) - (k - 1L) / 2
      }
    }
  }
  list(s = s, loop = loop)
}

# Per-loop direction + narrowing, the igraph "flower-petal" placement
# (rigraph R/plot.R): a self-loop points into the *largest angular gap* between
# its vertex's non-loop incident edges, so it lands in empty space; several loops
# on one vertex spread evenly across that gap. `narrow` (1 = wide .. 0.2 = tight)
# reports how crowded the gap is, for the loop's width (see loop_grob(width=)).
# Returns per-edge vectors (NA angle for non-loops). `ei` is the N x 2 endpoint
# index matrix; `xy` the layout.
.loop_geometry <- function(ei, xy) {
  m <- nrow(ei)
  angle <- rep(NA_real_, m)
  narrow <- rep(1, m)
  if (!m) {
    return(list(angle = angle, narrow = narrow))
  }
  is_loop <- ei[, 1] == ei[, 2]
  for (v in unique(ei[is_loop, 1])) {
    idx <- which(is_loop & ei[, 1] == v)
    n_loops <- length(idx)
    inc <- which(!is_loop & (ei[, 1] == v | ei[, 2] == v))
    if (!length(inc)) {
      # no other edges: the whole circle is free
      angs <- utils::head(seq(0, 2 * pi, length.out = n_loops + 1), -1)
      gap_span <- 2 * pi
    } else {
      other <- ifelse(ei[inc, 1] == v, ei[inc, 2], ei[inc, 1])
      na <- sort(
        (atan2(xy[other, 2] - xy[v, 2], xy[other, 1] - xy[v, 1]) + 2 * pi) %%
          (2 * pi)
      )
      gaps <- diff(c(na, na[1] + 2 * pi))
      gi <- which.max(gaps)
      gap_span <- gaps[gi]
      # place the loops strictly inside the largest gap (endpoints excluded so a
      # loop never sits on an incident edge)
      angs <- seq(na[gi], na[gi] + gap_span, length.out = n_loops + 2)
      angs <- angs[-c(1, n_loops + 2)] %% (2 * pi)
    }
    angle[idx] <- angs
    per <- gap_span / n_loops
    narrow[idx] <- pmin(1, pmax(0.2, per / (pi / 4))) # full width if >= 45 deg
  }
  list(angle = angle, narrow = narrow)
}

# Warn when user vertex/edge attributes collide with the reserved layout columns
# (x/y/xend/yend). The layout coordinates are the geometry, so they win and
# overwrite the attribute -- but that clobber would otherwise be silent.
.warn_layout_clash <- function(nms, reserved, what) {
  clash <- intersect(nms, reserved)
  if (length(clash)) {
    cli::cli_warn(c(
      "{what} attribute{?s} {.val {clash}} overwritten by the graph layout coordinates.",
      i = "Rename on the graph to keep the original values."
    ))
  }
  invisible()
}

# Build the node table: vertex attributes + a `name` id + layout x/y, in the
# graph's canonical vertex order (coords attached before any user reorder).
.node_table <- function(g, xy) {
  n <- igraph::vcount(g)
  vdf <- igraph::as_data_frame(g, what = "vertices")
  nm <- if ("name" %in% names(vdf)) {
    as.character(vdf$name)
  } else {
    as.character(seq_len(n))
  }
  extra <- vdf[, setdiff(names(vdf), "name"), drop = FALSE]
  .warn_layout_clash(names(extra), c("x", "y"), "Vertex")
  node <- data.frame(name = nm, stringsAsFactors = FALSE)
  if (ncol(extra)) {
    node <- cbind(node, extra)
  }
  node$x <- xy[, 1]
  node$y <- xy[, 2]
  rownames(node) <- NULL
  node
}

# Build the edge table: edge attributes + endpoint coords resolved *by vertex
# index* (never by row position), with parallel/reciprocal offsets baked in.
.edge_table <- function(g, xy) {
  edf <- igraph::as_data_frame(g, what = "edges")
  .warn_layout_clash(names(edf), c("x", "y", "xend", "yend"), "Edge")
  ei <- igraph::ends(g, igraph::E(g), names = FALSE)
  m <- nrow(ei)
  if (!m) {
    edf$x <- edf$y <- edf$xend <- edf$yend <- numeric(0)
    edf$.from_i <- edf$.to_i <- integer(0)
    edf$.offset_s <- edf$.loop_angle <- edf$.loop_narrow <- numeric(0)
    return(edf)
  }
  # Endpoint vertex indices (into node-table / vertex order) so the edge emitter
  # can look up each endpoint's node radius for exact per-node capping.
  edf$.from_i <- ei[, 1]
  edf$.to_i <- ei[, 2]
  # Endpoints are the raw node coordinates (native). Parallel/reciprocal edges are
  # separated at *render* by vellum's device-space `offset =` (mm), so the spacing
  # tracks the mm node markers; here we only record the signed offset index `s`.
  edf$x <- xy[ei[, 1], 1]
  edf$y <- xy[ei[, 1], 2]
  edf$xend <- xy[ei[, 2], 1]
  edf$yend <- xy[ei[, 2], 2]
  edf$.offset_s <- .edge_offsets(ei)$s
  # self-loop direction (into the largest incident-edge gap) + narrowing
  lg <- .loop_geometry(ei, xy)
  edf$.loop_angle <- lg$angle
  edf$.loop_narrow <- lg$narrow
  edf
}

# Padded x/y limits for the panel. Node markers have an (absolute mm) size that
# extends past their centre, and self-loops bulge outward, so the raw layout
# bbox (+ the trainer's 5%) clips nodes at the extremes -- pad by a fraction of
# the larger range so typical nodes/loops sit inside the panel.
.GRAPH_PAD_FRAC <- 0.15

.graph_limits <- function(xy) {
  rx <- range(xy[, 1])
  ry <- range(xy[, 2])
  pad <- .GRAPH_PAD_FRAC * max(diff(rx), diff(ry), 1)
  list(x = rx + c(-pad, pad), y = ry + c(-pad, pad))
}

# Dendrogram limits: pad each axis by a fraction of *its own* range (the height
# and spread axes have unrelated scales, so a shared pad -- `.graph_limits` --
# would crush the shorter one). A little extra room for the leaf labels.
.dendro_limits <- function(xy) {
  ax <- function(r) {
    d <- diff(r)
    p <- if (d > 0) 0.12 * d else 0.5
    r + c(-p, p)
  }
  list(x = ax(range(xy[, 1])), y = ax(range(xy[, 2])))
}

# Per-vertex node radius (mm) so the edge emitter can cap each edge exactly at its
# endpoints' node boundaries -- resolution-independently, since vellum resolves
# the mm caps in device space at render (see `segments_grob(start_cap=/end_cap=)`
# and `loop_grob()`, added for this; _docs/HANDOVER-response.md). Returns per-edge
# source/target radii (keyed through the edge endpoint indices) plus the per-vertex
# radii. NULL when the panel has no edge layer or node layer (nothing to cap).
.graph_caps <- function(resolved, edge_data, node_n, scales) {
  has_edges <- any(vapply(
    resolved,
    function(L) identical(L$mark, "edges"),
    logical(1)
  ))
  node_L <- Find(function(L) identical(L$mark, "nodes"), resolved)
  fi <- edge_data[[".from_i"]]
  ti <- edge_data[[".to_i"]]
  if (!has_edges || is.null(node_L) || is.null(fi) || is.null(ti)) {
    return(NULL)
  }
  # Node radius (mm), in vertex order. vellum's points_grob `size` *is* the
  # radius (size = 20mm renders a 40mm-diameter marker), so the cap == size.
  # Default matches the node emitter's `.aes_size(L, scales, 1)`.
  r_mm <- rep_len(.aes_size(node_L, scales, 1), node_n)
  # Per-edge perpendicular offset (mm): the signed parallel-edge index scaled by
  # the larger endpoint radius, so parallel/reciprocal edges spread across the
  # node face. Applied device-side by segments_grob(offset=) -> tracks the nodes.
  # `.offset_s` is in the canonical (low->high vertex) frame, but vellum offsets
  # each edge along *its own* from->to normal, so reversed edges (from > to) need
  # a sign flip -- otherwise a reciprocal a->b / b->a pair lands on the same side.
  s <- edge_data[[".offset_s"]] %||% rep(0, length(fi))
  dir_sign <- ifelse(fi <= ti, 1, -1)
  offset_mm <- s * dir_sign * pmax(r_mm[fi], r_mm[ti]) * .EDGE_OFFSET_MM_FRAC
  list(
    node_r = r_mm,
    start_cap = r_mm[fi],
    end_cap = r_mm[ti],
    offset = offset_mm,
    loop_angle = edge_data[[".loop_angle"]],
    loop_narrow = edge_data[[".loop_narrow"]]
  )
}

# The void-like default theme for a graph: no axes, ticks, gridlines, or panel
# background -- arbitrary layout coordinates must not read as meaningful axes.
.theme_vgraph <- function() {
  .merge_theme(
    .theme_gray_complete(),
    list(
      panel.background = element_blank(),
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      axis.line = element_blank(),
      strip.background = element_blank()
    )
  )
}

# --- data reduction (N3): augment + filter, applied before layout -----------
#
# Reductions run on the igraph *before* the layout, so the picture is of the
# reduced subgraph (a filtered layout, not a full layout with holes). Order:
# augment (metrics on the input graph) -> filter_edges -> filter_nodes ->
# k_core. Metrics are computed on the input graph, so a `filter_nodes = degree
# > 2` reads the full-graph degree -- documented, and the predictable choice.

# Vertex metrics `augment=` can attach. Each is `f(g) -> per-vertex vector`.
.AUGMENT_METRICS <- list(
  degree = function(g) igraph::degree(g),
  in_degree = function(g) igraph::degree(g, mode = "in"),
  out_degree = function(g) igraph::degree(g, mode = "out"),
  betweenness = function(g) igraph::betweenness(g),
  closeness = function(g) igraph::closeness(g),
  eigen = function(g) igraph::eigen_centrality(g)$vector,
  coreness = function(g) igraph::coreness(g),
  components = function(g) as.integer(igraph::components(g)$membership),
  community = function(g) {
    as.integer(igraph::membership(
      igraph::cluster_louvain(igraph::as_undirected(g, mode = "collapse"))
    ))
  }
)

# `augment = TRUE` attaches this default set.
.AUGMENT_DEFAULT <- c("degree", "components")

# Attach vertex metrics as graph attributes (so they flow into the node table and
# can be mapped, or referenced by `filter_nodes`). `augment` is TRUE (the default
# set), or a character vector of metric names.
.graph_augment <- function(g, augment, call = rlang::caller_env()) {
  metrics <- if (isTRUE(augment)) .AUGMENT_DEFAULT else as.character(augment)
  bad <- setdiff(metrics, names(.AUGMENT_METRICS))
  if (length(bad)) {
    cli::cli_abort(
      c(
        "Unknown {.arg augment} metric{?s}: {.val {bad}}.",
        i = "Choose from {.val {names(.AUGMENT_METRICS)}}."
      ),
      call = call
    )
  }
  for (m in metrics) {
    g <- igraph::set_vertex_attr(g, m, value = .AUGMENT_METRICS[[m]](g))
  }
  g
}

# Evaluate a filter predicate (captured quosure) against the vertex/edge
# attribute table and return the keep mask. Errors clearly on a non-logical or
# wrong-length result (a mis-typed predicate must not silently drop everything).
.graph_keep_mask <- function(quo, tbl, n, what, call = rlang::caller_env()) {
  keep <- rlang::eval_tidy(quo, tbl)
  if (!is.logical(keep) || length(keep) != n) {
    cli::cli_abort(
      c(
        "{.arg filter_{what}} must evaluate to a logical vector, one per {what}.",
        i = "Got {.obj_type_friendly {keep}} of length {length(keep)} for {n} {what}{?s}."
      ),
      call = call
    )
  }
  keep & !is.na(keep)
}

# Drop edges failing `filter_edges` (vertices kept).
.graph_filter_edges <- function(g, quo, call = rlang::caller_env()) {
  edf <- igraph::as_data_frame(g, what = "edges")
  keep <- .graph_keep_mask(quo, edf, igraph::ecount(g), "edge", call)
  igraph::delete_edges(g, igraph::E(g)[!keep])
}

# Drop vertices failing `filter_nodes` (their incident edges go too).
.graph_filter_nodes <- function(g, quo, call = rlang::caller_env()) {
  vdf <- igraph::as_data_frame(g, what = "vertices")
  keep <- .graph_keep_mask(quo, vdf, igraph::vcount(g), "node", call)
  igraph::delete_vertices(g, igraph::V(g)[!keep])
}

# Keep the k-core: vertices whose coreness is >= k (and induced edges).
.graph_kcore <- function(g, k, call = rlang::caller_env()) {
  if (!is.numeric(k) || length(k) != 1L || !is.finite(k)) {
    cli::cli_abort("{.arg k_core} must be a single number.", call = call)
  }
  core <- igraph::coreness(g)
  igraph::delete_vertices(g, igraph::V(g)[core < k])
}

# Apply the reduction pipeline in a fixed, documented order, then guard against a
# graph reduced to nothing (a layout of zero vertices is meaningless).
.graph_reduce <- function(
  g,
  augment,
  fe_quo,
  fn_quo,
  k_core,
  call = rlang::caller_env()
) {
  if (!isFALSE(augment)) {
    g <- .graph_augment(g, augment, call)
  }
  if (!rlang::quo_is_null(fe_quo)) {
    g <- .graph_filter_edges(g, fe_quo, call)
  }
  if (!rlang::quo_is_null(fn_quo)) {
    g <- .graph_filter_nodes(g, fn_quo, call)
  }
  if (!is.null(k_core)) {
    g <- .graph_kcore(g, k_core, call)
  }
  if (igraph::vcount(g) == 0L) {
    cli::cli_abort(
      c(
        "The filters removed every vertex.",
        i = "Loosen {.arg filter_nodes} / {.arg filter_edges} / {.arg k_core}."
      ),
      call = call
    )
  }
  g
}

#' Start a graph (network) plot
#'
#' `vgraph()` begins a node-link diagram from an `igraph` graph. It computes a
#' layout (vertex `x`/`y`), builds a node table and an edge table, and returns a
#' [PlotSpec] with the aspect locked (`coord_equal`) and a void theme (no axes or
#' gridlines) -- publication-quality defaults with no tuning. Add layers with
#' [mark_edges()], [mark_nodes()], and [mark_node_text()]; edges default to the
#' edge table and nodes to the node table.
#'
#' The default layout is stress majorization
#' (`graphlayouts::layout_with_stress`): it is deterministic (same graph -> same
#' picture) and minimizes the difference between drawn and graph-theoretic
#' distance. Above `10^4` nodes it falls back to sparse stress automatically.
#' Reciprocal and parallel edges are drawn as parallel straight lines offset off
#' the centre line (not curved); self-loops as small loops.
#'
#' `igraph` (and `graphlayouts`, for its layouts) are optional dependencies (in
#' `Suggests`); `vgraph()` errors with an install hint if they are not available.
#'
#' @param g An `igraph` graph (or a two-column edge-list matrix).
#' @param layout A layout: a name (`"stress"` (default), `"sparse_stress"`,
#'   `"backbone"`, `"fr"`, `"kk"`, `"circle"`, `"tree"`, `"sugiyama"`, ...), a
#'   supplied `N x 2` coordinate matrix, or a function `f(g, ...)` returning one.
#' @param ... Extra arguments forwarded to the layout function (e.g. `pivots =`
#'   for sparse stress).
#' @param augment Opt-in vertex metrics to attach as graph attributes (so they
#'   can be mapped -- `mark_nodes(size = degree)` -- or used by `filter_nodes`).
#'   `FALSE` (default) attaches nothing; `TRUE` attaches `degree` and
#'   `components`; a character vector picks from `"degree"`, `"in_degree"`,
#'   `"out_degree"`, `"betweenness"`, `"closeness"`, `"eigen"`, `"coreness"`,
#'   `"components"`, `"community"`. Never computed silently -- vellum does not
#'   guess which centrality you meant.
#' @param filter_nodes,filter_edges Data-masked predicates that keep the vertices
#'   / edges for which they are `TRUE`, evaluated against the vertex / edge
#'   attribute table -- e.g. `filter_edges = weight > 0.5`,
#'   `filter_nodes = degree >= 2` (with `augment`). The graph is reduced *before*
#'   the layout, so the picture is of the subgraph, not a full layout with holes.
#' @param k_core Keep only the k-core: vertices with coreness `>= k_core` (and
#'   their induced edges). `NULL` (default) keeps everything.
#' @param seed Integer seed for stochastic layouts (`"fr"`, `"drl"`), making the
#'   figure reproducible. The global RNG stream is restored afterwards.
#' @param width,height Page size in inches.
#' @param dpi Output resolution in dots per inch (see [vplot()]).
#' @return A [PlotSpec] whose data is the node table and whose `edge_data` is the
#'   edge table.
#' @details
#' Reductions apply in a fixed order: `augment` (metrics computed on the input
#' graph) -> `filter_edges` -> `filter_nodes` -> `k_core`. Augmented metrics thus
#' reflect the *input* graph, not the post-filter subgraph.
#' @seealso [mark_edges()], [mark_nodes()], [mark_node_text()]
#' @examples
#' \dontrun{
#' g <- igraph::make_graph("Zachary")
#' vgraph(g, layout = "stress") |>
#'   mark_edges() |>
#'   mark_nodes(size = 3)
#'
#' # attach degree, then plot only the 2-core, sized by degree
#' vgraph(g, augment = TRUE, k_core = 2) |>
#'   mark_edges() |>
#'   mark_nodes(size = degree)
#' }
#' @export
vgraph <- function(
  g,
  layout = "stress",
  ...,
  augment = FALSE,
  filter_nodes = NULL,
  filter_edges = NULL,
  k_core = NULL,
  seed = 42L,
  width = 6,
  height = 4,
  dpi = 96
) {
  .need_pkg("igraph", "vgraph()")
  g <- .as_igraph(g)
  .check_dpi(dpi)
  g <- .graph_reduce(
    g,
    augment,
    rlang::enquo(filter_edges),
    rlang::enquo(filter_nodes),
    k_core
  )
  xy <- .with_seed(seed, .graph_layout(g, layout, ...))
  # Networks want equal x/y scaling (aspect-locked); a dendrogram instead fills
  # the panel, its height and leaf-spread axes scaling independently.
  is_dendro <- is.character(layout) && identical(tolower(layout), "dendrogram")
  lim <- if (is_dendro) .dendro_limits(xy) else .graph_limits(xy)
  coord <- if (is_dendro) {
    CoordSpec(kind = "cartesian")
  } else {
    CoordSpec(kind = "fixed", ratio = 1)
  }
  PlotSpec(
    data = .node_table(g, xy),
    edge_data = .edge_table(g, xy),
    coord = coord,
    theme = .theme_vgraph(),
    scales = list(
      ScaleSpec(aesthetic = "x", type = "continuous", domain = lim$x),
      ScaleSpec(aesthetic = "y", type = "continuous", domain = lim$y)
    ),
    width = as.double(width),
    height = as.double(height),
    dpi = as.double(dpi)
  )
}

#' Dendrogram from a clustering
#'
#' `vdendrogram()` turns a base `hclust`/`dendrogram` (e.g. from [stats::hclust()])
#' into a ready node-link dendrogram: leaves spread along a line, each merge drawn
#' at its height, joined by right-angle brackets. It is a thin preset over
#' `vgraph(<clustering>, layout = "dendrogram")` plus bracket edges and leaf
#' labels; for full control, use that path and add marks yourself.
#'
#' @param x An `hclust` or `dendrogram` object.
#' @param direction Growth direction (leaves opposite the root): `"down"`
#'   (default; root at top), `"up"`, `"left"`, or `"right"`.
#' @param k Optionally cut the tree into `k` clusters ([stats::cutree()]) and
#'   colour branches and leaf labels by cluster; branches above the cut stay
#'   neutral. `NULL` (default) draws a single-colour tree.
#' @param labels Draw the leaf labels? Default `TRUE`.
#' @param width,height,dpi Page size (inches) and resolution.
#' @return A [PlotSpec].
#' @seealso [vgraph()] for the general node-link path (`layout = "dendrogram"`
#'   or `"unrooted"`).
#' @examples
#' hc <- hclust(dist(USArrests))
#' vdendrogram(hc, k = 3)
#' @export
vdendrogram <- function(
  x,
  direction = c("down", "up", "left", "right"),
  k = NULL,
  labels = TRUE,
  width = 7,
  height = 5,
  dpi = 96
) {
  if (inherits(x, "dendrogram")) {
    x <- stats::as.hclust(x)
  }
  if (!inherits(x, "hclust")) {
    cli::cli_abort("{.arg x} must be an {.cls hclust} or {.cls dendrogram}.")
  }
  direction <- match.arg(direction)
  g <- .hclust_to_igraph(x, k = k)
  axis <- if (direction %in% c("down", "up")) "v" else "h"
  p <- vgraph(
    g,
    layout = "dendrogram",
    direction = direction,
    width = width,
    height = height,
    dpi = dpi
  )
  # Bracket edges (corner at the parent's height); colour by cluster when cut.
  p <- if (is.null(k)) {
    mark_edges(p, routing = "elbow", elbow_at = "start", elbow_axis = axis)
  } else {
    mark_edges(
      p,
      color = cluster,
      routing = "elbow",
      elbow_at = "start",
      elbow_axis = axis
    )
  }
  if (isTRUE(labels)) {
    p <- if (is.null(k)) {
      mark_node_text(p, label = label, size = 2.5, dist = 1)
    } else {
      mark_node_text(p, label = label, color = cluster, size = 2.5, dist = 1)
    }
  }
  p
}
