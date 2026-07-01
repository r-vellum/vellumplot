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

# Perpendicular offset between parallel/reciprocal edges, as a fraction of the
# layout bounding-box diagonal. Kept small so lines stay visually adjacent.
.EDGE_OFFSET_FRAC <- 0.06

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
  bipartite = list(pkg = "igraph", fn = "layout_as_bipartite")
)

# Auto-swap full stress for the pivot-based variant above this vertex count
# (full stress needs the whole N x N distance matrix).
.STRESS_SPARSE_THRESHOLD <- 1e4

# Coerce a user graph to igraph, or error. tbl_graph already inherits "igraph".
.as_igraph <- function(g, call = rlang::caller_env()) {
  if (inherits(g, "igraph")) {
    return(g)
  }
  if (is.matrix(g) && ncol(g) == 2L) {
    return(igraph::graph_from_edgelist(g, directed = FALSE))
  }
  cli::cli_abort(
    c(
      "{.arg g} must be an {.cls igraph} graph (or a 2-column edge-list matrix).",
      i = "Got {.obj_type_friendly {g}}."
    ),
    call = call
  )
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
  ei <- igraph::ends(g, igraph::E(g), names = FALSE)
  m <- nrow(ei)
  if (!m) {
    edf$x <- edf$y <- edf$xend <- edf$yend <- numeric(0)
    return(edf)
  }
  x0 <- xy[ei[, 1], 1]
  y0 <- xy[ei[, 1], 2]
  x1 <- xy[ei[, 2], 1]
  y1 <- xy[ei[, 2], 2]

  off <- .edge_offsets(ei)
  diag <- sqrt(diff(range(xy[, 1]))^2 + diff(range(xy[, 2]))^2)
  d <- .EDGE_OFFSET_FRAC * (if (is.finite(diag) && diag > 0) diag else 1)
  lo <- pmin(ei[, 1], ei[, 2])
  hi <- pmax(ei[, 1], ei[, 2])
  ux <- xy[hi, 1] - xy[lo, 1]
  uy <- xy[hi, 2] - xy[lo, 2]
  len <- sqrt(ux^2 + uy^2)
  len[len == 0] <- 1 # loops / coincident nodes -> no meaningful perpendicular
  nx <- -uy / len
  ny <- ux / len
  shift <- off$s * d

  edf$x <- x0 + shift * nx
  edf$y <- y0 + shift * ny
  edf$xend <- x1 + shift * nx
  edf$yend <- y1 + shift * ny
  edf
}

# Padded x/y limits for the panel. Node markers have an (absolute mm) size that
# extends past their centre, and self-loops bulge outward, so the raw layout
# bbox (+ the trainer's 5%) clips nodes at the extremes -- pad by a fraction of
# the larger range so typical nodes/loops sit inside the panel.
.GRAPH_PAD_FRAC <- 0.1

.graph_limits <- function(xy) {
  rx <- range(xy[, 1])
  ry <- range(xy[, 2])
  pad <- .GRAPH_PAD_FRAC * max(diff(rx), diff(ry), 1)
  list(x = rx + c(-pad, pad), y = ry + c(-pad, pad))
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
#' @param seed Integer seed for stochastic layouts (`"fr"`, `"drl"`), making the
#'   figure reproducible. The global RNG stream is restored afterwards.
#' @param width,height Page size in inches.
#' @param dpi Output resolution in dots per inch (see [vplot()]).
#' @return A [PlotSpec] whose data is the node table and whose `edge_data` is the
#'   edge table.
#' @seealso [mark_edges()], [mark_nodes()], [mark_node_text()]
#' @examples
#' \dontrun{
#' g <- igraph::make_graph("Zachary")
#' vgraph(g, layout = "stress") |>
#'   mark_edges() |>
#'   mark_nodes(size = 3)
#' }
#' @export
vgraph <- function(
  g,
  layout = "stress",
  ...,
  seed = 42L,
  width = 6,
  height = 4,
  dpi = 96
) {
  .need_pkg("igraph", "vgraph()")
  g <- .as_igraph(g)
  .check_dpi(dpi)
  xy <- .with_seed(seed, .graph_layout(g, layout, ...))
  lim <- .graph_limits(xy)
  PlotSpec(
    data = .node_table(g, xy),
    edge_data = .edge_table(g, xy),
    coord = CoordSpec(kind = "fixed", ratio = 1),
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
