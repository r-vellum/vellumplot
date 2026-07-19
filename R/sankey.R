#' @include classes.R vplot.R theme.R vgraph.R
NULL

# --- Sankey / flow diagrams --------------------------------------------------
#
# A sankey is a layered flow diagram: nodes stacked in columns (layers) with
# ribbons between them whose width encodes a value. Like `vgraph()` (network),
# the layout is computed R-side into native [0, 1] coordinates and drawn through
# ordinary vellum primitives (node rects + filled Bézier ribbons); the plot is
# axis-free (a void theme), free-aspect. See `_docs/TIER2-PLAN.md` Phase 5a.
#
# Input is a *flow list*: one row per flow with `from`, `to`, `value`. Nodes are
# the union of `from`/`to`; a node that is both a source and a target makes the
# diagram multi-stage. The DAG is layered by longest-path; within a layer nodes
# keep first-appearance order (no crossing minimisation in v1); ribbon slices are
# ordered by the opposite node's height to reduce crossings locally.

# Layout marks (whole-plot types with a global R-side layout in an axis-free
# panel): they take no other layers and cannot be faceted (guarded in the seam).
.LAYOUT_MARKS <- c("sankey", "sunburst")

# Geometry constants (native [0, 1] units).
.SANKEY_NODE_WIDTH <- 0.04
.SANKEY_NODE_GAP <- 0.02

# Drawing constants.
.SANKEY_RIBBON_ALPHA <- 0.5 # ribbon fill opacity (ribbons overlap)
.SANKEY_LABEL_FONTSIZE <- 8 # node-label font size (pt)
# Horizontal margin (native fraction) reserved each side of the [0, 1] node band
# so the outer-column labels -- source labels sit left of x = 0, terminal labels
# right of x = 1 -- render inside the panel instead of clipping at its edge.
.SANKEY_LABEL_MARGIN <- 0.15

# Longest-path layer index per node (0 = a source with no incoming flow).
# Iterates the edge relation to a fixpoint; non-convergence within N+1 passes
# means a cycle (a sankey must be a DAG).
.sankey_layers <- function(fi, ti, n, call = rlang::caller_env()) {
  layer <- integer(n)
  for (iter in seq_len(n + 1L)) {
    prop <- tapply(layer[fi] + 1L, ti, max)
    idx <- as.integer(names(prop))
    new <- layer
    new[idx] <- pmax(layer[idx], as.integer(prop))
    if (identical(new, layer)) {
      return(layer)
    }
    layer <- new
  }
  cli::cli_abort(
    c(
      "{.fn vsankey} flows must form a DAG (no cycles).",
      i = "A node cannot reach itself through the flows."
    ),
    call = call
  )
}

# Compute the full sankey layout in native [0, 1] coordinates. Returns a `nodes`
# data frame (name, x0/x1/y0/y1, colour) and a `ribbons` data frame (per flow:
# left/right x and the source/target y-slices, + value and source colour).
.sankey_layout <- function(
  from,
  to,
  value,
  node_width = .SANKEY_NODE_WIDTH,
  node_gap = .SANKEY_NODE_GAP,
  call = rlang::caller_env()
) {
  from <- as.character(from)
  to <- as.character(to)
  value <- as.numeric(value)
  ok <- is.finite(value) & value > 0 & !is.na(from) & !is.na(to)
  if (!all(ok)) {
    cli::cli_warn(
      "Dropping {sum(!ok)} flow{?s} with a non-positive/NA value or NA endpoint.",
      call = call
    )
    from <- from[ok]
    to <- to[ok]
    value <- value[ok]
  }
  if (!length(value)) {
    cli::cli_abort("{.fn vsankey} needs at least one valid flow.", call = call)
  }

  nodes <- unique(c(from, to)) # first-appearance order
  n <- length(nodes)
  fi <- match(from, nodes)
  ti <- match(to, nodes)
  layer <- .sankey_layers(fi, ti, n, call = call)
  nlayers <- max(layer) + 1L

  # Node value = max(total in, total out) so conservation reads at every node.
  sum_by <- function(idx) {
    s <- numeric(n)
    agg <- tapply(value, idx, sum)
    s[as.integer(names(agg))] <- agg
    s
  }
  nodeval <- pmax(sum_by(fi), sum_by(ti))

  # A single value->height scale across all layers (so ribbon widths match on
  # both ends): set by the layer that is fullest once its inter-node gaps are
  # removed.
  layers_list <- split(seq_len(n), layer)
  avail <- vapply(
    layers_list,
    function(ns) 1 - (length(ns) - 1L) * node_gap,
    numeric(1)
  )
  lval <- vapply(layers_list, function(ns) sum(nodeval[ns]), numeric(1))
  ky <- min((avail / lval)[lval > 0])

  # Node rectangles: x by layer; y stacked and centred within each layer.
  node_x0 <- (layer / max(1L, nlayers - 1L)) * (1 - node_width)
  node_x1 <- node_x0 + node_width
  node_y0 <- numeric(n)
  node_y1 <- numeric(n)
  for (ns in layers_list) {
    h <- nodeval[ns] * ky
    total <- sum(h) + (length(ns) - 1L) * node_gap
    cum <- (1 - total) / 2 # centre the column
    for (j in seq_along(ns)) {
      node_y0[ns[j]] <- cum
      node_y1[ns[j]] <- cum + h[j]
      cum <- cum + h[j] + node_gap
    }
  }

  # Ribbon y-slices: stack a node's outgoing flows down its right edge (ordered
  # by the target's height, to reduce crossings), and incoming down its left edge
  # (ordered by the source's height). Slice thickness = value * ky.
  nf <- length(value)
  assign_slices <- function(node_idx, order_key) {
    o <- order(node_idx, order_key)
    y1 <- numeric(nf)
    y0 <- numeric(nf)
    top <- node_y1
    for (k in o) {
      nd <- node_idx[k]
      th <- value[k] * ky
      y1[k] <- top[nd]
      y0[k] <- top[nd] - th
      top[nd] <- y0[k]
    }
    list(y0 = y0, y1 = y1)
  }
  tgt_yc <- (node_y0[ti] + node_y1[ti]) / 2
  src_yc <- (node_y0[fi] + node_y1[fi]) / 2
  s <- assign_slices(fi, tgt_yc)
  t <- assign_slices(ti, src_yc)

  pal <- .qual_palette(n)
  list(
    nodes = data.frame(
      name = nodes,
      x0 = node_x0,
      x1 = node_x1,
      y0 = node_y0,
      y1 = node_y1,
      colour = pal,
      stringsAsFactors = FALSE
    ),
    ribbons = data.frame(
      xl = node_x1[fi],
      xr = node_x0[ti],
      sy0 = s$y0,
      sy1 = s$y1,
      ty0 = t$y0,
      ty1 = t$y1,
      value = value,
      colour = pal[fi],
      stringsAsFactors = FALSE
    )
  )
}

# One edge of a ribbon as a horizontal cubic-Bézier polyline from (x0,y0) to
# (x1,y1), control points pulled to the mid-x for the classic S-curve.
.sankey_bezier <- function(x0, y0, x1, y1, n = 40L) {
  t <- seq(0, 1, length.out = n)
  xm <- (x0 + x1) / 2
  mt <- 1 - t
  bx <- mt^3 * x0 + 3 * mt^2 * t * xm + 3 * mt * t^2 * xm + t^3 * x1
  by <- mt^3 * y0 + 3 * mt^2 * t * y0 + 3 * mt * t^2 * y1 + t^3 * y1
  list(x = bx, y = by)
}

#' Sankey (flow) diagram
#'
#' `vsankey()` draws a layered flow diagram from a *flow list* — one row per
#' flow with a `from` node, a `to` node, and a `value` (the ribbon width). Nodes
#' are the union of `from`/`to`; a node that is both a source and a target makes
#' the diagram multi-stage. Like [vgraph()], it returns a ready [PlotSpec] with an
#' axis-free, aspect-free panel; `mark_sankey()` is the layer it adds and can be
#' used directly on a plot you have set up yourself.
#'
#' The flows must form a DAG (no cycles). Node order within a column is
#' first-appearance; ribbons are ordered to reduce crossings locally but v1 does
#' no global crossing minimisation. Nodes are coloured from the built-in
#' qualitative palette.
#'
#' @param data A data frame of flows.
#' @param from,to,value Columns (tidy-eval): the source node, target node, and
#'   flow value.
#' @param label Draw node labels? Default `TRUE`.
#' @param width,height,dpi Page size (inches) and resolution.
#' @return A [PlotSpec] (`vsankey()`) or the modified plot (`mark_sankey()`).
#' @examples
#' flows <- data.frame(
#'   from = c("A", "A", "B", "C", "C"),
#'   to = c("B", "C", "D", "D", "E"),
#'   value = c(4, 6, 4, 4, 2)
#' )
#' vsankey(flows, from, to, value)
#' @export
vsankey <- function(
  data,
  from,
  to,
  value,
  label = TRUE,
  width = 8,
  height = 5,
  dpi = 96
) {
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame of flows.")
  }
  .check_dim(width, "width")
  .check_dim(height, "height")
  .check_dpi(dpi)
  p <- PlotSpec(
    data = data,
    coord = CoordSpec(kind = "cartesian"), # free aspect; axis-free via the theme
    theme = .theme_vgraph(),
    width = as.double(width),
    height = as.double(height),
    dpi = as.double(dpi)
  )
  mark_sankey(
    p,
    from = {{ from }},
    to = {{ to }},
    value = {{ value }},
    label = label
  )
}

#' @rdname vsankey
#' @param plot A [PlotSpec].
#' @export
mark_sankey <- function(plot, from, to, value, label = TRUE) {
  .check_plot(plot)
  .add_layer(
    plot,
    "sankey",
    rlang::enquos(from = from, to = to, value = value),
    const_params = list(label = isTRUE(label))
  )
}
