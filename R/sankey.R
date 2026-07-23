#' @include classes.R vplot.R theme.R vgraph.R
NULL

# --- Sankey / flow diagrams --------------------------------------------------
#
# A sankey is a layered flow diagram: nodes stacked in columns (layers) with
# ribbons between them whose width encodes a value. Like `vgraph()` (network),
# the layout is computed R-side into native [0, 1] coordinates and drawn through
# ordinary vellum primitives (node rects + filled Bézier ribbons); the plot is
# axis-free (a void theme), free-aspect.
#
# Input is a *flow list*: one row per flow with `from`, `to`, `value`. Nodes are
# the union of `from`/`to`; a node that is both a source and a target makes the
# diagram multi-stage. The DAG is layered by longest-path; nodes within a layer
# are then ordered by the Sugiyama barycenter heuristic (`.sankey_order`) to
# minimise ribbon crossings, and ribbon slices are ordered by the opposite node's
# height so ribbons meet each node edge in matching order.

# Layout marks (whole-plot types with a global R-side layout in an axis-free
# panel): they take no other layers and cannot be faceted (guarded in the seam).
.LAYOUT_MARKS <- c("sankey", "hierarchy", "chord")

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

# Direction-split neighbour lists (+ matching flow values) per node. Splitting on
# a factor with all node levels guarantees length-`n` lists with empty entries for
# nodes lacking in/out edges, so isolated-side nodes fall out for free.
.sankey_adjacency <- function(fi, ti, value, n) {
  ti_f <- factor(ti, levels = seq_len(n))
  fi_f <- factor(fi, levels = seq_len(n))
  list(
    up = split(fi, ti_f), # sources feeding into each node
    up_w = split(value, ti_f),
    dn = split(ti, fi_f), # targets each node feeds out to
    dn_w = split(value, fi_f)
  )
}

# Order nodes within each layer to reduce ribbon crossings: the classic Sugiyama
# layered barycenter heuristic (as used by d3-sankey). Alternating down/up sweeps
# reorder each layer by the value-weighted mean of its neighbours' normalised
# positions in the adjacent, already-placed layers; a node with no neighbour on
# the swept side holds its position. Deterministic (stable tie-break on the
# current order, no RNG) and cheap -- O(iterations * (n log n + edges)). Returns a
# list shaped exactly like `split(seq_len(n), layer)` (a drop-in replacement) so
# the rest of the layout is unchanged.
.sankey_order <- function(fi, ti, value, layer, n, iterations = 8L) {
  nlayers <- max(layer) + 1L
  cols <- split(seq_len(n), factor(layer, levels = 0:(nlayers - 1L)))
  if (nlayers < 2L) {
    return(cols) # a single column: nothing can cross
  }
  adj <- .sankey_adjacency(fi, ti, value, n)

  pos <- numeric(n) # normalised rank within own layer (comparable across layers)
  set_layer_pos <- function(li) {
    col <- cols[[li]]
    m <- length(col)
    if (m) {
      pos[col] <<- (seq_len(m) - 0.5) / m
    }
  }
  for (li in seq_len(nlayers)) {
    set_layer_pos(li)
  }

  bary <- function(v, nbr, wt) {
    ns <- nbr[[v]]
    if (!length(ns)) {
      return(NA_real_)
    }
    sum(pos[ns] * wt[[v]]) / sum(wt[[v]])
  }
  reorder_layer <- function(li, side) {
    col <- cols[[li]]
    m <- length(col)
    if (m < 2L) {
      return(col)
    }
    nbr <- if (side == "up") adj$up else adj$dn
    wt <- if (side == "up") adj$up_w else adj$dn_w
    key <- vapply(col, bary, numeric(1), nbr = nbr, wt = wt)
    na <- is.na(key)
    key[na] <- pos[col][na] # no neighbour this side: hold position
    col[order(key, seq_along(col))] # stable, deterministic tie-break
  }

  prev <- NULL
  for (it in seq_len(iterations)) {
    down <- (it %% 2L) == 1L
    lis <- if (down) 2:nlayers else (nlayers - 1L):1L
    for (li in lis) {
      cols[[li]] <- reorder_layer(li, if (down) "up" else "dn")
      set_layer_pos(li)
    }
    sig <- unlist(cols, use.names = FALSE)
    if (!is.null(prev) && identical(sig, prev)) {
      break # fixed point
    }
    prev <- sig
  }
  cols
}

# Count ribbon crossings between consecutive layers (pairwise inversions of the
# span-1 edges by node position). Used in tests to assert the ordered layout does
# not increase crossings over first-appearance order.
.sankey_crossings <- function(fi, ti, layers_list, layer) {
  pos <- integer(length(layer))
  for (col in layers_list) {
    pos[col] <- seq_along(col)
  }
  total <- 0L
  for (L in sort(unique(layer))) {
    e <- which(layer[fi] == L & layer[ti] == L + 1L)
    if (length(e) < 2L) {
      next
    }
    su <- pos[fi[e]]
    sv <- pos[ti[e]]
    for (a in seq_along(e)) {
      for (b in seq_along(e)) {
        if (a < b && sign(su[a] - su[b]) * sign(sv[a] - sv[b]) < 0) {
          total <- total + 1L
        }
      }
    }
  }
  total
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
  flow_color = "source",
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
  layers_list <- .sankey_order(fi, ti, value, layer, n)
  avail <- vapply(
    layers_list,
    function(ns) 1 - (length(ns) - 1L) * node_gap,
    numeric(1)
  )
  # The inter-node gaps in the fullest column must leave room for the nodes
  # themselves. Past ~1/node_gap nodes the gaps alone exceed the panel height,
  # `avail` goes negative, and every node/ribbon height inverts and spills
  # outside [0, 1]. Refuse with a clear message rather than draw broken geometry.
  if (any(avail <= 0)) {
    worst <- max(lengths(layers_list))
    cli::cli_abort(c(
      "A {.field sankey} column has too many nodes for the inter-node gaps to fit.",
      i = "The widest column has {worst} nodes; reduce {.arg node_gap} (below {signif(1 / (worst - 1), 2)}) or the node count."
    ))
  }
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
  # (ordered by the source's height). Slices fill top-to-bottom, so the opposite
  # node highest on the page must take the topmost slice -- order by *descending*
  # `order_key` (a y-centre); the ascending order would connect the lowest
  # opposite node to the top slice and cross every ribbon. Thickness = value * ky.
  nf <- length(value)
  assign_slices <- function(node_idx, order_key) {
    o <- order(node_idx, -order_key)
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
  tgt <- assign_slices(ti, src_yc)

  pal <- .qual_palette(n)
  # Ribbon fill: the source node's colour (default) or the target's.
  rib_colour <- if (identical(flow_color, "target")) pal[ti] else pal[fi]
  list(
    nodes = data.frame(
      name = nodes,
      x0 = node_x0,
      x1 = node_x1,
      y0 = node_y0,
      y1 = node_y1,
      value = nodeval,
      colour = pal,
      stringsAsFactors = FALSE
    ),
    ribbons = data.frame(
      xl = node_x1[fi],
      xr = node_x0[ti],
      sy0 = s$y0,
      sy1 = s$y1,
      ty0 = tgt$y0,
      ty1 = tgt$y1,
      value = value,
      colour = rib_colour,
      colour_src = pal[fi], # source/target node colours, for gradient ribbons
      colour_tgt = pal[ti],
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
#' The flows must form a DAG (no cycles). Nodes within a column are ordered to
#' minimise ribbon crossings (a deterministic Sugiyama barycenter sweep), and
#' ribbons are stacked to meet each node in matching order. Nodes are coloured
#' from the built-in qualitative palette.
#'
#' @param data A data frame of flows.
#' @param from,to,value Columns (tidy-eval): the source node, target node, and
#'   flow value.
#' @param label Draw node labels? Default `TRUE`.
#' @param show_values Append each node's value to its label (e.g. `"Grid (60)"`)?
#'   Default `FALSE`. Ignored when `label = FALSE`.
#' @param flow_color Ribbon fill: `"source"` (default) colours each ribbon by its
#'   source node, `"target"` by its target node, and `"gradient"` fades each
#'   ribbon from its source colour to its target colour.
#' @param node_width Node-rectangle width, as a fraction of the plotting width
#'   (default `0.04`).
#' @param node_gap Vertical gap between the nodes in a column, as a fraction of
#'   the column height (default `0.02`).
#' @param width,height,dpi Page size (inches) and resolution.
#' @return A [PlotSpec] (`vsankey()`) or the modified plot (`mark_sankey()`).
#' @examples
#' flows <- data.frame(
#'   from = c("A", "A", "B", "C", "C"),
#'   to = c("B", "C", "D", "D", "E"),
#'   value = c(4, 6, 4, 4, 2)
#' )
#' vsankey(flows, from, to, value)
#' vsankey(flows, from, to, value, show_values = TRUE, flow_color = "target")
#' @export
vsankey <- function(
  data,
  from,
  to,
  value,
  label = TRUE,
  show_values = FALSE,
  flow_color = "source",
  node_width = 0.04,
  node_gap = 0.02,
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
    label = label,
    show_values = show_values,
    flow_color = flow_color,
    node_width = node_width,
    node_gap = node_gap
  )
}

#' @rdname vsankey
#' @param plot A [PlotSpec].
#' @export
mark_sankey <- function(
  plot,
  from,
  to,
  value,
  label = TRUE,
  show_values = FALSE,
  flow_color = "source",
  node_width = 0.04,
  node_gap = 0.02
) {
  .check_plot(plot)
  flow_color <- rlang::arg_match0(flow_color, c("source", "target", "gradient"))
  .check_node_fraction(node_width, "node_width")
  .check_node_fraction(node_gap, "node_gap")
  .add_layer(
    plot,
    "sankey",
    rlang::enquos(from = from, to = to, value = value),
    const_params = list(
      label = isTRUE(label),
      show_values = isTRUE(show_values),
      flow_color = flow_color,
      node_width = as.numeric(node_width),
      node_gap = as.numeric(node_gap)
    )
  )
}

# A sankey geometry fraction (node width / gap) must be a single number in [0, 1).
.check_node_fraction <- function(x, arg, call = rlang::caller_env()) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || x < 0 || x >= 1) {
    cli::cli_abort(
      "{.arg {arg}} must be a single number in {.code [0, 1)}.",
      call = call
    )
  }
}
