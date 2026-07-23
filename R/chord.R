#' @include classes.R vplot.R theme.R vgraph.R sankey.R
NULL

# --- vchord: chord diagram --------------------------------------------------
#
# A chord diagram wraps a weighted flow list (or a square flow matrix) onto a
# circle: one *sector* (arc) per node, sized by the node's total incident weight,
# and one *ribbon* per flow connecting a slice of the source sector to a slice of
# the target sector through the centre. Directed: each node's arc splits into an
# outgoing block then an incoming block, so a->b and b->a are distinct ribbons
# and a self-flow a->a loops from a's out-block to its own in-block. Like
# `vsankey()`/`vhierarchy()` the layout is computed R-side and drawn with batched
# vellum grobs in the axis-free, aspect-locked square panel.

# Ring radius: ribbons attach at `R_IN`; the sector band is [R_IN, R_OUT].
.CHORD_R_IN <- 0.9
.CHORD_R_OUT <- 1.0

# Compute the chord layout. Returns a list of two data frames -- `sectors` (one
# arc per node) and `ribbons` (one per flow, with its source/target sub-arc
# angles) -- plus the node palette. Angles use the package polar convention
# (12 o'clock, clockwise: `pi/2 - 2*pi*frac`).
.chord_layout <- function(
  from,
  to,
  value,
  gap = 0.02,
  sort = "input",
  link_color = "source",
  call = rlang::caller_env()
) {
  from <- as.character(from)
  to <- as.character(to)
  value <- as.numeric(value)
  if (!length(from)) {
    cli::cli_abort("{.fn vchord} needs at least one flow.", call = call)
  }
  if (length(to) != length(from) || length(value) != length(from)) {
    cli::cli_abort(
      "{.fn vchord}: {.arg from}, {.arg to}, {.arg value} must be the same length.",
      call = call
    )
  }
  if (any(!is.finite(value) | value < 0)) {
    cli::cli_abort(
      "{.fn vchord} {.arg value}s must be finite and non-negative.",
      call = call
    )
  }

  nodes <- unique(c(from, to)) # first-appearance order
  out_total <- vapply(nodes, function(nd) sum(value[from == nd]), numeric(1))
  in_total <- vapply(nodes, function(nd) sum(value[to == nd]), numeric(1))
  weight <- out_total + in_total
  if (identical(sort, "value")) {
    ord <- order(weight, decreasing = TRUE)
    nodes <- nodes[ord]
    out_total <- out_total[ord]
    in_total <- in_total[ord]
    weight <- weight[ord]
  }
  names(out_total) <- names(in_total) <- names(weight) <- nodes
  n <- length(nodes)
  gap_total <- n * gap
  if (gap_total >= 1) {
    cli::cli_abort(
      "{.fn vchord}: {.arg gap} too large -- {n} gaps leave no room for sectors.",
      call = call
    )
  }
  # One unit of weight is this fraction of the circle; a flow's sub-arc (on each
  # end) is `value * unit`, so out/in blocks and sector spans all stay consistent.
  totw <- sum(weight)
  unit <- if (totw > 0) (1 - gap_total) / totw else 0

  ang <- function(f) pi / 2 - 2 * pi * f

  # Sector bounds + per-node out/in block cursors (in fraction space).
  sec_theta0 <- numeric(n)
  sec_theta1 <- numeric(n)
  out_cur <- numeric(n)
  in_cur <- numeric(n)
  names(out_cur) <- names(in_cur) <- nodes
  cf <- 0
  for (k in seq_len(n)) {
    span <- weight[k] * unit
    sec_theta0[k] <- ang(cf + span) # smaller angle (clockwise end)
    sec_theta1[k] <- ang(cf) # larger angle (start)
    out_cur[k] <- cf # out block starts at the sector start
    in_cur[k] <- cf + out_total[k] * unit # in block follows the out block
    cf <- cf + span + gap
  }
  pal <- stats::setNames(.qual_palette(n), nodes)
  sectors <- data.frame(
    node = nodes,
    theta0 = sec_theta0,
    theta1 = sec_theta1,
    colour = unname(pal[nodes]),
    stringsAsFactors = FALSE
  )

  # Ribbons: consume each flow's slice from its source out-block and target
  # in-block, in `sort` order (so both ends stay consistent).
  flow_ord <- if (identical(sort, "value")) {
    order(value, decreasing = TRUE)
  } else {
    seq_along(from)
  }
  m <- length(flow_ord)
  sa0 <- sa1 <- ta0 <- ta1 <- numeric(m)
  src <- tgt <- character(m)
  self <- logical(m)
  rv <- numeric(m)
  for (idx in seq_len(m)) {
    r <- flow_ord[idx]
    i <- from[r]
    j <- to[r]
    v <- value[r]
    w <- v * unit
    # source slice from i's out block
    fa <- out_cur[i]
    out_cur[i] <- fa + w
    # target slice from j's in block
    fb <- in_cur[j]
    in_cur[j] <- fb + w
    sa0[idx] <- ang(fa)
    sa1[idx] <- ang(fa + w)
    ta0[idx] <- ang(fb)
    ta1[idx] <- ang(fb + w)
    src[idx] <- i
    tgt[idx] <- j
    self[idx] <- i == j
    rv[idx] <- v
  }
  ribbons <- data.frame(
    src = src,
    tgt = tgt,
    self = self,
    value = rv,
    sa0 = sa0,
    sa1 = sa1,
    ta0 = ta0,
    ta1 = ta1,
    colour = unname(pal[if (identical(link_color, "target")) tgt else src]),
    stringsAsFactors = FALSE
  )
  # Draw the widest ribbons first so thin ones read on top.
  ribbons <- ribbons[order(ribbons$value, decreasing = TRUE), , drop = FALSE]
  rownames(ribbons) <- NULL

  list(sectors = sectors, ribbons = ribbons)
}

# Sample points along a circle arc (radius r) from angle a0 to a1, `nseg`+1 pts.
.chord_arc <- function(r, a0, a1, nseg = 24L) {
  a <- seq(a0, a1, length.out = nseg + 1L)
  list(x = r * cos(a), y = r * sin(a))
}

# Sample a quadratic Bezier from point A to point B with the control at the
# origin, so the ribbon edge bows through the centre. `nseg`+1 points.
.chord_bezier <- function(ax, ay, bx, by, nseg = 24L) {
  t <- seq(0, 1, length.out = nseg + 1L)
  w <- (1 - t)^2 # control at (0, 0): B(t) = (1-t)^2 A + t^2 B
  list(x = w * ax + t^2 * bx, y = w * ay + t^2 * by)
}

#' Chord diagram
#'
#' `vchord()` draws a weighted flow as a circular chord diagram: one arc
#' (*sector*) per node, sized by the node's total incident weight, and one ribbon
#' per flow joining a slice of the source sector to a slice of the target sector
#' through the centre. Input is a flow list -- `from`, `to`, `value` -- or a
#' square flow matrix (row = from, column = to; `dimnames` are the node names).
#' Like [vgraph()] / [vsankey()] it returns a ready, axis-free, aspect-locked
#' [PlotSpec]; `mark_chord()` is the layer it adds.
#'
#' Flows are directed: each node's arc splits into an outgoing block then an
#' incoming block, so `a -> b` and `b -> a` are separate ribbons and a self-flow
#' `a -> a` loops from the node's out-block to its own in-block. Sectors are
#' coloured by node (a qualitative palette); ribbons take their source node's
#' colour by default (`link_color = "target"` to colour by target).
#'
#' @param data A data frame of flows, or a square numeric matrix (row = from,
#'   column = to).
#' @param from,to,value Columns (tidy-eval) for the data-frame form: the source
#'   node, target node, and flow weight. Omit for the matrix form.
#' @param gap Gap between sectors, as a fraction of the circle each (default
#'   `0.02`).
#' @param sort Sector and ribbon order: `"input"` (default, first-appearance) or
#'   `"value"` (largest weight first).
#' @param link_color Colour ribbons by their `"source"` (default) or `"target"`
#'   node.
#' @param direction How to show a ribbon's direction: `"gradient"` (default,
#'   fade from opaque at the source to faint at the target), `"gap"` (stop the
#'   ribbon short of the target sector, leaving a small gap), `"both"`, or
#'   `"none"`.
#' @param label Label each sector with its node name? Default `TRUE`.
#' @param width,height,dpi Page size (inches) and resolution.
#' @return A [PlotSpec] (`vchord()`) or the modified plot (`mark_chord()`).
#' @examples
#' flows <- data.frame(
#'   from = c("A", "A", "B", "C"),
#'   to = c("B", "C", "C", "A"),
#'   value = c(3, 2, 4, 1)
#' )
#' vchord(flows, from, to, value)
#' @export
vchord <- function(
  data,
  from,
  to,
  value,
  gap = 0.02,
  sort = c("input", "value"),
  link_color = c("source", "target"),
  direction = c("gradient", "gap", "both", "none"),
  label = TRUE,
  width = 6,
  height = 6,
  dpi = 96
) {
  sort <- match.arg(sort)
  link_color <- match.arg(link_color)
  direction <- match.arg(direction)
  .check_dim(width, "width")
  .check_dim(height, "height")
  .check_dpi(dpi)
  # Matrix input carries no from/to/value channels: unroll it to a flow frame
  # with those column names and reference them; a data frame keeps the user's.
  if (is.matrix(data)) {
    flows <- .chord_as_flows(data)
    fq <- rlang::quo(from)
    tq <- rlang::quo(to)
    vq <- rlang::quo(value)
  } else {
    flows <- .chord_as_flows(data)
    fq <- rlang::enquo(from)
    tq <- rlang::enquo(to)
    vq <- rlang::enquo(value)
  }
  p <- PlotSpec(
    data = flows,
    coord = CoordSpec(kind = "fixed", ratio = 1), # square, aspect-locked
    theme = .theme_vgraph(),
    width = as.double(width),
    height = as.double(height),
    dpi = as.double(dpi)
  )
  mark_chord(
    p,
    from = !!fq,
    to = !!tq,
    value = !!vq,
    gap = gap,
    sort = sort,
    link_color = link_color,
    direction = direction,
    label = label
  )
}

#' @rdname vchord
#' @param plot A [PlotSpec].
#' @export
mark_chord <- function(
  plot,
  from,
  to,
  value,
  gap = 0.02,
  sort = c("input", "value"),
  link_color = c("source", "target"),
  direction = c("gradient", "gap", "both", "none"),
  label = TRUE
) {
  .check_plot(plot)
  .add_layer(
    plot,
    "chord",
    rlang::enquos(from = from, to = to, value = value),
    const_params = list(
      gap = as.numeric(gap),
      sort = match.arg(sort),
      link_color = match.arg(link_color),
      direction = match.arg(direction),
      label = isTRUE(label)
    )
  )
}

# Coerce `vchord()` input to a from/to/value flow data frame: pass a flow data
# frame straight through (the mark reads its channels), or unroll a square matrix
# (row = from, column = to; non-zero cells become flows).
.chord_as_flows <- function(data, call = rlang::caller_env()) {
  if (is.matrix(data)) {
    nm <- dimnames(data)
    rn <- nm[[1]] %||% as.character(seq_len(nrow(data)))
    cn <- nm[[2]] %||% as.character(seq_len(ncol(data)))
    grid <- expand.grid(i = seq_len(nrow(data)), j = seq_len(ncol(data)))
    v <- data[cbind(grid$i, grid$j)]
    keep <- is.finite(v) & v > 0
    return(data.frame(
      from = rn[grid$i[keep]],
      to = cn[grid$j[keep]],
      value = v[keep],
      stringsAsFactors = FALSE
    ))
  }
  if (!is.data.frame(data)) {
    cli::cli_abort(
      "{.arg data} must be a data frame of flows or a square matrix.",
      call = call
    )
  }
  data
}
