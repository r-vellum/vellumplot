#' @include classes.R vplot.R theme.R vgraph.R sankey.R
NULL

# --- Sunburst / radial hierarchy --------------------------------------------
#
# A sunburst draws a tree as concentric rings of sectors: depth -> ring, and a
# node's angular span is its share of its parent's span. Like `vsankey()` /
# `vgraph()`, the layout is computed R-side (a recursive radial partition) into
# native coordinates and drawn with one batched `sector_grob`; the plot is
# axis-free and aspect-locked (a square). See `_docs/TIER2-PLAN.md` Phase 5b.
#
# Input is a *parent list* (d3 `stratify()`): `id`, `parent` (NA at the root),
# `value` (leaf values; an internal node's value is the sum of its children).
# The root is the centre, not a wedge; depth-1 nodes form the innermost ring.

# Recursive radial partition of a parent-list hierarchy. Returns one row per
# non-root node: `id`, `depth`, ring radii `r0`/`r1` (native, in [0, 1] scaled by
# `inner_radius`), and the angular span `theta0`/`theta1` (radians). Colour is by
# depth ring. Validates the input is a single-rooted tree.
.sunburst_layout <- function(
  id,
  parent,
  value,
  inner_radius = 0,
  call = rlang::caller_env()
) {
  id <- as.character(id)
  parent <- as.character(parent)
  value <- as.numeric(value)
  n <- length(id)
  if (!n) {
    cli::cli_abort("{.fn vsunburst} needs at least one node.", call = call)
  }
  if (anyDuplicated(id)) {
    cli::cli_abort("{.fn vsunburst} {.arg id}s must be unique.", call = call)
  }

  is_root <- is.na(parent) | !nzchar(parent)
  if (sum(is_root) != 1L) {
    cli::cli_abort(
      "{.fn vsunburst} needs exactly one root (a node with no {.arg parent}).",
      call = call
    )
  }
  root <- which(is_root)
  pidx <- match(parent, id)
  if (any(is.na(pidx) & !is_root)) {
    cli::cli_abort(
      "{.fn vsunburst}: every non-root {.arg parent} must be an {.arg id}.",
      call = call
    )
  }

  children <- lapply(seq_len(n), function(i) which(pidx == i)) # input order

  # Depth by BFS from the root; a revisit means a cycle, an unreached node means
  # a disconnected forest.
  depth <- rep(NA_integer_, n)
  depth[root] <- 0L
  queue <- root
  while (length(queue)) {
    i <- queue[1L]
    queue <- queue[-1L]
    for (c in children[[i]]) {
      if (!is.na(depth[c])) {
        cli::cli_abort(
          "{.fn vsunburst} {.arg parent} relations must form a tree (no cycles).",
          call = call
        )
      }
      depth[c] <- depth[i] + 1L
      queue <- c(queue, c)
    }
  }
  if (anyNA(depth)) {
    cli::cli_abort(
      "{.fn vsunburst}: every node must descend from the root.",
      call = call
    )
  }
  D <- max(depth)
  if (D < 1L) {
    cli::cli_abort(
      "{.fn vsunburst} needs at least one level below the root.",
      call = call
    )
  }

  # A leaf's own `value` drives its angular span, so it must be a finite,
  # non-negative number (internal nodes derive their value and may be NA). Reject
  # a bad leaf value with a clear message rather than the cryptic error it would
  # otherwise trigger downstream ("missing value where TRUE/FALSE needed").
  is_leaf <- lengths(children) == 0L
  bad <- is_leaf & (!is.finite(value) | value < 0)
  if (any(bad)) {
    cli::cli_abort(
      c(
        "{.fn vsunburst} leaf {.arg value}s must be finite and non-negative.",
        i = "Node{?s} {.val {id[bad]}} {?has a/have} missing or negative value{?s}."
      ),
      call = call
    )
  }

  # Node value bottom-up: a leaf uses its own `value`, an internal node the sum
  # of its children.
  val <- numeric(n)
  for (i in order(depth, decreasing = TRUE)) {
    ch <- children[[i]]
    val[i] <- if (length(ch)) sum(val[ch]) else value[i]
  }

  # Angular fractions: the root owns [0, 1); each node splits its span among its
  # children in proportion to value, in input order.
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
    for (c in ch) {
      w <- if (tot > 0) val[c] / tot * span else span / length(ch)
      f0[c] <<- cum
      f1[c] <<- cum + w
      cum <- cum + w
      assign_frac(c)
    }
  }
  assign_frac(root)

  w <- (1 - inner_radius) / D
  pal <- grDevices::hcl.colors(D, "Batlow")
  keep <- depth >= 1L
  data.frame(
    id = id[keep],
    depth = depth[keep],
    r0 = inner_radius + (depth[keep] - 1L) * w,
    r1 = inner_radius + depth[keep] * w,
    theta0 = 2 * pi * f0[keep],
    theta1 = 2 * pi * f1[keep],
    colour = pal[depth[keep]],
    stringsAsFactors = FALSE
  )
}

#' Sunburst (radial hierarchy) diagram
#'
#' `vsunburst()` draws a tree as concentric rings of sectors: depth maps to a
#' ring and each node's angular span is its share of its parent's. Input is a
#' *parent list* — `id`, `parent` (`NA` at the root), and `value` (leaf values;
#' an internal node's value is the sum of its children). Like [vgraph()] /
#' [vsankey()] it returns a ready, axis-free, aspect-locked [PlotSpec];
#' `mark_sunburst()` is the layer it adds.
#'
#' The root is the centre (not drawn as a wedge); `inner_radius` opens a hole.
#' Nodes are coloured by depth. The input must be a single-rooted tree.
#'
#' @param data A data frame describing a hierarchy (a parent list).
#' @param id,parent,value Columns (tidy-eval): the node id, its parent id
#'   (`NA`/`""` for the root), and its value (used for leaves).
#' @param inner_radius Central hole radius, a fraction in `[0, 1)`; `0` (default)
#'   fills to the centre.
#' @param inner.radius Deprecated; renamed to `inner_radius`.
#' @param width,height,dpi Page size (inches) and resolution.
#' @return A [PlotSpec] (`vsunburst()`) or the modified plot (`mark_sunburst()`).
#' @examples
#' h <- data.frame(
#'   id = c("root", "A", "B", "A1", "A2", "B1"),
#'   parent = c(NA, "root", "root", "A", "A", "B"),
#'   value = c(NA, NA, NA, 3, 2, 4)
#' )
#' vsunburst(h, id, parent, value)
#' @export
vsunburst <- function(
  data,
  id,
  parent,
  value,
  inner_radius = 0,
  width = 6,
  height = 6,
  dpi = 96,
  inner.radius = NULL
) {
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame (a parent list).")
  }
  inner_radius <- .renamed_arg(
    inner_radius,
    inner.radius,
    "inner_radius",
    "inner.radius"
  )
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
  mark_sunburst(
    p,
    id = {{ id }},
    parent = {{ parent }},
    value = {{ value }},
    inner_radius = inner_radius
  )
}

#' @rdname vsunburst
#' @param plot A [PlotSpec].
#' @export
mark_sunburst <- function(
  plot,
  id,
  parent,
  value,
  inner_radius = 0,
  inner.radius = NULL
) {
  .check_plot(plot)
  inner_radius <- .renamed_arg(
    inner_radius,
    inner.radius,
    "inner_radius",
    "inner.radius"
  )
  .check_inner_radius(inner_radius)
  .add_layer(
    plot,
    "sunburst",
    rlang::enquos(id = id, parent = parent, value = value),
    const_params = list(inner_radius = as.numeric(inner_radius))
  )
}
