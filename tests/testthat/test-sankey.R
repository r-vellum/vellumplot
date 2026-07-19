# vsankey() / mark_sankey(): layered flow-diagram layout + emit.

flows <- data.frame(
  from = c("A", "A", "B", "C", "C"),
  to = c("B", "C", "D", "D", "E"),
  value = c(4, 6, 4, 4, 2)
)

test_that("vsankey() returns an axis-free, free-aspect PlotSpec with one sankey layer", {
  p <- vsankey(flows, from, to, value)
  expect_s3_class(p, "vellumplot::PlotSpec")
  expect_length(p@layers, 1L)
  expect_identical(p@layers[[1]]@mark, "sankey")
  expect_identical(p@coord@kind, "cartesian") # not aspect-locked (unlike sunburst)
})

test_that("layer assignment is longest-path from sources", {
  lay <- vellumplot:::.sankey_layout(flows$from, flows$to, flows$value)
  fi <- match(flows$from, lay$nodes$name)
  ti <- match(flows$to, lay$nodes$name)
  layer <- vellumplot:::.sankey_layers(fi, ti, nrow(lay$nodes))
  names(layer) <- lay$nodes$name
  expect_equal(layer[["A"]], 0)
  expect_equal(layer[["B"]], 1)
  expect_equal(layer[["C"]], 1)
  expect_equal(layer[["D"]], 2)
  expect_equal(layer[["E"]], 2)
})

test_that("node height is proportional to max(in, out) value", {
  lay <- vellumplot:::.sankey_layout(flows$from, flows$to, flows$value)
  h <- setNames(lay$nodes$y1 - lay$nodes$y0, lay$nodes$name)
  # A: out = 10; D: in = 8. A is the fullest node -> tallest.
  expect_gt(h[["A"]], h[["D"]])
  # ratio tracks the value ratio (single ky scale across layers)
  expect_equal(unname(h[["A"]] / h[["D"]]), 10 / 8, tolerance = 1e-6)
})

test_that("ribbon slice thickness is proportional to flow value", {
  lay <- vellumplot:::.sankey_layout(flows$from, flows$to, flows$value)
  r <- lay$ribbons
  th <- r$sy1 - r$sy0
  # the A->C flow (value 6) is 1.5x the A->B flow (value 4)
  ab <- which(flows$from == "A" & flows$to == "B")
  ac <- which(flows$from == "A" & flows$to == "C")
  expect_equal(th[ac] / th[ab], 6 / 4, tolerance = 1e-6)
})

test_that("cycles are rejected", {
  expect_error(
    vellumplot:::.sankey_layout(c("A", "B"), c("B", "A"), c(1, 1)),
    "DAG"
  )
})

test_that("non-positive / NA flows are dropped with a warning", {
  expect_warning(
    lay <- vellumplot:::.sankey_layout(
      c("A", "A", "B"),
      c("B", "C", "C"),
      c(4, 0, 3)
    ),
    "Dropping"
  )
  expect_equal(nrow(lay$ribbons), 2)
})

test_that("a sankey plot cannot be faceted or share the panel with other marks", {
  expect_error(
    vellum::as_vellum_scene(
      vplot(flows) |>
        mark_sankey(from, to, value) |>
        mark_point(x = value, y = value)
    ),
    "no other layers"
  )
  d <- data.frame(
    from = c("A", "A"),
    to = c("B", "C"),
    value = c(1, 2),
    g = c("x", "y")
  )
  expect_error(
    vellum::as_vellum_scene(
      vplot(d) |> mark_sankey(from, to, value) |> facet_wrap(~g)
    ),
    "faceted"
  )
})

test_that("labels widen the x domain so outer-column labels stay on-panel", {
  # with labels: margin reserved each side (before the scale's own expansion)
  dom_lab <- vellumplot:::.build_panels(
    vsankey(flows, from, to, value)
  )$scales$x$domain
  expect_lt(dom_lab[1], 0)
  expect_gt(dom_lab[2], 1)
  # without labels: nodes fill [0, 1] (only the scale's default expansion beyond)
  dom_no <- vellumplot:::.build_panels(
    vsankey(flows, from, to, value, label = FALSE)
  )$scales$x$domain
  expect_gt(dom_no[1], dom_lab[1])
  expect_lt(dom_no[2], dom_lab[2])
})

test_that("vsankey renders, with and without labels", {
  f1 <- local_tempfile(fileext = ".png")
  render_plot(vsankey(flows, from, to, value), f1)
  expect_gt(file.info(f1)$size, 0)
  f2 <- local_tempfile(fileext = ".png")
  render_plot(vsankey(flows, from, to, value, label = FALSE), f2)
  expect_gt(file.info(f2)$size, 0)
})

# --- crossing minimization -------------------------------------------------

# Build (fi, ti, value, layer, n) for a flow list, as .sankey_layout does.
sankey_parts <- function(from, to, value) {
  nodes <- unique(c(from, to))
  n <- length(nodes)
  fi <- match(from, nodes)
  ti <- match(to, nodes)
  layer <- vellumplot:::.sankey_layers(fi, ti, n)
  base <- split(seq_len(n), factor(layer, levels = 0:max(layer)))
  opt <- vellumplot:::.sankey_order(fi, ti, value, layer, n)
  list(fi = fi, ti = ti, layer = layer, n = n, base = base, opt = opt)
}

test_that("node ordering never increases crossings, and reduces them here", {
  p <- sankey_parts(
    c("Coal", "Gas", "Coal", "Solar", "Grid", "Grid"),
    c("Grid", "Grid", "Export", "Grid", "Homes", "Industry"),
    c(30, 20, 10, 15, 40, 25)
  )
  cx <- function(o) vellumplot:::.sankey_crossings(p$fi, p$ti, o, p$layer)
  expect_lte(cx(p$opt), cx(p$base))
  expect_lt(cx(p$opt), cx(p$base)) # this example has avoidable crossings
})

test_that("a crafted inversion is fully resolved (crossings -> 0)", {
  # Nodes 1,2 (layer 0) and 3,4 (layer 1); edges 1->4 and 2->3 cross under the
  # initial node-index order. Reordering layer 1 to {4,3} uncrosses them.
  fi <- c(1L, 2L)
  ti <- c(4L, 3L)
  value <- c(1, 1)
  layer <- c(0L, 0L, 1L, 1L)
  base <- split(1:4, factor(layer, levels = 0:1))
  opt <- vellumplot:::.sankey_order(fi, ti, value, layer, 4L)
  expect_gt(vellumplot:::.sankey_crossings(fi, ti, base, layer), 0)
  expect_equal(vellumplot:::.sankey_crossings(fi, ti, opt, layer), 0)
})

test_that("ordering is deterministic", {
  p <- sankey_parts(
    c("Coal", "Gas", "Coal", "Solar", "Grid", "Grid"),
    c("Grid", "Grid", "Export", "Grid", "Homes", "Industry"),
    c(30, 20, 10, 15, 40, 25)
  )
  again <- vellumplot:::.sankey_order(
    p$fi,
    p$ti,
    c(30, 20, 10, 15, 40, 25),
    p$layer,
    p$n
  )
  expect_identical(p$opt, again)
})

test_that("degenerate inputs order without error", {
  # single edge (two layers, one node each)
  p1 <- sankey_parts("x", "y", 1)
  expect_equal(
    vellumplot:::.sankey_crossings(p1$fi, p1$ti, p1$opt, p1$layer),
    0
  )
  # a disconnected second component keeps a valid ordering
  p2 <- sankey_parts(c("a", "c"), c("b", "d"), c(1, 1))
  expect_length(unlist(p2$opt), p2$n)
  expect_setequal(unlist(p2$opt), seq_len(p2$n))
})
