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

test_that("vsankey renders, with and without labels", {
  f1 <- local_tempfile(fileext = ".png")
  render_plot(vsankey(flows, from, to, value), f1)
  expect_gt(file.info(f1)$size, 0)
  f2 <- local_tempfile(fileext = ".png")
  render_plot(vsankey(flows, from, to, value, label = FALSE), f2)
  expect_gt(file.info(f2)$size, 0)
})
