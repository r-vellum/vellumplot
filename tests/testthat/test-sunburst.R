# vsunburst() / mark_sunburst(): radial-partition hierarchy layout + emit.

h <- data.frame(
  id = c("root", "A", "B", "A1", "A2", "B1"),
  parent = c(NA, "root", "root", "A", "A", "B"),
  value = c(NA, NA, NA, 3, 2, 4)
)

test_that("vsunburst() returns an aspect-locked PlotSpec with one sunburst layer", {
  p <- vsunburst(h, id, parent, value)
  expect_s3_class(p, "vellumplot::PlotSpec")
  expect_length(p@layers, 1L)
  expect_identical(p@layers[[1]]@mark, "sunburst")
  expect_identical(p@coord@kind, "fixed") # square, unlike sankey
})

test_that("depth maps to rings; the root is not drawn", {
  lay <- vellumplot:::.sunburst_layout(h$id, h$parent, h$value)
  expect_false("root" %in% lay$id) # root is the centre, not a sector
  d <- setNames(lay$depth, lay$id)
  expect_equal(unname(d[c("A", "B", "A1", "A2", "B1")]), c(1, 1, 2, 2, 2))
  # rings are contiguous: depth-1 r1 == depth-2 r0 (D = 2, so w = 0.5)
  expect_equal(lay$r1[lay$id == "A"], lay$r0[lay$id == "A1"], tolerance = 1e-9)
})

test_that("angular span is proportional to value; children fill the parent", {
  lay <- vellumplot:::.sunburst_layout(h$id, h$parent, h$value)
  span <- setNames(lay$theta1 - lay$theta0, lay$id)
  # A = A1(3)+A2(2) = 5, B = B1(4) = 4  ->  A:B span = 5:4
  expect_equal(unname(span[["A"]] / span[["B"]]), 5 / 4, tolerance = 1e-9)
  # the two top-level nodes fill the full circle
  expect_equal(unname(span[["A"]] + span[["B"]]), 2 * pi, tolerance = 1e-9)
  # A's children fill A's span
  expect_equal(
    unname(span[["A1"]] + span[["A2"]]),
    unname(span[["A"]]),
    tolerance = 1e-9
  )
})

test_that("inner.radius opens a centre hole (shifts the innermost ring out)", {
  lay0 <- vellumplot:::.sunburst_layout(
    h$id,
    h$parent,
    h$value,
    inner_radius = 0
  )
  lay3 <- vellumplot:::.sunburst_layout(
    h$id,
    h$parent,
    h$value,
    inner_radius = 0.3
  )
  expect_equal(min(lay0$r0), 0)
  expect_equal(min(lay3$r0), 0.3)
})

test_that("non-tree inputs are rejected", {
  expect_error(
    vellumplot:::.sunburst_layout(c("a", "b"), c(NA, NA), c(1, 1)),
    "one root"
  )
  expect_error(
    vellumplot:::.sunburst_layout(c("a", "b"), c("b", "a"), c(1, 1)),
    "one root|cycle|descend"
  )
  expect_error(
    vellumplot:::.sunburst_layout(c("r", "a"), c(NA, "ghost"), c(NA, 1)),
    "parent"
  )
})

test_that("a sunburst plot cannot be faceted or share the panel", {
  hh <- data.frame(
    id = c("r", "a", "b"),
    parent = c(NA, "r", "r"),
    value = c(NA, 1, 2),
    g = c("x", "x", "y")
  )
  expect_error(
    vellum::as_vellum_scene(
      vplot(hh) |> mark_sunburst(id, parent, value) |> facet_wrap(~g)
    ),
    "faceted"
  )
})

test_that("a non-finite or negative leaf value is a clear error", {
  bad_na <- data.frame(
    id = c("root", "A", "B"),
    parent = c(NA, "root", "root"),
    value = c(NA, NA, 2)
  )
  expect_error(
    vellumplot:::.sunburst_layout(bad_na$id, bad_na$parent, bad_na$value),
    "finite and non-negative"
  )
  bad_neg <- data.frame(
    id = c("root", "A", "B"),
    parent = c(NA, "root", "root"),
    value = c(NA, 1, -2)
  )
  expect_error(
    vellumplot:::.sunburst_layout(bad_neg$id, bad_neg$parent, bad_neg$value),
    "finite and non-negative"
  )
})

test_that("inner.radius is validated in both constructors", {
  expect_error(vsunburst(h, id, parent, value, inner.radius = 1), "\\[0, 1\\)")
  expect_error(
    vplot(h) |> mark_sunburst(id, parent, value, inner.radius = -0.1),
    "\\[0, 1\\)"
  )
})

test_that("vsunburst renders, flat and with a hole", {
  f1 <- local_tempfile(fileext = ".png")
  render_plot(vsunburst(h, id, parent, value), f1)
  expect_gt(file.info(f1)$size, 0)
  f2 <- local_tempfile(fileext = ".png")
  render_plot(vsunburst(h, id, parent, value, inner.radius = 0.4), f2)
  expect_gt(file.info(f2)$size, 0)
})
