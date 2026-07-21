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

test_that("the first top-level wedge starts at 12 o'clock, winding clockwise", {
  lay <- vellumplot:::.sunburst_layout(h$id, h$parent, h$value)
  # `sector_grob` fills theta0 -> theta1 increasing (CCW), so a clockwise wedge
  # from the top has its *upper* edge (theta1) at pi/2 and extends to smaller
  # angles. The first input child (A) owns fraction 0, so its theta1 == pi/2.
  expect_equal(lay$theta1[lay$id == "A"], pi / 2, tolerance = 1e-9)
  expect_lt(lay$theta0[lay$id == "A"], lay$theta1[lay$id == "A"]) # winds clockwise
})

test_that("wedges are coloured by branch (not by depth), lightened outward", {
  lay <- vellumplot:::.sunburst_layout(h$id, h$parent, h$value)
  col <- setNames(lay$colour, lay$id)
  # sibling branches differ (the old depth-colouring made these identical)
  expect_false(col[["A"]] == col[["B"]])
  # a child shares its branch's hue but is lightened (paler) than the parent ring
  lum <- function(x) sum(farver::decode_colour(x))
  expect_gt(lum(col[["A1"]]), lum(col[["A"]])) # A1 lighter than A
  expect_gt(lum(col[["B1"]]), lum(col[["B"]]))
})

test_that("inner_radius opens a centre hole (shifts the innermost ring out)", {
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

test_that("inner_radius is validated in both constructors", {
  expect_error(vsunburst(h, id, parent, value, inner_radius = 1), "\\[0, 1\\)")
  expect_error(
    vplot(h) |> mark_sunburst(id, parent, value, inner_radius = -0.1),
    "\\[0, 1\\)"
  )
})


test_that("vsunburst renders, flat and with a hole", {
  f1 <- local_tempfile(fileext = ".png")
  render_plot(vsunburst(h, id, parent, value), f1)
  expect_gt(file.info(f1)$size, 0)
  f2 <- local_tempfile(fileext = ".png")
  render_plot(vsunburst(h, id, parent, value, inner_radius = 0.4), f2)
  expect_gt(file.info(f2)$size, 0)
})

# ---- labels ---------------------------------------------------------------

test_that("layout carries each node's value and the root as an attribute", {
  lay <- vellumplot:::.sunburst_layout(h$id, h$parent, h$value)
  expect_true("value" %in% names(lay))
  val <- setNames(lay$value, lay$id)
  # leaves keep their own value; an internal node sums its subtree
  expect_equal(unname(val[["A1"]]), 3)
  expect_equal(unname(val[["A"]]), 5) # A1(3) + A2(2)
  root <- attr(lay, "root")
  expect_identical(root$id, "root")
  expect_equal(root$value, 9) # whole-tree total 3+2+4
})

test_that(".upright_rot() keeps text upright, in (-90, 90]", {
  # a full sweep of mid-angles, both orientations, must never point down
  th <- seq(-pi, pi, length.out = 33)
  for (along in c("radius", "tangent")) {
    r <- vellumplot:::.upright_rot(th, along)
    expect_true(all(r > -90 & r <= 90))
  }
  # horizontal segment: radial baseline is flat, tangential is vertical
  expect_equal(vellumplot:::.upright_rot(0, "radius"), 0)
  expect_equal(vellumplot:::.upright_rot(0, "tangent"), 90)
})

test_that(".contrast_ink() picks white on dark fills, black on light", {
  ink <- vellumplot:::.contrast_ink(c("#0b1f2a", "#f0f0f0", "black", "white"))
  expect_identical(ink, c("white", "black", "white", "black"))
})

test_that("labels render, and show_values / root_label are drawables", {
  f <- local_tempfile(fileext = ".png")
  render_plot(
    vsunburst(h, id, parent, value, show_values = TRUE, root_label = TRUE),
    f
  )
  expect_gt(file.info(f)$size, 0)
})

test_that("small wedges drop their label (fewer text grobs than segments)", {
  text_n <- function(p) {
    sc <- vellum::as_vellum_scene(p)
    root <- vellum:::.materialize(sc)
    n <- 0L
    walk <- function(node) {
      if (S7::S7_inherits(node, vellum:::gtree)) {
        for (ch in node@children) {
          walk(ch)
        }
      } else if (S7::S7_inherits(node, vellum:::grob_text)) {
        n <<- n + 1L
      }
    }
    walk(root)
    n
  }
  # one dominant branch plus many slivers: most slivers can't fit a label
  wide <- data.frame(
    id = c("root", "Big", paste0("s", 1:12)),
    parent = c(NA, "root", rep("root", 12)),
    value = c(NA, 500, rep(1, 12))
  )
  n_seg <- nrow(vellumplot:::.sunburst_layout(wide$id, wide$parent, wide$value))
  n_txt <- text_n(vsunburst(wide, id, parent, value, width = 4, height = 4))
  expect_lt(n_txt, n_seg) # at least one sliver went unlabelled
})
