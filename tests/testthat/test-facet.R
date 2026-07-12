# G3 (resolve lattice) + G4 (faceting).

test_that("facet_wrap()/facet_grid() require a formula", {
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
  expect_error(facet_wrap(p, "cyl"), "formula")
  expect_error(facet_grid(p, "x"), "formula")
})

test_that(".layer_panel_idx matches a layer's own data and falls back when the facet var is absent", {
  d <- data.frame(x = 1:4, y = 1:4, g = factor(c("a", "b", "a", "b")))
  p <- vplot(d) |> mark_point(x = x, y = y) |> facet_wrap(~g)
  fa <- vellumplot:::.facet_assign(p)
  panel_a <- Filter(function(pn) identical(pn$lvl$wrap, "a"), fa$panels)[[1]]
  # a layer whose own data carries the facet var -> only its matching rows
  ld <- data.frame(x = 10:12, y = 1:3, g = factor(c("a", "b", "a")))
  expect_identical(
    vellumplot:::.layer_panel_idx(p@facet, ld, panel_a),
    c(1L, 3L)
  )
  # data lacking the facet var -> every row (the layer draws on every panel)
  nod <- data.frame(x = 1:3, y = 1:3)
  expect_identical(
    vellumplot:::.layer_panel_idx(p@facet, nod, panel_a),
    1:3
  )
})

test_that("numeric facets order numerically, not lexicographically (H20)", {
  d <- data.frame(x = 1:6, y = 1:6, f = c(2, 10, 1, 2, 10, 1))
  p <- vplot(d) |> mark_point(x = x, y = y) |> facet_wrap(~f)
  fa <- vellumplot:::.facet_assign(p)
  expect_identical(fa$wrap_labels, c("1", "2", "10")) # not "1","10","2"
})

test_that("multi-variable grid facets order by each variable's type (H20)", {
  d <- data.frame(
    x = 1:4,
    y = 1:4,
    r = c(2, 10, 2, 10),
    cc = factor(c("b", "a", "b", "a"), levels = c("b", "a"))
  )
  p <- vplot(d) |>
    mark_point(x = x, y = y) |>
    facet_grid(r ~ cc)
  fa <- vellumplot:::.facet_assign(p)
  expect_identical(fa$row_labels, c("2", "10")) # numeric rows numerically
  expect_identical(fa$col_labels, c("b", "a")) # cols follow factor level order
})

test_that("facet_wrap() records a wrap FacetSpec", {
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg) |> facet_wrap(~cyl)
  expect_true(S7::S7_inherits(p@facet, vellumplot:::FacetSpec))
  expect_identical(p@facet@type, "wrap")
})

test_that("scales='free' sets the resolve lattice to independent", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    facet_wrap(~cyl, scales = "free")
  expect_identical(vellumplot:::.resolve_for(p, "x"), "independent")
  expect_identical(vellumplot:::.resolve_for(p, "y"), "independent")
  p2 <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    facet_wrap(~cyl, scales = "free_x")
  expect_identical(vellumplot:::.resolve_for(p2, "x"), "independent")
  expect_identical(vellumplot:::.resolve_for(p2, "y"), "shared")
})

test_that("resolve_scale() sets resolutions and defaults to shared", {
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
  expect_identical(vellumplot:::.resolve_for(p, "y"), "shared")
  p <- resolve_scale(p, y = "independent")
  expect_identical(vellumplot:::.resolve_for(p, "y"), "independent")
  expect_error(resolve_scale(p, y = "nonsense"))
})

test_that("resolve_scale() rejects unknown aesthetics", {
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
  expect_error(resolve_scale(p, color = "independent"), "aesthetic")
  expect_error(resolve_scale(p, Y = "shared"), "aesthetic")
})

test_that("facet_wrap() builds one panel per level on a wrapped grid", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    facet_wrap(~cyl, ncol = 2)
  built <- vellumplot:::.build_panels(p)
  expect_length(built$panels, 3) # cyl in {4,6,8}
  expect_equal(built$fa$C, 2)
  expect_equal(built$fa$R, 2)
  # every row went into exactly one panel
  expect_equal(
    sum(vapply(built$panels, function(q) length(q$idx), integer(1))),
    nrow(mtcars)
  )
})

test_that("facet_grid() builds an R x C panel grid", {
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg) |> facet_grid(am ~ cyl)
  built <- vellumplot:::.build_panels(p)
  expect_equal(built$fa$R, 2) # am in {0,1}
  expect_equal(built$fa$C, 3) # cyl in {4,6,8}
  expect_length(built$panels, 6)
})

test_that("shared scales give every panel the same domain; free gives different", {
  shared <- vellumplot:::.build_panels(
    vplot(mtcars) |> mark_point(x = wt, y = mpg) |> facet_wrap(~cyl)
  )
  dom <- lapply(shared$panels, function(p) p$x_sc$domain)
  expect_true(all(vapply(
    dom,
    function(d) isTRUE(all.equal(d, dom[[1]])),
    logical(1)
  )))

  free <- vellumplot:::.build_panels(
    vplot(mtcars) |>
      mark_point(x = wt, y = mpg) |>
      facet_wrap(~cyl, scales = "free_x")
  )
  fdom <- lapply(free$panels, function(p) p$x_sc$domain)
  expect_false(isTRUE(all.equal(fdom[[1]], fdom[[2]])))
})

test_that("faceted plots render to a PNG", {
  f1 <- local_tempfile(fileext = ".png")
  render_plot(
    vplot(mtcars) |> mark_point(x = wt, y = mpg) |> facet_wrap(~cyl),
    f1
  )
  expect_gt(file.info(f1)$size, 0)

  f2 <- local_tempfile(fileext = ".png")
  render_plot(
    vplot(mtcars) |> mark_point(x = wt, y = mpg) |> facet_grid(am ~ cyl),
    f2
  )
  expect_gt(file.info(f2)$size, 0)
})

test_that("a single-panel plot is a 1x1 grid", {
  built <- vellumplot:::.build_panels(
    vplot(mtcars) |> mark_point(x = wt, y = mpg)
  )
  expect_equal(built$fa$R, 1)
  expect_equal(built$fa$C, 1)
  expect_length(built$panels, 1)
})
