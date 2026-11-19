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
  expect_error(resolve_scale(p, label = "independent"), "aesthetic")
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

test_that("a discrete colour scale keeps every level when the faceting variable is also the colour (#145)", {
  # facet_wrap(~cyl) evaluates factor(cyl) per panel, so each panel's resolved
  # layer carries a factor with only its own level; pooling must still recover
  # every level -- not collapse to the first panel's, greying out the rest.
  built <- vellumplot:::.build_panels(
    vplot(mtcars) |>
      mark_point(x = wt, y = mpg, color = factor(cyl)) |>
      facet_wrap(~cyl)
  )
  cs <- built$scales$color
  expect_equal(cs$levels, c("4", "6", "8"))
  expect_length(cs$colors, 3L)
  expect_false(anyNA(cs$colors))

  # identical to the unfaceted training (same levels, same colours)
  unfaceted <- vellumplot:::.build_panels(
    vplot(mtcars) |> mark_point(x = wt, y = mpg, color = factor(cyl))
  )$scales$color
  expect_equal(cs$levels, unfaceted$levels)
  expect_equal(cs$colors, unfaceted$colors)
})

# VP-9: a layer's own data is split on whichever facet variables it carries,
# and facet levels are the union over the plot data and every layer's data.

test_that("a layer with only some facet variables draws in its own row (VP-9)", {
  ann <- data.frame(am = c(0, 1), wt = 4, mpg = 30, lab = c("auto", "manual"))
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    mark_text(data = ann, x = wt, y = mpg, label = lab) |>
    facet_grid(am ~ cyl)
  b <- .build_panels(p)
  # one label per panel: am = 0 in row 1, am = 1 in row 2, across all columns
  labs <- vapply(
    b$panels,
    function(pn) paste(pn$resolved[[2]]$values$label, collapse = "|"),
    character(1)
  )
  rows <- vapply(b$panels, function(pn) pn$r, integer(1))
  expect_identical(labs[rows == 1L], rep("auto", 3))
  expect_identical(labs[rows == 2L], rep("manual", 3))
})

test_that("a layer with only some wrap variables repeats across the rest (VP-9)", {
  ann <- data.frame(am = 1, wt = 4, mpg = 30)
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    mark_point(data = ann, x = wt, y = mpg) |>
    facet_wrap(~ cyl + am)
  b <- .build_panels(p)
  n <- vapply(b$panels, function(pn) pn$resolved[[2]]$n, integer(1))
  expect_identical(b$fa$wrap_labels[n == 1L], c("4, 1", "6, 1", "8, 1"))
  expect_true(all(n[!grepl(", 1$", b$fa$wrap_labels)] == 0L))
})

test_that("a facet level only a layer's data has gets its own panel (VP-9)", {
  extra <- data.frame(cyl = c(5, 10), wt = 3, mpg = 20)
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    mark_point(data = extra, x = wt, y = mpg) |>
    facet_wrap(~cyl)
  b <- .build_panels(p)
  # pooled levels sort numerically, as one table's would
  expect_identical(b$fa$wrap_labels, c("4", "5", "6", "8", "10"))
  n1 <- vapply(b$panels, function(pn) pn$resolved[[1]]$n, integer(1))
  n2 <- vapply(b$panels, function(pn) pn$resolved[[2]]$n, integer(1))
  expect_identical(n1, c(11L, 0L, 7L, 14L, 0L))
  expect_identical(n2, c(0L, 1L, 0L, 0L, 1L))
  expect_no_error(vellum::as_vellum_scene(p))
  # a grid unions each dimension on its own
  g <- .facet_assign(
    vplot(mtcars) |>
      mark_point(x = wt, y = mpg) |>
      mark_point(data = data.frame(am = 2, wt = 3, mpg = 20), x = wt, y = mpg) |>
      facet_grid(am ~ cyl)
  )
  expect_identical(g$row_labels, c("0", "1", "2"))
  expect_identical(g$col_labels, c("4", "6", "8"))
})

test_that("factor facet levels union across tables in level order (VP-9)", {
  d <- data.frame(x = 1:2, y = 1:2, g = factor(c("b", "a"), levels = c("b", "a")))
  ld <- data.frame(x = 1, y = 1, g = factor("c", levels = c("c", "b")))
  fa <- .facet_assign(
    vplot(d) |>
      mark_point(x = x, y = y) |>
      mark_point(data = ld, x = x, y = y) |>
      facet_wrap(~g)
  )
  expect_identical(fa$wrap_labels, c("b", "a", "c"))
})
