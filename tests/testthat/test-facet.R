# G3 (resolve lattice) + G4 (faceting).

test_that("facet_wrap()/facet_grid() require a formula", {
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
  expect_error(facet_wrap(p, "cyl"), "formula")
  expect_error(facet_grid(p, "x"), "formula")
})

test_that("facet_wrap() records a wrap FacetSpec", {
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg) |> facet_wrap(~cyl)
  expect_true(S7::S7_inherits(p@facet, quill:::FacetSpec))
  expect_identical(p@facet@type, "wrap")
})

test_that("scales='free' sets the resolve lattice to independent", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    facet_wrap(~cyl, scales = "free")
  expect_identical(quill:::.resolve_for(p, "x"), "independent")
  expect_identical(quill:::.resolve_for(p, "y"), "independent")
  p2 <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    facet_wrap(~cyl, scales = "free_x")
  expect_identical(quill:::.resolve_for(p2, "x"), "independent")
  expect_identical(quill:::.resolve_for(p2, "y"), "shared")
})

test_that("resolve_scale() sets resolutions and defaults to shared", {
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
  expect_identical(quill:::.resolve_for(p, "y"), "shared")
  p <- resolve_scale(p, y = "independent")
  expect_identical(quill:::.resolve_for(p, "y"), "independent")
  expect_error(resolve_scale(p, y = "nonsense"))
})

test_that("facet_wrap() builds one panel per level on a wrapped grid", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    facet_wrap(~cyl, ncol = 2)
  built <- quill:::.build_panels(p)
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
  built <- quill:::.build_panels(p)
  expect_equal(built$fa$R, 2) # am in {0,1}
  expect_equal(built$fa$C, 3) # cyl in {4,6,8}
  expect_length(built$panels, 6)
})

test_that("shared scales give every panel the same domain; free gives different", {
  shared <- quill:::.build_panels(
    vplot(mtcars) |> mark_point(x = wt, y = mpg) |> facet_wrap(~cyl)
  )
  dom <- lapply(shared$panels, function(p) p$x_sc$domain)
  expect_true(all(vapply(
    dom,
    function(d) isTRUE(all.equal(d, dom[[1]])),
    logical(1)
  )))

  free <- quill:::.build_panels(
    vplot(mtcars) |>
      mark_point(x = wt, y = mpg) |>
      facet_wrap(~cyl, scales = "free_x")
  )
  fdom <- lapply(free$panels, function(p) p$x_sc$domain)
  expect_false(isTRUE(all.equal(fdom[[1]], fdom[[2]])))
})

test_that("faceted plots render to a PNG", {
  f1 <- withr::local_tempfile(fileext = ".png")
  render_plot(
    vplot(mtcars) |> mark_point(x = wt, y = mpg) |> facet_wrap(~cyl),
    f1
  )
  expect_gt(file.info(f1)$size, 0)

  f2 <- withr::local_tempfile(fileext = ".png")
  render_plot(
    vplot(mtcars) |> mark_point(x = wt, y = mpg) |> facet_grid(am ~ cyl),
    f2
  )
  expect_gt(file.info(f2)$size, 0)
})

test_that("a single-panel plot is a 1x1 grid", {
  built <- quill:::.build_panels(
    vplot(mtcars) |> mark_point(x = wt, y = mpg)
  )
  expect_equal(built$fa$R, 1)
  expect_equal(built$fa$C, 1)
  expect_length(built$panels, 1)
})
