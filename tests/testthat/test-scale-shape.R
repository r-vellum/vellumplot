# Shape as a mapped aesthetic.

train <- function(p) {
  vellumplot:::.train_scales(p, vellumplot:::.resolve_layers(p))
}

test_that("a mapped shape trains a discrete shape scale", {
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg, shape = factor(cyl))
  sh <- train(p)$shape
  expect_false(is.null(sh))
  expect_identical(sh$kind, "shape")
  expect_identical(sh$levels, c("4", "6", "8"))
  expect_identical(sh$shapes, c("circle", "square", "triangle"))
  expect_identical(sh$map("6"), "square")
})

test_that("shape is NULL when unmapped", {
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
  expect_null(train(p)$shape)
})

test_that("scale_shape(values=) overrides the default shapes", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, shape = factor(cyl)) |>
    scale_shape(values = c("plus", "cross", "diamond"))
  expect_identical(train(p)$shape$shapes, c("plus", "cross", "diamond"))
})

test_that("more shape levels than the palette errors, not silent recycling (H42)", {
  # The default palette has 8 shapes, so 9 levels overflow it.
  d <- data.frame(x = 1:9, y = 1:9, g = factor(letters[1:9]))
  p <- vplot(d) |> mark_point(x = x, y = y, shape = g)
  expect_error(train(p), "Not enough shapes")
})

test_that("triangle_down and star extend the default palette and are valid values", {
  # 7th/8th default shapes cover 7-8 levels without an explicit scale.
  expect_identical(
    vellumplot:::.SHAPE_PALETTE[7:8],
    c("triangle_down", "star")
  )
  d <- data.frame(x = 1:8, y = 1:8, g = factor(letters[1:8]))
  sh <- train(vplot(d) |> mark_point(x = x, y = y, shape = g))$shape
  expect_identical(sh$shapes[7:8], c("triangle_down", "star"))
  # accepted as explicit scale_shape() values, and render without error
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, shape = factor(cyl)) |>
    scale_shape(values = c("triangle_down", "star", "circle"))
  expect_identical(train(p)$shape$shapes, c("triangle_down", "star", "circle"))
  f <- local_tempfile(fileext = ".png")
  render_plot(p, f)
  expect_gt(file.info(f)$size, 0)
})

test_that("a constant shape param still works (no scale)", {
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg, shape = "square")
  expect_null(train(p)$shape)
  f <- local_tempfile(fileext = ".png")
  render_plot(p, f)
  expect_gt(file.info(f)$size, 0)
})

test_that("mapped shapes render distinct glyphs", {
  # two groups, well separated in x, with different shapes -> ink in both halves
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg, shape = factor(cyl))
  img <- render_px(p)
  expect_true(has_ink(img, rows = c(0.1, 0.9), cols = c(0.1, 0.5)))
  expect_true(has_ink(img, rows = c(0.1, 0.9), cols = c(0.5, 0.9)))
})
