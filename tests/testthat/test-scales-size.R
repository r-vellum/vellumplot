# Size scale: range/limits/breaks overrides.

train <- function(p) {
  quill:::.train_scales(p, quill:::.resolve_layers(p))
}

test_that("scale_size(range=) sets the output mm range", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, size = hp) |>
    scale_size(range = c(2, 10))
  sz <- train(p)$size
  expect_gte(min(sz$legend_sizes), 2 - 1e-9)
  expect_lte(max(sz$map(range(mtcars$hp))), 10 + 1e-9)
})

test_that("default size range is c(1, 4) via scale_size", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, size = hp) |>
    scale_size()
  rng <- range(train(p)$size$map(range(mtcars$hp)))
  expect_equal(rng, c(1, 4), tolerance = 1e-6)
})

test_that("scale_size(limits=) sets the data domain", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, size = hp) |>
    scale_size(limits = c(0, 400))
  expect_identical(train(p)$size$range, c(0, 400))
})

test_that("explicit size breaks are honoured", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, size = hp) |>
    scale_size(breaks = c(100, 200, 300))
  expect_identical(train(p)$size$legend_breaks, c(100, 200, 300))
})

test_that("a larger range renders larger points", {
  small <- render_px(
    vplot(mtcars) |>
      mark_point(x = wt, y = mpg, size = hp) |>
      scale_size(range = c(1, 2))
  )
  big <- render_px(
    vplot(mtcars) |>
      mark_point(x = wt, y = mpg, size = hp) |>
      scale_size(range = c(4, 10))
  )
  ink <- function(img) {
    sum(rowSums(img[,, 1:3, drop = FALSE] <= 0.3, dims = 2) == 3)
  }
  expect_gt(ink(big), ink(small))
})
