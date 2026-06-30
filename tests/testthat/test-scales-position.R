# Position scale breadth: discrete limits, transforms, explicit breaks.

train <- function(p) {
  vellumplot:::.train_scales(p, vellumplot:::.resolve_layers(p))
}

test_that("scale_x_discrete(limits=) reorders / subsets levels", {
  p <- vplot(mtcars) |>
    mark_bar(x = factor(cyl)) |>
    scale_x_discrete(limits = c("8", "4", "6"))
  x <- train(p)$x
  expect_identical(x$labels, c("8", "4", "6"))
  expect_identical(x$map("8"), 1L)
  expect_identical(x$map("6"), 3L)
})

test_that("trans='sqrt' transforms the map and is continuous", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    scale_y_continuous(trans = "sqrt")
  y <- train(p)$y
  expect_equal(y$map(16), 4)
  expect_false(y$discrete)
})

test_that("trans='reverse' flips the axis via a decreasing domain", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    scale_x_continuous(trans = "reverse")
  x <- train(p)$x
  # The data mapping stays identity; the domain decreases so the axis reverses
  # (negating the data too would double-flip and cancel out).
  expect_equal(x$map(3), 3)
  expect_gt(x$domain[1], x$domain[2])
})

test_that("an unknown transform errors", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    scale_x_continuous(trans = "wobble")
  expect_error(train(p), "transform")
})

test_that("explicit position breaks/labels are honoured", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    scale_x_continuous(breaks = c(2, 4), labels = c("two", "four"))
  x <- train(p)$x
  expect_equal(x$breaks, c(2, 4))
  expect_identical(x$labels, c("two", "four"))
})

test_that("log10 transform is preserved (breaks within range, log-mapped)", {
  d <- data.frame(x = c(1, 10, 100, 1000), y = 1:4)
  p <- vplot(d) |>
    mark_point(x = x, y = y) |>
    scale_x_continuous(trans = "log10")
  x <- train(p)$x
  expect_equal(x$map(100), 2) # log10(100)
  expect_true(all(x$breaks >= log10(1) - 1e-9 & x$breaks <= log10(1000) + 1e-9))
})
