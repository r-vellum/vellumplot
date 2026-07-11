# Per-layer data: a layer overrides the plot data.

resolve <- function(p) vellumplot:::.resolve_layers(p)
train <- function(p) vellumplot:::.train_scales(p, resolve(p))

hi <- mtcars[which.max(mtcars$mpg), , drop = FALSE]

test_that("a layer resolves against its own data", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    mark_point(data = hi, x = wt, y = mpg)
  r <- resolve(p)
  expect_identical(r[[1]]$n, nrow(mtcars))
  expect_identical(r[[2]]$n, 1L)
  expect_identical(r[[2]]$values$x, hi$wt)
})

test_that("layer data must be a data frame", {
  expect_error(
    vplot(mtcars) |> mark_point(x = wt, y = mpg, data = 1:3),
    "data frame"
  )
})

test_that("an own-data layer expands the shared scales", {
  far <- data.frame(wt = 99, mpg = 0)
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    mark_point(data = far, x = wt, y = mpg)
  sc <- train(p)
  expect_gte(sc$x$domain[2], 99)
  expect_lte(sc$y$domain[1], 0)
})

test_that("under faceting, own-data WITH the facet var is subset per panel", {
  d2 <- data.frame(wt = c(2, 3, 4), mpg = c(30, 20, 15), cyl = c(4, 6, 8))
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    facet_wrap(~cyl) |>
    mark_point(data = d2, x = wt, y = mpg, color = "red")
  built <- vellumplot:::.build_panels(p)
  ns <- vapply(built$panels, function(pp) pp$resolved[[2]]$n, integer(1))
  expect_identical(ns, c(1L, 1L, 1L))
})

test_that("under faceting, own-data WITHOUT the facet var draws on every panel", {
  d3 <- data.frame(wt = 4, mpg = 30) # no cyl
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    facet_wrap(~cyl) |>
    mark_text(data = d3, x = wt, y = mpg, label = "*")
  built <- vellumplot:::.build_panels(p)
  ns <- vapply(built$panels, function(pp) pp$resolved[[2]]$n, integer(1))
  expect_true(all(ns == 1L))
})

test_that("two layers with different data both render", {
  f <- local_tempfile(fileext = ".png")
  render_plot(
    vplot(mtcars) |>
      mark_point(x = wt, y = mpg) |>
      mark_point(data = hi, x = wt, y = mpg, color = "red", size = 4),
    f
  )
  expect_gt(file.info(f)$size, 0)
})

test_that("the layer print marks an own-data layer", {
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg, data = hi)
  expect_match(
    vellumplot:::.format_layer(p@layers[[1]]),
    "own data",
    fixed = TRUE
  )
})
