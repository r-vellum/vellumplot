# mark_line(window=): rolling / cumulative / offset transforms.

train <- function(p) {
  vellumplot:::.resolve_layers(p)[[1]]
}

test_that("no window leaves the line as an identity stat", {
  L <- (vplot(mtcars) |> mark_line(x = wt, y = mpg))@layers[[1]]
  expect_identical(L@stat, "identity")
})

test_that("a window op sets the window stat", {
  L <- (vplot(mtcars) |> mark_line(x = wt, y = mpg, window = "mean"))@layers[[
    1
  ]]
  expect_identical(L@stat, "window")
  expect_identical(L@stat_params$op, "mean")
  expect_identical(L@stat_params$align, "right")
})

test_that("a trailing mean smooths the series and keeps every x (partial edges)", {
  d <- data.frame(x = 1:30, y = c(1:15, 15:1) + rep(c(-1, 1), 15))
  raw <- d$y
  r <- train(
    vplot(d) |> mark_line(x = x, y = y, window = list(op = "mean", k = 5))
  )
  expect_equal(r$n, 30) # partial = TRUE fills the edges, so no rows dropped
  expect_equal(r$values$x, d$x)
  # the moving average is smoother: smaller successive differences
  expect_lt(stats::sd(diff(r$values$y)), stats::sd(diff(raw)))
})

test_that("a non-partial window drops the incomplete leading edge", {
  d <- data.frame(x = 1:10, y = 1:10)
  r <- train(
    vplot(d) |>
      mark_line(
        x = x,
        y = y,
        window = list(op = "mean", k = 4, partial = FALSE)
      )
  )
  # first 3 rows have < k points and are dropped
  expect_equal(r$n, 7)
  expect_equal(min(r$values$x), 4)
})

test_that("cumsum accumulates and preserves length", {
  d <- data.frame(x = 1:5, y = c(1, 2, 3, 4, 5))
  r <- train(vplot(d) |> mark_line(x = x, y = y, window = "cumsum"))
  expect_equal(r$values$y, cumsum(d$y))
})

test_that("lag shifts by k and drops the undefined head", {
  d <- data.frame(x = 1:5, y = c(10, 20, 30, 40, 50))
  r <- train(
    vplot(d) |> mark_line(x = x, y = y, window = list(op = "lag", k = 1))
  )
  # y[i] becomes previous y; the first (NA) row is dropped
  expect_equal(r$n, 4)
  expect_equal(r$values$y, c(10, 20, 30, 40))
  expect_equal(r$values$x, c(2, 3, 4, 5))
})

test_that("the window runs per group, ordered by x", {
  d <- data.frame(
    x = c(3, 1, 2, 3, 1, 2),
    y = c(30, 10, 20, 300, 100, 200),
    g = rep(c("a", "b"), each = 3)
  )
  r <- train(
    vplot(d) |>
      mark_line(x = x, y = y, color = g, window = "cumsum")
  )
  # within group a (ordered x = 1,2,3): cumsum(10,20,30) = 10,30,60
  a <- r$values$y[as.character(r$values$color) == "a"]
  expect_equal(sort(a), c(10, 30, 60))
})

test_that("an unknown window op errors clearly", {
  expect_error(
    vplot(mtcars) |> mark_line(x = wt, y = mpg, window = "bogus"),
    "op"
  )
  expect_error(
    vplot(mtcars) |>
      mark_line(x = wt, y = mpg, window = list(op = "mean", align = "x")),
    "align"
  )
})

test_that("a windowed line renders", {
  d <- data.frame(x = 1:50, y = sin(1:50 / 5))
  f <- local_tempfile(fileext = ".png")
  render_plot(
    vplot(d) |> mark_line(x = x, y = y, window = list(op = "mean", k = 5)),
    f
  )
  expect_gt(file.info(f)$size, 0)
})
