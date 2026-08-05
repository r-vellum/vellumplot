# Phase 1a: reference / summary / count marks.

svg <- function(p) paste(plot_svg(p), collapse = "")
compile <- function(p) vellumplot:::.compile_plot(p)

d <- data.frame(x = 1:20, y = (1:20) + c(-1, 1))
pr <- data.frame(
  g = c("a", "b", "c"),
  y = c(2, 4, 3),
  lo = c(1, 3, 2),
  hi = c(3, 5, 4)
)

test_that("mark_abline draws sloped reference lines and round-trips", {
  base <- vplot(d) |> mark_point(x = x, y = y)
  expect_no_error(compile(base |> mark_abline(slope = 1, intercept = 0)))
  expect_no_error(compile(base |> mark_abline(slope = c(1, -1), intercept = 0)))
  expect_no_error(from_spec(as_spec(
    base |> mark_abline(slope = 2, intercept = 1)
  )))
})

test_that("mark_function draws y = f(x) over the panel", {
  base <- vplot(d) |> mark_point(x = x, y = y)
  expect_no_error(compile(base |> mark_function(fun = function(x) x)))
  expect_no_error(compile(base |> mark_function(fun = sqrt, n = 51)))
  expect_error(vplot(d) |> mark_function(), "needs a .*fun")
})

test_that("mark_pointrange and mark_crossbar draw identity summaries", {
  expect_no_error(compile(
    vplot(pr) |> mark_pointrange(x = g, y = y, ymin = lo, ymax = hi)
  ))
  expect_no_error(compile(
    vplot(pr) |> mark_crossbar(x = g, y = y, ymin = lo, ymax = hi)
  ))
  expect_no_error(from_spec(as_spec(
    vplot(pr) |> mark_pointrange(x = g, y = y, ymin = lo, ymax = hi)
  )))
})

test_that("mark_count collapses coincident points and sizes by overlap", {
  cc <- data.frame(x = c(1, 1, 1, 2, 2), y = c(1, 1, 1, 2, 2))
  resolved <- vellumplot:::.build_panels(vplot(cc) |> mark_count(x = x, y = y))
  L <- resolved$panels[[1]]$resolved[[1]]
  # two unique points, counts 3 and 2
  expect_setequal(L$values$size, c(3, 2))
  expect_no_error(from_spec(as_spec(vplot(cc) |> mark_count(x = x, y = y))))
})
