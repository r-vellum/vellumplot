# lims()/xlim()/ylim(): shortcuts that declare a scale carrying only its limits.

test_that("xlim()/ylim() set a continuous position window", {
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg) |> xlim(0, 6)
  b <- vellumplot:::.build_panels(p)
  # expanded 5% around c(0, 6)
  expect_equal(b$scales$x$domain, scales::expand_range(c(0, 6), mul = 0.05))

  p2 <- vplot(mtcars) |> mark_point(x = wt, y = mpg) |> ylim(10, 35)
  b2 <- vellumplot:::.build_panels(p2)
  expect_equal(b2$scales$y$domain, scales::expand_range(c(10, 35), mul = 0.05))
})

test_that("lims() dispatches per named aesthetic", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    lims(x = c(0, 6), y = c(10, 35))
  b <- vellumplot:::.build_panels(p)
  expect_equal(b$scales$x$domain, scales::expand_range(c(0, 6), mul = 0.05))
  expect_equal(b$scales$y$domain, scales::expand_range(c(10, 35), mul = 0.05))
})

test_that("a character xlim sets discrete levels (order/subset)", {
  df <- data.frame(g = c("a", "b", "c"), y = c(1, 2, 3))
  p <- vplot(df) |> mark_bar(x = g, y = y) |> xlim("c", "a")
  b <- vellumplot:::.build_panels(p)
  expect_equal(b$scales$x$labels, c("c", "a"))
})

test_that("a descending continuous limit reverses the axis", {
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg) |> xlim(6, 0)
  b <- vellumplot:::.build_panels(p)
  expect_gt(b$scales$x$domain[1], b$scales$x$domain[2])
})

test_that("continuous limits must be length 2, and lims args must be named", {
  expect_error(
    vplot(mtcars) |> mark_point(x = wt, y = mpg) |> xlim(1, 2, 3),
    "length-2"
  )
  expect_error(
    vplot(mtcars) |> mark_point(x = wt, y = mpg) |> lims(c(0, 1)),
    "named"
  )
})

test_that("a plot with limits still renders", {
  f <- local_tempfile(fileext = ".png")
  expect_no_error(
    render_plot(vplot(mtcars) |> mark_point(x = wt, y = mpg) |> xlim(0, 6), f)
  )
})
