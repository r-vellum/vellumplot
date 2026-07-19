# Uncertainty marks: mark_halfeye() (slab + interval) and mark_interval().

test_that("the marks record their type and params", {
  he <- (vplot(mtcars) |>
    mark_halfeye(x = factor(cyl), y = mpg, .width = c(0.5, 0.9)))@layers[[1]]
  expect_identical(he@mark, "halfeye")
  expect_equal(he@stat_params$width, c(0.5, 0.9))
  iv <- (vplot(mtcars) |>
    mark_interval(x = factor(cyl), y = mpg, point = "mean"))@layers[[1]]
  expect_identical(iv@mark, "interval")
  expect_identical(iv@stat_params$point, "mean")
})

test_that("the point-interval is a median + nested equal-tailed quantiles", {
  set.seed(1)
  v <- rnorm(20000)
  pit <- vellumplot:::.point_interval(v, c(0.66, 0.95), "median")
  expect_equal(pit$point, stats::median(v))
  # 95% interval strictly contains the 66% interval
  expect_lt(pit$ints[[2]][1], pit$ints[[1]][1])
  expect_gt(pit$ints[[2]][2], pit$ints[[1]][2])
  # equal-tailed 95% ~ +/- 1.96
  expect_equal(unname(pit$ints[[2]]), c(-1.96, 1.96), tolerance = 0.1)
})

test_that("point = 'mean' uses the mean as the centre", {
  v <- c(rep(0, 9), 100) # mean 10, median 0
  pit <- vellumplot:::.point_interval(v, 0.5, "mean")
  expect_equal(pit$point, mean(v))
})

test_that("a halfeye extends the x domain to the right of the last category", {
  set.seed(1)
  d <- data.frame(g = rep(c("a", "b", "c"), each = 200), v = rnorm(600))
  sc <- vellumplot:::.build_panels(
    vplot(d) |> mark_halfeye(x = g, y = v)
  )$scales
  # three band positions are 1..3; the one-sided slab pushes the max past 3
  expect_gt(sc$x$domain[2], 3)
})

test_that("mark_interval does not widen the x domain (no slab)", {
  set.seed(1)
  d <- data.frame(g = rep(c("a", "b", "c"), each = 200), v = rnorm(600))
  sc <- vellumplot:::.build_panels(
    vplot(d) |> mark_interval(x = g, y = v)
  )$scales
  expect_lte(sc$x$domain[2], 3.5) # stays within the band grid (no rightward slab)
})

test_that("halfeye and interval render, plain and grouped", {
  set.seed(1)
  d <- data.frame(
    g = rep(c("a", "b", "c"), each = 300),
    v = rnorm(900, rep(0:2, each = 300))
  )
  for (p in list(
    vplot(d) |> mark_halfeye(x = g, y = v),
    vplot(d) |> mark_interval(x = g, y = v),
    vplot(d) |> mark_halfeye(x = g, y = v, color = g)
  )) {
    f <- local_tempfile(fileext = ".png")
    render_plot(p, f)
    expect_gt(file.info(f)$size, 0)
  }
})

test_that("width = is rejected (a likely typo for .width)", {
  expect_error(
    vplot(mtcars) |> mark_halfeye(x = factor(cyl), y = mpg, width = 0.9),
    "\\.width"
  )
  expect_error(
    vplot(mtcars) |> mark_interval(x = factor(cyl), y = mpg, width = 0.9),
    "\\.width"
  )
})

test_that("a category with < 2 finite observations is skipped with a warning", {
  d <- data.frame(
    g = c("a", "a", "a", "a", "b"), # b has a single observation
    v = c(1, 2, 3, 4, 5)
  )
  f <- local_tempfile(fileext = ".png")
  expect_warning(
    render_plot(vplot(d) |> mark_interval(x = g, y = v), f),
    "Skipping.*b"
  )
  expect_gt(file.info(f)$size, 0) # group a still draws
})
