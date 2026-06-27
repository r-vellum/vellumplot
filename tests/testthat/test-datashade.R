# G7: datashaded density marks.

test_that("mark_datashade() records a datashade layer", {
  p <- vplot(data.frame(x = 1:3, y = 1:3)) |> mark_datashade(x = x, y = y)
  L <- p@layers[[1]]
  expect_identical(L@mark, "datashade")
  expect_equal(L@stat_params$how, "eq_hist")
})

test_that("mark_point(auto = TRUE) records the auto flag", {
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg, auto = TRUE)
  expect_true(p@layers[[1]]@stat_params$auto)
})

test_that("a datashade layer still trains position scales from the data", {
  d <- data.frame(x = c(0, 10), y = c(-5, 5))
  p <- vplot(d) |> mark_datashade(x = x, y = y)
  sc <- vellumplot:::.build_panels(p)$scales
  expect_lt(sc$x$domain[1], 0)
  expect_gt(sc$x$domain[2], 10)
})

test_that("datashade renders, and auto falls back to markers below the threshold", {
  set.seed(1)
  n <- 20000
  df <- data.frame(x = rnorm(n), y = rnorm(n))
  f1 <- withr::local_tempfile(fileext = ".png")
  render_plot(vplot(df) |> mark_datashade(x = x, y = y), f1)
  expect_gt(file.info(f1)$size, 0)
  # small data with auto = TRUE just draws normal points (no error)
  f2 <- withr::local_tempfile(fileext = ".png")
  render_plot(vplot(mtcars) |> mark_point(x = wt, y = mpg, auto = TRUE), f2)
  expect_gt(file.info(f2)$size, 0)
})
