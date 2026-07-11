# coord_cartesian(): view-window zoom (clip, not drop).

train <- function(p) {
  vellumplot:::.train_scales(p, vellumplot:::.resolve_layers(p))
}

test_that("coord_cartesian(xlim=) zooms the trained x domain", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    coord_cartesian(xlim = c(2, 4))
  x <- train(p)$x
  # 5% expansion around c(2, 4)
  expect_equal(x$domain, scales::expand_range(c(2, 4), mul = 0.05))
  expect_true(all(x$breaks >= 2 & x$breaks <= 4))
})

test_that("zoom clips but does not drop data, and renders", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    coord_cartesian(ylim = c(20, 30))
  # all rows still resolve (no filtering)
  res <- vellumplot:::.resolve_layers(p)
  expect_identical(res[[1]]$n, nrow(mtcars))
  f <- local_tempfile(fileext = ".png")
  render_plot(p, f)
  expect_gt(file.info(f)$size, 0)
})

test_that("coord limits take precedence over scale limits", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    scale_x_continuous(limits = c(0, 6)) |>
    coord_cartesian(xlim = c(2, 4))
  expect_equal(train(p)$x$domain, scales::expand_range(c(2, 4), mul = 0.05))
})

test_that("coord_cartesian without limits is a no-op vs default", {
  base <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
  same <- base |> coord_cartesian()
  expect_equal(train(base)$x$domain, train(same)$x$domain)
})

test_that("coord zoom works under faceting", {
  f <- local_tempfile(fileext = ".png")
  render_plot(
    vplot(mtcars) |>
      mark_point(x = wt, y = mpg) |>
      facet_wrap(~cyl) |>
      coord_cartesian(xlim = c(2, 4)),
    f
  )
  expect_gt(file.info(f)$size, 0)
})
