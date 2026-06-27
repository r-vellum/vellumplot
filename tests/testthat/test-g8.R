# G8: themes + concat composition.

test_that("themes set the visual settings", {
  expect_identical(vellumplot:::.theme_of(vplot(mtcars))$panel_bg, "grey92")
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg) |> theme_minimal()
  expect_true(is.na(p@theme$panel_bg))
  expect_identical(p@theme$grid_col, "grey92")
  p2 <- vplot(mtcars) |> mark_point(x = wt, y = mpg) |> theme_bw()
  expect_identical(p2@theme$panel_bg, "white")
})

test_that("set_theme() overrides on top of the current theme", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    theme_bw() |>
    set_theme(panel_bg = "ivory")
  expect_identical(p@theme$panel_bg, "ivory")
  expect_identical(p@theme$grid_col, "grey90") # untouched theme_bw value
})

test_that("themed plots render", {
  for (thm in list(theme_minimal, theme_bw, theme_gray)) {
    f <- withr::local_tempfile(fileext = ".png")
    render_plot(thm(vplot(mtcars) |> mark_point(x = wt, y = mpg)), f)
    expect_gt(file.info(f)$size, 0)
  }
})

test_that("hconcat/vconcat/concat build a composition grid", {
  a <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
  b <- vplot(mtcars) |> mark_point(x = hp, y = mpg)
  h <- hconcat(a, b)
  expect_true(S7::S7_inherits(h, vellumplot:::PlotComposition))
  expect_equal(c(h@nrow, h@ncol), c(1, 2))
  expect_equal(c(vconcat(a, b)@nrow, vconcat(a, b)@ncol), c(2, 1))
  g <- concat(a, b, a, ncol = 2)
  expect_equal(c(g@nrow, g@ncol), c(2, 2)) # 3 plots wrap to 2x2
})

test_that("composition page size scales with the grid", {
  a <- vplot(mtcars, width = 4, height = 3) |> mark_point(x = wt, y = mpg)
  h <- hconcat(a, a)
  expect_equal(h@width, 8)
  expect_equal(h@height, 3)
})

test_that("a composition renders and render_plot rejects non-plots", {
  a <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
  b <- vplot(mtcars) |> mark_histogram(x = mpg, bins = 8)
  f <- withr::local_tempfile(fileext = ".png")
  render_plot(hconcat(a, b), f)
  expect_gt(file.info(f)$size, 0)
  expect_error(render_plot(mtcars, f), "PlotSpec")
})
