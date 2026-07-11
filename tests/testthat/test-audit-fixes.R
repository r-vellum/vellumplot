# Regression tests for the audit pass (scale training edge cases, composition
# nesting, insets, robustness guards).

train <- function(p) {
  vellumplot:::.train_scales(p, vellumplot:::.resolve_layers(p))
}
panels <- function(p) vellumplot:::.build_panels(p)

test_that("a log-scaled bar drops the 0 baseline instead of erroring", {
  d <- data.frame(x = c("a", "b", "c"), y = c(10, 100, 1000))
  p <- vplot(d) |> mark_bar(x = x, y = y) |> scale_y_continuous(trans = "log10")
  f <- local_tempfile(fileext = ".png")
  expect_no_error(render_plot(p, f))
  ysc <- panels(p)$scales$y
  expect_true(all(is.finite(ysc$domain)))
})

test_that("a sqrt-scaled area renders", {
  d <- data.frame(x = 1:5, y = c(1, 4, 9, 16, 25))
  p <- vplot(d) |> mark_area(x = x, y = y) |> scale_y_continuous(trans = "sqrt")
  f <- local_tempfile(fileext = ".png")
  expect_no_error(render_plot(p, f))
})

test_that("descending coord limits reverse the axis domain", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    coord_cartesian(xlim = c(4, 2))
  dom <- panels(p)$scales$x$domain
  expect_gt(dom[1], dom[2])
})

test_that("labels without matching breaks error rather than recycle", {
  base <- vplot(mtcars) |> mark_point(x = wt, y = mpg, color = hp)
  expect_error(
    panels(base |> scale_color_continuous(labels = c("a", "b"))),
    "one entry per break"
  )
})

test_that("discrete colour breaks select and order the legend levels", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, color = factor(cyl)) |>
    scale_color_discrete(breaks = c("8", "4"))
  sc <- panels(p)$scales$color
  expect_identical(sc$levels, c("8", "4"))
  expect_length(sc$colors, 2L)
})

test_that("discrete colour labels ride along with their levels", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, color = factor(cyl)) |>
    scale_color_discrete(labels = c("four", "six", "eight"))
  sc <- panels(p)$scales$color
  expect_identical(sc$labels, c("four", "six", "eight"))
})

test_that("scale_shape rejects unknown shape names at declaration", {
  expect_error(
    vplot(mtcars) |>
      mark_point(x = wt, y = mpg, shape = factor(cyl)) |>
      scale_shape(values = "star"),
    "Unknown shape"
  )
})

test_that("a composition nesting a faceted sub-plot renders all panels", {
  a <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
  fac <- a |> facet_wrap(~cyl)
  comp <- hconcat(a, vconcat(a, fac))
  expect_false(vellumplot:::.comp_alignable(comp))
  f <- local_tempfile(fileext = ".png")
  expect_no_error(render_plot(comp, f))
})

test_that("an inset drawn below the base still renders", {
  a <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
  b <- vplot(mtcars) |> mark_point(x = hp, y = mpg)
  f <- local_tempfile(fileext = ".png")
  expect_no_error(render_plot(inset(a, b, on_top = FALSE), f))
})

test_that("a boxplot tolerates NA in y", {
  d <- data.frame(g = rep(c("a", "b"), each = 5), y = c(1, 2, NA, 4, 5, 2, 3, 4, NA, 6))
  p <- vplot(d) |> mark_boxplot(x = g, y = y)
  f <- local_tempfile(fileext = ".png")
  expect_no_error(render_plot(p, f))
})

test_that("ragged and mis-counted design layouts error clearly", {
  a <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
  b <- vplot(mtcars) |> mark_point(x = hp, y = mpg)
  expect_error(concat(a, b, design = "AAB\nCC"), "same width")
  expect_error(concat(a, b, design = "AB\nCD"), "one area per plot")
})

test_that("resolve_scale requires named arguments", {
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg) |> facet_wrap(~cyl)
  expect_error(resolve_scale(p, "independent"), "must be named")
})
