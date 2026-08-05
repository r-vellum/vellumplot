# Phase 1b: sina position + raincloud composition.

svg <- function(p) paste(plot_svg(p), collapse = "")
panels <- function(p) vellumplot:::.build_panels(p)

set.seed(1)
d <- data.frame(
  g = rep(c("a", "b", "c"), each = 60),
  y = c(rnorm(60), rnorm(60, 1.5), rgamma(60, 2))
)

test_that("sina position spreads points and accepts a constructor", {
  expect_no_error(svg(vplot(d) |> mark_point(x = g, y = y, position = "sina")))
  expect_no_error(svg(
    vplot(d) |> mark_point(x = g, y = y, position = position_sina(width = 0.9))
  ))
  # the offset actually moves points off the category centre
  base <- panels(vplot(d) |> mark_point(x = g, y = y))
  # (a plain point layer keeps x at the category; a sina layer is applied in the
  #  emitter, so just assert it renders differently)
  expect_false(identical(
    svg(vplot(d) |> mark_point(x = g, y = y)),
    svg(vplot(d) |> mark_point(x = g, y = y, position = "sina"))
  ))
})

test_that("mark_raincloud composes a halfeye and sina points", {
  p <- vplot(d) |> mark_raincloud(x = g, y = y)
  expect_length(p@layers, 2L)
  expect_identical(p@layers[[1]]@mark, "halfeye")
  expect_identical(p@layers[[2]]@mark, "point")
  expect_no_error(svg(p))
})

test_that("sina and raincloud tolerate NA values in y", {
  dn <- data.frame(
    g = rep(c("a", "b"), each = 20),
    y = c(rnorm(19), NA, rnorm(20))
  )
  expect_no_error(svg(vplot(dn) |> mark_point(x = g, y = y, position = "sina")))
  expect_no_error(svg(vplot(dn) |> mark_raincloud(x = g, y = y)))
})

test_that("mark_raincloud forwards colour and does not train a spurious alpha scale", {
  p <- vplot(d) |> mark_raincloud(x = g, y = y, color = g)
  b <- panels(p)
  expect_false(is.null(b$scales$color)) # colour is mapped
  expect_null(b$scales$alpha) # the constant alpha must stay a param
})
