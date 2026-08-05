# Phase 4: scale_*_steps aliases for the binned colour scales.

test_that("scale_color_steps / scale_fill_steps alias the binned scales", {
  expect_identical(scale_color_steps, scale_color_binned)
  expect_identical(scale_fill_steps, scale_fill_binned)
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg, color = hp)
  expect_no_error(plot_svg(scale_color_steps(p, n = 4)))
})
