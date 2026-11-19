# Phase 4: scale_*_steps aliases for the binned colour scales.

test_that("scale_color_steps / scale_fill_steps are ggplot2's two-colour scales", {
  expect_identical(scale_colour_steps, scale_color_steps)
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg, color = hp)
  expect_no_error(plot_svg(scale_color_steps(p, low = "white", n.breaks = 4)))
})
