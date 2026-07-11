# guides(): per-scale legend control (guide = "none" / guide_legend()).

df <- data.frame(wt = mtcars$wt, mpg = mtcars$mpg, cyl = factor(mtcars$cyl))

test_that("guides(color = 'none') drops the colour legend", {
  p <- vplot(df) |>
    mark_point(x = wt, y = mpg, color = cyl) |>
    guides(color = "none")
  expect_length(
    vellumplot:::.legend_guides(vellumplot:::.build_panels(p)$scales),
    0L
  )
})

test_that("guide_none() hides a legend even when no scale is declared", {
  p <- vplot(df) |>
    mark_point(x = wt, y = mpg, color = cyl) |>
    guides(color = guide_none())
  expect_length(
    vellumplot:::.legend_guides(vellumplot:::.build_panels(p)$scales),
    0L
  )
  # the colour mapping still applies (marks are still coloured)
  b <- vellumplot:::.build_panels(p)
  expect_false(is.null(b$scales$color))
})

test_that("guide_legend(reverse=) reverses the key order only", {
  p <- vplot(df) |>
    mark_point(x = wt, y = mpg, color = cyl) |>
    guides(color = guide_legend(reverse = TRUE))
  b <- vellumplot:::.build_panels(p)
  expect_identical(b$scales$color$levels, rev(c("4", "6", "8")))
  # the data -> colour mapping is unchanged (still maps "4" to its trained colour)
  base <- vellumplot:::.build_panels(
    vplot(df) |> mark_point(x = wt, y = mpg, color = cyl)
  )
  expect_identical(b$scales$color$map("4"), base$scales$color$map("4"))
})

test_that("guide_legend(title=) overrides the legend title", {
  p <- vplot(df) |>
    mark_point(x = wt, y = mpg, color = cyl) |>
    guides(color = guide_legend(title = "Cylinders"))
  expect_identical(vellumplot:::.build_panels(p)$scales$color$name, "Cylinders")
})

test_that("guides() applies to size/shape/alpha/linetype and requires named args", {
  p <- vplot(df) |>
    mark_point(x = wt, y = mpg, size = mpg) |>
    guides(size = "none")
  expect_length(
    vellumplot:::.legend_guides(vellumplot:::.build_panels(p)$scales),
    0L
  )
  expect_error(
    vplot(df) |> mark_point(x = wt, y = mpg) |> guides("none"),
    "named"
  )
})

test_that("guided plots render", {
  f <- local_tempfile(fileext = ".png")
  p <- vplot(df) |>
    mark_point(x = wt, y = mpg, color = cyl) |>
    guides(color = guide_legend(reverse = TRUE))
  expect_no_error(render_plot(p, f))
})
