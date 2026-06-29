# Generalized guide list: colour, size, shape.

train <- function(p) {
  vellumplot:::.train_scales(p, vellumplot:::.resolve_layers(p))
}
kinds <- function(p) {
  vapply(
    vellumplot:::.legend_guides(train(p)),
    function(g) g$kind,
    character(1)
  )
}

test_that("guides are built per mapped channel in colour/size/shape order", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, color = hp, size = disp, shape = factor(cyl))
  expect_identical(kinds(p), c("color_continuous", "size", "shape"))
})

test_that("only mapped channels produce guides", {
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg, shape = factor(cyl))
  expect_identical(kinds(p), "shape")
  p0 <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
  expect_length(kinds(p0), 0)
})

test_that("a plot with three guides renders", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, color = hp, size = disp, shape = factor(cyl))
  f <- withr::local_tempfile(fileext = ".png")
  render_plot(p, f)
  expect_gt(file.info(f)$size, 0)
})

test_that(".legend_width handles a shape guide without error", {
  built <- vellumplot:::.build_panels(
    vplot(mtcars) |>
      mark_point(x = wt, y = mpg, color = hp, shape = factor(carb))
  )
  rt <- vellumplot:::.resolve_theme(vellumplot:::.theme_default())
  guides <- vellumplot:::.legend_guides(built$scales)
  expect_length(guides, 2)
  expect_silent(vellumplot:::.legend_width(guides, rt))
})

test_that("legend.position = none drops all guides", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, color = hp, shape = factor(cyl)) |>
    theme(legend.position = "none")
  built <- vellumplot:::.build_panels(p)
  lay <- vellumplot:::.build_layout(
    built,
    vellumplot:::.legend_guides(built$scales),
    p@labels,
    vellumplot:::.resolve_theme(p@theme)
  )
  expect_true(is.na(lay$legend_col))
})
