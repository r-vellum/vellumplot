# Identity scales: use data values verbatim as the aesthetic, with no legend.

df <- data.frame(
  x = 1:3, y = 1:3,
  col = c("red", "green", "blue"),
  sz = c(2, 5, 9),
  shp = c("circle", "square", "diamond")
)

test_that("scale_color_identity() maps colours verbatim and draws no legend", {
  p <- vplot(df) |> mark_point(x = x, y = y, color = col) |> scale_color_identity()
  b <- vellumplot:::.build_panels(p)
  expect_identical(b$scales$color$kind, "identity")
  expect_identical(b$scales$color$map(c("red", "blue")), c("red", "blue"))
  expect_length(vellumplot:::.legend_guides(b$scales), 0L)
})

test_that("scale_fill_identity() behaves like the colour identity", {
  p <- vplot(df) |> mark_point(x = x, y = y, fill = col) |> scale_fill_identity()
  b <- vellumplot:::.build_panels(p)
  expect_identical(b$scales$color$kind, "identity")
})

test_that("scale_size_identity() uses raw sizes and draws no legend", {
  p <- vplot(df) |> mark_point(x = x, y = y, size = sz) |> scale_size_identity()
  b <- vellumplot:::.build_panels(p)
  expect_identical(b$scales$size$map(c(2, 9)), c(2, 9))
  expect_length(vellumplot:::.legend_guides(b$scales), 0L)
})

test_that("scale_shape_identity() uses shape names verbatim and draws no legend", {
  p <- vplot(df) |> mark_point(x = x, y = y, shape = shp) |> scale_shape_identity()
  b <- vellumplot:::.build_panels(p)
  expect_identical(b$scales$shape$map(c("circle", "diamond")), c("circle", "diamond"))
  expect_length(vellumplot:::.legend_guides(b$scales), 0L)
})

test_that("identity-scaled plots render", {
  f <- local_tempfile(fileext = ".png")
  p <- vplot(df) |>
    mark_point(x = x, y = y, color = col, size = sz) |>
    scale_color_identity() |>
    scale_size_identity()
  expect_no_error(render_plot(p, f))
})
