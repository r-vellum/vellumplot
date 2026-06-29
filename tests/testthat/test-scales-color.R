# Colour scale breadth: palettes, manual, gradient, fill mirror.

train <- function(p) {
  vellumplot:::.train_scales(p, vellumplot:::.resolve_layers(p))
}

test_that("a named palette resolves via hcl.colors and differs from default", {
  base <- vplot(mtcars) |> mark_point(x = wt, y = mpg, color = hp)
  def <- train(base)$color$pal256
  blues <- train(base |> scale_color_continuous(palette = "Blues"))$color$pal256
  expect_length(blues, 256)
  expect_false(identical(def, blues))
})

test_that("palette names match case/space-insensitively", {
  base <- vplot(mtcars) |> mark_point(x = wt, y = mpg, color = hp)
  a <- train(base |> scale_color_continuous(palette = "viridis"))$color$pal256
  b <- train(base |> scale_color_continuous(palette = "Viridis"))$color$pal256
  expect_identical(a, b)
})

test_that("scale_color_manual maps named values to levels", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, color = factor(cyl)) |>
    scale_color_manual(values = c("4" = "red", "6" = "blue", "8" = "green"))
  cl <- train(p)$color
  expect_identical(cl$levels, c("4", "6", "8"))
  expect_identical(cl$colors, c("red", "blue", "green"))
})

test_that("unnamed manual values recycle by level order; missing named -> grey", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, color = factor(cyl)) |>
    scale_color_manual(values = c("4" = "red"))
  cl <- train(p)$color
  expect_identical(cl$colors[1], "red")
  expect_true(all(cl$colors[2:3] == "grey50"))
})

test_that("scale_color_gradient sets a two-point ramp", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, color = hp) |>
    scale_color_gradient(low = "white", high = "black")
  pal <- train(p)$color$pal256
  expect_length(pal, 256)
  expect_false(identical(
    pal,
    train(vplot(mtcars) |> mark_point(x = wt, y = mpg, color = hp))$color$pal256
  ))
})

test_that("fill mirror trains the colour scale", {
  p <- vplot(mtcars) |>
    mark_bar(x = factor(cyl), fill = factor(cyl)) |>
    scale_fill_manual(values = c("4" = "red", "6" = "blue", "8" = "green"))
  expect_identical(train(p)$color$colors, c("red", "blue", "green"))
})

test_that("an unknown palette name errors", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, color = hp) |>
    scale_color_continuous(palette = "definitelynotapalette")
  expect_error(train(p), "palette")
})

test_that("explicit colour breaks/labels are honoured", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, color = hp) |>
    scale_color_continuous(breaks = c(100, 200), labels = c("lo", "hi"))
  cl <- train(p)$color
  expect_identical(cl$legend_labels, c("lo", "hi"))
})
