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

test_that("scale_fill_continuous applies a palette to a fill aesthetic", {
  base <- vplot(mtcars) |> mark_bar(x = factor(cyl), fill = hp)
  def <- train(base)$color$pal256
  blues <- train(base |> scale_fill_continuous(palette = "Blues"))$color$pal256
  expect_length(blues, 256)
  expect_false(identical(def, blues))
})

test_that("scale_fill_gradient sets a two-point fill ramp", {
  base <- vplot(mtcars) |> mark_bar(x = factor(cyl), fill = hp)
  pal <- train(
    base |> scale_fill_gradient(low = "white", high = "black")
  )$color$pal256
  expect_length(pal, 256)
  expect_false(identical(pal, train(base)$color$pal256))
})

test_that("scale_color_binned cuts a continuous colour aesthetic into classes", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, color = hp) |>
    scale_color_binned(n = 4)
  cl <- train(p)$color
  expect_identical(cl$kind, "binned")
})

test_that("a binned colour scale with too-few breaks errors clearly (H43)", {
  # user breaks skip .binned_breaks; a length-1 (or empty) breaks used to crash
  # in colorRampPalette(...)(0).
  p1 <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, color = hp) |>
    scale_color_binned(breaks = 100)
  expect_error(train(p1), "at least 2 breaks")
  p0 <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, color = hp) |>
    scale_color_binned(breaks = numeric(0))
  expect_error(train(p0), "at least 2 breaks")
  # two breaks (one class) is the minimum and still trains
  p2 <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, color = hp) |>
    scale_color_binned(breaks = c(100, 200))
  expect_identical(train(p2)$color$kind, "binned")
})

test_that("scale_color_manual / scale_fill_manual reject missing or bad values", {
  base <- vplot(mtcars) |> mark_point(x = wt, y = mpg, color = factor(cyl))
  expect_error(scale_color_manual(base), "character vector")
  expect_error(scale_color_manual(base, values = 42), "character vector")
  expect_error(
    scale_color_manual(base, values = character(0)),
    "character vector"
  )
  expect_error(scale_fill_manual(base, values = 1:3), "character vector")
})
