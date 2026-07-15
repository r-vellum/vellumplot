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

# Perceptual (Oklab) interpolation is the default for continuous/binned ramps.

test_that("a two-point continuous ramp blends perceptually (Oklab) by default", {
  withr::local_options(vellumplot.color.interpolation = NULL) # default
  df <- data.frame(x = 1:3, y = 1:3, z = c(0, 0.5, 1))
  cl <- train(vplot(df) |> mark_point(x = x, y = y, color = z) |>
    scale_color_gradient(low = "black", high = "white"))$color
  midR <- as.integer(grDevices::col2rgb(cl$pal256[128]))[1]
  # Oklab midpoint (~99, 50% perceived lightness) is clearly darker than the
  # sRGB code midpoint (~127).
  expect_lt(midR, 112L)
  expect_gt(midR, 85L)
})

test_that("options(vellumplot.color.interpolation='srgb') restores sRGB blending", {
  df <- data.frame(x = 1:3, y = 1:3, z = c(0, 0.5, 1))
  p <- vplot(df) |> mark_point(x = x, y = y, color = z) |>
    scale_color_gradient(low = "black", high = "white")
  withr::local_options(vellumplot.color.interpolation = "srgb")
  midR <- as.integer(grDevices::col2rgb(train(p)$color$pal256[128]))[1]
  expect_true(abs(midR - 127L) < 6L)
})

test_that("a designed perceptual palette (batlow default) is unchanged by the space", {
  base <- vplot(mtcars) |> mark_point(x = wt, y = mpg, color = hp)
  ok <- withr::with_options(list(vellumplot.color.interpolation = "oklab"), train(base)$color$pal256)
  sr <- withr::with_options(list(vellumplot.color.interpolation = "srgb"), train(base)$color$pal256)
  expect_identical(ok, sr) # already 256 dense perceptual stops -> resample is a no-op
})

test_that("binned colour scales also use the perceptual ramp", {
  df <- data.frame(x = 1:20, y = 1:20, z = 1:20)
  cl <- train(vplot(df) |> mark_point(x = x, y = y, color = z) |>
    scale_color_binned(palette = c("black", "white"), n = 4))$color
  expect_identical(cl$kind, "binned")
  # the middle class colour is perceptual, not the sRGB code-midpoint grey
  mids <- as.integer(grDevices::col2rgb(cl$colors[2]))[1]
  expect_lt(mids, 130L)
})
