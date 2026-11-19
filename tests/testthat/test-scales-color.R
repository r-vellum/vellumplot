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

test_that("scale_fill_manual() trains the fill scale", {
  p <- vplot(mtcars) |>
    mark_bar(x = factor(cyl), fill = factor(cyl)) |>
    scale_fill_manual(values = c("4" = "red", "6" = "blue", "8" = "green"))
  expect_identical(train(p)$fill$colors, c("red", "blue", "green"))
  expect_null(train(p)$color)
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
  base <- vplot(mtcars) |> mark_bar(x = factor(cyl), y = mpg, fill = hp)
  def <- train(base)$fill$pal256
  blues <- train(base |> scale_fill_continuous(palette = "Blues"))$fill$pal256
  expect_length(blues, 256)
  expect_false(identical(def, blues))
})

test_that("scale_fill_gradient sets a two-point fill ramp", {
  base <- vplot(mtcars) |> mark_bar(x = factor(cyl), y = mpg, fill = hp)
  pal <- train(
    base |> scale_fill_gradient(low = "white", high = "black")
  )$fill$pal256
  expect_length(pal, 256)
  expect_false(identical(pal, train(base)$fill$pal256))
})

test_that("scale_color_binned cuts a continuous colour aesthetic into classes", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, color = hp) |>
    scale_color_binned(n = 4)
  cl <- train(p)$color
  expect_identical(cl$type, "binned")
})

test_that("a binned colour scale with too-few breaks errors clearly (H43)", {
  # user breaks skip .binned_breaks; a length-1 (or empty) breaks used to crash
  # in colorRampPalette(...)(0).
  p1 <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, color = hp) |>
    scale_color_binned(style = "quantile", breaks = 100)
  expect_error(train(p1), "at least 2 breaks")
  p0 <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, color = hp) |>
    scale_color_binned(style = "quantile", breaks = numeric(0))
  expect_error(train(p0), "at least 2 breaks")
  # two breaks (one class) is the minimum and still trains
  p2 <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, color = hp) |>
    scale_color_binned(breaks = c(100, 200))
  expect_identical(train(p2)$color$type, "binned")
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
  cl <- train(
    vplot(df) |>
      mark_point(x = x, y = y, color = z) |>
      scale_color_gradient(low = "black", high = "white")
  )$color
  midR <- as.integer(grDevices::col2rgb(cl$pal256[128]))[1]
  # Oklab midpoint (~99, 50% perceived lightness) is clearly darker than the
  # sRGB code midpoint (~127).
  expect_lt(midR, 112L)
  expect_gt(midR, 85L)
})

test_that("options(vellumplot.color.interpolation='srgb') restores sRGB blending", {
  df <- data.frame(x = 1:3, y = 1:3, z = c(0, 0.5, 1))
  p <- vplot(df) |>
    mark_point(x = x, y = y, color = z) |>
    scale_color_gradient(low = "black", high = "white")
  withr::local_options(vellumplot.color.interpolation = "srgb")
  midR <- as.integer(grDevices::col2rgb(train(p)$color$pal256[128]))[1]
  expect_true(abs(midR - 127L) < 6L)
})

test_that("a designed perceptual palette (batlow default) is unchanged by the space", {
  base <- vplot(mtcars) |> mark_point(x = wt, y = mpg, color = hp)
  ok <- withr::with_options(
    list(vellumplot.color.interpolation = "oklab"),
    train(base)$color$pal256
  )
  sr <- withr::with_options(
    list(vellumplot.color.interpolation = "srgb"),
    train(base)$color$pal256
  )
  expect_identical(ok, sr) # already 256 dense perceptual stops -> resample is a no-op
})

test_that("binned colour scales also use the perceptual ramp", {
  df <- data.frame(x = 1:20, y = 1:20, z = 1:20)
  cl <- train(
    vplot(df) |>
      mark_point(x = x, y = y, color = z) |>
      scale_color_binned(palette = c("black", "white"), n = 4)
  )$color
  expect_identical(cl$type, "binned")
  # the middle class colour is perceptual, not the sRGB code-midpoint grey
  mids <- as.integer(grDevices::col2rgb(cl$colors[2]))[1]
  expect_lt(mids, 130L)
})

test_that("scale_color_gradient2 centres the mid colour on the midpoint", {
  df <- data.frame(x = 1:5, y = 1:5, z = c(-2, -1, 0, 1, 2))
  cl <- train(
    vplot(df) |>
      mark_point(x = x, y = y, color = z) |>
      scale_color_gradient2(
        low = "blue",
        mid = "white",
        high = "red",
        midpoint = 0
      )
  )$color
  expect_equal(cl$midpoint, 0)
  # the midpoint value maps to ~white; the extremes to ~blue / ~red
  mid_rgb <- as.integer(grDevices::col2rgb(cl$map(0)))
  expect_true(all(mid_rgb > 240L))
  lo <- as.integer(grDevices::col2rgb(cl$map(-2)))
  hi <- as.integer(grDevices::col2rgb(cl$map(2)))
  expect_gt(lo[3], lo[1]) # blue dominates the low end
  expect_gt(hi[1], hi[3]) # red dominates the high end
})

test_that("gradient2 rescales about the midpoint, not the data centre", {
  # asymmetric range about 0: the midpoint colour must still land on value 0,
  # not on the data mean (which is positive here).
  df <- data.frame(x = 1:4, y = 1:4, z = c(-1, 2, 4, 8))
  cl <- train(
    vplot(df) |>
      mark_point(x = x, y = y, color = z) |>
      scale_color_gradient2(low = "blue", mid = "white", high = "red")
  )$color
  mid_rgb <- as.integer(grDevices::col2rgb(cl$map(0)))
  expect_true(all(mid_rgb > 240L)) # value 0 is still ~white
})

test_that("scale_fill_gradient2 mirrors the colour version", {
  df <- data.frame(x = 1:3, y = 1:3, z = c(-1, 0, 1))
  cl <- train(
    vplot(df) |>
      mark_tile(x = x, y = y, fill = z) |>
      scale_fill_gradient2(midpoint = 0)
  )$fill
  expect_equal(cl$midpoint, 0)
})

test_that("a diverging colorbar reports midpoint in its descriptor", {
  df <- data.frame(x = 1:5, y = 1:5, z = c(-2, -1, 0, 1, 2))
  bars <- vellum::scene_model(
    vellum::as_vellum_scene(
      vplot(df) |>
        mark_point(x = x, y = y, color = z) |>
        scale_color_gradient2(midpoint = 0)
    )
  )$elements
  metas <- bars$meta[!vapply(bars$meta, is.null, logical(1))]
  cbars <- Filter(function(m) !is.null(m$colorbar), metas)
  expect_gt(length(cbars), 0)
  expect_true(isTRUE(cbars[[1]]$colorbar$diverging))
  expect_equal(cbars[[1]]$colorbar$midpoint, 0)
})

test_that("the fill scale trains on every fill value when a layer maps color", {
  # VP-1 / #195: tiles filled by z under points coloured by a subset of z. With
  # one shared scale the subset once set the range; the fill scale now trains
  # on all of z, the colour scale on the points' values alone (as ggplot2).
  d <- data.frame(x = rep(1:10, 10), y = rep(1:10, each = 10), z = 1:100)
  p <- vplot(d) |>
    mark_tile(x = x, y = y, fill = z) |>
    mark_point(data = subset(d, z > 50), x = x, y = y, color = z)
  sc <- train(p)
  expect_identical(sc$fill$type, "continuous")
  expect_identical(sc$fill$range, c(1, 100))
  expect_identical(sc$color$range, c(51, 100))
  # the tiles below the points' range get distinct colours, not one squished end
  expect_false(identical(sc$fill$map(1), sc$fill$map(50)))
})

test_that("discrete color and fill layers train separate scales (VP-48)", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, color = factor(cyl)) |>
    mark_point(x = wt, y = qsec, fill = factor(gear))
  sc <- train(p)
  expect_identical(sc$color$levels, c("4", "6", "8"))
  expect_identical(sc$fill$levels, c("3", "4", "5"))
  # two variables, two legends; the fill keys keep their own namespace
  expect_length(.legend_guides(sc), 2L)
  expect_identical(sc$fill$series_aes, "fill")
})

test_that("a quantitative fill beside a discrete color is two scales", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, color = factor(cyl)) |>
    mark_point(x = wt, y = qsec, fill = hp)
  sc <- train(p)
  expect_identical(sc$color$type, "discrete")
  expect_identical(sc$fill$type, "continuous")
})

test_that("one channel mapped to mixed variable types is a clear error", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, color = factor(cyl)) |>
    mark_point(x = wt, y = qsec, color = hp)
  expect_error(train(p), "incompatible variable types")
  expect_snapshot(train(p), error = TRUE)
})

test_that("an explicit colour scale kind settles mixed color types", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, color = factor(cyl)) |>
    mark_point(x = wt, y = qsec, color = carb) |>
    scale_color_discrete()
  cl <- train(p)$color
  expect_identical(cl$type, "discrete")
  expect_true(all(c("4", "6", "8", "1", "2") %in% cl$levels))
})

test_that("colour and fill bars fold into one only with the same keys and palette", {
  d <- data.frame(x = rep(1:10, 10), y = rep(1:10, each = 10), z = 1:100)
  # a subset on the points: other breaks, two colour bars (as ggplot2)
  p <- vplot(d) |>
    mark_tile(x = x, y = y, fill = z) |>
    mark_point(data = subset(d, z > 50), x = x, y = y, color = z)
  guides <- .legend_guides(.build_panels(p)$scales)
  expect_length(guides, 2L)
  expect_no_error(.compile_plot(p))
  # one variable over the same rows: one colour bar
  same <- vplot(d) |>
    mark_tile(x = x, y = y, fill = z) |>
    mark_point(x = x, y = y, color = z)
  guides <- .legend_guides(.build_panels(same)$scales)
  expect_length(guides, 1L)
  expect_identical(guides[[1]]$kind, "color_continuous")
  # ... unless the palettes differ
  apart <- same |> scale_fill_viridis_c()
  expect_length(.legend_guides(.build_panels(apart)$scales), 2L)
})
