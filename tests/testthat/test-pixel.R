# Pixel tests: probe the rendered raster (via vellum::scene_raster()). These
# confirm marks land where the trained scales map them and that colour encodings
# actually paint.

test_that("the panel has a grey background", {
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
  img <- render_px(p)
  # grey92 background should cover a large share of the panel area
  expect_gt(count_near(img, c(0.92, 0.92, 0.92)), 0.1 * prod(dim(img)[1:2]))
})

test_that("points land where the scales map them (orientation + y-up)", {
  df <- data.frame(x = c(0, 10), y = c(0, 10))
  p <- vplot(df, width = 4, height = 3) |> mark_point(x = x, y = y, size = 6)
  img <- render_px(p)
  # (10,10) -> top-right of the panel; (0,0) -> bottom-left. Probe regions stay
  # inside the panel interior, away from the axis-label gutters.
  expect_true(has_ink(img, rows = c(0.02, 0.18), cols = c(0.86, 0.98)))
  expect_true(has_ink(img, rows = c(0.70, 0.84), cols = c(0.16, 0.30)))
  # the empty interior corners stay clear of ink
  expect_false(has_ink(img, rows = c(0.08, 0.25), cols = c(0.20, 0.36)))
  expect_false(has_ink(img, rows = c(0.55, 0.72), cols = c(0.80, 0.96)))
})

test_that("a discrete colour encoding paints each category colour", {
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg, color = factor(cyl))
  sc <- quill:::.train_scales(p, quill:::.resolve_layers(p))
  img <- render_px(p)
  for (hex in sc$color$colors) {
    rgb <- as.numeric(grDevices::col2rgb(hex)) / 255
    expect_gt(count_near(img, rgb, tol = 0.08), 0)
  }
})

test_that("a continuous colour encoding produces a range of hues", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, color = hp) |>
    scale_color_continuous()
  img <- render_px(p)
  r <- img[,, 1]
  g <- img[,, 2]
  b <- img[,, 3]
  # saturated pixels (not white / grey / black): meaningful channel spread
  sat <- pmax(r, g, b) - pmin(r, g, b) > 0.12
  keys <- unique(paste(round(r[sat], 1), round(g[sat], 1), round(b[sat], 1)))
  expect_gt(length(keys), 8)
})
