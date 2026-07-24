# clip_to() / set_mask(): clip or mask a plot to a geometry.

diamond <- data.frame(x = c(10, 18, 10, 2), y = c(2, 10, 18, 10))

test_that("clip_to() attaches a hard-clip ClipSpec", {
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg) |> clip_to(diamond)
  cs <- p@clip
  expect_s3_class(cs, "vellumplot::ClipSpec")
  expect_identical(cs@kind, "clip")
  expect_identical(cs@type, "alpha")
  expect_false(cs@invert)
  expect_length(cs@region$rings, 1L)
  expect_equal(dim(cs@region$rings[[1]]), c(4L, 2L))
})

test_that("set_mask() attaches a soft-mask ClipSpec (NULL region = vignette)", {
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg) |> set_mask(feather = 0.5)
  cs <- p@clip
  expect_identical(cs@kind, "mask")
  expect_identical(cs@type, "luminance")
  expect_null(cs@region)
  expect_equal(cs@feather, 0.5)
})

test_that("clip_to() requires a region; feather is clamped", {
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
  expect_error(clip_to(p), "region.*is required")
  expect_equal((p |> set_mask(feather = 5))@clip@feather, 0.9) # clamped
})

test_that(".clip_region normalises data-frame, matrix, and grouped input", {
  r1 <- vellumplot:::.clip_region(diamond)
  expect_length(r1$rings, 1L)
  r2 <- vellumplot:::.clip_region(as.matrix(diamond))
  expect_equal(r2$rings[[1]], unname(as.matrix(diamond)))
  grouped <- rbind(
    cbind(diamond, group = "a"),
    cbind(data.frame(x = c(0, 1, 1), y = c(0, 0, 1)), group = "b")
  )
  r3 <- vellumplot:::.clip_region(grouped)
  expect_length(r3$rings, 2L)
})

test_that(".clip_region reads polygon rings from an sf object", {
  skip_if_not_installed("sf")
  poly <- sf::st_sfc(sf::st_polygon(list(
    cbind(c(0, 2, 2, 0, 0), c(0, 0, 2, 2, 0))
  )))
  r <- vellumplot:::.clip_region(sf::st_sf(geometry = poly))
  expect_length(r$rings, 1L)
  expect_equal(nrow(r$rings[[1]]), 5L)
})

test_that("clip, invert, and vignette all render", {
  grid <- expand.grid(x = 1:12, y = 1:12)
  grid$z <- with(grid, sin(x / 3) + cos(y / 3))
  plots <- list(
    vplot(grid) |> mark_tile(x = x, y = y, fill = z) |> clip_to(diamond),
    vplot(grid) |>
      mark_tile(x = x, y = y, fill = z) |>
      clip_to(diamond, invert = TRUE),
    vplot(grid) |> mark_tile(x = x, y = y, fill = z) |> set_mask(feather = 0.4)
  )
  for (p in plots) {
    f <- local_tempfile(fileext = ".png")
    expect_no_error(render_plot(p, f))
    expect_gt(file.info(f)$size, 0)
  }
})

test_that("a clip renders (and carries a mask) in PDF", {
  grid <- expand.grid(x = 1:8, y = 1:8)
  grid$z <- grid$x + grid$y
  p <- vplot(grid) |> mark_tile(x = x, y = y, fill = z) |> clip_to(diamond)
  f <- local_tempfile(fileext = ".pdf")
  render_plot(p, f)
  raw <- readBin(f, "raw", file.info(f)$size)
  expect_gt(length(grepRaw("/Mask", raw, fixed = TRUE)), 0L)
})

test_that("a clip under polar coordinates warns and is ignored", {
  p <- vplot(data.frame(g = c("a", "b", "c"), n = c(3, 5, 2))) |>
    mark_bar(x = g, y = n) |>
    coord_polar() |>
    clip_to(diamond)
  expect_warning(
    render_plot(p, local_tempfile(fileext = ".png")),
    "only applied under cartesian"
  )
})

test_that("no clip leaves the render path untouched", {
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
  expect_null(p@clip)
  expect_no_error(render_plot(p, local_tempfile(fileext = ".png")))
})

test_that("clip_layer() clips only the most-recent layer", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    clip_to(diamond) # <- would be plot-level; ensure layer clip is separate
  p2 <- vplot(mtcars) |>
    mark_tile(x = wt, y = mpg) |>
    clip_layer(diamond) |>
    mark_point(x = wt, y = mpg)
  # the tile layer carries a clip; the later point layer does not
  expect_s3_class(p2@layers[[1]]@clip, "vellumplot::ClipSpec")
  expect_null(p2@layers[[2]]@clip)
  # and it is a layer clip, not a plot-level one
  expect_null(p2@clip)
})

test_that("clip_layer() needs a layer and a region", {
  expect_error(clip_layer(vplot(mtcars), diamond), "needs a layer")
  expect_error(
    vplot(mtcars) |> mark_point(x = wt, y = mpg) |> clip_layer(),
    "region.*is required"
  )
})

test_that("a per-layer clip renders with other layers full-bleed", {
  grid <- expand.grid(x = 1:10, y = 1:10)
  grid$z <- grid$x + grid$y
  p <- vplot(grid) |>
    mark_tile(x = x, y = y, fill = z) |>
    clip_layer(diamond) |>
    mark_point(x = x, y = y, size = 1)
  f <- local_tempfile(fileext = ".png")
  expect_no_error(render_plot(p, f))
  expect_gt(file.info(f)$size, 0)
})
