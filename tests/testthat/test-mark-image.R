# Images (flags/logos) drawn at data points via mark_image().

# A real, non-square fixture ships with magick (used for aspect-ratio checks).
img <- system.file("images", "building.jpg", package = "magick")
d <- data.frame(x = 1:3, y = c(2, 1, 3), src = rep(img, 3))

test_that("constructor sets mark and carries src/size", {
  L <- (vplot(d) |> mark_image(x = x, y = y, src = src, size = 8))@layers[[1]]
  expect_identical(L@mark, "image")
  expect_true("src" %in% names(L@encoding))
})

test_that("a bare literal src is a constant param, a column is a channel", {
  const <- (vplot(d) |> mark_image(x = x, y = y, src = "logo.png"))@layers[[1]]
  expect_identical(const@params$src, "logo.png")
  expect_false("src" %in% names(const@encoding))

  mapped <- (vplot(d) |> mark_image(x = x, y = y, src = src))@layers[[1]]
  expect_true("src" %in% names(mapped@encoding))
})

test_that("src resolves as a per-row column", {
  r <- vellumplot:::.resolve_layers(
    vplot(d) |> mark_image(x = x, y = y, src = src)
  )[[1]]
  expect_length(r$values$src, 3)
  expect_identical(r$n, 3L)
})

test_that("mapped src, constant src, and coord_flip all render", {
  skip_if_not_installed("magick")
  plots <- list(
    vplot(d) |> mark_image(x = x, y = y, src = src),
    vplot(d) |> mark_image(x = x, y = y, src = img), # constant image
    vplot(d) |>
      mark_image(x = x, y = y, src = src) |>
      coord_flip()
  )
  for (p in plots) {
    f <- local_tempfile(fileext = ".png")
    render_plot(p, f)
    expect_gt(file.info(f)$size, 0)
  }
})

test_that("mark_image renders to SVG (image element embedded)", {
  skip_if_not_installed("magick")
  f <- local_tempfile(fileext = ".svg")
  render_plot(vplot(d) |> mark_image(x = x, y = y, src = src, size = 12), f)
  expect_gt(file.info(f)$size, 0)
})

test_that(".read_image returns pixel dims and an as.raster-able object", {
  skip_if_not_installed("magick")
  im <- vellumplot:::.read_image(img)
  expect_true(im$iw > 0 && im$ih > 0)
  expect_s3_class(grDevices::as.raster(im$raster), "raster")
})

test_that(".read_image errors on a missing or empty path", {
  skip_if_not_installed("magick")
  expect_error(vellumplot:::.read_image("no-such-file.png"), "not found")
  expect_error(vellumplot:::.read_image(""), "non-empty")
})

test_that("mark_image rejects a sketch aesthetic", {
  expect_error(
    vplot(d) |> mark_image(x = x, y = y, src = src, sketch = "wobbly"),
    "not an aesthetic"
  )
})
