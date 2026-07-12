# Resolution (dpi) control: the authored dpi drives the exported PNG's pixel
# dimensions (width * dpi by height * dpi), and render_plot() can override it.

test_that("authored dpi scales the rendered pixel dimensions", {
  p <- vplot(mtcars, width = 6, height = 4) |> mark_point(x = wt, y = mpg)
  # scene_raster() returns [channel, x, y]; default 96 dpi -> 6*96 by 4*96.
  d <- dim(vellum::scene_raster(p))
  expect_equal(d, c(4, 576, 384))

  p2 <- vplot(mtcars, width = 6, height = 4, dpi = 150) |>
    mark_point(x = wt, y = mpg)
  expect_equal(dim(vellum::scene_raster(p2)), c(4, 900, 600))
})

test_that("vplot() stores dpi on the spec", {
  expect_equal(vplot(mtcars)@dpi, 96)
  expect_equal(vplot(mtcars, dpi = 300)@dpi, 300)
})

test_that("render_plot(dpi =) overrides the authored resolution", {
  p <- vplot(mtcars, width = 4, height = 3) |> mark_point(x = wt, y = mpg)
  lo <- local_tempfile(fileext = ".png")
  hi <- local_tempfile(fileext = ".png")
  expect_equal(render_plot(p, lo, dpi = 72), lo)
  render_plot(p, hi, dpi = 300)
  # A denser raster is a larger file; a weak but robust smoke check that dpi
  # reached the backend without needing a PNG decoder.
  expect_gt(file.size(hi), file.size(lo))
})

test_that("render_plot() returns the path invisibly", {
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
  f <- local_tempfile(fileext = ".png")
  expect_invisible(render_plot(p, f))
})

test_that("a composition inherits its first sub-plot's dpi", {
  a <- vplot(mtcars, dpi = 150) |> mark_point(x = wt, y = mpg)
  b <- vplot(mtcars) |> mark_point(x = hp, y = mpg)
  expect_equal(hconcat(a, b)@dpi, 150)
  # explicit dpi wins over inheritance
  expect_equal(concat(a, b, dpi = 200)@dpi, 200)
})

test_that("an invalid dpi is rejected", {
  expect_error(vplot(mtcars, dpi = 0), "positive number")
  expect_error(vplot(mtcars, dpi = -10), "positive number")
  expect_error(vplot(mtcars, dpi = c(96, 150)), "single positive number")
  expect_error(vplot(mtcars, dpi = Inf), "positive number") # now rejected too
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
  f <- local_tempfile(fileext = ".png")
  expect_error(render_plot(p, f, dpi = "hi"), "positive number")
  expect_error(concat(p, p, dpi = 0), "positive number")
})

test_that("an invalid width/height is rejected (H41)", {
  expect_error(vplot(mtcars, width = 0), "width")
  expect_error(vplot(mtcars, width = -1), "width")
  expect_error(vplot(mtcars, height = NA), "height")
  expect_error(vplot(mtcars, height = Inf), "height")
  expect_error(vplot(mtcars, width = c(5, 6)), "single positive number")
  # valid dims still construct
  p <- vplot(mtcars, width = 5, height = 3)
  expect_identical(c(p@width, p@height), c(5, 3))
})
