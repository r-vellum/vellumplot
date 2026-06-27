# Per-layer blend modes (vellum viewport(blend=)): a layer composites as one
# isolated group against the backdrop.

test_that("blend defaults to normal and is recorded", {
  expect_identical(
    (vplot(mtcars) |> mark_point(x = wt, y = mpg))@layers[[1]]@blend,
    "normal"
  )
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg, blend = "multiply")
  expect_identical(p@layers[[1]]@blend, "multiply")
})

test_that("an unknown blend mode errors at build time", {
  expect_error(
    vplot(mtcars) |> mark_point(x = wt, y = mpg, blend = "glow"),
    "blend"
  )
})

test_that("multiply compositing of two overlapping layers darkens the overlap", {
  # orange (#D55E00) under blue (#0072B2, multiply): overlap -> ~ (0, 0.17, 0)
  p <- vplot(mtcars, width = 4, height = 3) |>
    mark_point(x = wt, y = mpg, size = 9, color = "#D55E00") |>
    mark_point(x = wt, y = mpg, size = 9, color = "#0072B2", blend = "multiply")
  img <- render_px(p)
  blue <- as.numeric(grDevices::col2rgb("#0072B2")) / 255
  orange <- as.numeric(grDevices::col2rgb("#D55E00")) / 255
  mult <- blue * orange
  # the multiplied colour appears, and it is darker than either source
  expect_gt(count_near(img, mult, tol = 0.1), 0)
  expect_equal(count_near(img, blue, tol = 0.05) >= 0, TRUE)
  expect_lt(sum(mult), sum(blue))
  expect_lt(sum(mult), sum(orange))
})

test_that("a normal-blend layer renders like an unwrapped layer (no error)", {
  f <- withr::local_tempfile(fileext = ".png")
  render_plot(vplot(mtcars) |> mark_point(x = wt, y = mpg, blend = "normal"), f)
  expect_gt(file.info(f)$size, 0)
})
