# Binned position scales: scale_x_binned() / scale_y_binned() bin a continuous
# axis (ticks at boundaries, data at bin centres). Phase 2.

test_that("scale_x/y_binned() declare a binned position scale", {
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg) |> scale_x_binned(n = 6)
  sc <- Filter(function(s) s@aesthetic == "x", p@scales)
  expect_length(sc, 1L)
  expect_identical(sc[[1]]@type, "binned")
  expect_identical(sc[[1]]@n, 6)
})

test_that("a binned position scale ticks at boundaries and maps to bin centres", {
  sc <- .train_position_binned(
    "x",
    mtcars$wt,
    ScaleSpec(aesthetic = "x", type = "binned", style = "equal", n = 5),
    "wt"
  )
  expect_identical(sc$type, "binned")
  expect_false(sc$discrete)
  expect_length(sc$breaks, 6L) # n+1 boundaries
  expect_equal(sc$band_width, diff(sc$breaks)[1]) # equal bins -> uniform width
  # a value in the first bin maps to that bin's centre
  b <- sc$breaks
  expect_equal(sc$map(b[1] + 1e-6), (b[1] + b[2]) / 2)
  # the top edge lands in the last bin (rightmost.closed)
  expect_equal(sc$map(max(b)), (b[length(b) - 1] + b[length(b)]) / 2)
})

test_that("explicit breaks override style/n", {
  sc <- .train_position_binned(
    "x",
    1:100,
    ScaleSpec(aesthetic = "x", type = "binned", breaks = c(0, 25, 50, 100)),
    "x"
  )
  expect_equal(sc$breaks, c(0, 25, 50, 100))
})

test_that("binned position renders (points and bars)", {
  f <- tempfile(fileext = ".svg")
  on.exit(unlink(f), add = TRUE)
  expect_no_error(render_plot(
    vplot(mtcars) |> mark_point(x = wt, y = mpg) |> scale_x_binned(n = 6),
    f
  ))
  expect_no_error(render_plot(
    vplot(mtcars) |> mark_bar(x = wt) |> scale_x_binned(n = 6),
    f
  ))
})
