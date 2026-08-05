# Regression tests for the REVIEW4 Batch 4 interop-fidelity fixes.

# SR4 -----------------------------------------------------------------------
test_that("an integer stat param survives the round-trip as an integer", {
  p <- vplot(mtcars) |> mark_histogram(x = mpg, bins = 20L)

  b_ir <- from_spec(as_spec(p))@layers[[1]]@stat_params$bins
  expect_true(is.integer(b_ir))
  expect_identical(b_ir, 20L)

  b_json <- spec_from_json(spec_to_json(p))@layers[[1]]@stat_params$bins
  expect_true(is.integer(b_json))
  expect_identical(b_json, 20L)
})

test_that("a double stat param stays a double", {
  p <- vplot(mtcars) |> mark_smooth(x = wt, y = mpg, span = 0.6)
  span <- from_spec(as_spec(p))@layers[[1]]@stat_params$span
  expect_false(is.integer(span))
  expect_equal(span, 0.6)
})

# SR3 -----------------------------------------------------------------------
test_that("a Vega-Lite channel collision is reported, not silently dropped", {
  # color and fill both map onto the one Vega-Lite "color" channel.
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, color = factor(cyl), fill = factor(gear))
  expect_warning(
    r <- spec_to_vegalite(p),
    "already used"
  )
})
