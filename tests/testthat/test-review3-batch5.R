# Regression tests for REVIEW3 Batch 5 (opportunistic correctness/robustness).

# --- 5A: guides(fill=) alias match + coord limit validation ------------------

test_that("guides(fill=) updates the manual fill scale instead of shadowing it", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    scale_fill_manual(values = c("red", "green", "blue")) |>
    guides(fill = guide_legend(reverse = TRUE))
  # One color/fill scale (the manual one, now carrying the guide) -- not two,
  # where an empty guide-only "color" scale would shadow the palette.
  cf <- Filter(function(s) s@aesthetic %in% c("color", "fill"), p@scales)
  expect_length(cf, 1L)
  expect_identical(cf[[1]]@palette, c("red", "green", "blue"))
  expect_false(is.null(cf[[1]]@guide))
})

test_that("guides(color=) still hides a color legend (alias match keeps working)", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, color = factor(cyl)) |>
    scale_color_manual(values = c("red", "green", "blue")) |>
    guides(color = "none")
  cf <- Filter(function(s) s@aesthetic %in% c("color", "fill"), p@scales)
  expect_length(cf, 1L)
  expect_identical(cf[[1]]@guide, "none")
  expect_identical(cf[[1]]@palette, c("red", "green", "blue"))
})

test_that("coord constructors reject a malformed xlim/ylim early", {
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
  expect_error(coord_cartesian(p, xlim = c(1, 2, 3)), "length-2")
  expect_error(coord_flip(p, ylim = c(1, 2, 3)), "length-2")
  expect_error(coord_fixed(p, xlim = 5), "length-2")
  expect_error(coord_sf(p, xlim = c(1, 2, 3)), "length-2")
  # NULL and length-2 still pass.
  expect_no_error(coord_cartesian(p, xlim = c(0, 6), ylim = NULL))
})
