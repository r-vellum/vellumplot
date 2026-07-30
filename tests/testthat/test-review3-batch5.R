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

# --- 5B: mark-emitter robustness --------------------------------------------

test_that("a bar sparkline with an NA in the series still draws its bars", {
  # Before: max(rawy) without na.rm made `top` NA, so every bar height went NA
  # and the sparkline drew blank. After: only the NA bar drops.
  spk <- vsparkline(c(3, 1, NA, 5, 2), type = "bar")
  f <- local_tempfile(fileext = ".png")
  expect_no_error(render_plot(spk, f))
  px <- render_px(spk)
  # bars are drawn in grey30 -> some clearly non-white pixels exist.
  expect_gt(mean(px[,, 1] < 0.6), 0)
})

test_that("mark_datashade validates `how` at construction", {
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
  expect_error(mark_datashade(p, x = wt, y = mpg, how = "bogus"))
  expect_no_error(mark_datashade(p, x = wt, y = mpg, how = "log"))
})

test_that("mark_errorbar honours a mapped cap width without misplacing caps", {
  d <- data.frame(
    x = c("a", "b"),
    lo = c(1, 2),
    hi = c(3, 4),
    w = c(0.2, 0.8)
  )
  p <- vplot(d) |> mark_errorbar(x = x, ymin = lo, ymax = hi, width = w)
  f <- local_tempfile(fileext = ".png")
  expect_no_error(render_plot(p, f))
})

test_that("mark_hex honours a mapped alpha via the trained scale", {
  set.seed(1)
  d <- data.frame(x = rnorm(200), y = rnorm(200))
  p <- vplot(d) |> mark_hex(x = x, y = y, alpha = y)
  f <- local_tempfile(fileext = ".png")
  expect_no_error(render_plot(p, f))
})
