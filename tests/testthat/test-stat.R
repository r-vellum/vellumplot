# G5: data-space statistical transforms (bin, count, smooth) + after_stat().

test_that("mark_histogram() records a bin stat", {
  p <- vplot(mtcars) |> mark_histogram(x = mpg, bins = 8)
  L <- p@layers[[1]]
  expect_identical(L@mark, "bar")
  expect_identical(L@stat, "bin")
  expect_equal(L@stat_params$bins, 8)
})

test_that("after_stat() marks a stage-2 channel", {
  p <- vplot(mtcars) |> mark_histogram(x = mpg, y = after_stat(density))
  ch <- p@layers[[1]]@encoding$y
  expect_true(ch@after)
  expect_identical(rlang::as_label(rlang::quo_get_expr(ch@expr)), "density")
})

test_that("the bin stat counts all rows across bins", {
  p <- vplot(mtcars) |> mark_histogram(x = mpg, bins = 8)
  r <- quill:::.resolve_layers(p)[[1]]
  expect_equal(length(r$values$x), 8) # one row per bin
  expect_equal(sum(r$values$y), nrow(mtcars)) # counts cover every row
})

test_that("after_stat(density) integrates to ~1 over bin widths", {
  p <- vplot(mtcars) |>
    mark_histogram(x = mpg, bins = 10, y = after_stat(density))
  r <- quill:::.resolve_layers(p)[[1]]
  width <- diff(sort(r$values$x))[1]
  expect_equal(sum(r$values$y) * width, 1, tolerance = 0.02)
})

test_that("the bin stat carries the bin width into the layer (bars touch)", {
  p <- vplot(mtcars) |> mark_histogram(x = mpg, bins = 8)
  r <- quill:::.resolve_layers(p)[[1]]
  expect_false(is.null(r$values$width))
  # the propagated width equals the spacing between adjacent bin centres, so
  # histogram bars meet edge-to-edge rather than leaving a 10% gap
  expect_equal(r$values$width[1], diff(sort(r$values$x))[1], tolerance = 1e-8)
})

test_that("the count stat preserves a custom factor level order on x", {
  d <- data.frame(
    g = factor(c("hi", "lo", "mid"), levels = c("lo", "mid", "hi"))
  )
  p <- vplot(d) |> mark_bar(x = g)
  r <- quill:::.resolve_layers(p)[[1]]
  expect_s3_class(r$values$x, "factor")
  expect_equal(levels(r$values$x), c("lo", "mid", "hi"))
})

test_that("the count stat tallies rows per category", {
  d <- data.frame(g = c("a", "a", "b"))
  p <- vplot(d) |> mark_bar(x = g)
  r <- quill:::.resolve_layers(p)[[1]]
  expect_setequal(r$values$x, c("a", "b"))
  expect_equal(r$values$y[match("a", r$values$x)], 2)
})

test_that("the smooth stat fits a dense line with a ribbon", {
  p <- vplot(mtcars) |> mark_smooth(x = wt, y = mpg, method = "lm")
  r <- quill:::.resolve_layers(p)[[1]]
  expect_equal(r$n, 80) # dense prediction grid
  expect_false(is.null(r$values$ymin))
  expect_true(all(r$values$ymin <= r$values$y & r$values$y <= r$values$ymax))
  # fitted y replaces the raw response (negative slope for wt vs mpg)
  expect_lt(r$values$y[80], r$values$y[1])
})

test_that("smooth respects se = FALSE", {
  p <- vplot(mtcars) |> mark_smooth(x = wt, y = mpg, se = FALSE)
  r <- quill:::.resolve_layers(p)[[1]]
  expect_null(r$values$ymin)
})

test_that("the y axis covers the smooth ribbon extent", {
  p <- vplot(mtcars) |> mark_smooth(x = wt, y = mpg)
  sc <- quill:::.build_panels(p)$scales
  r <- quill:::.resolve_layers(p)[[1]]
  expect_lte(sc$y$domain[1], min(r$values$ymin))
  expect_gte(sc$y$domain[2], max(r$values$ymax))
})

test_that("after_stat without a stat is an error; only lm smoothing", {
  expect_error(
    vellum::as_vellum_scene(
      vplot(mtcars) |> mark_point(x = wt, y = after_stat(count))
    ),
    "after_stat"
  )
  expect_error(
    vellum::as_vellum_scene(
      vplot(mtcars) |> mark_smooth(x = wt, y = mpg, method = "loess")
    ),
    "lm"
  )
})

test_that("statistical marks render", {
  f1 <- withr::local_tempfile(fileext = ".png")
  render_plot(vplot(mtcars) |> mark_histogram(x = mpg, bins = 10), f1)
  expect_gt(file.info(f1)$size, 0)
  f2 <- withr::local_tempfile(fileext = ".png")
  render_plot(
    vplot(mtcars) |>
      mark_point(x = wt, y = mpg) |>
      mark_smooth(x = wt, y = mpg),
    f2
  )
  expect_gt(file.info(f2)$size, 0)
})
