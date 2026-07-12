# Geometry marks: area / ribbon / step.

train <- function(p) {
  vellumplot:::.train_scales(p, vellumplot:::.resolve_layers(p))
}

d <- data.frame(
  x = 1:10,
  y = c(3, 5, 4, 7, 6, 8, 7, 9, 8, 10),
  lo = c(1, 3, 2, 5, 4, 6, 5, 7, 6, 8),
  hi = c(5, 7, 6, 9, 8, 10, 9, 11, 10, 12)
)

test_that("constructors set the right mark", {
  expect_identical(
    (vplot(d) |> mark_area(x = x, y = y))@layers[[1]]@mark,
    "area"
  )
  expect_identical(
    (vplot(d) |> mark_ribbon(x = x, ymin = lo, ymax = hi))@layers[[1]]@mark,
    "ribbon"
  )
  expect_identical(
    (vplot(d) |> mark_step(x = x, y = y))@layers[[1]]@mark,
    "step"
  )
})

test_that("area forces the y axis through zero", {
  sc <- train(vplot(d) |> mark_area(x = x, y = y))
  expect_lte(sc$y$domain[1], 0)
})

test_that(".clamp_baseline pins a non-finite baseline to the domain floor (H21)", {
  expect_equal(vellumplot:::.clamp_baseline(c(-Inf, 1, 2), c(0, 3)), c(0, 1, 2))
  expect_equal(vellumplot:::.clamp_baseline(c(NaN, 1), c(1, 5)), c(1, 1))
  # a reversed (decreasing) domain clamps to its numeric min, not domain[1]
  expect_equal(vellumplot:::.clamp_baseline(c(-Inf, 2), c(4, 1)), c(1, 2))
})

test_that("a log-y area draws finite geometry from the axis floor (H21)", {
  dp <- data.frame(x = 1:5, y = c(1, 10, 100, 1000, 10000))
  # the baseline 0 maps to -Inf on log10; without the clamp the polygon is degenerate
  expect_no_error(render_px(
    vplot(dp) |> mark_area(x = x, y = y) |> scale_y_continuous(trans = "log10")
  ))
  img <- render_px(
    vplot(dp) |>
      mark_area(x = x, y = y, fill = "black") |>
      scale_y_continuous(trans = "log10")
  )
  expect_true(has_ink(img, rows = c(0.3, 0.9), cols = c(0.55, 0.9)))
})

test_that("ribbon pools ymin/ymax into the y domain", {
  sc <- train(vplot(d) |> mark_ribbon(x = x, ymin = lo, ymax = hi))
  expect_lte(sc$y$domain[1], min(d$lo))
  expect_gte(sc$y$domain[2], max(d$hi))
})

test_that("area / ribbon / step render (incl. flipped)", {
  builds <- list(
    function(p) mark_area(p, x = x, y = y),
    function(p) mark_ribbon(p, x = x, ymin = lo, ymax = hi),
    function(p) mark_step(p, x = x, y = y),
    function(p) mark_step(p, x = x, y = y, direction = "vh")
  )
  for (b in builds) {
    f <- local_tempfile(fileext = ".png")
    render_plot(b(vplot(d)), f)
    expect_gt(file.info(f)$size, 0)
    f2 <- local_tempfile(fileext = ".png")
    render_plot(b(vplot(d)) |> coord_flip(), f2)
    expect_gt(file.info(f2)$size, 0)
  }
})

test_that("area fills below the curve (ink near the baseline)", {
  img <- render_px(vplot(d) |> mark_area(x = x, y = y, fill = "black"))
  # the lower-left region (above baseline, under the rising curve) is filled
  expect_true(has_ink(img, rows = c(0.6, 0.9), cols = c(0.2, 0.5)))
})

test_that("grouped ribbon (per colour) renders", {
  g <- rbind(
    cbind(d, grp = "a"),
    cbind(transform(d, lo = lo + 2, hi = hi + 2), grp = "b")
  )
  f <- local_tempfile(fileext = ".png")
  render_plot(
    vplot(g) |> mark_ribbon(x = x, ymin = lo, ymax = hi, fill = grp),
    f
  )
  expect_gt(file.info(f)$size, 0)
})

test_that("area and ribbon share .emit_band and honour a per-group fill", {
  g <- rbind(
    cbind(d, grp = "a"),
    cbind(transform(d, y = y + 3, lo = lo + 3, hi = hi + 3), grp = "b")
  )
  # both marks draw one band per group -> two distinct fills in the render
  rib <- render_px(
    vplot(g) |>
      mark_ribbon(x = x, ymin = lo, ymax = hi, fill = grp) |>
      scale_fill_manual(values = c(a = "red", b = "blue"))
  )
  expect_gt(count_near(rib, c(1, 0, 0), 0.15), 50)
  expect_gt(count_near(rib, c(0, 0, 1), 0.15), 50)
  ar <- render_px(
    vplot(g) |>
      mark_area(x = x, y = y, fill = grp, position = "identity") |>
      scale_fill_manual(values = c(a = "red", b = "blue"))
  )
  expect_gt(count_near(ar, c(1, 0, 0), 0.15), 50)
  expect_gt(count_near(ar, c(0, 0, 1), 0.15), 50)
})
