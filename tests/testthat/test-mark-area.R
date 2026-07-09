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
    f <- withr::local_tempfile(fileext = ".png")
    render_plot(b(vplot(d)), f)
    expect_gt(file.info(f)$size, 0)
    f2 <- withr::local_tempfile(fileext = ".png")
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
  f <- withr::local_tempfile(fileext = ".png")
  render_plot(
    vplot(g) |> mark_ribbon(x = x, ymin = lo, ymax = hi, fill = grp),
    f
  )
  expect_gt(file.info(f)$size, 0)
})
