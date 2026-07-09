# Statistical marks: boxplot / errorbar / linerange / summary.

train <- function(p) {
  vellumplot:::.train_scales(p, vellumplot:::.resolve_layers(p))
}
resolve1 <- function(p) vellumplot:::.resolve_layers(p)[[1]]

eb <- data.frame(
  g = c("a", "b", "c"),
  m = c(2, 4, 3),
  lo = c(1, 3, 2.5),
  hi = c(3, 5, 3.5)
)

test_that("constructors set mark/stat", {
  expect_identical(
    (vplot(mtcars) |> mark_boxplot(x = factor(cyl), y = mpg))@layers[[1]]@mark,
    "boxplot"
  )
  expect_identical(
    (vplot(eb) |> mark_errorbar(x = g, ymin = lo, ymax = hi))@layers[[1]]@mark,
    "errorbar"
  )
  expect_identical(
    (vplot(eb) |> mark_linerange(x = g, ymin = lo, ymax = hi))@layers[[1]]@mark,
    "linerange"
  )
  sl <- (vplot(mtcars) |> mark_summary(x = factor(cyl), y = mpg))@layers[[1]]
  expect_identical(sl@mark, "point")
  expect_identical(sl@stat, "aggregate")
})

test_that("stat aggregate computes the per-group function", {
  r <- resolve1(vplot(mtcars) |> mark_summary(x = factor(cyl), y = mpg))
  expect_identical(r$n, 3L)
  expect_equal(
    sort(round(r$values$y, 2)),
    sort(round(tapply(mtcars$mpg, mtcars$cyl, mean), 2)),
    ignore_attr = TRUE
  )
  r2 <- resolve1(
    vplot(mtcars) |> mark_summary(x = factor(cyl), y = mpg, fun = median)
  )
  expect_equal(
    sort(r2$values$y),
    sort(as.numeric(tapply(mtcars$mpg, mtcars$cyl, median)))
  )
})

test_that("boxplot y axis covers the raw data (not forced to zero)", {
  sc <- train(vplot(mtcars) |> mark_boxplot(x = factor(cyl), y = mpg))
  expect_gt(sc$y$domain[1], 0) # mpg minimum is ~10, so the axis need not include 0
})

test_that("errorbar pools ymin/ymax into the y domain", {
  sc <- train(vplot(eb) |> mark_errorbar(x = g, ymin = lo, ymax = hi))
  expect_lte(sc$y$domain[1], min(eb$lo))
  expect_gte(sc$y$domain[2], max(eb$hi))
})

test_that("boxplot / errorbar / linerange / summary render (incl. flipped)", {
  builds <- list(
    function() vplot(mtcars) |> mark_boxplot(x = factor(cyl), y = mpg),
    function() vplot(eb) |> mark_errorbar(x = g, ymin = lo, ymax = hi),
    function() vplot(eb) |> mark_linerange(x = g, ymin = lo, ymax = hi),
    function() vplot(mtcars) |> mark_summary(x = factor(cyl), y = mpg),
    function() {
      vplot(mtcars) |> mark_boxplot(x = factor(cyl), y = mpg) |> coord_flip()
    },
    function() {
      vplot(eb) |> mark_errorbar(x = g, ymin = lo, ymax = hi) |> coord_flip()
    }
  )
  for (mk in builds) {
    f <- withr::local_tempfile(fileext = ".png")
    render_plot(mk(), f)
    expect_gt(file.info(f)$size, 0)
  }
})

test_that("boxplot draws ink across the panel (boxes + whiskers)", {
  img <- render_px(vplot(mtcars) |> mark_boxplot(x = factor(cyl), y = mpg))
  expect_true(has_ink(img, rows = c(0.2, 0.8), cols = c(0.1, 0.9)))
})

test_that("a boxplot + jittered points overlay renders", {
  f <- withr::local_tempfile(fileext = ".png")
  render_plot(
    vplot(mtcars) |>
      mark_boxplot(x = factor(cyl), y = mpg) |>
      mark_point(x = factor(cyl), y = mpg, position = "jitter"),
    f
  )
  expect_gt(file.info(f)$size, 0)
})
