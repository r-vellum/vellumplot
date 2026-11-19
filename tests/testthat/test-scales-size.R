# Size scale: range/limits/breaks overrides.

train <- function(p) {
  vellumplot:::.train_scales(p, vellumplot:::.resolve_layers(p))
}

test_that("scale_size(range=) sets the output mm range", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, size = hp) |>
    scale_size(range = c(2, 10))
  sz <- train(p)$size
  expect_gte(min(sz$legend_sizes), 2 - 1e-9)
  expect_lte(max(sz$map(range(mtcars$hp))), 10 + 1e-9)
})

test_that("default size range is c(1, 4) via scale_size", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, size = hp) |>
    scale_size()
  rng <- range(train(p)$size$map(range(mtcars$hp)))
  expect_equal(rng, c(1, 4), tolerance = 1e-6)
})

test_that("scale_size(limits=) sets the data domain", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, size = hp) |>
    scale_size(limits = c(0, 400))
  expect_identical(train(p)$size$range, c(0, 400))
})

test_that("explicit size breaks are honoured", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, size = hp) |>
    scale_size(breaks = c(100, 200, 300))
  expect_identical(train(p)$size$legend_breaks, c(100, 200, 300))
})

test_that("a larger range renders larger points", {
  small <- render_px(
    vplot(mtcars) |>
      mark_point(x = wt, y = mpg, size = hp) |>
      scale_size(range = c(1, 2))
  )
  big <- render_px(
    vplot(mtcars) |>
      mark_point(x = wt, y = mpg, size = hp) |>
      scale_size(range = c(4, 10))
  )
  ink <- function(img) {
    sum(rowSums(img[,, 1:3, drop = FALSE] <= 0.3, dims = 2) == 3)
  }
  expect_gt(ink(big), ink(small))
})

# --- VP-39: categorical size / alpha ----------------------------------------

test_that("a factor mapped to size trains a discrete scale keyed by its levels", {
  d <- transform(mtcars, cyl = factor(cyl))
  p <- vplot(d) |> mark_point(x = wt, y = mpg, size = cyl)
  expect_warning(
    .build_panels(p),
    class = "vellumplot_discrete_sized_aes"
  )
  sc <- suppressWarnings(.build_panels(p)$scales$size)
  expect_identical(sc$type, "discrete")
  expect_identical(sc$levels, c("4", "6", "8"))
  expect_identical(sc$legend_labels, c("4", "6", "8"))
  # evenly spaced in area over the size range, smallest first
  expect_equal(sc$legend_sizes^2, seq(1, 16, length.out = 3))
  expect_equal(sc$map(c("8", "4")), sc$legend_sizes[c(3, 1)])
  expect_no_error(suppressWarnings(vellum::as_vellum_scene(p)))
})

test_that("an ordered factor or a character trains discrete too", {
  d <- data.frame(
    x = 1:3,
    y = 1:3,
    o = factor(
      c("lo", "mid", "hi"),
      levels = c("lo", "mid", "hi"),
      ordered = TRUE
    ),
    ch = c("b", "a", "c")
  )
  p <- vplot(d) |> mark_point(x = x, y = y, size = o)
  # ordered: an order the size can show, so no warning (ggplot2's ordinal scale)
  expect_no_warning(.build_panels(p))
  sc <- .build_panels(p)$scales$size
  expect_identical(sc$levels, c("lo", "mid", "hi"))
  p2 <- vplot(d) |> mark_point(x = x, y = y, alpha = ch)
  expect_warning(.build_panels(p2), "alpha")
  sc2 <- suppressWarnings(.build_panels(p2)$scales$alpha)
  expect_identical(sc2$type, "discrete")
  expect_identical(sc2$levels, c("a", "b", "c"))
  expect_equal(sc2$legend_alphas, c(0.1, 0.55, 1))
})

test_that("a discrete size legend merges with the colour legend of the same variable", {
  d <- transform(mtcars, cyl = factor(cyl))
  p <- vplot(d) |> mark_point(x = wt, y = mpg, size = cyl, color = cyl)
  sc <- suppressWarnings(.build_panels(p)$scales)
  g <- .legend_guides(sc)
  expect_length(g, 1L)
  expect_identical(g[[1]]$kind, "merged")
  expect_identical(g[[1]]$sc$labels, c("4", "6", "8"))
  expect_length(g[[1]]$sc$sizes_mm, 3L)
})

test_that("a discrete size scale honours breaks, range and reverse", {
  d <- transform(mtcars, cyl = factor(cyl))
  p <- vplot(d) |>
    mark_point(x = wt, y = mpg, size = cyl) |>
    scale_size(breaks = c("4", "8"), range = c(2, 6))
  sc <- suppressWarnings(.build_panels(p)$scales$size)
  expect_identical(sc$levels, c("4", "8"))
  # the mapping still spans every level; breaks only pick the legend keys
  expect_equal(sc$legend_sizes, c(2, 6))
  expect_equal(sc$map("6"), sqrt(20))
  rev <- .reverse_guide(sc)
  expect_identical(rev$levels, c("8", "4"))
  expect_identical(rev$legend_labels, c("8", "4"))
})
