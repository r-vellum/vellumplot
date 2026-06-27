# Compile-level tests: the spec compiles to a vellum scene and trains sensible
# scales (no pixel inspection here).

test_that("as_vellum_scene() returns a vellum scene", {
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
  sc <- vellum::as_vellum_scene(p)
  expect_true(inherits(sc, "vellum::vellum_scene"))
})

test_that("render_plot() writes a PNG; render() dispatches through the seam", {
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
  f1 <- withr::local_tempfile(fileext = ".png")
  render_plot(p, f1)
  expect_true(file.exists(f1) && file.info(f1)$size > 0)

  f2 <- withr::local_tempfile(fileext = ".png")
  vellum::render(p, f2)
  expect_true(file.exists(f2) && file.info(f2)$size > 0)
})

test_that("an empty plot (no layers) errors clearly", {
  expect_error(vellum::as_vellum_scene(vplot(mtcars)), "Nothing to draw")
})

test_that("continuous position scales train range + expansion + breaks", {
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
  sc <- vellumplot:::.train_scales(p, vellumplot:::.resolve_layers(p))
  rng <- range(mtcars$wt)
  # expanded domain brackets the data range by ~5% each side
  expect_lt(sc$x$domain[1], rng[1])
  expect_gt(sc$x$domain[2], rng[2])
  expect_equal(sc$x$domain, scales::expand_range(rng, mul = 0.05), tolerance = 1e-8)
  # breaks lie within the data range
  expect_true(all(sc$x$breaks >= rng[1] & sc$x$breaks <= rng[2]))
  expect_identical(sc$x$name, "wt")
})

test_that("scales are trained across all layers", {
  p <- vplot(data.frame(a = 1:3, b = 1:3)) |>
    mark_point(x = a, y = b) |>
    mark_point(x = I(a + 100), y = b)
  sc <- vellumplot:::.train_scales(p, vellumplot:::.resolve_layers(p))
  expect_gt(sc$x$domain[2], 100)
})

test_that("a continuous colour scale quantizes to <=256 style groups", {
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg, color = hp) |> scale_color_continuous()
  sc <- vellumplot:::.train_scales(p, vellumplot:::.resolve_layers(p))
  expect_identical(sc$color$kind, "continuous")
  many <- sc$color$map(seq(min(mtcars$hp), max(mtcars$hp), length.out = 1000))
  expect_lte(length(unique(many)), 256)
})

test_that("a discrete colour scale maps each level to a distinct colour", {
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg, color = factor(cyl))
  sc <- vellumplot:::.train_scales(p, vellumplot:::.resolve_layers(p))
  expect_identical(sc$color$kind, "discrete")
  expect_length(sc$color$levels, 3)
  expect_length(unique(sc$color$colors), 3)
})
