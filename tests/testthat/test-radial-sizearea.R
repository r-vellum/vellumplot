# coord_radial() (partial arc + donut) and scale_size_area() (area-proportional).

sm_xy <- function(p) {
  e <- vellum::scene_model(vellum::as_vellum_scene(p))$elements
  e[, c("x", "y")]
}

test_that("coord_radial sets the polar coord with end + inner_radius", {
  co <- (vplot(mtcars) |>
    mark_bar(x = factor(cyl)) |>
    coord_radial(theta = "x", end = pi, inner_radius = 0.3))@coord
  expect_identical(co@kind, "polar")
  expect_equal(co@end, pi)
  expect_equal(co@rmin, 0.3)
})

test_that("coord_radial() with defaults matches coord_polar() geometry", {
  base <- vplot(mtcars) |> mark_bar(x = factor(cyl))
  expect_equal(
    sm_xy(base |> coord_radial(theta = "x")),
    sm_xy(base |> coord_polar(theta = "x"))
  )
})

test_that("a partial arc changes the geometry", {
  base <- vplot(mtcars) |> mark_bar(x = factor(cyl))
  full <- sm_xy(base |> coord_radial(theta = "x"))
  half <- sm_xy(
    base |> coord_radial(theta = "x", start = -pi / 2, end = pi / 2)
  )
  expect_false(isTRUE(all.equal(full, half)))
})

test_that("inner_radius must be in [0, 1)", {
  expect_error(
    vplot(mtcars) |>
      mark_bar(x = factor(cyl)) |>
      coord_radial(inner_radius = 1),
    "inner_radius"
  )
  expect_error(
    vplot(mtcars) |>
      mark_bar(x = factor(cyl)) |>
      coord_radial(inner_radius = -0.2),
    "inner_radius"
  )
})

test_that("scale_size_area maps value to area with 0 at 0", {
  sc <- vellumplot:::.build_panels(
    vplot(mtcars) |>
      mark_point(x = wt, y = mpg, size = hp) |>
      scale_size_area(max_size = 10)
  )$scales
  hi <- max(mtcars$hp)
  expect_equal(sc$size$map(0), 0)
  expect_equal(sc$size$map(hi), 10)
  # area (size^2) is proportional to the value: size(v)^2 / v is constant
  s1 <- sc$size$map(0.25 * hi)
  s2 <- sc$size$map(hi)
  expect_equal(s1^2 / (0.25 * hi), s2^2 / hi, tolerance = 1e-8)
  # and 25% of the max value is half the max radius
  expect_equal(s1, 5, tolerance = 1e-8)
})

test_that("scale_size_area defaults to max_size 6", {
  sc <- vellumplot:::.build_panels(
    vplot(mtcars) |>
      mark_point(x = wt, y = mpg, size = hp) |>
      scale_size_area()
  )$scales
  expect_equal(sc$size$map(max(mtcars$hp)), 6)
})

test_that("coord_radial and scale_size_area render", {
  f1 <- local_tempfile(fileext = ".png")
  render_plot(
    vplot(mtcars) |>
      mark_bar(x = factor(cyl)) |>
      coord_radial(inner_radius = 0.4),
    f1
  )
  expect_gt(file.info(f1)$size, 0)
  f2 <- local_tempfile(fileext = ".png")
  render_plot(
    vplot(mtcars) |>
      mark_point(x = wt, y = mpg, size = hp) |>
      scale_size_area(max_size = 10),
    f2
  )
  expect_gt(file.info(f2)$size, 0)
})
