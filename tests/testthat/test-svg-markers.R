# SVG icon markers: shape = a path `d` string or a `.svg` file, drawn as crisp
# vector markers via vellum's svg_grob() instead of a built-in points_grob shape.

star <- "M12 2l3 7h7l-5.5 4.5 2 7-6.5-4.5-6.5 4.5 2-7L2 9h7z"
heart <- "M12 21s-7-4.35-9.5-8.5C1 9 2.5 5 6 5c2 0 3.5 1.5 4 2.5C10.5 6.5 12 5 14 5c3.5 0 5 4 3.5 7.5C19 16.65 12 21 12 21z"

svg_of <- function(p) vellum::scene_svg(vellum::as_vellum_scene(p))
n_paths <- function(p) lengths(gregexpr("<path", svg_of(p)))

test_that(".shape_svg_d distinguishes built-in markers from SVG icons", {
  expect_true(all(is.na(vellumplot:::.shape_svg_d(c(
    "circle",
    "square",
    "star"
  )))))
  expect_identical(vellumplot:::.shape_svg_d(star), star)
})

test_that("a literal `d` shape is a constant that draws a vector path per point", {
  d <- data.frame(x = 1:5, y = 1:5)
  base <- n_paths(vplot(d) |> mark_point(x = x, y = y)) # built-in circles: 0 <path>
  icons <- n_paths(vplot(d) |> mark_point(x = x, y = y, shape = star))
  expect_gt(icons, base) # icons are drawn as <path>
})

test_that("a literal-`d` constant shape trains no scale or legend", {
  # a literal string is a constant param (a bare symbol like `shape = star`
  # would be a mapping, exactly as `color = somevar` is)
  d <- data.frame(x = 1:3, y = 1:3)
  svg <- svg_of(
    vplot(d) |> mark_point(x = x, y = y, shape = "M0 0 L10 0 L5 10 Z")
  )
  expect_no_match(svg, "legend")
})

test_that("mark_point with an SVG shape does not perturb scene_model()", {
  # a keyed multi-sub-path svg path is not yet wired for interactivity, but it
  # must never break scene_model() when a layer is otherwise interactive.
  d <- data.frame(x = 1:3, y = 1:3, g = c("a", "b", "c"))
  p <- vplot(d) |> mark_point(x = x, y = y, shape = star, data_id = g)
  expect_no_error(vellum::scene_model(vellum::as_vellum_scene(p)))
})

test_that("scale_shape maps a variable to SVG icons, with an icon legend", {
  d <- data.frame(x = 1:6, y = 1:6, g = rep(c("a", "b"), 3))
  p <- vplot(d) |>
    mark_point(x = x, y = y, shape = g) |>
    scale_shape(values = c(a = star, b = heart))
  expect_no_error(vellum::as_vellum_scene(p))
  # two distinct icons across marks + legend keys -> several <path> elements
  expect_gt(n_paths(p), 6L)
})

test_that("scale_shape still rejects a genuine bad shape name", {
  expect_error(
    scale_shape(
      vplot(mtcars) |> mark_point(x = wt, y = mpg, shape = factor(cyl)),
      values = c("circle", "notashape")
    ),
    "Unknown shape"
  )
})

test_that("a .svg file shape is read and drawn", {
  skip_if_not_installed("xml2")
  f <- withr::local_tempfile(fileext = ".svg")
  writeLines(sprintf('<svg viewBox="0 0 24 24"><path d="%s"/></svg>', star), f)
  d <- data.frame(x = 1:3, y = 1:3)
  expect_no_error(vellum::as_vellum_scene(
    vplot(d) |> mark_point(x = x, y = y, shape = f)
  ))
  expect_gt(n_paths(vplot(d) |> mark_point(x = x, y = y, shape = f)), 0L)
})

test_that("built-in shapes are unchanged (no <path> markers, no error)", {
  d <- data.frame(
    x = 1:5,
    y = 1:5,
    g = factor(rep(c("a", "b", "c"), length.out = 5))
  )
  expect_no_error(vellum::as_vellum_scene(
    vplot(d) |>
      mark_point(x = x, y = y, shape = g) |>
      scale_shape(values = c("circle", "triangle", "square"))
  ))
})
