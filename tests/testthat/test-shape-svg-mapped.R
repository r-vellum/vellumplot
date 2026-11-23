# VP-112: a mapped `shape` whose values are SVG paths is still a mapping (the
# levels cycle through the shape palette), but it warns, pointing to the
# literal-string form and scale_shape_identity().

star <- "M12 2l3 7h7l-5.5 4.5 2 7-6.5-4.5-6.5 4.5 2-7L2 9h7z"
sd <- data.frame(x = 1:3, y = 1:3, s = c(star, "M0 0 L10 0 L5 8 Z", star))
scene_of <- function(p) vellum::as_vellum_scene(p)

test_that(".looks_like_svg_shape() recognises path data and .svg files", {
  f <- vellumplot:::.looks_like_svg_shape
  expect_identical(
    f(c(star, "m10,20 l5 5z", "M-1.5e2-3 h4", "icon.SVG")),
    rep(TRUE, 4)
  )
  expect_identical(
    f(c("M 3", "Male", "M", "circle", "M1 2 x", NA)),
    rep(FALSE, 6)
  )
})

test_that("mapping a column of SVG paths warns once and keeps the mapping", {
  expect_warning(
    sc <- scene_of(vplot(sd) |> mark_point(x = x, y = y, shape = s)),
    class = "vellumplot_shape_svg_mapped"
  )
  w <- 0L
  withCallingHandlers(
    scene_of(vplot(sd) |> mark_point(x = x, y = y, shape = s)),
    vellumplot_shape_svg_mapped = function(cnd) {
      w <<- w + 1L
      invokeRestart("muffleWarning")
    }
  )
  expect_identical(w, 1L)
  # the levels still map to palette markers, with a two-key shape legend
  p <- vplot(sd) |> mark_point(x = x, y = y, shape = s)
  sh <- suppressWarnings(
    vellumplot:::.train_shape(p, vellumplot:::.resolve_layers(p))
  )
  expect_length(sh$levels, 2L)
})

test_that("an environment variable holding one path is that shape (VP-126)", {
  # a non-column symbol with one value is a setting, like the literal string
  p <- star
  expect_no_warning(
    a <- scene_of(vplot(sd) |> mark_point(x = x, y = y, shape = p))
  )
  b <- scene_of(vplot(sd) |> mark_point(
    x = x, y = y, shape = "M12 2l3 7h7l-5.5 4.5 2 7-6.5-4.5-6.5 4.5 2-7L2 9h7z"
  ))
  expect_identical(vellum::scene_svg(a), vellum::scene_svg(b))
  # forcing the mapping with .env$ keeps the VP-112 warning
  expect_warning(
    scene_of(vplot(sd) |> mark_point(x = x, y = y, shape = .env$p)),
    "literal string"
  )
})

test_that("the literal string, scale_shape_identity() and plain levels don't warn", {
  expect_no_warning(scene_of(
    vplot(sd) |> mark_point(x = x, y = y, shape = "M0 0 L10 0 L5 8 Z")
  ))
  expect_no_warning(scene_of(
    vplot(sd) |> mark_point(x = x, y = y, shape = s) |> scale_shape_identity()
  ))
  mt <- transform(mtcars, fcyl = factor(cyl))
  expect_no_warning(scene_of(
    vplot(mt) |> mark_point(x = wt, y = mpg, shape = fcyl)
  ))
})
