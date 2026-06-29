# Rich-text (md()) labels in titles, axis names, and legend names. The drawing
# path is vellum's; these tests assert vellumplot threads the md() object through
# without coercing it to a string and that layout/print stay sound.

test_that("md() is re-exported and yields a rich-text label", {
  lab <- md("**bold** ^2^")
  expect_false(is.character(lab))
  expect_true(inherits(lab, "vellum::vellum_md_label"))
})

test_that("scale_*(name = md()) reaches the trained scale un-coerced", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    scale_x_continuous(name = md("*weight*"))
  sc <- vellumplot:::.train_scales(p, vellumplot:::.resolve_layers(p))
  expect_false(is.character(sc$x$name))
  expect_true(inherits(sc$x$name, "vellum::vellum_md_label"))
})

test_that("labs(title = md()) reaches spec@labels un-coerced", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    labs(title = md("**Fuel** economy"))
  expect_false(is.character(p@labels$title))
  expect_true(inherits(p@labels$title, "vellum::vellum_md_label"))
})

test_that("a rich title renders ink in the top band", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    labs(title = md("**A** rich title"))
  img <- render_px(p)
  expect_true(has_ink(img, rows = c(0, 0.08), cols = c(0, 0.6)))
})

test_that("a rich axis title renders the plot without error", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    labs(y = md("Efficiency (mi gal^-1^)"))
  expect_no_error(render_px(p))
})

test_that("legend width is finite/positive with a rich legend name", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, color = factor(cyl)) |>
    labs(color = md("*cylinders*"))
  built <- vellumplot:::.build_panels(p)
  guides <- vellumplot:::.legend_guides(built$scales)
  rt <- vellumplot:::.resolve_theme(vellumplot:::.theme_default())
  w <- vellumplot:::.legend_width(guides, rt)
  expect_true(vctrs::field(w, "value") > 0)
  expect_no_error(vellumplot:::.build_layout(built, guides, p@labels, rt))
})

test_that("summary()/print() tolerate rich labels", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, color = hp) |>
    scale_x_continuous(name = md("*x*")) |>
    labs(title = md("**T**"), color = md("_c_"))
  expect_no_error(capture.output(summary(p)))
  expect_no_error(capture.output(print(p)))
})

test_that("plain-string titles still render ink (fast path unchanged)", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    labs(title = "A Plain Title")
  img <- render_px(p)
  expect_true(has_ink(img, rows = c(0, 0.08), cols = c(0, 0.6)))
})
