# G2 (finish): mark_bar() + discrete position scale, size scale + size legend,
# and stacked legends.

bar_df <- data.frame(
  cat = factor(c("a", "b", "c")),
  val = c(3, 1, 2)
)

test_that("mark_bar() appends a bar layer", {
  p <- vplot(bar_df) |> mark_bar(x = cat, y = val)
  expect_length(p@layers, 1)
  expect_identical(p@layers[[1]]@mark, "bar")
})

test_that("a discrete position scale maps levels to bands", {
  p <- vplot(bar_df) |> mark_bar(x = cat, y = val)
  sc <- vellumplot:::.train_scales(p, vellumplot:::.resolve_layers(p))
  expect_true(sc$x$discrete)
  expect_equal(sc$x$domain, c(0.5, 3.5))
  expect_identical(sc$x$labels, c("a", "b", "c"))
  expect_equal(sc$x$map(c("a", "c")), c(1, 3))
})

test_that("bars force the y axis to include zero", {
  p <- vplot(bar_df) |> mark_bar(x = cat, y = val)
  sc <- vellumplot:::.train_scales(p, vellumplot:::.resolve_layers(p))
  expect_lte(sc$y$domain[1], 0)
})

test_that("a bar with no y counts rows per category", {
  d <- data.frame(g = factor(c("a", "a", "a", "b", "c", "c")))
  p <- vplot(d) |> mark_bar(x = g)
  r <- vellumplot:::.resolve_layers(p)[[1]]
  expect_equal(r$values$x, c("a", "b", "c"))
  expect_equal(r$values$y, c(3, 1, 2))
  sc <- vellumplot:::.train_scales(p, vellumplot:::.resolve_layers(p))
  expect_identical(sc$y$name, "count")
})

test_that("a bar with no y counts per unique x value (stat = count)", {
  p <- vplot(data.frame(x = c(1, 1, 2, 3))) |> mark_bar(x = x)
  r <- vellumplot:::.resolve_layers(p)[[1]]
  expect_setequal(as.numeric(r$values$x), c(1, 2, 3))
  expect_equal(r$values$y[match("1", as.character(r$values$x))], 2)
})

test_that("a mapped size trains a size scale and is used by points", {
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg, size = hp)
  sc <- vellumplot:::.train_scales(p, vellumplot:::.resolve_layers(p))
  expect_false(is.null(sc$size))
  rng <- range(mtcars$hp)
  expect_equal(sc$size$map(rng), vellumplot:::.SIZE_RANGE)
})

test_that("colour + size produce two stacked legend guides", {
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg, color = qsec, size = hp)
  sc <- vellumplot:::.train_scales(p, vellumplot:::.resolve_layers(p))
  guides <- vellumplot:::.legend_guides(sc)
  expect_length(guides, 2)
  expect_identical(vapply(guides, function(g) g$kind, character(1)),
                   c("color_continuous", "size"))
})

test_that("a bar chart renders with bars on a zero baseline", {
  p <- vplot(bar_df, width = 4, height = 3) |> mark_bar(x = cat, y = val)
  img <- render_png(p)
  # the grey panel is broken up by the default bar fill ("grey35")
  fill <- as.numeric(grDevices::col2rgb("grey35")) / 255
  expect_gt(count_near(img, fill, tol = 0.05), 0.03 * prod(dim(img)[1:2]))
})
