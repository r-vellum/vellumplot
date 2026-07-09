# theme() / set_theme() public API + layout honouring.

resolve_of <- function(p) vellumplot:::.resolve_theme(p@theme)

test_that("theme() merges element-wise onto the current theme", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    theme_bw() |>
    theme(axis.title = element_text(size = 20))
  rt <- resolve_of(p)
  expect_identical(rt[["axis.title.x"]]@size, 20)
  expect_identical(rt[["axis.title.x"]]@colour, "black") # kept from preset
  # bw gridline colour untouched by the title override
  expect_identical(rt[["panel.grid.major.x"]]@colour, "grey90")
})

test_that("theme() accumulates across chained calls", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    theme(plot.title = element_text(size = 30)) |>
    theme(legend.position = "none")
  rt <- resolve_of(p)
  expect_identical(rt[["plot.title"]]@size, 30)
  expect_identical(rt[["legend.position"]], "none")
})

test_that("theme() rejects unknown names and wrong element classes", {
  base <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
  expect_error(theme(base, not.a.slot = element_text()), "Unknown")
  expect_error(theme(base, panel.grid = element_text()), "panel.grid")
})

test_that("set_theme() maps the legacy 4 keys onto element slots", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    set_theme(panel_bg = "ivory", label_col = "navy")
  rt <- resolve_of(p)
  expect_identical(rt[["panel.background"]]@fill, "ivory")
  expect_identical(rt[["axis.text.x"]]@colour, "navy")
  expect_identical(rt[["legend.text"]]@colour, "navy")
})

test_that("set_theme() legacy NA maps to element_blank", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    set_theme(panel_bg = NA, grid_col = NA)
  rt <- resolve_of(p)
  expect_true(vellumplot:::.is_blank(rt[["panel.background"]]))
  expect_true(vellumplot:::.is_blank(rt[["panel.grid.major.x"]]))
})

test_that("a blank text element collapses its gutter track to zero", {
  zero <- vellumplot:::.track_w(element_blank(), "a label", 1.4)
  sized <- vellumplot:::.track_w(element_text(size = 11), "a label", 1.4)
  expect_identical(zero, vellum::unit(0, "mm"))
  expect_false(identical(zero, sized))
})

test_that("legend.position = none drops the legend column", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, color = hp) |>
    theme(legend.position = "none")
  lay <- vellumplot:::.build_layout(
    vellumplot:::.build_panels(p),
    vellumplot:::.legend_guides(vellumplot:::.build_panels(p)$scales),
    p@labels,
    vellumplot:::.resolve_theme(p@theme)
  )
  expect_true(is.na(lay$legend_col))
})

test_that("legend.key.size / legend.spacing / legend.margin resolve and validate", {
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg, color = factor(cyl))
  rt <- resolve_of(p)
  expect_identical(rt[["legend.key.size"]], 4)
  expect_identical(rt[["legend.spacing"]], 3.5)
  expect_identical(rt[["legend.margin"]], c(2, 2, 2, 2))

  rt2 <- resolve_of(p |> theme(legend.key.size = 8, legend.margin = 3))
  expect_identical(rt2[["legend.key.size"]], 8)
  # a length-1 margin recycles to four sides in the metrics
  m <- vellumplot:::.legend_metrics(rt2)
  expect_identical(m$margin, c(3, 3, 3, 3))

  expect_error(theme(p, legend.key.size = -1), "legend.key.size")
  expect_error(theme(p, legend.spacing = "big"), "legend.spacing")
  expect_error(theme(p, legend.margin = c(1, 2)), "legend.margin")
})

test_that("a bigger legend.key.size widens the reserved legend column", {
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg, color = factor(cyl))
  g <- vellumplot:::.legend_guides(vellumplot:::.build_panels(p)$scales)
  small <- vellumplot:::.legend_width(g, resolve_of(p))
  big <- vellumplot:::.legend_width(g, resolve_of(p |> theme(legend.key.size = 12)))
  expect_true(big > small)
})
