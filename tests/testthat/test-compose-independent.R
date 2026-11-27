# Compositions laid out cell by cell (the independent path) and the
# hconcat() / vconcat() sizing arguments.

svg_of <- function(x) vellum::scene_svg(vellum::as_vellum_scene(x))
n_text <- function(svg, txt) {
  lengths(regmatches(
    svg,
    gregexpr(paste0(">", txt, "</text>"), svg, fixed = TRUE)
  ))
}
panels_of <- function(x) {
  sm <- vellum::scene_model(vellum::as_vellum_scene(x))
  sm$panels[grepl("panel-", sm$panels$name), , drop = FALSE]
}

test_that("vconcat() takes heights and hconcat() takes widths (VP-151)", {
  a <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
  b <- vplot(mtcars) |> mark_histogram(x = mpg, bins = 10)
  v <- vconcat(a, b, heights = c(3, 1))
  expect_identical(v@nrow, 2)
  expect_identical(v@ncol, 1)
  expect_identical(v@heights, c(3, 1))
  h <- hconcat(a, b, widths = c(2, 1))
  expect_identical(h@ncol, 2)
  expect_identical(h@widths, c(2, 1))
  # the panels follow the weights
  pan <- panels_of(v)
  ht <- pan$py1 - pan$py0
  expect_gt(ht[1] / ht[2], 2)
})

test_that("hconcat() / vconcat() reject non-plots and foreign arguments (VP-151)", {
  a <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
  expect_error(hconcat(a, a, heights = c(3, 1)), "does not take")
  expect_error(vconcat(a, a, widths = c(3, 1)), "does not take")
  expect_error(vconcat(a, a, ncol = 2), "does not take")
  expect_error(vconcat(a, c(3, 1)), "Argument 2")
  expect_error(hconcat(a, "x"), "Argument 2")
})

test_that("a non-aligned composition draws its title, subtitle and caption (VP-152)", {
  p3 <- vplot(mtcars) |> mark_histogram(x = mpg, bins = 10)
  faceted <- vplot(mtcars) |> mark_point(x = wt, y = mpg) |> facet_wrap(~am)
  plain <- concat(faceted, p3, ncol = 2)
  comp <- plain |>
    compose_annotation(
      title = "FIGTITLE",
      subtitle = "FIGSUB",
      caption = "FIGCAP",
      tag_levels = "A"
    )
  expect_false(vellumplot:::.comp_alignable(comp))
  svg <- svg_of(comp)
  for (txt in c("FIGTITLE", "FIGSUB", "FIGCAP", "A", "B")) {
    expect_identical(n_text(svg, txt), 1L, label = txt)
  }
  # the bands push the cells down and up
  pan <- panels_of(comp)
  base <- panels_of(plain)
  expect_identical(nrow(pan), nrow(base))
  expect_true(all(pan$py0 > base$py0 + 5))
  expect_true(all(pan$py1 < base$py1))
  # no annotation, no change
  expect_identical(
    vellum::scene_svg(vellum::as_vellum_scene(plain)),
    vellum::scene_svg(vellum::as_vellum_scene(concat(faceted, p3, ncol = 2)))
  )
})

test_that("a composition holding a nested one draws both titles (VP-152)", {
  p1 <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
  p3 <- vplot(mtcars) |> mark_histogram(x = mpg, bins = 10)
  inner <- concat(p1, p1, ncol = 2) |> compose_annotation(title = "INNER")
  outer <- concat(inner, p3, ncol = 1) |> compose_annotation(title = "OUTER")
  expect_false(vellumplot:::.comp_alignable(outer))
  svg <- svg_of(outer)
  expect_identical(n_text(svg, "OUTER"), 1L)
  expect_identical(n_text(svg, "INNER"), 1L)
  # a non-alignable nested composition draws its own title too
  fac <- p1 |> facet_wrap(~am)
  inner2 <- concat(fac, p3, ncol = 2) |> compose_annotation(caption = "INCAP")
  svg2 <- svg_of(concat(inner2, p3, ncol = 1))
  expect_identical(n_text(svg2, "INCAP"), 1L)
})

test_that("a non-aligned composition collects identical legends (VP-152)", {
  pc <- vplot(mtcars) |> mark_point(x = wt, y = mpg, color = factor(cyl))
  fac <- pc |> facet_wrap(~am)
  comp <- concat(fac, pc, ncol = 2)
  expect_false(vellumplot:::.comp_alignable(comp))
  expect_identical(n_text(svg_of(comp), "factor(cyl)"), 1L)
  keep <- concat(fac, pc, ncol = 2, guides = "keep")
  expect_identical(n_text(svg_of(keep), "factor(cyl)"), 2L)
  # the collected legend follows the composition's legend.position ...
  bottom <- comp |> theme(legend.position = "bottom")
  expect_identical(n_text(svg_of(bottom), "factor(cyl)"), 1L)
  pb <- panels_of(bottom)
  pr <- panels_of(comp)
  # ... a row under the cells (wider, shorter panels than a right column)
  expect_gt(max(pb$px1), max(pr$px1))
  expect_lt(max(pb$py1), max(pr$py1))
  # and "none" drops it
  none <- comp |> theme(legend.position = "none")
  expect_identical(n_text(svg_of(none), "factor(cyl)"), 0L)
})

test_that("legends inside a panel or beside each facet panel stay put (VP-152)", {
  pc <- vplot(mtcars) |> mark_point(x = wt, y = mpg, color = factor(cyl))
  fac <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, color = factor(gear)) |>
    facet_wrap(~am) |>
    resolve_scale(color = "independent")
  comp <- concat(fac, pc, ncol = 2)
  svg <- svg_of(comp)
  # the per-panel legends stay beside their panels, one per facet panel
  expect_identical(n_text(svg, "factor(gear)"), 2L)
  expect_identical(n_text(svg, "factor(cyl)"), 1L)
  col <- vellumplot:::.comp_collect_independent(
    comp,
    vellumplot:::.resolve_theme(vellumplot:::.theme_default())
  )
  expect_length(col$figguides, 1L)
  expect_null(col$comp@plots[[1]]@theme)
})

test_that("aligned compositions leave fixed-aspect cells unstretched (VP-154)", {
  sets <- list(A = 1:10, B = 5:15)
  v <- vvenn(sets)
  expect_false(vellumplot:::.comp_alignable(hconcat(v, v)))
  pan <- panels_of(hconcat(v, v, height = 3))
  w <- pan$px1 - pan$px0
  h <- pan$py1 - pan$py0
  expect_equal(w, h, tolerance = 0.01)
  # coord_fixed() and a theme aspect.ratio lock the aspect too
  a <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
  expect_true(vellumplot:::.comp_alignable(hconcat(a, a)))
  expect_false(vellumplot:::.comp_alignable(hconcat(a, a |> coord_fixed())))
  sq <- a |> theme(aspect.ratio = 1)
  expect_false(vellumplot:::.comp_alignable(hconcat(a, sq)))
  pan <- panels_of(hconcat(a, sq, height = 3))
  expect_equal(
    pan$px1[2] - pan$px0[2],
    pan$py1[2] - pan$py0[2],
    tolerance = 0.01
  )
})

test_that("a single-plot composition keeps its legend in its cell (VP-152)", {
  pc <- vplot(mtcars) |> mark_point(x = wt, y = mpg, color = factor(cyl))
  mini <- vplot(mtcars) |> mark_histogram(x = mpg, bins = 8)
  comp <- inset(pc, mini, left = 0.5, bottom = 0.5, right = 1, top = 1)
  col <- vellumplot:::.comp_collect_independent(
    comp,
    vellumplot:::.resolve_theme(vellumplot:::.theme_default())
  )
  expect_length(col$figguides, 0L)
  expect_identical(col$comp, comp)
})
