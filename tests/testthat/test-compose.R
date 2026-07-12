# Patchwork-style composition: alignment, guide collection, annotation/tagging,
# layout control, nesting, design/spanning, spacers and insets.

p_xy <- function(x = wt, y = mpg, ...) {
  vplot(mtcars) |> mark_point(x = {{ x }}, y = {{ y }}, ...)
}

test_that("a simple grid of single-panel plots is alignable", {
  a <- p_xy(wt, mpg)
  b <- p_xy(hp, mpg)
  expect_true(vellumplot:::.comp_alignable(hconcat(a, b)))
  # faceted / polar / nested fall back to the independent path
  fac <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    facet_wrap(~cyl)
  expect_false(vellumplot:::.comp_alignable(hconcat(a, fac)))
  expect_false(vellumplot:::.comp_alignable(hconcat(a, vconcat(a, b))))
})

test_that("panels sit on shared null tracks (so they align)", {
  # wide vs narrow y labels must not break alignment
  big <- transform(mtcars, mpg = mpg * 1e5)
  a <- vplot(big) |> mark_point(x = wt, y = mpg)
  b <- p_xy(hp, drat)
  comp <- hconcat(a, b)
  plans <- lapply(comp@plots, vellumplot:::.plan_plot)
  rt0 <- vellumplot:::.resolve_theme(vellumplot:::.theme_default())
  glo <- vellumplot:::.composition_grid(plans, comp, TRUE, rt0)
  rows <- vapply(glo$map, function(m) m$panel_row, integer(1))
  expect_equal(length(unique(rows)), 1L) # both panels in one row
  cols <- vapply(glo$map, function(m) m$panel_col, integer(1))
  expect_equal(length(unique(cols)), 2L) # distinct columns
})

test_that("identical legends collapse to one; keep leaves them per-plot", {
  a <- p_xy(wt, mpg, color = cyl)
  b <- p_xy(hp, mpg, color = cyl)
  rt0 <- vellumplot:::.resolve_theme(vellumplot:::.theme_default())
  gc <- vellumplot:::.composition_grid(
    lapply(list(a, b), vellumplot:::.plan_plot),
    hconcat(a, b),
    TRUE,
    rt0
  )
  expect_length(gc$figguides, 1L)
  expect_false(is.na(gc$figlegend_col))

  gk <- vellumplot:::.composition_grid(
    lapply(list(a, b), vellumplot:::.plan_plot),
    hconcat(a, b, guides = "keep"),
    FALSE,
    rt0
  )
  expect_length(gk$figguides, 0L)
})

test_that("distinct legends are not collapsed", {
  a <- p_xy(wt, mpg, color = cyl)
  b <- p_xy(hp, mpg, color = gear)
  rt0 <- vellumplot:::.resolve_theme(vellumplot:::.theme_default())
  g <- vellumplot:::.composition_grid(
    lapply(list(a, b), vellumplot:::.plan_plot),
    hconcat(a, b),
    TRUE,
    rt0
  )
  expect_length(g$figguides, 2L)
})

test_that("compose_annotation sets figure labels and tag spec", {
  a <- p_xy()
  b <- p_xy(hp, mpg)
  comp <- hconcat(a, b) |>
    compose_annotation(
      title = "T",
      subtitle = "S",
      caption = "C",
      tag_levels = "A"
    )
  expect_equal(comp@labels$title, "T")
  expect_equal(comp@labels$caption, "C")
  expect_equal(comp@tag$levels, "A")
  expect_error(compose_annotation(a, title = "x"), "PlotComposition")
})

test_that("auto-tag formatting covers the styles", {
  fmt <- function(n, lvl, ...) {
    vellumplot:::.format_tags(n, list(levels = lvl, ...))
  }
  expect_equal(fmt(3, "A"), c("A", "B", "C"))
  expect_equal(fmt(3, "a"), c("a", "b", "c"))
  expect_equal(fmt(3, "1"), c("1", "2", "3"))
  expect_equal(fmt(2, "i"), c("i", "ii"))
  expect_equal(fmt(2, "A", prefix = "(", suffix = ")"), c("(A)", "(B)"))
})

test_that("alphabetic auto-tags continue past 26 sub-plots (H28)", {
  fmt <- function(n, lvl) vellumplot:::.format_tags(n, list(levels = lvl))
  up <- fmt(28, "A")
  expect_identical(up[26:28], c("Z", "AA", "AB")) # no NA past 26
  expect_false(anyNA(up))
  lo <- fmt(27, "a")
  expect_identical(lo[26:27], c("z", "aa"))
})

test_that("widths/heights and byrow are stored and recycled", {
  a <- p_xy()
  comp <- concat(a, a, a, a, ncol = 2, widths = c(2, 1), byrow = FALSE)
  expect_equal(comp@widths, c(2, 1))
  expect_false(comp@byrow)
  expect_equal(vellumplot:::.size_weights(c(2, 1), 2), c(2, 1))
  expect_equal(vellumplot:::.size_weights(NULL, 3), c(1, 1, 1))
})

test_that("byrow controls cell order", {
  comp <- concat(p_xy(), p_xy(), p_xy(), p_xy(), ncol = 2, byrow = FALSE)
  # plot 2 should be row 2 col 1 when filling by column
  expect_equal(vellumplot:::.comp_cell(2, comp), list(r = 2L, c = 1L))
  comp2 <- concat(p_xy(), p_xy(), p_xy(), p_xy(), ncol = 2, byrow = TRUE)
  expect_equal(vellumplot:::.comp_cell(2, comp2), list(r = 1L, c = 2L))
})

test_that("design strings and area() lists parse to spanning areas", {
  d <- vellumplot:::.parse_design("AA\nBC", 3)
  expect_length(d, 3L)
  expect_equal(d[[1]], area(1, 1, 1, 2)) # A spans top row
  expect_equal(d[[2]], area(2, 1, 2, 1)) # B bottom-left
  expect_equal(d[[3]], area(2, 2, 2, 2)) # C bottom-right
  comp <- concat(p_xy(), p_xy(), p_xy(), design = "AA\nBC")
  expect_equal(c(comp@nrow, comp@ncol), c(2, 2))
})

test_that("malformed design geometry is rejected", {
  # area() requires t <= b and l <= r
  expect_error(area(2, 1, 1, 1), "t <= b")
  expect_error(area(1, 3, 1, 2), "l <= r")
  # a non-rectangular (L-shaped) letter can't be a spanning area
  expect_error(vellumplot:::.parse_design("AA\nA#", 1), "solid rectangle")
  # a valid solid rectangle still parses
  expect_length(vellumplot:::.parse_design("AA\nAA", 1), 1L)
})

test_that("plot_spacer and inset build the right objects", {
  expect_true(S7::S7_inherits(plot_spacer(), vellumplot:::Spacer))
  a <- p_xy()
  b <- vplot(mtcars) |> mark_histogram(x = mpg, bins = 8)
  ins <- inset(a, b, left = 0.5, bottom = 0.5)
  expect_true(S7::S7_inherits(ins, vellumplot:::PlotComposition))
  expect_length(ins@insets, 1L)
  expect_equal(ins@insets[[1]]$left, 0.5)
  expect_error(inset(a, b), NA) # default inset position is fine
})

test_that("theme() sets a composition's figure-level theme (H57)", {
  a <- p_xy(wt, mpg)
  b <- p_xy(hp, mpg)
  base <- hconcat(a, b) |> compose_annotation(title = "Fig")
  themed <- base |>
    theme(plot.title = element_text(size = 28, colour = "red"))
  # stored as the composition's own theme; the default (base) is untouched
  expect_false(is.null(themed@theme))
  expect_null(base@theme)
  # and the figure title band actually renders differently
  raster <- function(x) vellum::scene_raster(as_vellum_scene(x))
  expect_false(identical(raster(base), raster(themed)))
})

test_that("the full feature set renders without error", {
  a <- p_xy(wt, mpg, color = cyl)
  b <- p_xy(hp, mpg, color = cyl)
  c <- vplot(mtcars) |> mark_histogram(x = mpg, bins = 8)
  d <- p_xy(drat, qsec)
  render_ok <- function(x) {
    f <- local_tempfile(fileext = ".png")
    render_plot(x, f)
    expect_gt(file.info(f)$size, 0)
  }
  render_ok(hconcat(a, b)) # collected legend + alignment
  render_ok(
    concat(a, b, c, d, ncol = 2) |>
      compose_annotation(title = "fig", tag_levels = "A")
  )
  render_ok(concat(a, b, c, design = "AA\nBC")) # spanning
  render_ok(concat(a, plot_spacer(), plot_spacer(), b, ncol = 2)) # spacers
  render_ok(inset(a, c)) # inset
  render_ok(hconcat(a, vconcat(b, d))) # nesting (independent fallback)
  render_ok(wrap_plots(list(a, b, d), ncol = 3))
})

test_that("non-plot inputs are rejected with a clear error", {
  expect_error(concat(mtcars), "PlotSpec")
  expect_error(concat(p_xy(), 42), "Argument 2")
})
