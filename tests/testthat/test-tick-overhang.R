# End tick labels that hang past the panel get room when the plot margin
# cannot hold them (VP-138).

mm <- function(u) vellum::vl_convert(u, "mm")

edge_plot <- function(margin) {
  vplot(mtcars, width = 4, height = 2.5) |>
    mark_point(x = wt, y = mpg) |>
    scale_x_continuous(limits = c(1, 6), breaks = 1:6, expand = c(0, 0)) |>
    scale_y_continuous(
      limits = c(10, 35),
      breaks = c(10, 20, 35),
      expand = c(0, 0)
    ) |>
    theme(plot.margin = margin, axis.title.y = element_blank())
}

lay_of <- function(p) {
  pl <- vellumplot:::.build_panels(p)
  rt <- vellumplot:::.resolve_theme(vellumplot:::.theme_of(p))
  vellumplot:::.build_layout(
    pl,
    list(),
    p@labels,
    rt,
    page_width = p@width,
    page_height = p@height
  )
}

test_that("a zero margin gets a spacer for the top and right end labels", {
  lay <- lay_of(edge_plot(0))
  rt <- vellumplot:::.resolve_theme(vellumplot:::.theme_default())
  half_h <- mm(vellum::grobheight(
    vellumplot:::.axis_text_grob(
      "35",
      rt[["axis.text.y"]],
      c("right", "centre")
    )
  )) /
    2
  half_w <- mm(vellum::grobwidth(
    vellumplot:::.axis_text_grob("6", rt[["axis.text.x"]], c("centre", "top"))
  )) /
    2
  # the panel moved down / in by one spacer track
  expect_identical(lay$panel_row, 2L)
  expect_equal(mm(lay$heights[1]), half_h, tolerance = 1e-6)
  n <- length(lay$widths)
  expect_equal(mm(lay$widths[n]), half_w, tolerance = 1e-6)
  # every other index followed the shift
  expect_identical(lay$xlabels_row, 3L)
  expect_identical(lay$nrow_total, length(lay$heights))
  expect_no_error(vellum::as_vellum_scene(edge_plot(0)))
})

test_that("the default margin absorbs the overhang: no spacer", {
  lay <- lay_of(edge_plot(5.5))
  plain <- lay_of(
    vplot(mtcars, width = 4, height = 2.5) |>
      mark_point(x = wt, y = mpg) |>
      theme(axis.title.y = element_blank())
  )
  expect_identical(lay$panel_row, plain$panel_row)
  expect_identical(length(lay$widths), length(plain$widths))
  expect_identical(length(lay$heights), length(plain$heights))
})

test_that("an end break inside the panel needs less room", {
  p <- vplot(mtcars, width = 4, height = 2.5) |>
    mark_point(x = wt, y = mpg) |>
    theme(plot.margin = 0, axis.title.y = element_blank())
  lay <- lay_of(p)
  # default expansion keeps the top break well clear of the edge
  expect_identical(lay$panel_row, 1L)
})

test_that(".shift_tracks() leaves NA and unused zero indices alone", {
  lay <- list(
    panel_row = c(2L, 4L),
    wrapstrip_row = c(0L, 0L),
    tag_row = NA_integer_,
    panel_col = 3L,
    legend_cols = c(left = NA_integer_, right = 5L)
  )
  out <- vellumplot:::.shift_tracks(lay, 1L, 1L)
  expect_identical(out$panel_row, c(3L, 5L))
  expect_identical(out$wrapstrip_row, c(0L, 0L))
  expect_identical(out$tag_row, NA_integer_)
  expect_identical(out$panel_col, 4L)
  expect_identical(out$legend_cols, c(left = NA_integer_, right = 6L))
})
