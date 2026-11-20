# VP-59: in an aligned composition each sub-plot's legends sit on the side
# their guide (or the sub-plot's theme) asks for, not always on its right.

cls_mt <- transform(mtcars, fcyl = factor(cyl), fam = factor(am))
cls_panels <- function(x) {
  vellum::scene_model(vellum::as_vellum_scene(x))$panels
}
cls_row <- function(pan, name) pan[pan$name == name, , drop = FALSE]

test_that("a sub-plot's guide positions are honoured when aligned", {
  p1 <- vplot(cls_mt) |>
    mark_point(x = wt, y = mpg, color = fcyl) |>
    guides(color = guide_legend(position = "bottom"))
  p2 <- vplot(cls_mt) |>
    mark_point(x = hp, y = qsec, shape = fam) |>
    guides(shape = guide_legend(position = "left"))
  comp <- wrap_plots(list(p1, p2), guides = "keep")
  expect_true(vellumplot:::.comp_alignable(comp))
  pan <- cls_panels(comp)
  c1 <- cls_row(pan, "cell-1-panel-1-1")
  c2 <- cls_row(pan, "cell-2-panel-1-1")
  bottom <- cls_row(pan, "legend-bottom")
  left <- cls_row(pan, "legend-left")
  expect_identical(nrow(bottom), 1L)
  expect_identical(nrow(left), 1L)
  # below the first panel, within its columns; left of the second panel
  expect_gt(bottom$y0, c1$y1)
  expect_gte(bottom$x0, c1$px0)
  expect_lte(bottom$x1, c1$px1)
  expect_lt(left$x1, c2$x0)
  expect_gt(left$x0, c1$x1)
  # panels still line up across the row
  expect_equal(c1$py0, c2$py0)
  expect_equal(c1$py1, c2$py1)
  # no legend went to a right band
  expect_false("legend" %in% pan$name)
})

test_that("a sub-plot's theme legend position is honoured when aligned", {
  top <- vplot(cls_mt) |>
    mark_point(x = wt, y = mpg, color = fcyl) |>
    theme(legend.position = "top")
  plain <- vplot(cls_mt) |> mark_point(x = hp, y = qsec)
  pan <- cls_panels(wrap_plots(list(top, plain), guides = "keep"))
  leg <- cls_row(pan, "legend")
  c1 <- cls_row(pan, "cell-1-panel-1-1")
  expect_identical(nrow(leg), 1L)
  expect_lt(leg$y1, c1$y0)
  bands <- vellumplot:::.plot_bands(vellumplot:::.plan_plot(top), FALSE)
  expect_identical(bands$legends, "top")
  expect_identical(vellum::vl_convert(bands$R_legend, "mm"), 0)
})

test_that("collecting pools legends from every side; inside stays put", {
  p1 <- vplot(cls_mt) |>
    mark_point(x = wt, y = mpg, color = fcyl) |>
    guides(color = guide_legend(position = "bottom"))
  p2 <- vplot(cls_mt) |>
    mark_point(x = hp, y = qsec, shape = fam, color = fcyl) |>
    guides(
      shape = guide_legend(position = "left"),
      color = guide_legend(position = "inside")
    )
  plans <- lapply(list(p1, p2), vellumplot:::.plan_plot)
  pooled <- vellumplot:::.pool_guides(plans)
  expect_setequal(
    vapply(pooled, function(g) g$kind, ""),
    c("color_discrete", "shape")
  )
  pan <- cls_panels(wrap_plots(list(p1, p2), guides = "collect"))
  expect_true("legend" %in% pan$name)
  expect_true("legend-inside" %in% pan$name)
  expect_false(any(c("legend-left", "legend-bottom") %in% pan$name))
})

test_that("a right-hand legend is drawn as before", {
  p <- vplot(cls_mt) |> mark_point(x = wt, y = mpg, color = fcyl)
  q <- vplot(cls_mt) |> mark_point(x = hp, y = qsec)
  pan <- cls_panels(wrap_plots(list(p, q), guides = "keep"))
  leg <- cls_row(pan, "legend")
  c1 <- cls_row(pan, "cell-1-panel-1-1")
  c2 <- cls_row(pan, "cell-2-panel-1-1")
  expect_gt(leg$x0, c1$x1)
  expect_lt(leg$x1, c2$x0)
})

# VP-62: the collected legend follows the composition theme's legend.position,
# as patchwork's does.
test_that("a collected legend honours the composition's legend.position", {
  p1 <- vplot(cls_mt) |> mark_point(x = wt, y = mpg, color = fcyl)
  p2 <- vplot(cls_mt) |> mark_point(x = hp, y = qsec, color = fcyl)
  at <- function(pos) {
    comp <- wrap_plots(list(p1, p2), guides = "collect")
    if (!is.null(pos)) {
      comp <- theme(comp, legend.position = pos)
    }
    pan <- cls_panels(comp)
    list(
      leg = cls_row(pan, "legend"),
      c1 = cls_row(pan, "cell-1-panel-1-1"),
      c2 = cls_row(pan, "cell-2-panel-1-1")
    )
  }
  right <- at(NULL)
  expect_identical(nrow(right$leg), 1L)
  expect_gt(right$leg$x0, right$c2$x1)
  left <- at("left")
  expect_lt(left$leg$x1, left$c1$x0)
  top <- at("top")
  expect_lt(top$leg$y1, top$c1$y0)
  expect_gte(top$leg$x0, top$c1$x0)
  expect_lte(top$leg$x1, top$c2$x1)
  bottom <- at("bottom")
  expect_gt(bottom$leg$y0, bottom$c1$y1)
  expect_gte(bottom$leg$x0, bottom$c1$x0)
  expect_lte(bottom$leg$x1, bottom$c2$x1)
  # panels still line up, and a top / bottom legend takes no column
  expect_equal(top$c1$py0, top$c2$py0)
  expect_gt(top$c2$x1, right$c2$x1)
  none <- at("none")
  expect_identical(nrow(none$leg), 0L)
  # the sub-plots' own themes do not move the collected legend (as patchwork)
  sub <- wrap_plots(
    list(p1 |> theme(legend.position = "bottom"), p2),
    guides = "collect"
  )
  pan <- cls_panels(sub)
  expect_gt(cls_row(pan, "legend")$x0, cls_row(pan, "cell-2-panel-1-1")$x1)
})
