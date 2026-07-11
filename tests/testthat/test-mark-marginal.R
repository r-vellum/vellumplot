# Marginal plots: add_marginal() (density / histogram along the top and right
# edges of a single panel, sharing its scales; the ggMarginal analogue).

test_that("add_marginal() records a MarginalSpec with the requested options", {
  p <- vplot(faithful) |>
    mark_point(x = eruptions, y = waiting) |>
    add_marginal(
      type = "histogram",
      sides = "t",
      size = 0.2,
      bins = 15,
      group = TRUE
    )
  ms <- p@marginal
  expect_true(S7::S7_inherits(ms, vellumplot:::MarginalSpec))
  expect_identical(ms@type, "histogram")
  expect_identical(ms@sides, "t")
  expect_equal(ms@size, 0.2)
  expect_identical(ms@bins, 15L)
  expect_true(ms@group)
})

test_that("add_marginal() defaults to a top+right density", {
  ms <- (vplot(faithful) |>
    mark_point(x = eruptions, y = waiting) |>
    add_marginal())@marginal
  expect_identical(ms@type, "density")
  expect_identical(ms@sides, "tr")
  expect_equal(ms@size, 0.15)
  expect_false(ms@group)
})

test_that("add_marginal() validates its arguments", {
  base <- vplot(faithful) |> mark_point(x = eruptions, y = waiting)
  expect_error(add_marginal(base, sides = "x"), "top.*right|only")
  expect_error(add_marginal(base, sides = ""), "sides")
  expect_error(add_marginal(base, size = 0), "fraction")
  expect_error(add_marginal(base, size = 1.5), "fraction")
  expect_error(add_marginal(base, type = "violin"), "should be one of")
  expect_error(add_marginal(mtcars), "PlotSpec")
})

test_that("add_marginal() errors on unsupported layouts (facet / coord / aspect)", {
  f <- withr::local_tempfile(fileext = ".png")
  base <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
  expect_error(
    render_plot(base |> facet_wrap(~cyl) |> add_marginal(), f),
    "single panel"
  )
  expect_error(
    render_plot(base |> coord_flip() |> add_marginal(), f),
    "Cartesian"
  )
  expect_error(
    render_plot(base |> coord_polar() |> add_marginal(), f),
    "Cartesian"
  )
  expect_error(
    render_plot(base |> coord_fixed() |> add_marginal(), f),
    "Cartesian"
  )
  expect_error(
    render_plot(base |> theme(aspect.ratio = 1) |> add_marginal(), f),
    "aspect"
  )
})

test_that("add_marginal() errors when it has no numeric x/y layer to read from", {
  f <- withr::local_tempfile(fileext = ".png")
  expect_error(
    render_plot(vplot(mtcars) |> mark_histogram(x = mpg) |> add_marginal(), f),
    "numeric"
  )
})

test_that("group = TRUE requires a discrete colour mapping", {
  f <- withr::local_tempfile(fileext = ".png")
  # continuous colour -> error
  expect_error(
    render_plot(
      vplot(mtcars) |>
        mark_point(x = wt, y = mpg, color = hp) |>
        add_marginal(group = TRUE),
      f
    ),
    "discrete"
  )
  # discrete colour -> fine
  expect_no_error(
    render_plot(
      vplot(iris) |>
        mark_point(x = Sepal.Length, y = Sepal.Width, color = Species) |>
        add_marginal(group = TRUE),
      f
    )
  )
})

test_that(".build_layout adds marginal tracks only for the requested sides", {
  spec <- vplot(faithful) |> mark_point(x = eruptions, y = waiting)
  built <- vellumplot:::.build_panels(spec)
  rt <- vellumplot:::.resolve_theme(vellumplot:::.theme_default())
  co <- vellumplot:::.coord_of(spec)
  base <- vellumplot:::.build_layout(built, list(), list(), rt, FALSE, co, NULL)
  MS <- vellumplot:::MarginalSpec
  lay_tr <- vellumplot:::.build_layout(
    built,
    list(),
    list(),
    rt,
    FALSE,
    co,
    MS(type = "density", sides = "tr")
  )
  lay_t <- vellumplot:::.build_layout(
    built,
    list(),
    list(),
    rt,
    FALSE,
    co,
    MS(type = "density", sides = "t")
  )
  lay_r <- vellumplot:::.build_layout(
    built,
    list(),
    list(),
    rt,
    FALSE,
    co,
    MS(type = "density", sides = "r")
  )

  # NULL marginal leaves the single-panel layout untouched.
  expect_true(is.na(base$marg_top_row))
  expect_true(is.na(base$marg_right_col))

  # A top marginal adds two rows (the marginal + its gap); a right marginal adds
  # two columns.
  expect_false(is.na(lay_tr$marg_top_row))
  expect_false(is.na(lay_tr$marg_right_col))
  expect_equal(lay_tr$nrow_total, base$nrow_total + 2L)
  expect_equal(lay_tr$ncol_total, base$ncol_total + 2L)

  expect_false(is.na(lay_t$marg_top_row))
  expect_true(is.na(lay_t$marg_right_col))
  expect_equal(lay_t$nrow_total, base$nrow_total + 2L)
  expect_equal(lay_t$ncol_total, base$ncol_total)

  expect_true(is.na(lay_r$marg_top_row))
  expect_false(is.na(lay_r$marg_right_col))
  expect_equal(lay_r$ncol_total, base$ncol_total + 2L)
  expect_equal(lay_r$nrow_total, base$nrow_total)

  # The panel still lives above its marginal-top row and left of the marginal-
  # right column.
  expect_lt(lay_tr$marg_top_row, lay_tr$panel_row[1])
  expect_gt(lay_tr$marg_right_col, lay_tr$panel_col[1])
})

test_that(".marg_layer reuses the density / bin stats (identical to the marks)", {
  MS <- vellumplot:::MarginalSpec
  x <- faithful$eruptions

  # density
  md <- vellumplot:::.marg_layer(list(x = x), MS(type = "density", adjust = 1))
  expect_equal(md$L$values$y, stats::density(x, adjust = 1)$y)
  expect_equal(md$maxd, max(stats::density(x, adjust = 1)$y))

  # histogram: compare to mark_histogram's own resolved stat
  df <- data.frame(v = x)
  hl <- vellumplot:::.resolve_layer(
    (vplot(df) |> mark_histogram(x = v, bins = 20))@layers[[1]],
    df
  )
  hs <- vellumplot:::.apply_stat(hl)
  mh <- vellumplot:::.marg_layer(
    list(x = x),
    MS(type = "histogram", bins = 20L)
  )
  expect_equal(mh$L$values$y, hs$values$y)
  expect_equal(mh$L$values$x, hs$values$x)
})

test_that("marginal renders draw the density fill in the top and right margins", {
  img <- render_px(
    vplot(faithful) |>
      mark_point(x = eruptions, y = waiting) |>
      add_marginal(size = 0.2)
  )
  # The density areas fill in grey35 (~0.349). Count that fill per region.
  grey35 <- function(rows, cols) count_ink_grey(img, rows, cols, rgb = 0.349)
  # Top strip (above the panel) and right strip (beside the panel) carry the fill.
  expect_gt(grey35(c(0, 0.2), c(0.1, 0.9)), 1000) # top density
  expect_gt(grey35(c(0.2, 0.8), c(0.88, 1.0)), 500) # right density (rotated)
  # The top-right corner has no marginal, so no fill there.
  expect_lt(grey35(c(0, 0.15), c(0.88, 1.0)), 50)
})

test_that("marginal render smoke: density / histogram / single sides / grouped", {
  f <- withr::local_tempfile(fileext = ".png")
  base <- vplot(faithful) |> mark_point(x = eruptions, y = waiting)
  expect_no_error(render_plot(base |> add_marginal(), f))
  expect_no_error(render_plot(base |> add_marginal(type = "histogram"), f))
  expect_no_error(render_plot(base |> add_marginal(sides = "t"), f))
  expect_no_error(render_plot(base |> add_marginal(sides = "r"), f))
  expect_no_error(render_plot(
    vplot(iris) |>
      mark_point(x = Sepal.Length, y = Sepal.Width, color = Species) |>
      add_marginal(type = "histogram", group = TRUE),
    f
  ))
})
