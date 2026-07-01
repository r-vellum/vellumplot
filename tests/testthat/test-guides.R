# Generalized guide list: colour, size, shape.

train <- function(p) {
  quill:::.train_scales(p, quill:::.resolve_layers(p))
}
kinds <- function(p) {
  vapply(
    quill:::.legend_guides(train(p)),
    function(g) g$kind,
    character(1)
  )
}

test_that("guides are built per mapped channel in colour/size/shape order", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, color = hp, size = disp, shape = factor(cyl))
  expect_identical(kinds(p), c("color_continuous", "size", "shape"))
})

test_that("only mapped channels produce guides", {
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg, shape = factor(cyl))
  expect_identical(kinds(p), "shape")
  p0 <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
  expect_length(kinds(p0), 0)
})

test_that("a plot with three guides renders", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, color = hp, size = disp, shape = factor(cyl))
  f <- withr::local_tempfile(fileext = ".png")
  render_plot(p, f)
  expect_gt(file.info(f)$size, 0)
})

test_that(".legend_width handles a shape guide without error", {
  built <- quill:::.build_panels(
    vplot(mtcars) |>
      mark_point(x = wt, y = mpg, color = hp, shape = factor(carb))
  )
  rt <- quill:::.resolve_theme(quill:::.theme_default())
  guides <- quill:::.legend_guides(built$scales)
  expect_length(guides, 2)
  expect_silent(quill:::.legend_width(guides, rt))
})

test_that("legend.position = none drops all guides", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, color = hp, shape = factor(cyl)) |>
    theme(legend.position = "none")
  built <- quill:::.build_panels(p)
  lay <- quill:::.build_layout(
    built,
    quill:::.legend_guides(built$scales),
    p@labels,
    quill:::.resolve_theme(p@theme)
  )
  expect_true(is.na(lay$legend_col))
  expect_true(is.na(lay$legend_row))
})

# Layout placement of the legend for each side.
layout_for <- function(pos) {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, color = factor(cyl), size = disp) |>
    theme(legend.position = pos)
  built <- quill:::.build_panels(p)
  quill:::.build_layout(
    built,
    quill:::.legend_guides(built$scales),
    p@labels,
    quill:::.resolve_theme(p@theme)
  )
}

test_that("right/left take a legend column, not a row", {
  for (pos in c("right", "left")) {
    lay <- layout_for(pos)
    expect_false(is.na(lay$legend_col))
    expect_true(is.na(lay$legend_row))
  }
  # left places the legend before the y-title column; right after the panels.
  expect_lt(layout_for("left")$legend_col, layout_for("left")$ytitle_col)
  expect_gt(layout_for("right")$legend_col, layout_for("right")$panel_col[1])
})

test_that("top/bottom take a legend row, not a column", {
  for (pos in c("top", "bottom")) {
    lay <- layout_for(pos)
    expect_true(is.na(lay$legend_col))
    expect_false(is.na(lay$legend_row))
  }
  expect_lt(layout_for("top")$legend_row, layout_for("top")$panel_row[1])
  expect_gt(layout_for("bottom")$legend_row, layout_for("bottom")$panel_row[1])
})

test_that("each legend position renders", {
  for (pos in c("right", "left", "top", "bottom")) {
    p <- vplot(mtcars) |>
      mark_point(x = wt, y = mpg, color = hp, size = disp) |>
      theme(legend.position = pos)
    f <- withr::local_tempfile(fileext = ".png")
    render_plot(p, f)
    expect_gt(file.info(f)$size, 0)
  }
})

test_that("legend.position rejects unknown values", {
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
  expect_error(theme(p, legend.position = "middle"), "legend.position")
})
