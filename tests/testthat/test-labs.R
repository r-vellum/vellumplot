# ---- spec storage ----------------------------------------------------------

test_that("labs() stores plot-level labels", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    labs(title = "T", subtitle = "S", caption = "C", tag = "A")
  expect_identical(p@labels$title, "T")
  expect_identical(p@labels$subtitle, "S")
  expect_identical(p@labels$caption, "C")
  expect_identical(p@labels$tag, "A")
})

test_that("repeated labs() calls merge, last value winning", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    labs(title = "a") |>
    labs(subtitle = "b") |>
    labs(title = "c")
  expect_identical(p@labels$title, "c")
  expect_identical(p@labels$subtitle, "b")
})

test_that("colour/color/fill all fold into the color key", {
  p1 <- vplot(mtcars) |> mark_point(x = wt, y = mpg) |> labs(colour = "X")
  p2 <- vplot(mtcars) |> mark_point(x = wt, y = mpg) |> labs(fill = "X")
  expect_identical(p1@labels$color, "X")
  expect_identical(p2@labels$color, "X")
  expect_null(p1@labels$colour)
  expect_null(p1@labels$fill)
})

test_that("labs() validates its inputs", {
  expect_error(labs(1, title = "x"), "PlotSpec")
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
  expect_error(labs(p, foo = "x"))
})

# ---- title-override wiring & precedence -------------------------------------

test_that("labs() axis/legend overrides reach the trained scales", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, color = hp) |>
    labs(x = "Weight", y = "Miles/gal", color = "Power")
  sc <- vellumplot:::.train_scales(p, vellumplot:::.resolve_layers(p))
  expect_identical(sc$x$name, "Weight")
  expect_identical(sc$y$name, "Miles/gal")
  expect_identical(sc$color$name, "Power")
})

test_that("labs(fill =) titles the colour scale", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, color = hp) |>
    labs(fill = "Power")
  sc <- vellumplot:::.train_scales(p, vellumplot:::.resolve_layers(p))
  expect_identical(sc$color$name, "Power")
})

test_that("scale_*(name =) takes precedence over labs()", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    scale_x_continuous(name = "FromScale") |>
    labs(x = "FromLabs")
  sc <- vellumplot:::.train_scales(p, vellumplot:::.resolve_layers(p))
  expect_identical(sc$x$name, "FromScale")
})

test_that("labs(y =) overrides the bar 'count' default title", {
  base <- vplot(mtcars) |> mark_bar(x = factor(cyl))
  sc0 <- vellumplot:::.train_scales(base, vellumplot:::.resolve_layers(base))
  expect_identical(sc0$y$name, "count")

  withlab <- base |> labs(y = "Frequency")
  sc1 <- vellumplot:::.train_scales(
    withlab,
    vellumplot:::.resolve_layers(withlab)
  )
  expect_identical(sc1$y$name, "Frequency")
})

# ---- layout bands ----------------------------------------------------------

test_that("absent labels add no band tracks", {
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
  lay <- vellumplot:::.build_layout(
    vellumplot:::.build_panels(p),
    list(),
    p@labels
  )
  expect_true(is.na(lay$title_row))
  expect_true(is.na(lay$subtitle_row))
  expect_true(is.na(lay$caption_row))
  expect_true(is.na(lay$tag_row))
  expect_identical(lay$ncol_total, length(lay$widths))
})

test_that("present labels add bands that span the full width", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    labs(title = "T", caption = "C")
  lay <- vellumplot:::.build_layout(
    vellumplot:::.build_panels(p),
    list(),
    p@labels
  )
  expect_false(is.na(lay$title_row))
  expect_false(is.na(lay$caption_row))
  expect_true(is.na(lay$subtitle_row))
  # the title band sits above every panel row; caption below
  expect_lt(lay$title_row, min(lay$panel_row))
  expect_gt(lay$caption_row, max(lay$panel_row))
})

test_that("a tag sits in its own row above the title", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    labs(title = "T", tag = "A")
  lay <- vellumplot:::.build_layout(
    vellumplot:::.build_panels(p),
    list(),
    p@labels
  )
  expect_false(is.na(lay$tag_row))
  expect_lt(lay$tag_row, lay$title_row)
  expect_lt(lay$title_row, min(lay$panel_row))
})

# ---- rendering -------------------------------------------------------------

test_that("a title renders ink in the top band", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    labs(title = "A Title")
  img <- render_px(p)
  expect_true(has_ink(img, rows = c(0, 0.08), cols = c(0, 0.6)))
})

test_that("a caption renders ink in the bottom band", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    labs(caption = "a caption")
  img <- render_px(p)
  # caption sits in the bottom band, just above the plot.margin
  expect_true(has_ink(img, rows = c(0.85, 0.92), cols = c(0.4, 1)))
})

test_that("faceted plots with labs render", {
  f <- local_tempfile(fileext = ".png")
  render_plot(
    vplot(mtcars) |>
      mark_point(x = wt, y = mpg, color = hp) |>
      facet_wrap(~cyl) |>
      labs(title = "T", x = "Weight", color = "Power"),
    f
  )
  expect_gt(file.info(f)$size, 0)
})

test_that("concat carries per-plot labs", {
  a <- vplot(mtcars) |> mark_point(x = wt, y = mpg) |> labs(title = "A")
  b <- vplot(mtcars) |> mark_histogram(x = mpg, bins = 8) |> labs(title = "B")
  f <- local_tempfile(fileext = ".png")
  render_plot(hconcat(a, b), f)
  expect_gt(file.info(f)$size, 0)
})

# ---- box text: long title/subtitle/caption wrap to the page width ----------

# First row (top-down fraction) carrying any near-black ink across the width --
# the top of the drawn content (axis labels, ticks, points). A band that wraps
# to more lines reserves more height and pushes this down.
first_ink_row <- function(img, thresh = 0.3) {
  ink <- apply(img[,, 1:3, drop = FALSE] <= thresh, c(1, 2), all)
  which(apply(ink, 1, any))[1] / dim(img)[1]
}

test_that("a long subtitle wraps and reserves height (pushes content down)", {
  long <- paste(rep("wrap", 60), collapse = " ")
  base <- vplot(mtcars, width = 6, height = 4) |> mark_point(x = wt, y = mpg)
  short_top <- first_ink_row(render_px(base |> labs(subtitle = "short")))
  long_top <- first_ink_row(render_px(base |> labs(subtitle = long)))
  # the wrapped (multi-line) subtitle reserves noticeably more vertical space
  expect_gt(long_top, short_top + 0.05)
})

test_that("a long caption wraps upward, reserving height below the panel", {
  long <- paste(rep("wrap", 60), collapse = " ")
  base <- vplot(mtcars, width = 6, height = 4) |> mark_point(x = wt, y = mpg)
  # a long caption itself inks low, so measure the panel's grey floor: its
  # bottom edge rises when the caption band grows. Detect it as the last
  # *mostly-grey* row (the full-width panel band) rather than sampling one
  # column -- sparse caption text overlapping a single column varies with the
  # rendering font and gave a false floor low in the image.
  panel_floor <- function(sub) {
    img <- render_px(base |> labs(caption = sub))
    grey <- abs(img[,, 1] - img[,, 2]) < 0.03 &
      img[,, 1] > 0.85 &
      img[,, 1] < 0.97
    rows <- which(rowMeans(grey) > 0.5)
    rows[length(rows)] / dim(img)[1]
  }
  expect_lt(panel_floor(long), panel_floor("short") - 0.03)
})
