# coord_polar(): bars -> wedges (pie / rose), a square aspect-locked panel with
# collapsed gutters, and concentric/radial gridlines + labels drawn in-panel.

lay_polar <- function(p) {
  built <- vellumplot:::.build_panels(p)
  rt <- vellumplot:::.resolve_theme(vellumplot:::.theme_of(p))
  vellumplot:::.build_layout(
    built,
    list(),
    p@labels,
    rt,
    FALSE,
    vellumplot:::.coord_of(p)
  )
}
trackval <- function(u, i) vctrs::field(u, "value")[i]

# ---- spec-level ------------------------------------------------------------

test_that("coord_polar sets the polar coord with sane defaults", {
  p <- vplot(mtcars) |> mark_bar(x = factor(cyl)) |> coord_polar()
  expect_identical(p@coord@kind, "polar")
  expect_identical(p@coord@theta, "x")
  expect_equal(p@coord@start, 0)
  expect_equal(p@coord@direction, 1)
})

test_that("coord_polar validates theta and direction", {
  p <- vplot(mtcars) |> mark_bar(x = factor(cyl))
  expect_error(coord_polar(p, theta = "z"))
  expect_error(coord_polar(p, direction = 2))
})

test_that("a polar layout is square (respect) with collapsed axis gutters", {
  p <- vplot(mtcars) |> mark_bar(x = factor(cyl)) |> coord_polar()
  l <- lay_polar(p)
  expect_true(l$respect)
  # the y-title and x-title tracks collapse to zero width/height
  expect_equal(trackval(l$widths, l$ytitle_col), 0)
  expect_equal(trackval(l$heights, l$xtitle_row), 0)
})

test_that("print shows the polar coord parameters", {
  p <- vplot(mtcars) |>
    mark_bar(x = factor(cyl)) |>
    coord_polar(theta = "x", direction = -1)
  # cli writes the spec tree to the message stream
  out <- paste(capture.output(summary(p), type = "message"), collapse = " ")
  expect_match(out, "polar")
  expect_match(out, "theta=x")
})

# ---- rendering -------------------------------------------------------------

test_that("a pie (stacked bar, theta = y) renders slices in the matching fills", {
  df <- data.frame(cat = c("a", "b"), n = c(1, 1))
  p <- vplot(df) |>
    mark_bar(x = factor(1), y = n, fill = cat, position = "stack") |>
    coord_polar(theta = "y")
  img <- render_px(p)
  # two equal slices -> the two default qualitative fills appear in ~equal area
  pal <- vellumplot:::.qual_palette(2)
  hex2rgb <- function(h) grDevices::col2rgb(h)[, 1] / 255
  a <- count_near(img, hex2rgb(pal[1]))
  b <- count_near(img, hex2rgb(pal[2]))
  expect_gt(a, 1000)
  expect_gt(b, 1000)
  expect_lt(abs(a - b) / max(a, b), 0.25)
})

test_that("a rose (categorical bar, theta = x) renders and is centred ink", {
  p <- vplot(mtcars) |> mark_bar(x = factor(cyl)) |> coord_polar(theta = "x")
  img <- render_px(p)
  # wedge fill (default grey35) near the panel centre; thresh above 0.35 so the
  # mid-grey counts as ink
  expect_true(has_ink(
    img,
    rows = c(0.45, 0.6),
    cols = c(0.35, 0.55),
    thresh = 0.5
  ))
})

test_that("polar gridlines draw circles (ink off the cardinal axes)", {
  # a faint major-grid circle should put non-background ink on the diagonal,
  # which a square/box grid would not.
  p <- vplot(mtcars) |> mark_bar(x = factor(cyl)) |> coord_polar(theta = "x")
  img <- render_px(p)
  expect_true(has_ink(
    img,
    rows = c(0.2, 0.45),
    cols = c(0.2, 0.4),
    thresh = 0.97
  ))
})

test_that("faceted coord_polar renders to a file", {
  f <- withr::local_tempfile(fileext = ".png")
  render_plot(
    vplot(mtcars) |>
      mark_bar(x = factor(cyl)) |>
      facet_wrap(~am) |>
      coord_polar(theta = "x"),
    f
  )
  expect_gt(file.info(f)$size, 0)
})

test_that("a mapped fill still produces a legend column in polar", {
  df <- data.frame(cat = c("a", "b", "c"), n = c(2, 3, 4))
  p <- vplot(df) |>
    mark_bar(x = factor(1), y = n, fill = cat, position = "stack") |>
    coord_polar(theta = "y")
  l <- lay_polar(p)
  built <- vellumplot:::.build_panels(p)
  guides <- vellumplot:::.legend_guides(built$scales)
  expect_true(length(guides) > 0)
})
