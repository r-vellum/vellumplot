# vsparkline(): compact, axis-free word-sized charts.

test_that("vsparkline() returns a chrome-free single-layer PlotSpec", {
  p <- vsparkline(1:10)
  expect_s3_class(p, "vellumplot::PlotSpec")
  expect_length(p@layers, 1L)
  expect_identical(p@layers[[1]]@mark, "sparkline")
  expect_identical(p@coord@kind, "cartesian")
  # sized in inches from the mm default (20 x 6 mm)
  expect_equal(p@width, 20 / 25.4)
  expect_equal(p@height, 6 / 25.4)
})

test_that("the sparkline type and options ride on stat_params", {
  sp <- vsparkline(1:5, type = "winloss", win_color = "navy")@layers[[
    1
  ]]@stat_params
  expect_identical(sp$type, "winloss")
  expect_identical(sp$win_color, "navy")
})

test_that("vsparkline() validates its inputs", {
  expect_error(vsparkline(1), "length >= 2")
  expect_error(vsparkline(c(NA, NA)), "length >= 2")
  expect_error(vsparkline(1:5, type = "nope"), "should be one of")
  expect_error(vsparkline(1:5, units = "furlong"), "units")
})

test_that(".spark_inches converts units", {
  expect_equal(vellumplot:::.spark_inches(25.4, "mm"), 1)
  expect_equal(vellumplot:::.spark_inches(2.54, "cm"), 1)
  expect_equal(vellumplot:::.spark_inches(72, "pt"), 1)
  expect_equal(vellumplot:::.spark_inches(3, "in"), 3)
})

test_that("all sparkline shapes and point modes render", {
  set.seed(1)
  plots <- list(
    vsparkline(cumsum(rnorm(20))),
    vsparkline(cumsum(rnorm(20)), points = "last"),
    vsparkline(cumsum(rnorm(20)), points = "none"),
    vsparkline(rpois(15, 5), type = "bar", color = "steelblue"),
    vsparkline(sample(c(-1, 1), 20, replace = TRUE), type = "winloss")
  )
  for (p in plots) {
    expect_no_error(vellum::as_vellum_scene(p))
    f <- local_tempfile(fileext = ".png")
    expect_no_error(render_plot(p, f))
    expect_gt(file.info(f)$size, 0)
  }
})

test_that("a sparkline panel is drawn unclipped (boundary dots survive)", {
  # the max/min dots sit on the domain edge; with clipping they'd be sliced. Render
  # a case whose max is a lone peak and assert the firebrick dot is fully present
  # near the top of the box (a clipped dot would lose its top half / pixels).
  skip_if_not_installed("png")
  p <- vsparkline(
    c(1, 1, 1, 9, 1, 1),
    point_size = 3,
    point_color = "red",
    width = 40,
    height = 20
  )
  f <- local_tempfile(fileext = ".png")
  render_plot(p, f, dpi = 150)
  img <- png::readPNG(f)
  # red channel high, green/blue low = the firebrick dot
  red <- img[,, 1] > 0.6 & img[,, 2] < 0.3 & img[,, 3] < 0.3
  expect_gt(sum(red), 20L) # a solid dot's worth of red pixels present
  # and some of it is in the top 15% of rows (the peak dot, undipped)
  top <- red[seq_len(ceiling(0.15 * nrow(red))), ]
  expect_gt(sum(top), 0L)
})
