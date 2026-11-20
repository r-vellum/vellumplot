# guide_bins() (VP-45): binned legends with the bin boundaries on an axis.

# The legend text labels drawn (in order), from the SVG (axis labels blanked).
legend_texts <- function(p) {
  p <- p |>
    theme(axis.text.x = element_blank(), axis.text.y = element_blank())
  s <- plot_svg(p)
  m <- regmatches(s, gregexpr("<text[^>]*>[^<]*</text>", s))[[1]]
  m <- m[grepl('font-size="12"', m, fixed = TRUE)]
  sub("^<text[^>]*>([^<]*)</text>$", "\\1", m)
}

sized <- function() {
  vplot(mtcars) |>
    mark_point(x = wt, y = mpg, size = hp) |>
    scale_size_binned()
}

test_that("guide_bins() validates and records its options", {
  g <- guide_bins(show.limits = TRUE, reverse = TRUE, title = "HP")
  expect_identical(g$kind, "bins")
  expect_true(g$show.limits)
  expect_true(g$reverse)
  expect_true(g$axis)
  expect_error(guide_bins(show.limits = NA), "show.limits")
  expect_error(guide_bins(axis = "yes"), "axis")
})

test_that("a binned size scale under guide_bins() keys its bins", {
  p <- sized() |> guides(size = guide_bins(title = "Power"))
  sc <- vellumplot:::.build_panels(p)$scales$size
  expect_true(sc$bins)
  expect_false(sc$bins_limits)
  expect_identical(sc$name, "Power")
  k <- length(sc$legend_sizes)
  expect_length(sc$bin_breaks, k + 1L)
  expect_false(is.unsorted(sc$bin_breaks))
  # the drawn labels are the inner boundaries, not interval strings
  labs <- legend_texts(p)
  inner <- vellumplot:::.steps_break_labels(list(breaks = sc$bin_breaks))
  inner <- inner[-c(1L, k + 1L)]
  expect_identical(labs, inner)
  expect_false(any(grepl("[", labs, fixed = TRUE)))
  # never merged into another legend
  gl <- vellumplot:::.legend_guides(vellumplot:::.build_panels(p)$scales)
  expect_length(gl, 1L)
  expect_identical(gl[[1]]$kind, "size")
})

test_that("show.limits labels the outer limits; reverse flips the keys", {
  p <- sized() |> guides(size = guide_bins(show.limits = TRUE))
  sc <- vellumplot:::.build_panels(p)$scales$size
  labs <- legend_texts(p)
  expect_length(labs, length(sc$bin_breaks))
  r <- sized() |> guides(size = guide_bins(reverse = TRUE))
  sr <- vellumplot:::.build_panels(r)$scales$size
  expect_identical(sr$bin_breaks, rev(sc$bin_breaks))
  expect_identical(sr$legend_sizes, rev(sc$legend_sizes))
  expect_identical(
    legend_texts(r),
    rev(legend_texts(sized() |> guides(size = guide_bins())))
  )
})

test_that("binned colour scales take guide_bins() too", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, color = qsec) |>
    scale_color_binned(n = 4) |>
    guides(color = guide_bins(show.limits = TRUE))
  sc <- vellumplot:::.build_panels(p)$scales$color
  expect_true(sc$bins)
  expect_equal(sc$bin_breaks, sort(as.numeric(sc$breaks)))
  expect_length(legend_texts(p), length(sc$breaks))
  # and the fill scale, with its square swatches
  f <- vplot(data.frame(x = 1:6, y = 1, v = c(1, 3, 5, 7, 9, 11))) |>
    mark_tile(x = x, y = y, fill = v) |>
    scale_fill_binned(breaks = c(0, 4, 8, 12)) |>
    guides(fill = guide_bins())
  sf <- vellumplot:::.build_panels(f)$scales$fill
  expect_true(sf$bins)
  # ggplot2's binning (#254): the outer edges are the data limits, 1 and 11,
  # so the user's 0 and 12 fall outside and only 4 and 8 are labelled
  expect_equal(sf$bin_breaks, c(1, 4, 8, 11))
  expect_identical(legend_texts(f), c("4", "8"))
})

test_that("bins legends lay out vertically and horizontally", {
  m <- vellumplot:::.legend_metrics(vellumplot:::.resolve_theme(vellumplot:::.theme_default()))
  p <- sized() |> guides(size = guide_bins())
  sc <- vellumplot:::.build_panels(p)$scales$size
  g <- list(kind = "size", sc = sc, key = "size")
  v <- vellumplot:::.bins_geom(g, m)
  h <- vellumplot:::.bins_geom(g, m, horizontal = TRUE)
  k <- length(sc$legend_sizes)
  # keys stacked edge to edge (no row gap), the title above
  expect_equal(v$height, v$th + k * v$pitch)
  expect_equal(vellumplot:::.guide_height_mm(g, m), v$height)
  expect_equal(vellumplot:::.guide_width_mm(g, m), h$width)
  expect_gt(h$width, v$width)
  # show.limits adds headroom for the end labels
  gl <- g
  gl$sc$bins_limits <- TRUE
  expect_gt(vellumplot:::.bins_geom(gl, m)$height, v$height)
  # bottom legend renders, labels drawn once each
  b <- p |> theme(legend.position = "bottom")
  expect_no_error(vellum::as_vellum_scene(b))
  expect_identical(legend_texts(b), legend_texts(p))
  # without the axis: no axis line, labels kept
  a <- sized() |> guides(size = guide_bins(axis = FALSE))
  expect_identical(legend_texts(a), legend_texts(p))
  seg <- function(p) {
    el <- vellum::scene_model(vellum::as_vellum_scene(p))$elements
    sum(el$mark == "segment", na.rm = TRUE)
  }
  expect_lt(seg(a), seg(p))
})

test_that("guide_bins() on a scale that is not binned warns and keeps its legend", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, size = hp) |>
    guides(size = guide_bins())
  expect_warning(
    sc <- vellumplot:::.build_panels(p)$scales$size,
    "needs a binned scale"
  )
  expect_null(sc$bins)
})

test_that("guide_bins() survives the spec round trip", {
  p <- sized() |> guides(size = guide_bins(show.limits = TRUE))
  q <- from_spec(as_spec(p))
  gd <- Filter(function(s) !is.null(s@guide), q@scales)[[1]]@guide
  expect_identical(gd$kind, "bins")
  expect_true(gd$show.limits)
})
