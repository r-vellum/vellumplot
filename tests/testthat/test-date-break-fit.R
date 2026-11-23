# The date-break fit (VP-92) reads the axis guide's angle and n.dodge and
# estimates the panel as the layout sizes it, legends included (VP-101).
# Assertions are on the chosen breaks, with margins wide enough for the text
# metrics to differ between platforms.

.xlabels <- function(p) vellumplot:::.build_panels(p)$scales$x$labels

hr <- data.frame(
  t = seq(as.POSIXct("2024-03-01 00:00", tz = "UTC"), by = "10 min", length.out = 100),
  v = cos(seq_len(100) / 8)
)
hours <- function(width = 2.5) {
  vplot(hr, width = width, height = 2.5) |> mark_line(x = t, y = v)
}

test_that("rotated and dodged labels allow denser date breaks (VP-101)", {
  three_hourly <- c("00:00", "03:00", "06:00", "09:00", "12:00", "15:00")
  # horizontal: "00:00" labels 3 hours apart do not fit 2.5 inches
  expect_lt(length(.xlabels(hours())), 6L)
  # rotated, they need only their line height
  expect_identical(
    .xlabels(hours() |> guides(x = guide_axis(angle = 90))),
    three_hourly
  )
  # at 45 degrees they need their line height times sqrt(2); a little wider
  # so the margin covers platform text metrics, horizontal still too narrow
  expect_lt(length(.xlabels(hours(3))), 6L)
  expect_identical(
    .xlabels(hours(3) |> guides(x = guide_axis(angle = 45))),
    three_hourly
  )
  # staggered over two rows, neighbours in a row are two breaks apart
  expect_identical(
    .xlabels(hours() |> guides(x = guide_axis(n.dodge = 2))),
    three_hourly
  )
})

test_that("a wide legend narrows the panel the date labels must fit (VP-101)", {
  m <- data.frame(d = seq(as.Date("2024-01-01"), as.Date("2024-06-30"), by = "day"))
  m$v <- sin(seq_len(nrow(m)) / 9)
  m$g <- rep(c("a rather long legend label", "b"), length.out = nrow(m))
  p <- vplot(m, width = 4, height = 2.5) |> mark_line(x = d, y = v)
  expect_length(.xlabels(p), 6L) # monthly
  # the legend leaves about an inch of panel: monthly labels would collide
  expect_lt(length(.xlabels(p |> mark_line(x = d, y = v, color = g))), 6L)
  # ... unless the legend goes below
  expect_length(
    .xlabels(
      p |>
        mark_line(x = d, y = v, color = g) |>
        theme(legend.position = "bottom")
    ),
    6L
  )
})

test_that("the panel estimate matches the laid-out panel (VP-101)", {
  d <- data.frame(d = as.Date("2000-01-01") + 0:99, v = 1:100, g = c("long label", "b"))
  p <- vplot(d, width = 5, height = 3) |> mark_line(x = d, y = v, color = g)
  b <- vellumplot:::.build_panels(p)
  ctx <- vellumplot:::.axis_fit_ctx(p, b$scales, list(title = list()))
  rt <- vellumplot:::.resolve_theme(vellumplot:::.theme_of(p))
  est <- vellumplot:::.axis_fit_len_mm(p, rt, TRUE, ctx)
  el <- vellum::scene_model(vellum::as_vellum_scene(p))$elements
  pn <- el[el$mark == "rect" & el$panel == "panel-1-1", ]
  real <- max(pn$w) / 96 * 25.4
  expect_equal(est, real, tolerance = 0.01)
})

test_that("label clearance follows the rotation", {
  clr <- vellumplot:::.label_clearance_mm
  expect_equal(clr(10, 3, 0, TRUE), 11)
  expect_equal(clr(10, 3, 90, TRUE), 4)
  expect_equal(clr(10, 3, 45, TRUE), 4 * sqrt(2))
  # a vertical axis: horizontal text needs its height, rotated its width
  expect_equal(clr(10, 3, 0, FALSE), 4)
  expect_equal(clr(10, 3, 90, FALSE), 11)
})

test_that("a date axis keeps two labels when no breaks fit (VP-102)", {
  d <- data.frame(
    d = rep(seq(as.Date("2000-01-01"), as.Date("2019-12-31"), length.out = 80), 2),
    g = rep(c("a long legend label", "b"), each = 80)
  )
  d$v <- sin(seq_len(160) / 7) + rep(c(0, 2), each = 80)
  p <- vplot(d, width = 3, height = 2.5) |> mark_line(x = d, y = v, color = g)
  lab <- .xlabels(p)
  expect_gte(length(lab), 2L)
  expect_identical(lab[1], "2000")
  # the fallback directly: a fit that never passes keeps the first and last
  rng <- as.numeric(as.Date(c("2000-01-01", "2019-12-31")))
  out <- vellumplot:::.pretty_time_breaks(
    rng,
    as.Date("2000-01-01"),
    rng,
    fit = function(...) FALSE
  )
  expect_identical(out$labels, c("2000", "2010"))
  # without a fit the densest breaks stay
  one <- vellumplot:::.pretty_time_breaks(rng, as.Date("2000-01-01"), rng)
  expect_identical(one$labels, c("2000", "2005", "2010", "2015"))
})

test_that("each neighbour pair needs only its mean label width (VP-110)", {
  wmm <- function(l) {
    vellum::vl_convert(vellum::grobwidth(vellumplot:::.txt(l, 8.8)), "mm")
  }
  labs <- c("i", "WWWWWW", "i")
  wn <- wmm("i")
  ww <- wmm("WWWWWW")
  gap <- vellumplot:::.LABEL_GAP_MM
  fits <- function(len, ...) {
    vellumplot:::.labels_fit(0:2, labs, c(0, 2), len, 8.8, 0, TRUE, ...)
  }
  # neighbours one unit apart: a pair needs (wn + ww) / 2 + gap, not ww + gap
  pair <- (wn + ww) / 2 + gap
  expect_lt(pair + 0.5, ww + gap) # the widest-label rule would refuse this
  expect_true(fits(2 * (pair + 0.25)))
  expect_false(fits(2 * (pair - 0.25)))
  # order does not matter
  expect_true(vellumplot:::.labels_fit(
    c(2, 0, 1),
    labs[c(3, 1, 2)],
    c(0, 2),
    2 * (pair + 0.25),
    8.8,
    0,
    TRUE
  ))
  # dodged over two rows, the narrow labels are the row neighbours
  expect_true(fits(2 * (wn + gap) / 2 + 0.5, n_dodge = 2L))
  # one label per row: nothing to collide
  expect_true(fits(0.1, n_dodge = 3L))
})
