# Bars on a continuous x axis train the scale on their edges, not only their
# centres, so the end bars are not clipped (VP-90), as in ggplot2.

.xdomain <- function(p) vellumplot:::.build_panels(p)$scales$x$domain

test_that("bar edges widen a continuous x domain like ggplot2 (VP-90)", {
  d <- data.frame(x = 1:4, y = c(3, 5, 2, 4))
  # edges 0.55..4.45, plus ggplot2's default 5% expansion of 3.9
  expect_equal(.xdomain(vplot(d) |> mark_bar(x = x, y = y)), c(0.355, 4.645))
  # a stat's own width (unequal spacing: 90% of the smallest gap)
  d2 <- data.frame(x = c(1, 1.5, 4), y = 1:3)
  expect_equal(
    .xdomain(vplot(d2) |> mark_bar(x = x, y = y)),
    c(0.775, 4.225) + c(-1, 1) * 0.05 * 3.45
  )
  # coord_flip trains the same data-x domain
  expect_equal(
    .xdomain(vplot(d) |> mark_bar(x = x, y = y) |> coord_flip()),
    c(0.355, 4.645)
  )
  # a reversed axis stays reversed
  expect_equal(
    .xdomain(vplot(d) |> mark_bar(x = x, y = y) |> scale_x_reverse()),
    c(4.645, 0.355)
  )
})

test_that("dodged bars and date bars widen the domain too", {
  dg <- data.frame(x = rep(1:3, 2), y = 1:6, g = rep(c("a", "b"), each = 3))
  expect_equal(
    .xdomain(vplot(dg) |> mark_bar(x = x, y = y, fill = g, position = "dodge")),
    c(0.55, 3.45) + c(-1, 1) * 0.05 * 2.9
  )
  dt <- data.frame(t = as.Date("2024-01-01") + c(0, 10, 20), y = 1:3)
  dom <- .xdomain(vplot(dt) |> mark_bar(x = t, y = y))
  t0 <- as.numeric(as.Date("2024-01-01"))
  expect_equal(dom, t0 + c(-4.5, 24.5) + c(-1, 1) * 0.05 * 29)
})

test_that("explicit limits, discrete axes and other marks are unchanged", {
  d <- data.frame(x = 1:4, y = c(3, 5, 2, 4))
  expect_equal(
    .xdomain(vplot(d) |> mark_bar(x = x, y = y) |> xlim(0, 5)),
    c(-0.25, 5.25)
  )
  expect_equal(.xdomain(vplot(d) |> mark_point(x = x, y = y)), c(0.85, 4.15))
  dd <- data.frame(x = c("a", "b"), y = 1:2)
  # a discrete axis already pads half a band each side
  expect_equal(.xdomain(vplot(dd) |> mark_bar(x = x, y = y)), c(0.5, 2.5))
})

# The drawn bars of panel 1, in scene units, ordered left to right.
.bar_rects <- function(p) {
  el <- vellum::scene_model(vellum::as_vellum_scene(p))$elements
  r <- el[el$mark == "rect" & el$panel == "panel-1-1", ]
  r <- r[-which.max(r$w * r$h), ] # the panel background
  r[order(r$x), ]
}

test_that("mark_bar(width =) sizes the bars in data units, like ggplot2 (VP-98)", {
  d <- data.frame(x = c(10, 20, 40), y = c(3, 5, 2))
  p <- vplot(d) |> mark_bar(x = x, y = y, width = 4)
  # ggplot2: geom_col(width = 4) spans 8..42, then the 5% expansion of 34
  expect_equal(.xdomain(p), c(8, 42) + c(-1, 1) * 0.05 * 34)
  r <- .bar_rects(p)
  per_unit <- diff(r$x[1:2]) / 10
  expect_equal(r$w / per_unit, rep(4, 3))
  # flipped and reversed axes train the same extent
  expect_equal(.xdomain(p |> coord_flip()), c(6.3, 43.7))
  expect_equal(.xdomain(p |> scale_x_reverse()), c(43.7, 6.3))
  # a categorical axis: the width is a fraction of the unit band
  dc <- data.frame(x = c("a", "b"), y = 1:2)
  rc <- .bar_rects(vplot(dc) |> mark_bar(x = x, y = y, width = 0.5))
  expect_equal(rc$w / diff(rc$x), c(0.5, 0.5))
})

test_that("a dodged bar width trains the extent ggplot2 draws (VP-98)", {
  dd <- data.frame(
    x = c(1, 2, 4, 1, 2),
    y = 1:5,
    g = c("a", "a", "a", "b", "b")
  )
  # ggplot2 layer_data(): bars span 0.8..4.2 (the lone bar at 4 keeps 0.4)
  p <- vplot(dd) |>
    mark_bar(x = x, y = y, fill = g, position = "dodge", width = 0.4)
  expect_equal(.xdomain(p), c(0.8, 4.2) + c(-1, 1) * 0.05 * 3.4)
  r <- .bar_rects(p)
  per_unit <- diff(r$x[c(1, 3)]) # the two "a" bars at x = 1 and 2
  expect_equal(r$w / per_unit, c(0.2, 0.2, 0.2, 0.2, 0.4))
  # a wider dodge span spreads the slots: 0.7..4.2, as position_dodge(0.8)
  p2 <- vplot(dd) |>
    mark_bar(
      x = x,
      y = y,
      fill = g,
      position = position_dodge(width = 0.8),
      width = 0.4
    )
  expect_equal(.xdomain(p2), c(0.7, 4.2) + c(-1, 1) * 0.05 * 3.5)
})

test_that("a date bar width is in days (VP-98)", {
  dt <- data.frame(t = as.Date("2024-01-01") + c(0, 30, 60), y = 1:3)
  dom <- .xdomain(vplot(dt) |> mark_bar(x = t, y = y, width = 10))
  t0 <- as.numeric(as.Date("2024-01-01"))
  expect_equal(dom, t0 + c(-5, 65) + c(-1, 1) * 0.05 * 70)
})
