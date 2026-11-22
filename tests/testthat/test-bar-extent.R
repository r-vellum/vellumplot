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
