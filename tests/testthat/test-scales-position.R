# Position scale breadth: discrete limits, transforms, explicit breaks.

train <- function(p) {
  vellumplot:::.train_scales(p, vellumplot:::.resolve_layers(p))
}

test_that("scale_x_discrete(limits=) reorders / subsets levels", {
  p <- vplot(mtcars) |>
    mark_bar(x = factor(cyl)) |>
    scale_x_discrete(limits = c("8", "4", "6"))
  x <- train(p)$x
  expect_identical(x$labels, c("8", "4", "6"))
  expect_identical(x$map("8"), 1L)
  expect_identical(x$map("6"), 3L)
})

test_that("scale_y_discrete(limits=) reorders / subsets levels on the y axis", {
  p <- vplot(mtcars) |>
    mark_point(x = mpg, y = factor(cyl)) |>
    scale_y_discrete(limits = c("8", "4", "6"))
  y <- train(p)$y
  expect_identical(y$labels, c("8", "4", "6"))
  expect_identical(y$map("8"), 1L)
  expect_identical(y$map("6"), 3L)
})

test_that("default continuous labels don't group thousands (#27)", {
  # A 4-digit continuous variable (years, IDs) must render as "2010", not the
  # grouped "2 010" scales inserts by default.
  df <- data.frame(year = 2007:2025, n = seq_along(2007:2025))
  p <- vplot(df) |> mark_line(x = year, y = n)
  x <- train(p)$x
  expect_true("2010" %in% x$labels)
  expect_false(any(grepl("[[:space:],]", x$labels)))
})

test_that("trans='sqrt' transforms the map and is continuous", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    scale_y_continuous(trans = "sqrt")
  y <- train(p)$y
  expect_equal(y$map(16), 4)
  expect_false(y$discrete)
})

test_that("trans='symlog' is symmetric, linear through zero, log in the tails", {
  p <- vplot(data.frame(x = c(-100, -1, 0, 1, 100), y = 1:5)) |>
    mark_point(x = x, y = y) |>
    scale_x_continuous(trans = "symlog")
  x <- train(p)$x
  # sign(x) * log10(1 + |x|): zero stays put, and it is odd-symmetric
  expect_equal(x$map(0), 0)
  expect_equal(x$map(9), -x$map(-9))
  # compresses the tails: 100 is far less than 100x the position of 1
  expect_lt(x$map(100) / x$map(1), 10)
  expect_false(x$discrete)
})

test_that("symlog breaks sit at zero and signed powers of ten within range", {
  expect_equal(
    vellumplot:::.symlog_breaks(c(-100, 100)),
    c(-100, -10, -1, 0, 1, 10, 100)
  )
  expect_equal(vellumplot:::.symlog_breaks(c(0, 500)), c(0, 1, 10, 100))
})

test_that("a symlog axis reports its transform in the panel scales descriptor", {
  s <- vellum::scene_model(
    vellum::as_vellum_scene(
      vplot(data.frame(x = c(-10, 0, 10), y = 1:3)) |>
        mark_point(x = x, y = y) |>
        scale_x_continuous(trans = "symlog")
    )
  )$panels
  i <- match("panel-1-1", s$name)
  expect_equal(s$meta[[i]]$scales$x$transform, "symlog")
})

test_that("trans='reverse' flips the axis via a decreasing domain", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    scale_x_continuous(trans = "reverse")
  x <- train(p)$x
  # The data mapping stays identity; the domain decreases so the axis reverses
  # (negating the data too would double-flip and cancel out).
  expect_equal(x$map(3), 3)
  expect_gt(x$domain[1], x$domain[2])
})

test_that("segment endpoints (yend) and a literal baseline both train the axis", {
  # A segment-only plot must derive its y-domain from both the baseline (`y = 0`,
  # a positional literal) and the endpoint (`yend`); otherwise the domain
  # collapses onto 0 and the sticks render off-panel.
  d <- data.frame(i = 1:4, v = c(2, 5, 9, 12))
  p <- vplot(d) |> mark_segment(x = i, y = 0, xend = i, yend = v)
  y <- train(p)$y
  expect_lte(y$domain[1], 0)
  expect_gte(y$domain[2], 12)
})

test_that("an unknown transform errors", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    scale_x_continuous(trans = "wobble")
  expect_error(train(p), "transform")
})

test_that("explicit position breaks/labels are honoured", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    scale_x_continuous(breaks = c(2, 4), labels = c("two", "four"))
  x <- train(p)$x
  expect_equal(x$breaks, c(2, 4))
  expect_identical(x$labels, c("two", "four"))
})

test_that("log10 transform is preserved (breaks within range, log-mapped)", {
  d <- data.frame(x = c(1, 10, 100, 1000), y = 1:4)
  p <- vplot(d) |>
    mark_point(x = x, y = y) |>
    scale_x_continuous(trans = "log10")
  x <- train(p)$x
  expect_equal(x$map(100), 2) # log10(100)
  expect_true(all(x$breaks >= log10(1) - 1e-9 & x$breaks <= log10(1000) + 1e-9))
})

test_that("scale_x/y_continuous(expand=) controls axis padding and round-trips", {
  dom <- function(p, aes = "x") {
    vellumplot:::.build_panels(p)$scales[[aes]]$domain
  }
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
  # default keeps ~5% breathing room beyond the data
  expect_gt(diff(dom(p)), diff(range(mtcars$wt)))
  # expand = c(0, 0) clamps exactly to the data range
  expect_equal(dom(p |> scale_x_continuous(expand = c(0, 0))), range(mtcars$wt))
  # additive term pads in data units
  expect_equal(
    dom(p |> scale_x_continuous(expand = c(0, 1))),
    range(mtcars$wt) + c(-1, 1)
  )
  # round-trips through a spec
  q <- from_spec(as_spec(p |> scale_y_continuous(expand = c(0, 0))))
  expect_equal(q@scales[[1]]@expand, c(0, 0))
  # invalid values are rejected up front
  expect_error(scale_x_continuous(p, expand = -1), "non-negative")
  expect_error(scale_x_continuous(p, expand = c(1, 2, 3)), "length 1 or 2")
})
