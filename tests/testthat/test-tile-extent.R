# Tiles and rasters on a continuous axis train on their edges, so the end
# cells are not clipped (VP-99), as ggplot2 does.

.domains <- function(p) {
  s <- vellumplot:::.build_panels(p)$scales
  list(x = s$x$domain, y = s$y$domain)
}
.exp5 <- function(r) r + c(-1, 1) * 0.05 * diff(r)

test_that("tile edges widen continuous x and y like ggplot2 (VP-99)", {
  d <- data.frame(x = c(10, 20, 40), y = c(3, 5, 2))
  # ggplot2 layer_scales(): x 5..45, y 1.5..5.5 (resolution 10 and 1)
  expect_equal(
    .domains(vplot(d) |> mark_tile(x = x, y = y)),
    list(x = .exp5(c(5, 45)), y = .exp5(c(1.5, 5.5)))
  )
  # an explicit width / height (data units)
  expect_equal(
    .domains(vplot(d) |> mark_tile(x = x, y = y, width = 4, height = 0.5)),
    list(x = .exp5(c(8, 42)), y = .exp5(c(1.75, 5.25)))
  )
  tl <- expand.grid(x = c(0.5, 1.5, 3), y = c(10, 20, 30))
  p <- vplot(tl) |> mark_tile(x = x, y = y)
  expect_equal(.domains(p), list(x = .exp5(c(0, 3.5)), y = .exp5(c(5, 35))))
  # reversed and flipped axes keep the extent (and the reversal)
  expect_equal(
    .domains(p |> scale_x_reverse() |> scale_y_reverse()),
    list(x = rev(.exp5(c(0, 3.5))), y = rev(.exp5(c(5, 35))))
  )
  expect_equal(.domains(p |> coord_flip()), .domains(p))
})

test_that("raster cells widen the domain, and limits still win (VP-99)", {
  g <- expand.grid(x = 1:5, y = 1:5)
  expect_equal(
    .domains(vplot(g) |> mark_raster(x = x, y = y)),
    list(x = .exp5(c(0.5, 5.5)), y = .exp5(c(0.5, 5.5)))
  )
  expect_equal(
    .domains(vplot(g) |> mark_tile(x = x, y = y) |> xlim(1, 5))$x,
    .exp5(c(1, 5))
  )
  # a categorical axis already pads half a band
  dc <- data.frame(x = c("a", "b"), y = c(1, 2))
  expect_equal(.domains(vplot(dc) |> mark_tile(x = x, y = y))$x, c(0.5, 2.5))
})

test_that("a constant tile width is drawn (VP-99)", {
  d <- data.frame(x = c(10, 20, 40), y = c(1, 1, 1))
  el <- vellum::scene_model(
    vellum::as_vellum_scene(vplot(d) |> mark_tile(x = x, y = y, width = 4))
  )$elements
  r <- el[el$mark == "rect" & el$panel == "panel-1-1", ]
  r <- r[-which.max(r$w * r$h), ]
  r <- r[order(r$x), ]
  expect_equal(r$w / (diff(r$x[1:2]) / 10), rep(4, 3))
})

test_that("date tiles keep a date axis (VP-99)", {
  d <- data.frame(t = as.Date("2024-01-01") + c(0, 10, 20), y = 1)
  dom <- .domains(vplot(d) |> mark_tile(x = t, y = y))$x
  t0 <- as.numeric(as.Date("2024-01-01"))
  expect_equal(dom, t0 + .exp5(c(-5, 25)))
})
