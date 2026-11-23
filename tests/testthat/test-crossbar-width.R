# VP-105: a cross-bar's `width` is in data units, as an error bar's caps
# (VP-104) and a bar's width (VP-98) are, and as in ggplot2's geom_crossbar(),
# defaulting to 90% of the band, so cross-bars line up with bars and error
# bars, dodged or not, on a discrete or a continuous x.

# The boxes (centre x, width; native units) of a plot's cross-bars.
cb_boxes <- function(p) {
  s <- vellum::as_scene_spec(vellum::as_vellum_scene(p))
  val <- function(x) if (is.list(x)) x$v else x
  out <- list()
  walk <- function(node) {
    id <- val(node$id)
    if (
      identical(node$class, "grob_rect") &&
        is.character(id) &&
        grepl("crossbar", id)
    ) {
      out[[length(out) + 1L]] <<- data.frame(
        x = val(node$x$value),
        w = val(node$width$value)
      )
    }
    for (ch in node) {
      if (is.list(ch)) walk(ch)
    }
  }
  walk(s$root)
  do.call(rbind, out)
}

cb <- data.frame(x = c(10, 20, 30), y = c(2, 4, 3))
cb$lo <- cb$y - 1
cb$hi <- cb$y + 1

test_that("crossbar width is in data units on a continuous x", {
  b <- cb_boxes(
    vplot(cb) |> mark_crossbar(x = x, y = y, ymin = lo, ymax = hi, width = 4)
  )
  expect_equal(b$w, rep(4, 3))
  expect_equal(b$x, c(10, 20, 30))
  # the default is 90% of the resolution, a default bar's width
  b <- cb_boxes(vplot(cb) |> mark_crossbar(x = x, y = y, ymin = lo, ymax = hi))
  expect_equal(b$w, rep(9, 3))
})

test_that("crossbar width defaults to 90% of a discrete band", {
  d <- data.frame(g = c("a", "b"), y = 1:2, lo = 0:1, hi = 2:3)
  b <- cb_boxes(vplot(d) |> mark_crossbar(x = g, y = y, ymin = lo, ymax = hi))
  expect_equal(b$w, rep(0.9, 2))
  b <- cb_boxes(
    vplot(d) |> mark_crossbar(x = g, y = y, ymin = lo, ymax = hi, width = 0.5)
  )
  expect_equal(b$w, rep(0.5, 2))
})

test_that("the boxes train the continuous axis, as bars do", {
  p <- vplot(cb) |>
    mark_crossbar(x = x, y = y, ymin = lo, ymax = hi, width = 8)
  dom <- vellumplot:::.build_panels(p)$scales$x$domain
  expect_lte(dom[1], 6)
  expect_gte(dom[2], 34)
})

test_that("dodged cross-bars line up with dodged bars", {
  d <- data.frame(
    x = rep(c(1, 2, 4), 2),
    y = c(3, 5, 2, 4, 1, 3),
    g = rep(c("a", "b"), each = 3)
  )
  d$lo <- d$y - 0.5
  d$hi <- d$y + 0.5
  d$f <- factor(d$x)
  for (xx in c("x", "f")) {
    p <- vplot(d) |>
      mark_crossbar(
        x = .data[[xx]],
        y = y,
        ymin = lo,
        ymax = hi,
        group = g,
        position = "dodge"
      )
    b <- cb_boxes(p)
    b <- b[order(b$x), ]
    expect_equal(b$w, rep(0.45, 6), info = xx)
    pos <- if (xx == "x") c(1, 2, 4) else 1:3
    expect_equal(b$x, sort(c(pos - 0.225, pos + 0.225)), info = xx)
  }
})

test_that("the middle line is 2.5 times the box outline, as in ggplot2", {
  s <- vellum::as_scene_spec(vellum::as_vellum_scene(
    vplot(cb) |> mark_crossbar(x = x, y = y, ymin = lo, ymax = hi)
  ))
  val <- function(x) if (is.list(x)) x$v else x
  lwd <- list()
  walk <- function(node) {
    id <- val(node$id)
    if (is.character(id) && grepl("crossbar", id) && !is.null(node$gp)) {
      lwd[[node$class]] <<- c(lwd[[node$class]], val(node$gp$lwd))
    }
    for (ch in node) {
      if (is.list(ch)) walk(ch)
    }
  }
  walk(s$root)
  expect_equal(unique(lwd$grob_segments), 2.5 * unique(lwd$grob_rect))
})
