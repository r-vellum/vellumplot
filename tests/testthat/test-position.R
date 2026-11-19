# G6: position adjustments (stack / fill / dodge / jitter).

mt <- transform(mtcars, cyl = factor(cyl), am = factor(am))

test_that("bars stack by default", {
  expect_identical(
    (vplot(mt) |> mark_bar(x = cyl, fill = am))@layers[[1]]@position,
    "stack"
  )
})

test_that("stacking assigns non-overlapping [ymin, ymax] spans summing to the group total", {
  p <- vplot(mt) |> mark_bar(x = cyl, fill = am)
  r <- vellumplot:::.resolve_layers(p)[[1]]
  expect_false(is.null(r$values$ymin))
  # within each x the spans tile [0, total] with no gaps/overlap
  x <- as.character(r$values$x)
  for (xi in unique(x)) {
    rows <- which(x == xi)
    lo <- sort(r$values$ymin[rows])
    hi <- sort(r$values$ymax[rows])
    expect_equal(lo[1], 0)
    expect_equal(hi, c(lo[-1], max(hi))) # each ymax meets the next ymin
    expect_equal(max(hi), sum(table(mt$cyl, mt$am)[xi, ])) # = count in that x
  }
})

test_that("position = 'fill' normalises each x group to 1", {
  p <- vplot(mt) |> mark_bar(x = cyl, fill = am, position = "fill")
  r <- vellumplot:::.resolve_layers(p)[[1]]
  x <- as.character(r$values$x)
  tops <- vapply(
    unique(x),
    function(xi) max(r$values$ymax[x == xi]),
    numeric(1)
  )
  expect_equal(unname(tops), rep(1, length(tops)))
})

test_that("an ungrouped bar gets a 0..y span", {
  p <- vplot(data.frame(g = factor(c("a", "a", "b")))) |> mark_bar(x = g)
  r <- vellumplot:::.resolve_layers(p)[[1]]
  expect_equal(r$values$ymin, c(0, 0))
  expect_equal(r$values$ymax, r$values$y)
})

test_that("stacked totals drive the y domain", {
  p <- vplot(mt) |> mark_bar(x = cyl, fill = am)
  sc <- vellumplot:::.build_panels(p)$scales
  expect_gte(sc$y$domain[2], max(table(mt$cyl)))
})

test_that("dodge and jitter are recorded and render", {
  expect_identical(
    (vplot(mt) |> mark_bar(x = cyl, fill = am, position = "dodge"))@layers[[
      1
    ]]@position,
    "dodge"
  )
  for (pos in c("dodge", "fill", "stack")) {
    f <- local_tempfile(fileext = ".png")
    render_plot(vplot(mt) |> mark_bar(x = cyl, fill = am, position = pos), f)
    expect_gt(file.info(f)$size, 0)
  }
  fj <- local_tempfile(fileext = ".png")
  render_plot(
    vplot(mt) |> mark_point(x = cyl, y = mpg, position = "jitter"),
    fj
  )
  expect_gt(file.info(fj)$size, 0)
})

# --- Area stacking (H14) --------------------------------------------------
# mark_area() with a mapped fill stacks into a band by default, mirroring bars.

area_df <- data.frame(
  x = rep(1:4, times = 2),
  y = c(1, 2, 3, 4, 2, 1, 2, 1),
  g = factor(rep(c("a", "b"), each = 4))
)

test_that("mark_area() stacks by default", {
  expect_identical(
    (vplot(area_df) |> mark_area(x = x, y = y, fill = g))@layers[[1]]@position,
    "stack"
  )
})

test_that("stacked areas get non-overlapping [ymin, ymax] spans summing to the group total (H14)", {
  p <- vplot(area_df) |> mark_area(x = x, y = y, fill = g)
  r <- vellumplot:::.resolve_layers(p)[[1]]
  expect_false(is.null(r$values$ymin))
  x <- as.character(r$values$x)
  for (xi in unique(x)) {
    rows <- which(x == xi)
    lo <- sort(r$values$ymin[rows])
    hi <- sort(r$values$ymax[rows])
    expect_equal(lo[1], 0)
    expect_equal(hi, c(lo[-1], max(hi))) # spans tile with no gap/overlap
    expect_equal(max(hi), sum(area_df$y[area_df$x == as.numeric(xi)]))
  }
})

test_that("area position = 'fill' normalises each x to 1 (H14)", {
  p <- vplot(area_df) |> mark_area(x = x, y = y, fill = g, position = "fill")
  r <- vellumplot:::.resolve_layers(p)[[1]]
  x <- as.character(r$values$x)
  tops <- vapply(
    unique(x),
    function(xi) max(r$values$ymax[x == xi]),
    numeric(1)
  )
  expect_equal(unname(tops), rep(1, length(tops)))
})

test_that("an area with no fill mapping is unaffected by the stack default (H14)", {
  # One row per x and no group -> ymin = 0, ymax = y. (Rows sharing an x stack
  # even when ungrouped, as in ggplot2; see the VP-17 tests.)
  p <- vplot(area_df[area_df$g == "a", ]) |> mark_area(x = x, y = y)
  r <- vellumplot:::.resolve_layers(p)[[1]]
  expect_equal(r$values$ymin, rep(0, r$n))
  expect_equal(r$values$ymax, r$values$y)
})

test_that("stacked / filled / identity areas render", {
  for (pos in c("stack", "fill", "identity")) {
    f <- local_tempfile(fileext = ".png")
    render_plot(
      vplot(area_df) |> mark_area(x = x, y = y, fill = g, position = pos),
      f
    )
    expect_gt(file.info(f)$size, 0)
  }
})

test_that("a jitter seed makes the rendering reproducible", {
  p <- vplot(mt) |> mark_point(x = cyl, y = mpg, position = "jitter", seed = 42)
  a <- render_px(p)
  b <- render_px(p)
  expect_identical(a, b)
})

test_that("a jitter seed leaves the caller's RNG stream untouched", {
  p <- vplot(mt) |> mark_point(x = cyl, y = mpg, position = "jitter", seed = 1)
  set.seed(99)
  before <- runif(3)
  set.seed(99)
  render_px(p)
  after <- runif(3)
  expect_identical(before, after)
})

test_that("stack/dodge order follow factor levels, not alphabetical", {
  d <- data.frame(
    x = "p",
    g = factor(c("hi", "lo", "mid"), levels = c("lo", "mid", "hi"))
  )
  p <- vplot(d) |> mark_bar(x = x, fill = g)
  r <- vellumplot:::.resolve_layers(p)[[1]]
  # As ggplot2: the first level on top, the last at the bottom (legend order
  # read top-down), so bottom-to-top runs hi, mid, lo.
  ord <- order(r$values$ymin)
  expect_equal(as.character(r$values$fill)[ord], c("hi", "mid", "lo"))
})

# --- VP-17: signed stacks, ungrouped duplicates, mismatched-x areas ----------

stack_of <- function(p) {
  r <- vellumplot:::.resolve_layers(p)[[1]]
  as.data.frame(r$values[c("x", "y", "ymin", "ymax")])
}

test_that("positive and negative heights stack separately (VP-17)", {
  d <- data.frame(x = "a", y = c(2, -1, 3))
  s <- stack_of(vplot(d) |> mark_bar(x = x, y = y))
  expect_equal(s$ymin, c(0, -1, 2))
  expect_equal(s$ymax, c(2, 0, 5))
  # grouped too: negatives pile downward in the same group order
  d <- data.frame(
    x = "a",
    y = c(2, -1, 3, -2),
    g = factor(c("p", "p", "q", "q"))
  )
  s <- stack_of(vplot(d) |> mark_bar(x = x, y = y, fill = g))
  expect_equal(s$ymin, c(3, -3, 0, -2))
  expect_equal(s$ymax, c(5, -2, 3, 0))
})

test_that("position_fill() fills [0, 1] and [-1, 0] by sign (VP-17)", {
  d <- data.frame(
    x = "a",
    y = c(2, -1, 3, -2),
    g = factor(c("p", "p", "q", "q"))
  )
  s <- stack_of(vplot(d) |> mark_bar(x = x, y = y, fill = g, position = "fill"))
  expect_equal(range(s$ymin, s$ymax), c(-1, 1))
  expect_equal(s$ymax - s$ymin, c(0.4, 1 / 3, 0.6, 2 / 3))
})

test_that("ungrouped rows sharing an x stack in data order (VP-17)", {
  d <- data.frame(x = c("a", "a", "b"), y = c(1, 2, 4))
  s <- stack_of(vplot(d) |> mark_bar(x = x, y = y))
  expect_equal(s$ymin, c(0, 1, 0))
  expect_equal(s$ymax, c(1, 3, 4))
})

test_that("a stacked label sits inside its signed span (VP-17)", {
  d <- data.frame(x = "a", y = c(2, -1), lab = c("u", "d"))
  r <- vellumplot:::.resolve_layers(
    vplot(d) |>
      mark_text(x = x, y = y, label = lab, position = position_stack(0.5))
  )[[1]]
  expect_equal(r$values$y, c(1, -0.5))
})

test_that("areas at different x are interpolated onto the union and stack (VP-17)", {
  d <- data.frame(
    x = c(1, 2, 3, 1.5, 2.5),
    y = c(1, 2, 3, 4, 4),
    g = factor(c("a", "a", "a", "b", "b"))
  )
  r <- vellumplot:::.resolve_layers(
    vplot(d) |> mark_area(x = x, y = y, fill = g, tooltip = y)
  )[[1]]
  v <- as.data.frame(r$values[c("x", "y", "fill", "ymin", "ymax")])
  # a (first level) sits on b at every x where b exists
  a <- v[v$fill == "a", ]
  b <- v[v$fill == "b", ]
  at <- function(df, xx) df[abs(df$x - xx) < 1e-9, ]
  expect_equal(at(a, 2)$ymin, at(b, 2)$ymax)
  expect_equal(at(b, 2)$ymax, 4) # b interpolated at a's x
  expect_equal(at(a, 1.5)$ymin, 4)
  expect_equal(at(a, 1.5)$ymax, 4 + 1.5) # a interpolated at b's x
  expect_equal(at(a, 1)$ymin, 0) # outside b's range: on the baseline
  # b drops to zero just outside its edges
  expect_true(all(b$y[b$x < 1.5 | b$x > 2.5] == 0))
  # every added row is keyed back to source rows, and styles follow the group
  expect_length(r$stat_map$members, r$n)
  expect_true(all(lengths(r$values[c("x", "y", "fill")]) == r$n))
  expect_length(r$meta$data_id, r$n)
})

test_that("areas sharing their x values are not re-gridded (VP-17)", {
  r <- vellumplot:::.resolve_layers(
    vplot(area_df) |> mark_area(x = x, y = y, fill = g)
  )[[1]]
  expect_equal(r$n, nrow(area_df))
  expect_null(r$stat_map)
})

test_that("mismatched-x date areas keep their date x (VP-17)", {
  d <- data.frame(
    x = as.Date("2024-01-01") + c(0, 2, 4, 1, 3),
    y = 1,
    g = factor(c("a", "a", "a", "b", "b"))
  )
  r <- vellumplot:::.resolve_layers(
    vplot(d) |> mark_area(x = x, y = y, fill = g)
  )[[1]]
  expect_s3_class(r$values$x, "Date")
  expect_gt(r$n, nrow(d))
  f <- local_tempfile(fileext = ".png")
  render_plot(vplot(d) |> mark_area(x = x, y = y, fill = g), f)
  expect_gt(file.info(f)$size, 0)
})
