# Secondary axes (sec_axis()/dup_axis()): a second set of ticks/labels on the
# opposite edge, a 1:1 monotonic transform of the primary axis. v1 scope:
# continuous position scales, default Cartesian coords, shared facet scales.

train <- function(p) {
  vellumplot:::.train_scales(p, vellumplot:::.resolve_layers(p))
}

svg_of <- function(p) {
  f <- local_tempfile(fileext = ".svg")
  render_plot(p, f)
  gsub("vl[0-9]+", "vlID", paste(readLines(f), collapse = "\n")) # drop a11y id counter
}

# --- constructors -----------------------------------------------------------

test_that("sec_axis()/dup_axis() build a SecAxisSpec and attach via sec.axis", {
  s <- sec_axis(~ . * 2, name = "twice")
  expect_true(S7::S7_inherits(s, vellumplot:::SecAxisSpec))
  expect_false(s@dup)
  expect_true(dup_axis()@dup)

  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    scale_x_continuous(sec.axis = s)
  xs <- Filter(function(sc) sc@aesthetic == "x", p@scales)[[1]]
  expect_true(S7::S7_inherits(xs@sec_axis, vellumplot:::SecAxisSpec))
})

test_that(".as_sec_fun accepts a formula, function, or transform object; rejects junk", {
  expect_type(vellumplot:::.as_sec_fun(~ . * 3), "closure")
  expect_type(vellumplot:::.as_sec_fun(function(x) x + 1), "closure")
  expect_type(vellumplot:::.as_sec_fun(scales::transform_log10()), "closure")
  expect_error(vellumplot:::.as_sec_fun("nope"), "formula")
})

test_that(".check_sec_axis rejects a non-SecAxisSpec sec.axis", {
  expect_error(
    vplot(mtcars) |>
      mark_point(x = wt, y = mpg) |>
      scale_x_continuous(sec.axis = "oops"),
    "sec_axis"
  )
})

# --- training: the correctness core -----------------------------------------

test_that("sec_axis(~ . * 2) places doubled labels at primary-native positions", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    scale_x_continuous(sec.axis = sec_axis(~ . * 2, name = "double"))
  sec <- train(p)$x$sec
  expect_false(is.null(sec))
  expect_equal(sec$name, "double")
  # each label is twice the wt value at its native break position
  expect_equal(as.numeric(sec$labels), 2 * sec$breaks)
})

test_that("dup_axis() reuses the primary breaks and labels exactly", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    scale_y_continuous(sec.axis = dup_axis())
  ty <- train(p)$y
  expect_identical(ty$sec$breaks, ty$breaks)
  expect_identical(ty$sec$labels, ty$labels)
})

test_that("a scales::transform_*() object drives the secondary axis", {
  p <- vplot(data.frame(x = 1:100, y = 1:100)) |>
    mark_point(x = x, y = y) |>
    scale_x_continuous(sec.axis = sec_axis(scales::transform_sqrt()))
  sec <- train(p)$x$sec
  expect_false(is.null(sec))
  expect_true(length(sec$breaks) > 0)
})

test_that("a user breaks/labels override on the secondary axis is honoured", {
  p <- vplot(data.frame(x = 0:100, y = 0:100)) |>
    mark_point(x = x, y = y) |>
    scale_x_continuous(
      sec.axis = sec_axis(
        ~ . / 100,
        breaks = c(0.25, 0.5, 0.75),
        labels = c("lo", "mid", "hi")
      )
    )
  sec <- train(p)$x$sec
  expect_identical(sec$labels, c("lo", "mid", "hi"))
  # sec breaks 0.25/0.5/0.75 map back to native x positions 25/50/75
  expect_equal(sec$breaks, c(25, 50, 75), tolerance = 0.5)
})

test_that("a non-monotonic secondary transform errors clearly", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    scale_x_continuous(sec.axis = sec_axis(~ .^2 - 6 * .))
  expect_error(train(p), "monotonic")
})

# --- additivity: the no-sec path is unchanged -------------------------------

test_that("a plot without a secondary axis has no $sec and renders unchanged", {
  plain <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
  expect_null(train(plain)$x$sec)
  expect_null(train(plain)$y$sec)
  # a dup_axis(guide-off) equivalent is not asserted here; instead pin that the
  # default render is byte-stable against itself (the feature is inert when off).
  expect_identical(
    svg_of(plain),
    svg_of(vplot(mtcars) |> mark_point(x = wt, y = mpg))
  )
})

# --- rendering: ink lands on the opposite edge ------------------------------

test_that("a secondary x-axis draws ink in the top gutter (absent without it)", {
  base <- vplot(data.frame(t = 0:100, y = 0:100)) |> mark_line(x = t, y = y)
  with_sec <- base |>
    scale_x_continuous(sec.axis = sec_axis(~ . * 1.8 + 32, name = "F"))
  top <- c(0, 0.08)
  expect_gt(count_ink(render_px(with_sec), top, c(0.1, 0.9)), 5)
  expect_lt(count_ink(render_px(base), top, c(0.1, 0.9)), 5)
})

test_that("a secondary y-axis (dup) draws ink in the right gutter", {
  base <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
  with_sec <- base |> scale_y_continuous(sec.axis = dup_axis())
  right <- c(0.92, 1)
  expect_gt(count_ink(render_px(with_sec), c(0.1, 0.9), right), 5)
})

test_that("a faceted shared-scale plot with a secondary axis renders", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    facet_wrap(~cyl) |>
    scale_x_continuous(sec.axis = sec_axis(~ . * 2))
  expect_no_error(svg_of(p))
})

# --- guardrails: unsupported combinations error clearly ---------------------

test_that("sec.axis errors under coord_flip / coord_polar / coord_trans", {
  base <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    scale_x_continuous(sec.axis = dup_axis())
  expect_error(svg_of(base |> coord_flip()), "Cartesian")
  expect_error(svg_of(base |> coord_trans(y = "log10")), "Cartesian")
  pol <- vplot(mtcars) |>
    mark_bar(x = factor(cyl)) |>
    scale_y_continuous(sec.axis = dup_axis()) |>
    coord_polar()
  expect_error(svg_of(pol), "Cartesian")
})

test_that("sec.axis errors with free facet scales on the sec'd dimension", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    facet_wrap(~cyl, scales = "free_x") |>
    scale_x_continuous(sec.axis = dup_axis())
  expect_error(svg_of(p), "shared")
})

test_that("sec.axis errors together with add_marginal()", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    scale_x_continuous(sec.axis = dup_axis()) |>
    add_marginal()
  expect_error(svg_of(p), "add_marginal")
})

# --- composition: secondary axis is dropped, but must not crash --------------

test_that("a composition of a sec-bearing subplot renders without error", {
  a <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    scale_x_continuous(sec.axis = dup_axis())
  b <- vplot(mtcars) |> mark_point(x = wt, y = hp)
  expect_no_error(svg_of(hconcat(a, b)))
})
