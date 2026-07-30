# Regression tests for the audit pass (scale training edge cases, composition
# nesting, insets, robustness guards).

train <- function(p) {
  vellumplot:::.train_scales(p, vellumplot:::.resolve_layers(p))
}
panels <- function(p) vellumplot:::.build_panels(p)

test_that("a log-scaled bar drops the 0 baseline instead of erroring", {
  d <- data.frame(x = c("a", "b", "c"), y = c(10, 100, 1000))
  p <- vplot(d) |> mark_bar(x = x, y = y) |> scale_y_continuous(trans = "log10")
  f <- local_tempfile(fileext = ".png")
  expect_no_error(render_plot(p, f))
  ysc <- panels(p)$scales$y
  expect_true(all(is.finite(ysc$domain)))
})

test_that("a sqrt-scaled area renders", {
  d <- data.frame(x = 1:5, y = c(1, 4, 9, 16, 25))
  p <- vplot(d) |> mark_area(x = x, y = y) |> scale_y_continuous(trans = "sqrt")
  f <- local_tempfile(fileext = ".png")
  expect_no_error(render_plot(p, f))
})

test_that("descending coord limits reverse the axis domain", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    coord_cartesian(xlim = c(4, 2))
  dom <- panels(p)$scales$x$domain
  expect_gt(dom[1], dom[2])
})

test_that("labels without matching breaks error rather than recycle", {
  base <- vplot(mtcars) |> mark_point(x = wt, y = mpg, color = hp)
  expect_error(
    panels(base |> scale_color_continuous(labels = c("a", "b"))),
    "one entry per break"
  )
})

test_that("discrete colour breaks select and order the legend levels", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, color = factor(cyl)) |>
    scale_color_discrete(breaks = c("8", "4"))
  sc <- panels(p)$scales$color
  expect_identical(sc$levels, c("8", "4"))
  expect_length(sc$colors, 2L)
})

test_that("discrete colour labels ride along with their levels", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, color = factor(cyl)) |>
    scale_color_discrete(labels = c("four", "six", "eight"))
  sc <- panels(p)$scales$color
  expect_identical(sc$labels, c("four", "six", "eight"))
})

test_that("scale_shape rejects unknown shape names at declaration", {
  expect_error(
    vplot(mtcars) |>
      mark_point(x = wt, y = mpg, shape = factor(cyl)) |>
      scale_shape(values = "pentagon"),
    "Unknown shape"
  )
})

test_that("a composition nesting a faceted sub-plot renders all panels", {
  a <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
  fac <- a |> facet_wrap(~cyl)
  comp <- hconcat(a, vconcat(a, fac))
  expect_false(vellumplot:::.comp_alignable(comp))
  f <- local_tempfile(fileext = ".png")
  expect_no_error(render_plot(comp, f))
})

test_that("an inset drawn below the base still renders", {
  a <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
  b <- vplot(mtcars) |> mark_point(x = hp, y = mpg)
  f <- local_tempfile(fileext = ".png")
  expect_no_error(render_plot(inset(a, b, on_top = FALSE), f))
})

test_that("a boxplot tolerates NA in y", {
  d <- data.frame(
    g = rep(c("a", "b"), each = 5),
    y = c(1, 2, NA, 4, 5, 2, 3, 4, NA, 6)
  )
  p <- vplot(d) |> mark_boxplot(x = g, y = y)
  f <- local_tempfile(fileext = ".png")
  expect_no_error(render_plot(p, f))
})

test_that("ragged and mis-counted design layouts error clearly", {
  a <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
  b <- vplot(mtcars) |> mark_point(x = hp, y = mpg)
  expect_error(concat(a, b, design = "AAB\nCC"), "same width")
  expect_error(concat(a, b, design = "AB\nCD"), "one area per plot")
})

test_that("resolve_scale requires named arguments", {
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg) |> facet_wrap(~cyl)
  expect_error(resolve_scale(p, "independent"), "must be named")
})

# --- REVIEW3 correctness patch (C1-C9) --------------------------------------

test_that("C1: .bound_native tolerates NA bounds without an NA-subscript error", {
  sc <- list(domain = c(0, 10), map = function(x) x)
  expect_no_error(out <- vellumplot:::.bound_native(c(-Inf, NA, 5, Inf), sc))
  expect_identical(out[1], 0) # -Inf -> lower bound
  expect_identical(out[4], 10) # +Inf -> upper bound
  expect_true(is.na(out[2])) # NA stays NA (row is dropped downstream)
  expect_identical(out[3], 5) # finite value mapped through
})

test_that("C2: transition_time drops NA-time rows from every keyframe", {
  d <- data.frame(x = c(1, 2, 3, 4), y = c(1, 2, 3, 4), t = c(1, 2, 3, NA))
  d2 <- d[is.finite(d$t), ]
  p <- vplot(d) |> mark_point(x = x, y = y) |> transition_time(t)
  p2 <- vplot(d2) |> mark_point(x = x, y = y) |> transition_time(t)
  a <- animate(p, nframes = 6)
  a2 <- animate(p2, nframes = 6)
  # Same states, and identical per-keyframe element counts: the NA-time row must
  # not be injected as an all-NA point into any frame.
  expect_equal(a@states, a2@states)
  n1 <- vapply(
    a@scenes,
    function(s) nrow(vellum::scene_model(s)$elements),
    integer(1)
  )
  n2 <- vapply(
    a2@scenes,
    function(s) nrow(vellum::scene_model(s)$elements),
    integer(1)
  )
  expect_equal(n1, n2)
})

test_that("C4: a continuous colour scale honours a `lims()` domain", {
  # `lims(color = ...)` sets the colour scale domain; before the fix the
  # continuous colour trainer derived its range from data only and dropped it.
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, color = hp) |>
    lims(color = c(0, 400))
  sc <- panels(p)$scales$color
  expect_equal(sc$range, c(0, 400))
})

test_that("C5: a guide-only scale (no `type`) does not crash the position path", {
  # guides(x = "none") appends a ScaleSpec with no `type`; the position trainer
  # must treat a zero-info type as "infer", not error on logical(0).
  p <- vplot(mtcars) |>
    mark_point(x = factor(cyl), y = mpg) |>
    guides(x = "none")
  f <- local_tempfile(fileext = ".png")
  expect_no_error(render_plot(p, f))
  # And the default itself is length-1, not character(0).
  expect_identical(vellumplot:::ScaleSpec(aesthetic = "x")@type, "")
})

test_that("C6: vchord drops NA endpoints with a warning instead of breaking", {
  expect_warning(
    layout <- vellumplot:::.chord_layout(
      from = c("a", "b", NA),
      to = c("b", "a", "a"),
      value = c(1, 2, 3),
      gap = 0.02,
      sort = "input"
    ),
    "NA endpoint"
  )
  # Only the two finite-endpoint flows survive; no NA node, and sector angles are
  # finite (not poisoned by an NA per-node total).
  expect_setequal(layout$sectors$node, c("a", "b"))
  expect_false(anyNA(layout$sectors$node))
  expect_true(all(is.finite(layout$sectors$theta0)))
})

test_that("C7: binned 'pretty' breaks bracket the data extremes", {
  brks <- vellumplot:::.binned_breaks(c(2, 15, 38), n = 4, style = "pretty")
  expect_lte(min(brks), 2) # a cut at/below the minimum
  expect_gte(max(brks), 38) # a cut at/above the maximum
  # Every data value lands inside a class (findInterval within [1, k]).
  k <- length(brks) - 1L
  idx <- findInterval(c(2, 15, 38), brks, rightmost.closed = TRUE)
  expect_true(all(idx >= 1L & idx <= k))
})

test_that("C8: integer and logical columns keep their type across a round-trip", {
  d <- data.frame(
    i = 1:5,
    b = c(TRUE, FALSE, TRUE, FALSE, TRUE),
    x = as.numeric(1:5)
  )
  p <- vplot(d) |> mark_point(x = x, y = i)
  p2 <- from_spec(as_spec(p))
  expect_type(p2@data$i, "integer")
  expect_type(p2@data$b, "logical")
  expect_type(p2@data$x, "double")
})

test_that("C9: a constant `if_false` colour round-trips as a constant, not a var", {
  s <- list(
    selection = "sel",
    if_false = "red",
    empty = TRUE
  )
  rec <- list(field = "g", type = "nominal", condition = s)
  ch <- vellumplot:::.channel_from_ir(rec, env = globalenv())
  # "red" is a colour name, not a variable reference -- it must remain a plain
  # string, not become a quosure that evaluates the missing symbol `red`.
  expect_identical(ch@condition$if_false, "red")
  expect_false(rlang::is_quosure(ch@condition$if_false))
})
