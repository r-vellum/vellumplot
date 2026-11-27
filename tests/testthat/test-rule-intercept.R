# VP-156: `mark_rule(x = 0)` kept `x` as a position nothing drew, so the rule
# silently disappeared (and the axis was titled "0"). An `x` / `y` given to
# `mark_rule()` now means `xintercept` / `yintercept`.

base <- vplot(mtcars) |> mark_point(x = wt, y = mpg)

test_that("mark_rule(x =, y =) draws the same rules as the intercepts", {
  expect_identical(
    scene_digest(base |> mark_rule(x = 3) |> mark_rule(y = 20)),
    scene_digest(base |> mark_rule(xintercept = 3) |> mark_rule(yintercept = 20))
  )
  # A mapped `y` gives one rule per row, as a mapped `yintercept` does.
  h <- data.frame(m = c(15, 25))
  expect_identical(
    scene_digest(base |> mark_rule(y = m, data = h)),
    scene_digest(base |> mark_rule(yintercept = m, data = h))
  )
})

test_that("mark_rule(x = 0) draws a line and keeps the axis title", {
  p <- vplot(mtcars) |>
    mark_rule(x = 0) |>
    mark_point(x = wt - 3, y = mpg)
  expect_named(p@layers[[1]]@params, "xintercept")
  expect_identical(vellumplot:::.plot_titles(p)$title[["x"]], "wt - 3")
  segs <- Filter(
    function(e) e$mark == "rule" && e$kind == "mark",
    plot_provenance(p)
  )
  expect_length(segs, 1L)
})

test_that("x given alongside xintercept is dropped with a warning", {
  expect_warning(
    p <- vplot(mtcars) |> mark_rule(x = wt, xintercept = 3),
    "ignores `x` when `xintercept` is given"
  )
  expect_named(p@layers[[1]]@params, "xintercept")
  expect_length(p@layers[[1]]@encoding, 0L)
})

test_that("from_spec() and Vega-Lite rules read x as the intercept (VP-156)", {
  nrule <- function(p) {
    prov <- suppressWarnings(plot_provenance(p))
    length(Filter(function(e) e$mark == "rule" && e$kind == "mark", prov))
  }
  spec <- list(layers = list(
    list(mark = "point", encoding = list(x = list(field = "wt"), y = list(field = "mpg"))),
    list(mark = "rule", encoding = list(x = list(expr = "3")))
  ))
  p <- from_spec(spec, data = mtcars)
  expect_named(c(p@layers[[2]]@encoding, p@layers[[2]]@params), "xintercept")
  expect_identical(nrule(p), 1L)
  skip_if_not_installed("jsonlite")
  vl <- list(
    data = list(values = lapply(1:3, function(i) list(a = i, b = i))),
    layer = list(
      list(mark = "point", encoding = list(
        x = list(field = "a", type = "quantitative"),
        y = list(field = "b", type = "quantitative")
      )),
      list(mark = "rule", encoding = list(x = list(field = "a", type = "quantitative")))
    )
  )
  p3 <- spec_from_vegalite(as.character(jsonlite::toJSON(vl, auto_unbox = TRUE)))
  expect_identical(nrule(p3), 1L)
})

test_that("a rule with a second position is left alone", {
  ri <- vellumplot:::.rule_intercepts(
    "rule",
    list(x = 1, xmax = 2),
    list()
  )
  expect_named(ri$encoding, c("x", "xmax"))
  ri2 <- vellumplot:::.rule_intercepts("point", list(x = 1), list())
  expect_named(ri2$encoding, "x")
})

# VP-173: a plot of rules alone errored "No layer maps y"; the axis the rules
# span is drawn empty, as for a rug alone.
test_that("rules alone draw over an empty cross axis (VP-173)", {
  d <- data.frame(v = c(1, 3, 4))
  p <- vplot(d) |> mark_rule(x = v)
  expect_no_error(sc <- vellum::as_vellum_scene(p))
  segs <- Filter(
    function(e) e$mark == "rule" && e$kind == "mark",
    plot_provenance(p)
  )
  expect_length(segs, 1L)
  built <- vellumplot:::.build_panels(p)
  expect_length(built$scales$y$breaks, 0L)
  expect_null(built$scales$y$name)
  expect_equal(range(built$scales$x$breaks), c(1, 4))
  # The same for horizontal rules, and for constant intercepts.
  expect_no_error(vellum::as_vellum_scene(vplot(d) |> mark_rule(y = v)))
  expect_no_error(vellum::as_vellum_scene(vplot(d) |> mark_rule(xintercept = 2)))
  # Faceted, with free scales too.
  df <- data.frame(v = c(1, 3, 4), f = c("a", "b", "b"))
  expect_no_error(vellum::as_vellum_scene(
    vplot(df) |> mark_rule(x = v) |> facet_wrap(~f, scales = "free")
  ))
  # A plot with no position at all still asks for one.
  expect_error(
    vellum::as_vellum_scene(vplot(d) |> mark_point()),
    "No layer maps"
  )
})
