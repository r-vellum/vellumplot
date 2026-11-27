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
