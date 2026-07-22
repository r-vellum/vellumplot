# Structural scene snapshots: a refactor-safety net. Each snapshot pins the
# platform-stable structure of a reference plot (element counts per mark, panel
# names, data-space scale ranges) via `scene_digest()` (see helper.R). A
# behaviour-preserving refactor must leave these byte-identical; a diff here on a
# refactor PR is a regression to explain, not a snapshot to bless blindly.
#
# Reference plots use only base-package data and hard-dependency marks (no
# Suggests), so they always run — never skip — and the snapshot always applies.

test_that("scatter with a discrete colour legend and a line overlay", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, color = factor(cyl)) |>
    mark_line(x = wt, y = mpg)
  expect_snapshot(scene_digest(p))
})

test_that("bar chart (count stat)", {
  p <- vplot(mtcars) |> mark_bar(x = factor(cyl))
  expect_snapshot(scene_digest(p))
})

test_that("histogram", {
  p <- vplot(mtcars) |> mark_histogram(x = mpg)
  expect_snapshot(scene_digest(p))
})

test_that("boxplot grouped by a discrete x", {
  p <- vplot(mtcars) |> mark_boxplot(x = factor(cyl), y = mpg)
  expect_snapshot(scene_digest(p))
})

test_that("loess smooth over a scatter", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    mark_smooth(x = wt, y = mpg, method = "loess")
  expect_snapshot(scene_digest(p))
})

test_that("facet_wrap over a discrete variable", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    facet_wrap(~cyl)
  expect_snapshot(scene_digest(p))
})
