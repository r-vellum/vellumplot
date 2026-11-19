# Stat parameter validation (VP-21) and stat edge cases (VP-22).

test_that("bad stat parameters fail in the constructor, naming the argument", {
  p <- vplot(mtcars)
  expect_error(mark_histogram(p, x = mpg, bins = 0), "`bins`")
  expect_error(mark_histogram(p, x = mpg, bins = 2.5), "whole number")
  expect_error(mark_hex(p, x = wt, y = mpg, bins = -3), "`bins`")
  expect_error(mark_bin2d(p, x = wt, y = mpg, bins = NA), "`bins`")
  expect_error(mark_dotplot(p, x = mpg, binwidth = -1), "`binwidth`")
  expect_error(mark_dotplot(p, x = mpg, binwidth = 0), "positive")
  expect_error(mark_density(p, x = mpg, adjust = 0), "`adjust`")
  expect_error(mark_violin(p, x = cyl, y = mpg, adjust = -1), "`adjust`")
  expect_error(mark_ridgeline(p, x = mpg, y = cyl, height = 0), "`height`")
  expect_error(mark_smooth(p, x = wt, y = mpg, level = 2), "`level`")
  expect_error(mark_smooth(p, x = wt, y = mpg, level = 0), "between 0 and 1")
  expect_error(mark_smooth(p, x = wt, y = mpg, span = 0), "`span`")
  expect_error(mark_ellipse(p, x = wt, y = mpg, level = 1.5), "`level`")
  expect_error(mark_contour(p, x = wt, y = mpg, bins = 0), "`bins`")
  expect_error(mark_histogram(p, x = mpg, bins = "a"), "a string")
  # the error is reported from the constructor the user called
  err <- tryCatch(mark_histogram(p, x = mpg, bins = 0), error = identity)
  expect_match(format(conditionCall(err)), "mark_histogram")
})

test_that("valid stat parameters still pass", {
  p <- vplot(mtcars)
  expect_no_error(mark_histogram(p, x = mpg, bins = 5))
  expect_no_error(mark_dotplot(p, x = mpg, binwidth = NULL))
  expect_no_error(mark_ellipse(p, x = wt, y = mpg, type = "euclid", level = 3))
  expect_no_error(mark_smooth(p, x = wt, y = mpg, level = 0.9))
})

test_that("an ungrouped layer with too few points warns and is dropped (VP-22)", {
  p <- vplot(data.frame(x = 1, y = 2)) |>
    mark_point(x = x, y = y) |>
    mark_density(x = x)
  expect_warning(vellum::as_vellum_scene(p), "at least 2")
  m <- suppressWarnings(vellum::scene_model(vellum::as_vellum_scene(p)))
  # the point layer still draws
  expect_true("point" %in% m$elements$mark)
  # a smooth through one x value is dropped the same way
  q <- vplot(data.frame(x = c(1, 1), y = 1:2)) |> mark_smooth(x = x, y = y)
  expect_warning(vellum::as_vellum_scene(q), class = "vellumplot_stat_dropped")
})

test_that("a plot of zero rows draws an empty panel (VP-22)", {
  expect_no_error(vellum::as_vellum_scene(
    vplot(mtcars[0, ]) |> mark_histogram(x = mpg)
  ))
  expect_no_error(vellum::as_vellum_scene(
    vplot(mtcars[0, ]) |> mark_density(x = mpg)
  ))
})

test_that("a plot with no y at all names the missing axis", {
  expect_error(
    vellum::as_vellum_scene(vplot(mtcars) |> mark_point(x = mpg)),
    "No layer maps"
  )
})

test_that("mark_rug() works on one axis alone (VP-22)", {
  m <- vellum::scene_model(vellum::as_vellum_scene(
    vplot(mtcars) |> mark_rug(x = mpg)
  ))
  # one tick per row
  expect_gte(sum(m$elements$mark == "segment"), nrow(mtcars))
  expect_no_error(vellum::as_vellum_scene(vplot(mtcars) |> mark_rug(y = mpg)))
})

test_that("stats report rows left out for a non-finite input (VP-22)", {
  d <- data.frame(x = c(1, 2, Inf, 3, -Inf))
  expect_warning(
    vellum::as_vellum_scene(vplot(d) |> mark_histogram(x = x, bins = 3)),
    "Removed 2 rows containing non-finite values"
  )
  r <- suppressWarnings(
    vellumplot:::.resolve_layers(vplot(d) |> mark_histogram(x = x, bins = 3))
  )[[1]]
  expect_equal(sum(r$values$y), 3)
  expect_warning(
    vellum::as_vellum_scene(vplot(d) |> mark_density(x = x)),
    class = "vellumplot_nonfinite_rows"
  )
})
