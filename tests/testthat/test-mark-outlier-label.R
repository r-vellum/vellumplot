# Outlier callout (mark_outlier_label): keep and label only the y-outliers.

svg <- function(p) paste(plot_svg(p), collapse = "")
panels <- function(p) vellumplot:::.build_panels(p)
resolved2 <- function(p) panels(p)$panels[[1]]$resolved[[2]]

test_that("iqr flags the high mpg outlier and labels it", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    mark_outlier_label(x = wt, y = mpg, label = rownames(mtcars))
  expect_no_error(svg(p))
  L <- resolved2(p)
  expect_equal(L$n, 1L)
  expect_identical(L$values$label, "Toyota Corolla") # 33.9 mpg, the extreme
})

test_that("no mapped label falls back to the outlier's y value", {
  L <- resolved2(
    vplot(mtcars) |>
      mark_point(x = wt, y = mpg) |>
      mark_outlier_label(x = wt, y = mpg)
  )
  expect_equal(L$n, 1L)
  expect_identical(L$values$label, "33.9")
})

test_that("outliers are detected within each colour group", {
  d <- data.frame(
    x = seq_len(60),
    y = c(rnorm(29, 0, 0.5), 20, rnorm(29, 0, 0.5), -20),
    g = rep(c("a", "b"), each = 30)
  )
  L <- resolved2(
    vplot(d) |>
      mark_point(x = x, y = y, color = g) |>
      mark_outlier_label(x = x, y = y, color = g, label = x)
  )
  expect_equal(L$n, 2L) # one spike per group
  expect_setequal(as.character(L$values$color), c("a", "b"))
})

test_that("the sd method and k threshold both work", {
  base <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
  expect_no_error(svg(
    base |> mark_outlier_label(x = wt, y = mpg, method = "sd")
  ))
  # a huge k flags nothing -> an empty (blank) layer, still renders
  L <- resolved2(base |> mark_outlier_label(x = wt, y = mpg, k = 100))
  expect_equal(L$n, 0L)
})

test_that("mark_outlier_label round-trips through a spec", {
  expect_no_error(from_spec(as_spec(
    vplot(mtcars) |>
      mark_point(x = wt, y = mpg) |>
      mark_outlier_label(x = wt, y = mpg, label = rownames(mtcars))
  )))
})
