# Phase 6: statistical-annotation layer (mark_signif).

svg <- function(p) paste(plot_svg(p), collapse = "")
panels <- function(p) vellumplot:::.build_panels(p)

set.seed(1)
d <- data.frame(
  g = rep(c("a", "b", "c"), each = 30),
  y = c(rnorm(30), rnorm(30, 2), rnorm(30, 0.3))
)
base <- vplot(d) |> mark_boxplot(x = g, y = y)

test_that("mark_signif renders with default, explicit, stars, and t.test", {
  expect_no_error(svg(base |> mark_signif(x = g, y = y)))
  expect_no_error(svg(
    base |>
      mark_signif(x = g, y = y, comparisons = list(c("a", "b"), c("a", "c")))
  ))
  expect_no_error(svg(base |> mark_signif(x = g, y = y, label = "stars")))
  expect_no_error(svg(base |> mark_signif(x = g, y = y, method = "t.test")))
})

test_that("the signif stat computes one bracket per comparison, stacked upward", {
  p <- base |>
    mark_signif(x = g, y = y, comparisons = list(c("a", "b"), c("a", "c")))
  L <- panels(p)$panels[[1]]$resolved[[2]]
  expect_equal(L$n, 2L)
  expect_identical(L$values$x, c("a", "a")) # left endpoints
  expect_identical(L$values$.x2, c("b", "c")) # right endpoints
  expect_true(all(diff(L$values$y) > 0)) # stacked heights increase
  expect_true(all(grepl("^p = ", L$values$.blabel)))
})

test_that("the y-axis expands to fit the brackets", {
  b <- panels(base |> mark_signif(x = g, y = y))
  expect_gt(b$scales$y$data_range[2], max(d$y))
})

test_that("stars labels and a real difference read as significant", {
  L <- panels(
    base |>
      mark_signif(
        x = g,
        y = y,
        comparisons = list(c("a", "b")),
        label = "stars"
      )
  )$panels[[1]]$resolved[[2]]
  expect_true(L$values$.blabel %in% c("*", "**", "***", "****")) # a vs b (mean +2) is significant
})

test_that("mark_signif errors when a group has too few values, and round-trips", {
  thin <- data.frame(g = c("a", "b"), y = c(1, 2))
  expect_error(
    panels(vplot(thin) |> mark_signif(x = g, y = y)),
    "no comparable groups|finite"
  )
  expect_no_error(from_spec(as_spec(
    base |> mark_signif(x = g, y = y, comparisons = list(c("a", "b")))
  )))
})
