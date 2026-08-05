# Phase 1b: vwaffle() self-contained waffle chart.

svg <- function(p) paste(plot_svg(p), collapse = "")

test_that("vwaffle renders from values and from row counts", {
  df <- data.frame(part = c("a", "b", "c"), n = c(50, 30, 20))
  expect_no_error(svg(vwaffle(df, category = part, value = n)))
  raw <- data.frame(g = rep(c("x", "y"), c(30, 70)))
  expect_no_error(svg(vwaffle(raw, category = g)))
})

test_that("vwaffle allocates cells by share, summing to n_cells", {
  df <- data.frame(part = c("a", "b", "c"), n = c(50, 30, 20))
  b <- vellumplot:::.build_panels(vwaffle(
    df,
    category = part,
    value = n,
    n_cells = 100
  ))
  L <- b$panels[[1]]$resolved[[1]]
  tab <- table(as.character(L$values$fill))
  expect_equal(sum(tab), 100L)
  expect_equal(as.integer(tab[c("a", "b", "c")]), c(50L, 30L, 20L))
})

test_that("vwaffle validates inputs", {
  expect_error(vwaffle(1:3, category = x), "data frame")
  expect_error(
    vwaffle(data.frame(g = NA_character_), category = g),
    "no non-missing"
  )
})

test_that("vwaffle round-trips (theme styling aside)", {
  df <- data.frame(part = c("a", "b"), n = c(3, 1))
  expect_no_error(suppressWarnings(from_spec(as_spec(
    vwaffle(df, category = part, value = n)
  ))))
})
