# Regression tests for REVIEW3 5F: serialization / interop robustness.

test_that(".channel_from_ir resolves a non-syntactic field name as a column", {
  ch <- vellumplot:::.channel_from_ir(
    list(field = "Sepal Width", type = "quantitative"),
    env = globalenv()
  )
  # A `field` becomes a symbol (not parsed as code), so a name with a space
  # resolves as the column rather than throwing a parse error.
  expect_identical(rlang::quo_get_expr(ch@expr), as.name("Sepal Width"))
})

test_that("VL inline data rebuilds a column whose first row is missing/NA", {
  skip_if_not_installed("jsonlite")
  vl <- list(
    mark = "point",
    encoding = list(x = list(field = "a"), y = list(field = "b")),
    data = list(
      values = list(
        list(b = 10), # row 1 has no "a"
        list(a = 2, b = 20),
        list(a = 3, b = 30)
      )
    )
  )
  spec <- spec_from_vegalite(vl)
  # "a" is numeric c(NA, 2, 3), not a vapply type error keyed off row 1.
  expect_type(spec@data$a, "double")
  expect_equal(spec@data$a, c(NA, 2, 3))
})

test_that("a manifest round-trips through an SVG even with `--` in a column name", {
  skip_if_not_installed("jsonlite")
  d <- data.frame(`a--b` = 1:3, y = 4:6, check.names = FALSE)
  p <- vplot(d) |> mark_point(x = y, y = y)
  svg <- plot_svg(p, manifest = TRUE)
  v <- plot_verify(svg, d)
  expect_true(v$ok)
})

test_that("wrap_plots splices a named list positionally", {
  ps <- list(
    ncol = vplot(mtcars) |> mark_point(x = wt, y = mpg),
    a = vplot(mtcars) |> mark_point(x = hp, y = mpg)
  )
  # The element named "ncol" must be a sub-plot, not bound to the ncol argument.
  comp <- wrap_plots(ps, ncol = 1)
  expect_length(comp@plots, 2L)
})

test_that("a design list with a malformed area errors clearly", {
  a <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
  b <- vplot(mtcars) |> mark_point(x = hp, y = mpg)
  expect_error(
    concat(a, b, design = list(area(1, 1, 1, 1), list(t = 1, l = 1))),
    "area"
  )
})
