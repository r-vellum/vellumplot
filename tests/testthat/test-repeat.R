# repeat_(): replicate a view across fields -> a composition.

xfield <- function(spec) {
  rlang::as_label(rlang::quo_get_expr(spec@layers[[1]]@encoding$x@expr))
}

test_that("repeat_ returns a composition of N re-pointed specs", {
  comp <- repeat_(
    vplot(mtcars) |> mark_point(y = mpg),
    x = c("wt", "hp", "disp")
  )
  expect_true(S7::S7_inherits(comp, vellumplot:::PlotComposition))
  expect_length(comp@plots, 3)
  expect_identical(
    vapply(comp@plots, xfield, character(1)),
    c("wt", "hp", "disp")
  )
})

test_that("repeat_ adds the aesthetic even when the base doesn't map it", {
  # base maps only y; x is introduced by the repeat
  comp <- repeat_(vplot(mtcars) |> mark_point(y = mpg), x = c("wt", "hp"))
  expect_true(all(vapply(
    comp@plots,
    function(s) !is.null(s@layers[[1]]@encoding$x),
    logical(1)
  )))
})

test_that("multiple aesthetics are zipped", {
  comp <- repeat_(
    vplot(mtcars) |> mark_point(),
    x = c("wt", "hp"),
    y = c("mpg", "disp")
  )
  yfield <- function(s) {
    rlang::as_label(rlang::quo_get_expr(s@layers[[1]]@encoding$y@expr))
  }
  expect_identical(vapply(comp@plots, xfield, character(1)), c("wt", "hp"))
  expect_identical(vapply(comp@plots, yfield, character(1)), c("mpg", "disp"))
})

test_that("repeat_ validates its inputs", {
  expect_error(
    repeat_(vplot(mtcars) |> mark_point(y = mpg), c("wt", "hp")), # unnamed
    "named"
  )
  expect_error(
    repeat_(
      vplot(mtcars) |> mark_point(y = mpg),
      x = c("wt", "hp"),
      color = "cyl"
    ),
    "same length"
  )
})

test_that("a repeated composition renders", {
  f <- local_tempfile(fileext = ".png")
  render_plot(
    repeat_(vplot(mtcars) |> mark_point(y = mpg), x = c("wt", "hp", "disp")),
    f
  )
  expect_gt(file.info(f)$size, 0)
})
