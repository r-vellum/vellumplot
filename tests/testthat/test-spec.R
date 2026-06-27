# Spec-IR tests: building the spec is pure data manipulation, no rendering.

test_that("vplot() builds an empty PlotSpec carrying the data and page size", {
  p <- vplot(mtcars, width = 5, height = 3)
  expect_s3_class(p, "vellumplot::PlotSpec")
  expect_identical(p@data, mtcars)
  expect_equal(p@width, 5)
  expect_equal(p@height, 3)
  expect_length(p@layers, 0)
})

test_that("vplot() rejects non-data-frame input", {
  expect_error(vplot(1:10), "data frame")
})

test_that("mark_point() appends a layer with the right mark and channels", {
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg, color = hp)
  expect_length(p@layers, 1)
  L <- p@layers[[1]]
  expect_identical(L@mark, "point")
  expect_named(L@encoding, c("x", "y", "color"))
  expect_identical(vellumplot:::.channel_label(L@encoding$x), "wt")
})

test_that("scalar aesthetics become params, mapped ones become channels", {
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg, size = 3, color = "red")
  L <- p@layers[[1]]
  expect_named(L@encoding, c("x", "y"))
  expect_equal(L@params$size, 3)
  expect_equal(L@params$color, "red")
})

test_that("multiple marks stack into multiple layers", {
  p <- vplot(mtcars) |> mark_line(x = wt, y = mpg) |> mark_point(x = wt, y = mpg)
  expect_length(p@layers, 2)
  expect_identical(vapply(p@layers, function(L) L@mark, character(1)), c("line", "point"))
})

test_that("scale_*() appends and overrides last-wins per aesthetic", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    scale_x_continuous(limits = c(0, 6)) |>
    scale_x_continuous(limits = c(1, 5))
  xs <- Filter(function(s) s@aesthetic == "x", p@scales)
  expect_length(xs, 1)
  expect_equal(xs[[1]]@domain, c(1, 5))
})

test_that("channel type is inferred at resolve time", {
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg, color = factor(cyl))
  r <- vellumplot:::.resolve_layers(p)[[1]]
  expect_identical(r$types$x, "quantitative")
  expect_identical(r$types$color, "nominal")
})

test_that("print() shows a readable tree", {
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg, color = hp) |> scale_color_continuous()
  out <- cli::cli_fmt(print(p))
  expect_true(any(grepl("PlotSpec", out)))
  expect_true(any(grepl("mark_point", out)))
  expect_true(any(grepl("color", out)))
})

test_that("the spec round-trips through serialize()", {
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg, color = hp)
  p2 <- unserialize(serialize(p, NULL))
  expect_length(p2@layers, 1)
  expect_identical(vellumplot:::.channel_label(p2@layers[[1]]@encoding$y), "mpg")
})
