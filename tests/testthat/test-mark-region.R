# Group summary regions: mark_ellipse() and mark_hull().

test_that("mark_ellipse records an ellipse stat with its params", {
  p <- vplot(mtcars) |>
    mark_ellipse(x = wt, y = mpg, type = "norm", level = 0.9)
  L <- p@layers[[1]]
  expect_identical(L@mark, "ellipse")
  expect_identical(L@stat, "ellipse")
  expect_identical(L@stat_params$type, "norm")
  expect_equal(L@stat_params$level, 0.9)
})

test_that("an ellipse is a closed dense polygon, one piece per group", {
  p <- vplot(iris) |>
    mark_ellipse(x = Sepal.Length, y = Sepal.Width, color = Species)
  r <- vellumplot:::.resolve_layers(p)[[1]]
  # 51 segments + 1 closing vertex, per group
  expect_equal(r$n, 52 * 3)
  expect_equal(length(unique(r$values$.piece)), 3)
  expect_setequal(as.character(unique(r$values$color)), levels(iris$Species))
})

test_that("the ellipse extends the position domain beyond the raw points", {
  p <- vplot(mtcars) |> mark_ellipse(x = wt, y = mpg)
  sc <- vellumplot:::.build_panels(p)$scales
  r <- vellumplot:::.resolve_layers(p)[[1]]
  # the trained domain covers the whole ellipse boundary (which is what is
  # drawn), and the boundary bulges past the raw data on its lower side
  expect_lte(sc$x$domain[1], min(r$values$x))
  expect_gte(sc$x$domain[2], max(r$values$x))
  expect_lt(min(r$values$x), min(mtcars$wt))
})

test_that("ellipse types t / norm / euclid all fit", {
  for (ty in c("t", "norm", "euclid")) {
    r <- vellumplot:::.resolve_layers(
      vplot(mtcars) |> mark_ellipse(x = wt, y = mpg, type = ty)
    )[[1]]
    expect_equal(r$n, 52)
    expect_true(all(is.finite(r$values$x) & is.finite(r$values$y)))
  }
})

test_that("an unknown ellipse type errors", {
  expect_error(
    vellumplot:::.resolve_layers(
      vplot(mtcars) |> mark_ellipse(x = wt, y = mpg, type = "bogus")
    ),
    "type"
  )
})

test_that("a convex hull's vertices are a subset of the input points", {
  p <- vplot(mtcars) |> mark_hull(x = wt, y = mpg)
  r <- vellumplot:::.resolve_layers(p)[[1]]
  expect_true(all(r$values$x %in% mtcars$wt))
  expect_true(all(r$values$y %in% mtcars$mpg))
  # every hull point sits on the boundary of the data range
  expect_equal(max(r$values$x), max(mtcars$wt))
  expect_equal(min(r$values$x), min(mtcars$wt))
})

test_that("a grouped hull yields one piece per group", {
  p <- vplot(iris) |>
    mark_hull(x = Sepal.Length, y = Sepal.Width, color = Species)
  r <- vellumplot:::.resolve_layers(p)[[1]]
  expect_equal(length(unique(r$values$.piece)), 3)
})

test_that("regions need at least 3 points per group", {
  expect_error(
    suppressWarnings(
      vellumplot:::.resolve_layers(
        vplot(data.frame(x = 1:2, y = 1:2)) |> mark_hull(x = x, y = y)
      )
    ),
    "3 points"
  )
})

test_that("mark_ellipse / mark_hull need x and y", {
  expect_error(
    vellumplot:::.resolve_layers(vplot(mtcars) |> mark_ellipse(x = wt)),
    "both"
  )
})

test_that("regions render", {
  f <- local_tempfile(fileext = ".png")
  render_plot(
    vplot(iris) |>
      mark_point(x = Sepal.Length, y = Sepal.Width, color = Species) |>
      mark_ellipse(x = Sepal.Length, y = Sepal.Width, color = Species) |>
      mark_hull(x = Sepal.Length, y = Sepal.Width, fill = Species),
    f
  )
  expect_gt(file.info(f)$size, 0)
})
