# Density-shape marks: mark_violin, mark_ridgeline, mark_dotplot (new in 0.2.0).

set.seed(1)
df <- data.frame(g = rep(letters[1:3], each = 50), v = rnorm(150))

test_that("mark_violin() records its mark and renders (plain and filled)", {
  expect_identical(
    (vplot(df) |> mark_violin(x = g, y = v))@layers[[1]]@mark,
    "violin"
  )
  f <- withr::local_tempfile(fileext = ".png")
  expect_no_error(render_plot(vplot(df) |> mark_violin(x = g, y = v), f))
  expect_no_error(render_plot(
    vplot(df) |> mark_violin(x = g, y = v, fill = g),
    f
  ))
})

test_that("mark_violin() provenance ties each violin to its category's rows", {
  prov <- plot_provenance(vplot(df) |> mark_violin(x = g, y = v))
  vio <- Filter(function(e) e$mark == "violin", prov)
  expect_length(vio, 3L)
  expect_true(all(vapply(vio, function(e) length(e$rows) == 50L, logical(1))))
})

test_that("mark_ridgeline() records its mark and renders", {
  expect_identical(
    (vplot(df) |> mark_ridgeline(x = v, y = g))@layers[[1]]@mark,
    "ridgeline"
  )
  f <- withr::local_tempfile(fileext = ".png")
  expect_no_error(render_plot(vplot(df) |> mark_ridgeline(x = v, y = g), f))
  expect_no_error(render_plot(
    vplot(df) |> mark_ridgeline(x = v, y = g, fill = g),
    f
  ))
})

test_that("mark_dotplot() bins and stacks one dot per observation", {
  d <- data.frame(v = c(1, 1, 1, 2, 5))
  L <- vellumplot:::.resolve_layer(
    (vplot(d) |> mark_dotplot(x = v, binwidth = 0.5))@layers[[1]],
    d
  )
  sdf <- vellumplot:::.apply_stat(L)
  # one row per observation, y = stack height within the bin
  expect_equal(nrow(sdf$values$x |> as.data.frame()), 5L)
  expect_equal(max(sdf$values$y), 2.5) # three 1's stacked -> heights 0.5,1.5,2.5
})

test_that("mark_dotplot() renders", {
  f <- withr::local_tempfile(fileext = ".png")
  expect_no_error(render_plot(vplot(df) |> mark_dotplot(x = v), f))
})

# Regression: density-shape marks draw geometry beyond the raw data range (the
# density support for a violin, the ridge height above the top category for a
# ridgeline). Scale training must widen the panel to fit that footprint, else the
# tallest ridge / density tails are clipped (see NEWS: "no longer clip").
train <- function(p) {
  vellumplot:::.train_scales(p, vellumplot:::.resolve_layers(p))
}

test_that("mark_violin() trains y to cover the full density support", {
  sc <- train(vplot(iris) |> mark_violin(x = Species, y = Sepal.Length))
  supp <- unlist(lapply(levels(iris$Species), function(l) {
    range(stats::density(iris$Sepal.Length[iris$Species == l])$x)
  }))
  expect_lte(sc$y$domain[1], min(supp))
  expect_gte(sc$y$domain[2], max(supp))
})

test_that("mark_ridgeline() trains y above the top ridge and x over the support", {
  sc <- train(vplot(iris) |> mark_ridgeline(x = Sepal.Length, y = Species))
  k <- nlevels(iris$Species)
  # top ridge rises `scale` (default 1.4) above the top category baseline (k)
  expect_gte(sc$y$domain[2], k + 1.4)
  supp <- unlist(lapply(levels(iris$Species), function(l) {
    range(stats::density(iris$Sepal.Length[iris$Species == l])$x)
  }))
  expect_lte(sc$x$domain[1], min(supp))
  expect_gte(sc$x$domain[2], max(supp))
})

test_that("explicit limits are not widened by the density footprint", {
  vio <- train(
    vplot(iris) |>
      mark_violin(x = Species, y = Sepal.Length) |>
      coord_cartesian(ylim = c(4, 8))
  )
  # With the axis pinned, the violin footprint must add nothing: the y domain
  # matches a footprint-free mark under the same limits, and stays clear of the
  # density support top (~8.52) it would otherwise expand to.
  ref <- train(
    vplot(iris) |>
      mark_boxplot(x = Species, y = Sepal.Length) |>
      coord_cartesian(ylim = c(4, 8))
  )
  expect_equal(vio$y$domain, ref$y$domain)
  expect_lt(vio$y$domain[2], 8.5)
})
