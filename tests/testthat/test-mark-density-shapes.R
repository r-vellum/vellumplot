# Density-shape marks: mark_violin, mark_ridgeline, mark_dotplot (new in 0.2.0).

set.seed(1)
df <- data.frame(g = rep(letters[1:3], each = 50), v = rnorm(150))

test_that("mark_violin() records its mark and renders (plain and filled)", {
  expect_identical(
    (vplot(df) |> mark_violin(x = g, y = v))@layers[[1]]@mark,
    "violin"
  )
  f <- local_tempfile(fileext = ".png")
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
  f <- local_tempfile(fileext = ".png")
  expect_no_error(render_plot(vplot(df) |> mark_ridgeline(x = v, y = g), f))
  expect_no_error(render_plot(
    vplot(df) |> mark_ridgeline(x = v, y = g, fill = g),
    f
  ))
})

# Regression (H13): a per-category alpha must reach each shape, not collapse to
# the first category's value. Comparing a mapped alpha (distinct per category)
# against a flat constant alpha proves the mapping is threaded per category: if
# alpha were collapsed to `[1]`, the two renders would be identical.
alpha_df <- data.frame(
  g = rep(c("a", "b"), each = 50),
  v = rnorm(100),
  al = rep(c(0.15, 1), each = 50)
)

test_that("mark_violin() honours a per-category alpha (H13)", {
  mapped <- vplot(alpha_df) |>
    mark_violin(x = g, y = v, fill = g, alpha = al) |>
    scale_alpha_identity()
  flat <- vplot(alpha_df) |>
    mark_violin(x = g, y = v, fill = g, alpha = 0.15)
  expect_false(identical(
    vellum::scene_raster(mapped),
    vellum::scene_raster(flat)
  ))
})

test_that("mark_ridgeline() honours a per-category alpha (H13)", {
  mapped <- vplot(alpha_df) |>
    mark_ridgeline(x = v, y = g, fill = g, alpha = al) |>
    scale_alpha_identity()
  flat <- vplot(alpha_df) |>
    mark_ridgeline(x = v, y = g, fill = g, alpha = 0.15)
  expect_false(identical(
    vellum::scene_raster(mapped),
    vellum::scene_raster(flat)
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
  f <- local_tempfile(fileext = ".png")
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

# The footprint lives in data space but the domain in native (transformed)
# space: it must go through the scale transform, and the union must keep a
# reversed axis reversed.
vio_supp <- function() {
  unlist(lapply(c(4, 6, 8), function(l) {
    range(stats::density(mtcars$mpg[mtcars$cyl == l])$x)
  }))
}

test_that("a log10 violin axis covers the density support in log units", {
  sc <- train(
    vplot(mtcars) |>
      mark_violin(x = factor(cyl), y = mpg) |>
      scale_y_continuous(trans = "log10")
  )
  # the density is estimated on the transformed values (as ggplot2 does)
  supp <- unlist(lapply(c(4, 6, 8), function(l) {
    range(stats::density(log10(mtcars$mpg[mtcars$cyl == l]))$x)
  }))
  expect_lte(sc$y$domain[1], min(supp))
  expect_gte(sc$y$domain[2], max(supp))
  # a sane log domain (mpg ~ 10^0.9 .. 10^1.6), not raw values in log space
  expect_lt(sc$y$domain[2], 2)
})

test_that("a reversed violin axis stays reversed after the footprint union", {
  sc <- train(
    vplot(mtcars) |>
      mark_violin(x = factor(cyl), y = mpg) |>
      scale_y_continuous(trans = "reverse")
  )
  expect_gt(sc$y$domain[1], sc$y$domain[2])
  expect_gte(sc$y$domain[1], max(vio_supp()))
  expect_lte(sc$y$domain[2], min(vio_supp()))
})

test_that("free facet scales are widened by the density footprint too", {
  b <- .build_panels(
    vplot(mtcars) |>
      mark_violin(x = factor(cyl), y = mpg) |>
      facet_wrap(~am, scales = "free_y")
  )
  for (q in b$panels) {
    d <- mtcars[q$idx, , drop = FALSE]
    supp <- unlist(lapply(split(d$mpg, d$cyl), function(v) {
      if (length(v) >= 2) range(stats::density(v)$x)
    }))
    expect_lte(q$y_sc$domain[1], min(supp))
    expect_gte(q$y_sc$domain[2], max(supp))
  }
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

# VP-29: the density was estimated in data space and its support (padded ~3
# bandwidths, so below zero for data near 0) mapped through the scale, giving NaN
# outline vertices on a log/sqrt axis. It is now estimated on the transformed
# values, so every drawn vertex is finite and training still covers the shape.
drawn_polygons <- function(p) {
  cap <- list()
  local_mocked_bindings(
    .draw = function(scene, grob, rows = NULL) {
      if (inherits(grob, "vellum::grob_polygon")) {
        cap[[length(cap) + 1L]] <<- list(
          x = vctrs::field(grob@x, "value"),
          y = vctrs::field(grob@y, "value")
        )
      }
      scene
    }
  )
  vellumplot:::.compile_plot(p)
  cap
}

set.seed(29)
near0 <- data.frame(g = rep(c("a", "b"), 100), v = stats::rexp(200))

test_that("density shapes on log/sqrt axes draw only finite vertices (VP-29)", {
  plots <- list(
    vplot(near0) |>
      mark_violin(x = g, y = v) |>
      scale_y_continuous(trans = "log10"),
    vplot(near0) |>
      mark_violin(x = g, y = v) |>
      scale_y_continuous(trans = "sqrt"),
    vplot(near0) |>
      mark_halfeye(x = g, y = v) |>
      scale_y_continuous(trans = "log10"),
    vplot(near0) |>
      mark_ridgeline(x = v, y = g) |>
      scale_x_continuous(trans = "log10"),
    vplot(mtcars) |>
      mark_violin(x = factor(cyl), y = mpg) |>
      scale_y_continuous(trans = "log10")
  )
  for (p in plots) {
    polys <- expect_no_warning(drawn_polygons(p))
    expect_gt(length(polys), 0L)
    for (pg in polys) {
      expect_true(all(is.finite(pg$x)) && all(is.finite(pg$y)))
    }
  }
})

test_that("the trained log axis covers the drawn violin outline (VP-29)", {
  p <- vplot(near0) |>
    mark_violin(x = g, y = v) |>
    scale_y_continuous(trans = "log10")
  sc <- train(p)
  ys <- unlist(lapply(drawn_polygons(p), `[[`, "y"))
  expect_lte(sc$y$domain[1], min(ys))
  expect_gte(sc$y$domain[2], max(ys))
})

test_that("an identity-scale violin outline is the data-space density (VP-29)", {
  p <- vplot(iris) |> mark_violin(x = Species, y = Sepal.Length)
  polys <- drawn_polygons(p)
  ref <- stats::density(iris$Sepal.Length[iris$Species == "setosa"])
  expect_equal(polys[[1]]$y, c(ref$x, rev(ref$x)))
})

# VP-28: the domain was widened for the footprint after breaks were picked for
# the raw data range, so the tails ran past the last labelled tick.
test_that("a footprint-widened axis is re-broken over its new extent (VP-28)", {
  sc <- train(vplot(iris) |> mark_violin(x = Species, y = Sepal.Length))$y
  step <- min(diff(sc$breaks))
  expect_lte(sc$domain[2] - max(sc$breaks), step)
  expect_lte(min(sc$breaks) - sc$domain[1], step)
  expect_identical(sc$labels, format(sc$breaks, trim = TRUE))

  lg <- train(
    vplot(mtcars) |>
      mark_violin(x = factor(cyl), y = mpg) |>
      scale_y_continuous(trans = "log10")
  )$y
  lstep <- min(diff(lg$breaks))
  expect_lte(lg$domain[2] - max(lg$breaks), lstep)
  expect_lte(min(lg$breaks) - lg$domain[1], lstep)

  rg <- train(vplot(iris) |> mark_ridgeline(x = Sepal.Length, y = Species))$x
  expect_lte(rg$domain[2] - max(rg$breaks), min(diff(rg$breaks)))
})

test_that("explicit breaks survive the footprint widening (VP-28)", {
  sc <- train(
    vplot(iris) |>
      mark_violin(x = Species, y = Sepal.Length) |>
      scale_y_continuous(breaks = c(5, 6, 7))
  )$y
  expect_equal(sc$breaks, c(5, 6, 7))
})

test_that("free facet axes are re-broken over their footprint too (VP-28)", {
  b <- .build_panels(
    vplot(mtcars) |>
      mark_violin(x = factor(cyl), y = mpg) |>
      facet_wrap(~am, scales = "free_y")
  )
  for (q in b$panels) {
    sc <- q$y_sc
    expect_lte(sc$domain[2] - max(sc$breaks), min(diff(sc$breaks)))
  }
})
