# G7: datashaded density marks.

test_that("mark_datashade() records a datashade layer", {
  p <- vplot(data.frame(x = 1:3, y = 1:3)) |> mark_datashade(x = x, y = y)
  L <- p@layers[[1]]
  expect_identical(L@mark, "datashade")
  expect_equal(L@stat_params$how, "eq_hist")
})

test_that("mark_point(auto = TRUE) records the auto flag", {
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg, auto = TRUE)
  expect_true(p@layers[[1]]@stat_params$auto)
})

test_that("a datashade layer still trains position scales from the data", {
  d <- data.frame(x = c(0, 10), y = c(-5, 5))
  p <- vplot(d) |> mark_datashade(x = x, y = y)
  sc <- vellumplot:::.build_panels(p)$scales
  expect_lt(sc$x$domain[1], 0)
  expect_gt(sc$x$domain[2], 10)
})

test_that("datashade renders, and auto falls back to markers below the threshold", {
  set.seed(1)
  n <- 20000
  df <- data.frame(x = rnorm(n), y = rnorm(n))
  f1 <- local_tempfile(fileext = ".png")
  render_plot(vplot(df) |> mark_datashade(x = x, y = y), f1)
  expect_gt(file.info(f1)$size, 0)
  # small data with auto = TRUE just draws normal points (no error)
  f2 <- local_tempfile(fileext = ".png")
  render_plot(vplot(mtcars) |> mark_point(x = wt, y = mpg, auto = TRUE), f2)
  expect_gt(file.info(f2)$size, 0)
})

test_that("mark_datashade(blend=) wires the layer blend (per-category overlay)", {
  set.seed(1)
  n <- 5000
  df <- data.frame(x = rnorm(n), y = rnorm(n), g = sample(c("a", "b"), n, TRUE))
  p <- vplot(df) |>
    mark_datashade(
      x = x,
      y = y,
      data = subset(df, g == "a"),
      colors = c("black", "#e41a1c"),
      blend = "screen"
    ) |>
    mark_datashade(
      x = x,
      y = y,
      data = subset(df, g == "b"),
      colors = c("black", "#377eb8"),
      blend = "screen"
    )
  expect_identical(
    vapply(p@layers, function(L) L@blend, ""),
    c("screen", "screen")
  )
  f <- local_tempfile(fileext = ".png")
  expect_no_error(render_plot(p, f))
})

test_that("datashade feeds training only a coordinate range, keeps full coords for the emitter", {
  set.seed(1)
  n <- 1000
  df <- data.frame(x = rnorm(n), y = rnorm(n))
  p <- vplot(df) |> mark_datashade(x = x, y = y)
  L <- vellumplot:::.resolve_layers(p)[[1]]
  # training sees a 2-value range, not the full cloud...
  expect_length(L$values$x, 2L)
  expect_equal(L$values$x, range(df$x))
  # ...while the emitter still has every point
  expect_length(L$ds$x, n)
})

test_that("user scale limits short-circuit the position scan (no data-derived range)", {
  df <- data.frame(x = c(0, 100), y = c(0, 100))
  p <- vplot(df) |>
    mark_datashade(x = x, y = y) |>
    scale_x_continuous(limits = c(-10, 200))
  b <- vellumplot:::.build_panels(p)
  # domain comes from the user limits (expanded), not the data range
  expect_lt(b$scales$x$domain[1], 0)
  expect_gt(b$scales$x$domain[2], 100)
})

# --- categorical (count_cat) shading via a mapped colour aesthetic --------------

test_that("mark_datashade(color=) keeps the full category vector for the emitter, trains only levels", {
  set.seed(1)
  n <- 1000
  df <- data.frame(x = rnorm(n), y = rnorm(n), g = sample(c("a", "b", "c"), n, TRUE))
  L <- vellumplot:::.resolve_layers(vplot(df) |> mark_datashade(x = x, y = y, color = g))[[1]]
  # full-length category kept for aggregation...
  expect_length(L$ds$cat, n)
  # ...but colour training sees only the unique levels, never the full cloud
  expect_setequal(L$values$color, c("a", "b", "c"))
  expect_length(L$values$color, 3L)
})

test_that("mark_datashade(color=) trains a discrete colour scale with a square-key legend", {
  set.seed(1)
  n <- 2000
  df <- data.frame(x = rnorm(n), y = rnorm(n), g = sample(c("a", "b"), n, TRUE))
  b <- vellumplot:::.build_panels(vplot(df) |> mark_datashade(x = x, y = y, color = g))
  expect_identical(b$scales$color$kind, "discrete")
  expect_setequal(b$scales$color$levels, c("a", "b"))
  expect_identical(b$scales$color$key_glyph, "square")
})

test_that("categorical datashade renders (one raster + legend), fill= also works", {
  set.seed(1)
  n <- 5000
  df <- data.frame(
    x = c(rnorm(n, -2, 0.3), rnorm(n, 2, 0.3)),
    y = c(rnorm(n, -2, 0.3), rnorm(n, 2, 0.3)),
    g = rep(c("a", "b"), each = n)
  )
  f1 <- local_tempfile(fileext = ".png")
  expect_no_error(render_plot(vplot(df) |> mark_datashade(x = x, y = y, color = g), f1))
  expect_gt(file.info(f1)$size, 0)
  # fill maps to the same colour scale
  f2 <- local_tempfile(fileext = ".png")
  expect_no_error(render_plot(vplot(df) |> mark_datashade(x = x, y = y, fill = g), f2))
  expect_gt(file.info(f2)$size, 0)
})

test_that("span / clip are recorded and passed through without error", {
  set.seed(1)
  n <- 5000
  df <- data.frame(x = rnorm(n), y = rnorm(n))
  p <- vplot(df) |> mark_datashade(x = x, y = y, clip = c(0.02, 0.98))
  expect_equal(p@layers[[1]]@stat_params$clip, c(0.02, 0.98))
  f <- local_tempfile(fileext = ".png")
  expect_no_error(render_plot(p, f))
  expect_no_error(render_plot(vplot(df) |> mark_datashade(x = x, y = y, span = c(1, 20)), f))
})
