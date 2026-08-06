# G5: data-space statistical transforms (bin, count, smooth) + after_stat().

test_that("mark_histogram() records a bin stat", {
  p <- vplot(mtcars) |> mark_histogram(x = mpg, bins = 8)
  L <- p@layers[[1]]
  expect_identical(L@mark, "bar")
  expect_identical(L@stat, "bin")
  expect_equal(L@stat_params$bins, 8)
})

test_that("after_stat() marks a stage-2 channel", {
  p <- vplot(mtcars) |> mark_histogram(x = mpg, y = after_stat(density))
  ch <- p@layers[[1]]@encoding$y
  expect_true(ch@after)
  expect_identical(rlang::as_label(rlang::quo_get_expr(ch@expr)), "density")
})

test_that("the bin stat counts all rows across bins", {
  p <- vplot(mtcars) |> mark_histogram(x = mpg, bins = 8)
  r <- vellumplot:::.resolve_layers(p)[[1]]
  expect_equal(length(r$values$x), 8) # one row per bin
  expect_equal(sum(r$values$y), nrow(mtcars)) # counts cover every row
})

test_that("after_stat(density) integrates to ~1 over bin widths", {
  p <- vplot(mtcars) |>
    mark_histogram(x = mpg, bins = 10, y = after_stat(density))
  r <- vellumplot:::.resolve_layers(p)[[1]]
  width <- diff(sort(r$values$x))[1]
  expect_equal(sum(r$values$y) * width, 1, tolerance = 0.02)
})

test_that("the bin stat carries the bin width into the layer (bars touch)", {
  p <- vplot(mtcars) |> mark_histogram(x = mpg, bins = 8)
  r <- vellumplot:::.resolve_layers(p)[[1]]
  expect_false(is.null(r$values$width))
  # the propagated width equals the spacing between adjacent bin centres, so
  # histogram bars meet edge-to-edge rather than leaving a 10% gap
  expect_equal(r$values$width[1], diff(sort(r$values$x))[1], tolerance = 1e-8)
})

test_that("grouped histogram density integrates to 1 per group (H26)", {
  set.seed(1)
  df <- data.frame(
    v = c(rnorm(200), rnorm(80) + 5), # unequal group sizes
    g = factor(rep(c("a", "b"), c(200, 80)))
  )
  # position = "identity" so y stays the per-group density (stacking would sum
  # the groups' densities per bin and hide the normalisation).
  r <- vellumplot:::.resolve_layers(
    vplot(df) |>
      mark_histogram(
        x = v,
        fill = g,
        y = after_stat(density),
        bins = 12,
        position = "identity"
      )
  )[[1]]
  grp <- as.character(r$values$fill)
  dens <- r$values$y
  w <- r$values$width
  for (gi in unique(grp)) {
    sel <- grp == gi
    expect_equal(sum(dens[sel]) * w[sel][1], 1, tolerance = 0.02)
  }
})

test_that("binning stats abort cleanly on all-NA / empty input (H25)", {
  na_df <- data.frame(v = rep(NA_real_, 5), w = rep(NA_real_, 5))
  expect_error(
    vellumplot:::.resolve_layers(vplot(na_df) |> mark_histogram(x = v)),
    "finite"
  )
  expect_error(
    vellumplot:::.resolve_layers(vplot(na_df) |> mark_bin2d(x = v, y = w)),
    "finite"
  )
  expect_error(
    vellumplot:::.resolve_layers(vplot(na_df) |> mark_hex(x = v, y = w)),
    "finite"
  )
})

test_that("the count stat preserves a custom factor level order on x", {
  d <- data.frame(
    g = factor(c("hi", "lo", "mid"), levels = c("lo", "mid", "hi"))
  )
  p <- vplot(d) |> mark_bar(x = g)
  r <- vellumplot:::.resolve_layers(p)[[1]]
  expect_s3_class(r$values$x, "factor")
  expect_equal(levels(r$values$x), c("lo", "mid", "hi"))
})

test_that("the count stat tallies rows per category", {
  d <- data.frame(g = c("a", "a", "b"))
  p <- vplot(d) |> mark_bar(x = g)
  r <- vellumplot:::.resolve_layers(p)[[1]]
  expect_setequal(r$values$x, c("a", "b"))
  expect_equal(r$values$y[match("a", r$values$x)], 2)
})

test_that("the smooth stat fits a dense line with a ribbon", {
  p <- vplot(mtcars) |> mark_smooth(x = wt, y = mpg, method = "lm")
  r <- vellumplot:::.resolve_layers(p)[[1]]
  expect_equal(r$n, 80) # dense prediction grid
  expect_false(is.null(r$values$ymin))
  expect_true(all(r$values$ymin <= r$values$y & r$values$y <= r$values$ymax))
  # fitted y replaces the raw response (negative slope for wt vs mpg)
  expect_lt(r$values$y[80], r$values$y[1])
})

test_that("smooth respects se = FALSE", {
  p <- vplot(mtcars) |> mark_smooth(x = wt, y = mpg, se = FALSE)
  r <- vellumplot:::.resolve_layers(p)[[1]]
  expect_null(r$values$ymin)
})

test_that("the y axis covers the smooth ribbon extent", {
  p <- vplot(mtcars) |> mark_smooth(x = wt, y = mpg)
  sc <- vellumplot:::.build_panels(p)$scales
  r <- vellumplot:::.resolve_layers(p)[[1]]
  expect_lte(sc$y$domain[1], min(r$values$ymin))
  expect_gte(sc$y$domain[2], max(r$values$ymax))
})

test_that("after_stat without a stat is an error", {
  expect_error(
    vellum::as_vellum_scene(
      vplot(mtcars) |> mark_point(x = wt, y = after_stat(count))
    ),
    "after_stat"
  )
})

test_that("an unknown smoothing method errors", {
  expect_error(
    vellum::as_vellum_scene(
      vplot(mtcars) |> mark_smooth(x = wt, y = mpg, method = "bogus")
    ),
    "method"
  )
})

test_that("loess smoothing fits a dense line with a ribbon", {
  p <- vplot(mtcars) |> mark_smooth(x = wt, y = mpg, method = "loess")
  r <- vellumplot:::.resolve_layers(p)[[1]]
  expect_equal(r$n, 80)
  expect_false(is.null(r$values$ymin))
  expect_true(all(r$values$ymin <= r$values$y & r$values$y <= r$values$ymax))
})

test_that("auto smoothing resolves to loess for a small group", {
  # auto (< 1000 points) == loess: same fitted line as an explicit loess.
  auto <- vellumplot:::.resolve_layers(
    vplot(mtcars) |> mark_smooth(x = wt, y = mpg)
  )[[1]]
  loess <- vellumplot:::.resolve_layers(
    vplot(mtcars) |> mark_smooth(x = wt, y = mpg, method = "loess")
  )[[1]]
  expect_equal(auto$values$y, loess$values$y)
})

test_that("glm smoothing back-transforms a logistic fit into (0, 1)", {
  skip_if_not_installed("MASS")
  withr::with_seed(1, {
    d <- data.frame(x = runif(120, 0, 10))
    d$y <- rbinom(120, 1, stats::plogis(d$x - 5))
  })
  p <- vplot(d) |>
    mark_smooth(
      x = x,
      y = y,
      method = "glm",
      method.args = list(family = binomial())
    )
  r <- vellumplot:::.resolve_layers(p)[[1]]
  expect_true(all(r$values$y > 0 & r$values$y < 1))
  expect_true(all(r$values$ymin >= 0 & r$values$ymax <= 1))
  expect_lt(r$values$y[1], r$values$y[80]) # increasing
})

test_that("gam smoothing needs mgcv and fits a smooth", {
  skip_if_not_installed("mgcv")
  p <- vplot(mtcars) |> mark_smooth(x = wt, y = mpg, method = "gam")
  r <- vellumplot:::.resolve_layers(p)[[1]]
  expect_equal(r$n, 80)
  expect_false(is.null(r$values$ymin))
})

test_that("quantile regression draws a line only, one tau per layer", {
  skip_if_not_installed("quantreg")
  p <- vplot(mtcars) |>
    mark_smooth(x = wt, y = mpg, method = "rq", method.args = list(tau = 0.9))
  r <- vellumplot:::.resolve_layers(p)[[1]]
  expect_equal(r$n, 80)
  expect_null(r$values$ymin) # no confidence ribbon for rq
  expect_error(
    vellum::as_vellum_scene(
      vplot(mtcars) |>
        mark_smooth(
          x = wt,
          y = mpg,
          method = "rq",
          method.args = list(tau = c(0.1, 0.9))
        )
    ),
    "single"
  )
})

test_that("statistical marks render", {
  f1 <- local_tempfile(fileext = ".png")
  render_plot(vplot(mtcars) |> mark_histogram(x = mpg, bins = 10), f1)
  expect_gt(file.info(f1)$size, 0)
  f2 <- local_tempfile(fileext = ".png")
  render_plot(
    vplot(mtcars) |>
      mark_point(x = wt, y = mpg) |>
      mark_smooth(x = wt, y = mpg),
    f2
  )
  expect_gt(file.info(f2)$size, 0)
})

test_that("an empty facet cell does not crash an after_stat mark (renders blank)", {
  # facet_grid with a missing a x b combination leaves an empty cell; the stat
  # short-circuits to an empty layer, and its after_stat channels (count/n/...)
  # must not be evaluated against the column-less frame.
  set.seed(1)
  d <- data.frame(
    x = rnorm(60),
    y = rnorm(60),
    a = rep(c("p", "q"), 30),
    b = rep(c("m", "n", "o"), each = 20)
  )
  d <- d[!(d$a == "p" & d$b == "o"), ] # drop the p x o cell
  expect_no_error(plot_svg(
    vplot(d) |> mark_bin2d(x = x, y = y) |> facet_grid(a ~ b)
  ))
  expect_no_error(plot_svg(
    vplot(d) |> mark_count(x = x, y = y) |> facet_grid(a ~ b)
  ))
  expect_no_error(plot_svg(
    vplot(d) |> mark_hex(x = x, y = y) |> facet_grid(a ~ b)
  ))
  # an explicit after_stat() on a histogram in an empty cell is fine too
  expect_no_error(plot_svg(
    vplot(d) |>
      mark_histogram(x = x, y = after_stat(density)) |>
      facet_grid(a ~ b)
  ))
})
