# Small stat mismatches against ggplot2 (VP-23).

test_that("mark_count()'s prop is the share of the point's group", {
  r <- vellumplot:::.resolve_layers(
    vplot(mtcars) |>
      mark_count(x = cyl, y = gear, color = factor(am), size = after_stat(prop))
  )[[1]]
  sums <- as.vector(tapply(r$values$size, as.character(r$values$color), sum))
  expect_equal(sums, c(1, 1))
  # ungrouped: the share of the whole layer
  r1 <- vellumplot:::.resolve_layers(
    vplot(mtcars) |> mark_count(x = cyl, y = gear, size = after_stat(prop))
  )[[1]]
  expect_equal(sum(r1$values$size), 1)
})

test_that("the count stat computes prop, with density kept as an alias", {
  p <- vplot(mtcars) |> mark_bar(x = factor(cyl), y = after_stat(prop))
  r <- vellumplot:::.resolve_layers(p)[[1]]
  expect_equal(r$values$y, as.numeric(table(mtcars$cyl)) / nrow(mtcars))
  q <- vplot(mtcars) |> mark_bar(x = factor(cyl), y = after_stat(density))
  expect_equal(vellumplot:::.resolve_layers(q)[[1]]$values$y, r$values$y)
  # grouped: each group sums to 1
  g <- vplot(mtcars) |>
    mark_bar(
      x = factor(cyl),
      y = after_stat(prop),
      fill = factor(am),
      position = "identity"
    )
  rg <- vellumplot:::.resolve_layers(g)[[1]]
  expect_equal(
    as.vector(tapply(rg$values$y, as.character(rg$values$fill), sum)),
    c(1, 1)
  )
})

test_that("mark_density() is evaluated over the data range, as ggplot2's", {
  r <- vellumplot:::.resolve_layers(vplot(mtcars) |> mark_density(x = mpg))[[1]]
  expect_equal(range(r$values$x), range(mtcars$mpg))
  ref <- stats::density(
    mtcars$mpg,
    from = min(mtcars$mpg),
    to = max(mtcars$mpg)
  )
  expect_equal(r$values$y, ref$y)
  # grouped: every group spans the layer's range ...
  g <- vellumplot:::.resolve_layers(
    vplot(mtcars) |> mark_density(x = mpg, fill = factor(cyl))
  )[[1]]
  for (lev in c("4", "6", "8")) {
    sel <- as.character(g$values$fill) == lev
    expect_equal(range(g$values$x[sel]), range(mtcars$mpg))
  }
  # ... unless trim = TRUE, which ends each at its own data
  t <- vellumplot:::.resolve_layers(
    vplot(mtcars) |> mark_density(x = mpg, fill = factor(cyl), trim = TRUE)
  )[[1]]
  sel <- as.character(t$values$fill) == "6"
  expect_equal(range(t$values$x[sel]), range(mtcars$mpg[mtcars$cyl == 6]))
})

test_that("mark_hull(expand =) grows every hull by the same margin", {
  # a long thin hull and a small square, in a layer spanning 0..10 each way
  d <- data.frame(
    x = c(0, 10, 10, 0, 4, 5, 5, 4),
    y = c(0, 0, 0.2, 0.2, 9, 9, 10, 10),
    g = rep(c("thin", "square"), each = 4)
  )
  r <- vellumplot:::.resolve_layers(
    vplot(d) |> mark_hull(x = x, y = y, color = g, expand = 0.1)
  )[[1]]
  for (lev in c("thin", "square")) {
    sel <- as.character(r$values$color) == lev
    src <- d[d$g == lev, ]
    # 10% of the 10-unit span on each side
    expect_equal(min(r$values$x[sel]), min(src$x) - 1, tolerance = 1e-6)
    expect_equal(max(r$values$x[sel]), max(src$x) + 1, tolerance = 1e-6)
    expect_equal(min(r$values$y[sel]), min(src$y) - 1, tolerance = 1e-6)
    expect_equal(max(r$values$y[sel]), max(src$y) + 1, tolerance = 1e-6)
  }
  # every outline point is the margin away from the tight hull
  sel <- as.character(r$values$color) == "square"
  cx <- pmin(pmax(r$values$x[sel], 4), 5)
  cy <- pmin(pmax(r$values$y[sel], 9), 10)
  dist <- sqrt((r$values$x[sel] - cx)^2 + (r$values$y[sel] - cy)^2)
  expect_equal(dist, rep(1, length(dist)), tolerance = 1e-6)
})
