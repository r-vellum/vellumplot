# Discrete legend scales drop unused factor levels by default, as ggplot2 does,
# and `drop = FALSE` keeps them (VP-91).

.legend_levels <- function(p, aes = "color") {
  vellumplot:::.build_panels(p)$scales[[aes]]$levels
}
.legend_colours <- function(p, aes = "color") {
  s <- vellumplot:::.build_panels(p)$scales[[aes]]
  stats::setNames(s$colors, s$levels)
}

d <- data.frame(
  x = 1:4,
  y = 1:4,
  g = factor(c("a", "c", "a", "c"), levels = c("a", "b", "c", "d")),
  f = c("p", "p", "q", "q")
)

test_that("unused factor levels get no legend key (VP-91)", {
  p <- vplot(d) |> mark_point(x = x, y = y, color = g)
  expect_equal(.legend_levels(p), c("a", "c"))
  expect_equal(
    .legend_levels(vplot(d) |> mark_bar(x = x, y = y, fill = g), "fill"),
    c("a", "c")
  )
  expect_equal(
    .legend_levels(vplot(d) |> mark_point(x = x, y = y, shape = g), "shape"),
    c("a", "c")
  )
})

test_that("colours go to the remaining levels, as in ggplot2", {
  # ggplot2's hue palette for two levels
  p <- vplot(d) |> mark_point(x = x, y = y, color = g) |> scale_color_hue()
  expect_equal(unname(.legend_colours(p)), c("#F8766D", "#00BFC4"))
  # the default palette: the first two colours, not the 1st and 3rd of four
  all4 <- .legend_colours(
    vplot(d) |>
      mark_point(x = x, y = y, color = g) |>
      scale_color_discrete(drop = FALSE)
  )
  two <- .legend_colours(vplot(d) |> mark_point(x = x, y = y, color = g))
  expect_equal(unname(two), unname(all4[1:2]))
})

test_that("drop = FALSE keeps every level; limits win", {
  for (p in list(
    vplot(d) |>
      mark_point(x = x, y = y, color = g) |>
      scale_color_discrete(drop = FALSE),
    vplot(d) |>
      mark_point(x = x, y = y, color = g) |>
      scale_color_manual(values = c("red", "blue", "green", "black"), drop = FALSE),
    vplot(d) |>
      mark_point(x = x, y = y, color = g) |>
      scale_color_viridis_d(drop = FALSE)
  )) {
    expect_equal(.legend_levels(p), c("a", "b", "c", "d"))
  }
  p <- vplot(d) |>
    mark_point(x = x, y = y, shape = g) |>
    scale_shape(drop = FALSE)
  expect_equal(.legend_levels(p, "shape"), c("a", "b", "c", "d"))
  # explicit limits decide the levels
  p <- vplot(d) |>
    mark_point(x = x, y = y, color = g) |>
    scale_color_manual(values = c("red", "blue", "green"), limits = c("a", "b", "c"))
  expect_equal(.legend_levels(p), c("a", "b", "c"))
  expect_error(scale_color_discrete(vplot(d), drop = NA), "drop")
})

test_that("a level used in only some facet panels stays", {
  d2 <- d
  d2$g <- factor(c("b", "c", "a", "a"), levels = c("a", "b", "c", "d"))
  p <- vplot(d2) |> mark_point(x = x, y = y, color = g) |> facet_wrap(~f)
  expect_equal(.legend_levels(p), c("a", "b", "c"))
})

test_that("the discrete size and alpha scales take drop = (VP-100)", {
  sz <- function(p) vellumplot:::.build_panels(p)$scales$size
  al <- function(p) vellumplot:::.build_panels(p)$scales$alpha
  ps <- vplot(d) |> mark_point(x = x, y = y, size = g)
  pa <- vplot(d) |> mark_point(x = x, y = y, alpha = g)
  expect_equal(suppressWarnings(sz(ps))$levels, c("a", "c"))
  expect_equal(suppressWarnings(al(pa))$levels, c("a", "c"))
  keep <- c("a", "b", "c", "d")
  for (f in list(scale_size_discrete, scale_size_ordinal)) {
    s <- suppressWarnings(sz(f(ps, drop = FALSE)))
    expect_equal(s$levels, keep)
    expect_length(s$legend_sizes, 4)
    expect_equal(suppressWarnings(sz(f(ps)))$levels, c("a", "c"))
  }
  s <- sz(scale_size_manual(ps, values = 1:4, drop = FALSE))
  expect_equal(s$levels, keep)
  for (f in list(scale_alpha_discrete, scale_alpha_ordinal)) {
    s <- suppressWarnings(al(f(pa, drop = FALSE)))
    expect_equal(s$levels, keep)
    expect_length(s$legend_alphas, 4)
  }
  s <- al(scale_alpha_manual(pa, values = c(0.2, 0.4, 0.6, 0.8), drop = FALSE))
  expect_equal(s$levels, keep)
  expect_error(scale_size_discrete(ps, drop = NA), "drop")
})
