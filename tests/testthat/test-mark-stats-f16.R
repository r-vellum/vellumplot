# The stat-based marks of F-16: mark_density_2d() / mark_density_2d_filled(),
# mark_quantile(), and horizontal error bars (orientation on mark_errorbar()).

prov_of <- function(p, mark) {
  pr <- attr(as_vellum_scene(p), "vellumplot_provenance")
  Filter(function(e) identical(e$mark, mark), pr)
}
test_that("mark_density_2d() traces ggplot2's geom_density_2d() contours", {
  skip_if_not_installed("MASS")
  p <- vplot(faithful) |> mark_density_2d(x = eruptions, y = waiting)
  L <- p@layers[[1]]
  expect_identical(L@mark, "contour")
  expect_identical(L@stat, "density_2d")
  # not coloured by level by default (ggplot2 draws one colour)
  expect_null(L@encoding$color)
  v <- vellumplot:::.resolve_layers(p)[[1]]$values
  # ggplot2::layer_data(ggplot(faithful, aes(eruptions, waiting)) +
  #   geom_density_2d()): 21 pieces, 2112 vertices, levels 0.002..0.024
  expect_length(unique(v$.piece), 21L)
  expect_length(v$x, 2112L)
  lv <- vellumplot:::.resolve_layers(
    vplot(faithful) |>
      mark_density_2d(x = eruptions, y = waiting, color = after_stat(level))
  )[[1]]$values$color
  expect_equal(sort(unique(lv)), seq(0.002, 0.024, by = 0.002))
})

test_that("contour_var, adjust and h follow ggplot2", {
  skip_if_not_installed("MASS")
  lv <- vellumplot:::.resolve_layers(
    vplot(faithful) |>
      mark_density_2d(
        x = eruptions,
        y = waiting,
        contour_var = "count",
        adjust = c(0.5, 1),
        color = after_stat(level)
      )
  )[[1]]
  # ggplot2: levels 1..8, 15 pieces, 1475 vertices
  expect_equal(sort(unique(lv$values$color)), 1:8)
  expect_length(unique(lv$values$.piece), 15L)
  expect_length(lv$values$x, 1475L)
  nd <- vellumplot:::.resolve_layers(
    vplot(faithful) |>
      mark_density_2d(
        x = eruptions,
        y = waiting,
        contour_var = "ndensity",
        color = after_stat(level)
      )
  )[[1]]$values$color
  expect_lt(max(nd), 1)
  # an explicit bandwidth equal to the default changes nothing
  h <- c(
    MASS::bandwidth.nrd(faithful$eruptions),
    MASS::bandwidth.nrd(faithful$waiting)
  )
  a <- vellumplot:::.resolve_layers(
    vplot(faithful) |> mark_density_2d(x = eruptions, y = waiting)
  )[[1]]$values
  b <- vellumplot:::.resolve_layers(
    vplot(faithful) |> mark_density_2d(x = eruptions, y = waiting, h = h)
  )[[1]]$values
  expect_identical(a$x, b$x)
  expect_error(
    mark_density_2d(vplot(faithful), contour_var = "foo"),
    "contour_var"
  )
  expect_error(mark_density_2d(vplot(faithful), adjust = c(1, 2, 3)), "adjust")
  expect_error(mark_density_2d(vplot(faithful), h = -1), "h")
})

test_that("grouped 2-D densities estimate one field per group", {
  skip_if_not_installed("MASS")
  v <- vellumplot:::.resolve_layers(
    vplot(iris) |>
      mark_density_2d(x = Sepal.Length, y = Sepal.Width, color = Species)
  )[[1]]$values
  # ggplot2 (colour = Species): 15 (group, piece) lines, 1914 vertices
  expect_length(unique(v$.piece), 15L)
  expect_length(v$x, 1914L)
  expect_no_error(vellum::as_vellum_scene(
    vplot(iris) |>
      mark_density_2d(x = Sepal.Length, y = Sepal.Width, color = Species) |>
      facet_wrap(~Species)
  ))
})

test_that("filled 2-D density bands close inside the data grid", {
  skip_if_not_installed("MASS")
  p <- vplot(faithful) |> mark_density_2d_filled(x = eruptions, y = waiting)
  expect_identical(p@layers[[1]]@mark, "contour_filled")
  v <- vellumplot:::.resolve_layers(p)[[1]]$values
  # within the data range (the padded border is not drawn)
  expect_equal(range(v$x), range(faithful$eruptions), tolerance = 1e-4)
  expect_equal(range(v$y), range(faithful$waiting), tolerance = 1e-4)
  # the empty top-left corner (eruptions 1.8, waiting 90) is painted by
  # ggplot2's lowest band, (0, b1], in the darkest colour -- not left as the
  # light panel background (VP-45)
  img <- render_px(p)
  expect_lt(
    mean(img[round(dim(img)[1] * 0.12), round(dim(img)[2] * 0.2), 1:3]),
    0.3
  )
})

test_that("mark_quantile() matches ggplot2's geom_quantile()", {
  skip_if_not_installed("quantreg")
  p <- vplot(mtcars) |>
    mark_quantile(x = disp, y = mpg, quantiles = c(0.1, 0.9))
  v <- vellumplot:::.resolve_layers(p)[[1]]$values
  # ggplot2::layer_data(ggplot(mtcars, aes(disp, mpg)) +
  #   geom_quantile(quantiles = c(0.1, 0.9)))$y[c(1, 100, 101, 200)]
  expect_equal(
    v$y[c(1, 100, 101, 200)],
    c(22.00712, 9.49122, 32.77014, 13.24532),
    tolerance = 1e-6
  )
  expect_equal(range(v$x), range(mtcars$disp))
  expect_length(prov_of(p, "line"), 2L)
  # one line per (group, quantile)
  g <- vplot(mtcars) |> mark_quantile(x = disp, y = mpg, color = factor(am))
  expect_length(prov_of(g, "line"), 6L)
  # after_stat(quantile) maps
  q <- vplot(mtcars) |>
    mark_quantile(x = disp, y = mpg, linetype = after_stat(factor(quantile)))
  expect_no_error(vellum::as_vellum_scene(q))
  expect_error(mark_quantile(vplot(mtcars), quantiles = 1.5), "quantiles")
  expect_error(mark_quantile(vplot(mtcars), formula = "y ~ x"), "formula")
})

test_that("error bars with xmin/xmax are horizontal (geom_errorbarh)", {
  est <- data.frame(
    term = c("a", "b", "c"),
    est = c(0, 0.8, 1.7),
    lo = c(-1, 0.2, 1),
    hi = c(1, 1.5, 2.4)
  )
  p <- vplot(est) |> mark_errorbar(y = term, xmin = lo, xmax = hi)
  L <- vellumplot:::.resolve_layers(p)[[1]]
  expect_identical(vellumplot:::.range_orient(L), "y")
  el <- vellum::scene_model(vellum::as_vellum_scene(p))$elements
  bars <- el[grepl("errorbar", el$id) & el$mark == "segment", ]
  # 3 horizontal bars + 6 vertical caps
  expect_identical(sum(bars$h < 1e-6 & bars$w > 0), 3L)
  expect_identical(sum(bars$w < 1e-6 & bars$h > 0), 6L)
  # vertical stays the default; orientation forces either
  v <- vplot(est) |> mark_errorbar(x = term, ymin = lo, ymax = hi)
  expect_identical(
    vellumplot:::.range_orient(vellumplot:::.resolve_layers(v)[[1]]),
    "x"
  )
  expect_error(mark_errorbar(vplot(est), orientation = "z"), "orientation")
  for (q in list(
    vplot(est) |> mark_linerange(y = term, xmin = lo, xmax = hi),
    vplot(est) |> mark_pointrange(y = term, x = est, xmin = lo, xmax = hi)
  )) {
    expect_identical(
      vellumplot:::.range_orient(vellumplot:::.resolve_layers(q)[[1]]),
      "y"
    )
    expect_no_error(vellum::as_vellum_scene(q))
  }
})

test_that("the stat marks round-trip through the spec serializer", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("quantreg")
  for (p in list(
    vplot(faithful) |>
      mark_density_2d(x = eruptions, y = waiting, adjust = c(1, 2)),
    vplot(faithful) |> mark_density_2d_filled(x = eruptions, y = waiting),
    vplot(mtcars) |> mark_quantile(x = disp, y = mpg, quantiles = c(0.2, 0.8)),
    vplot(mtcars) |>
      mark_errorbar(y = factor(cyl), xmin = mpg - 1, xmax = mpg + 1)
  )) {
    s <- as_spec(p)
    p2 <- from_spec(s)
    expect_identical(as_spec(p2)$layers, s$layers)
    expect_no_error(vellum::as_vellum_scene(p2))
  }
})
