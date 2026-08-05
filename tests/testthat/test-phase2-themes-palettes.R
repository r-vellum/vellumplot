# Phase 2: theme presets, viridis/brewer scales, gradientn, limits=, layouts.

svg <- function(p) paste(plot_svg(p), collapse = "")
panels <- function(p) vellumplot:::.build_panels(p)
p <- vplot(mtcars) |> mark_point(x = wt, y = mpg, color = hp)
pd <- vplot(mtcars) |> mark_point(x = wt, y = mpg, color = factor(cyl))

test_that("new theme presets render and round-trip", {
  for (f in list(theme_light, theme_dark, theme_linedraw)) {
    expect_no_error(svg(f(pd)))
    expect_no_error(suppressWarnings(from_spec(as_spec(f(pd)))))
  }
})

test_that("viridis / brewer / distiller scales render", {
  expect_no_error(svg(scale_color_viridis_c(p)))
  expect_no_error(svg(scale_color_viridis_c(p, option = "inferno")))
  expect_no_error(svg(scale_color_viridis_d(pd)))
  expect_no_error(svg(scale_fill_viridis_d(
    pd |> mark_point(x = wt, y = mpg, fill = factor(cyl))
  )))
  expect_no_error(svg(scale_color_brewer(pd, palette = "Set 2")))
  expect_no_error(svg(scale_color_distiller(p, palette = "Blues")))
  expect_no_error(svg(scale_colour_viridis_c(p))) # British alias
})

test_that("gradientn interpolates an n-stop ramp", {
  expect_no_error(svg(scale_color_gradientn(
    p,
    colours = c("navy", "white", "firebrick")
  )))
  expect_error(scale_color_gradientn(p, colours = 1:3), "colours")
})

test_that("limits= fixes the continuous colour domain", {
  b <- panels(scale_color_continuous(p, limits = c(50, 250)))
  expect_equal(b$scales$color$range, c(50, 250))
  expect_error(scale_color_continuous(p, limits = c(1, 2, 3)), "length-2")
})

test_that("the widened layout registry resolves the new igraph layouts", {
  skip_if_not_installed("igraph")
  g <- igraph::sample_gnp(25, 0.15)
  for (lay in c("gem", "graphopt", "dh", "lgl")) {
    expect_no_error(svg(
      vgraph(g, layout = lay) |> mark_edges() |> mark_nodes()
    ))
  }
})
