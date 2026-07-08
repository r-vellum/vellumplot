# Alt text: quill authors an accessible name (plot title) + text alternative
# (plot_alt) and wires them into the compiled scene's title/desc, so every
# compiled plot is an accessible SVG. See R/alt.R.

test_that("plot_alt() auto-describes chart type, mappings, and n", {
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg, color = hp)
  alt <- plot_alt(p)
  expect_match(alt, "scatter plot", fixed = TRUE)
  expect_match(alt, "wt", fixed = TRUE) # x mapping
  expect_match(alt, "mpg", fixed = TRUE) # y mapping
  expect_match(alt, "colour shows hp", fixed = TRUE) # colour mapping
  expect_match(alt, "32 observations", fixed = TRUE) # nrow(mtcars)
})

test_that("labs(alt=) overrides the auto description", {
  manual <- "Heavier cars get fewer miles per gallon."
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    labs(alt = manual)
  expect_identical(plot_alt(p), manual)
})

test_that("axis-title overrides from labs() flow into the alt text", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    labs(x = "Weight (1000 lbs)", y = "Miles per gallon")
  alt <- plot_alt(p)
  expect_match(alt, "Weight (1000 lbs)", fixed = TRUE)
  expect_match(alt, "Miles per gallon", fixed = TRUE)
})

test_that("multiple marks are all named", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    mark_smooth(x = wt, y = mpg)
  alt <- plot_alt(p)
  expect_match(alt, "scatter plot", fixed = TRUE)
  expect_match(alt, "smoothed-trend plot", fixed = TRUE)
})

test_that("faceting is reported", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    facet_wrap(~cyl)
  expect_match(plot_alt(p), "Faceted by cyl", fixed = TRUE)
})

test_that("a compiled plot carries title (name) and alt (desc) in its SVG", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    labs(title = "Fuel economy", alt = "Weight versus fuel economy for 32 cars.")
  svg <- vellum::scene_svg(as_vellum_scene(p))
  expect_match(svg, 'role="img"', fixed = TRUE)
  expect_match(svg, "<title", fixed = TRUE)
  expect_match(svg, "Fuel economy", fixed = TRUE)
  expect_match(svg, "<desc", fixed = TRUE)
  expect_match(svg, "Weight versus fuel economy for 32 cars.", fixed = TRUE)
})

test_that("an untitled plot still gets an auto description (accessible by default)", {
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
  svg <- vellum::scene_svg(as_vellum_scene(p))
  # no plot title -> no accessible name, but the auto alt is always present
  expect_no_match(svg, "<title", fixed = TRUE)
  expect_match(svg, "<desc", fixed = TRUE)
  expect_match(svg, "scatter plot", fixed = TRUE)
})

test_that("plot_alt() summarises a composition", {
  p1 <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
  p2 <- vplot(mtcars) |> mark_bar(x = cyl)
  alt <- plot_alt(hconcat(p1, p2))
  expect_match(alt, "composition of 2 plots", fixed = TRUE)
})

test_that("plot_alt() errors on a non-plot", {
  expect_error(plot_alt(mtcars), "PlotSpec")
})
