# Feature 2: Vega-Lite interoperability (spec_to_vegalite / spec_from_vegalite).

test_that("a basic plot maps to a Vega-Lite single view", {
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg, color = cyl)
  vl <- spec_to_vegalite(p)
  expect_identical(vl$mark$type, "point")
  expect_identical(vl$encoding$x$field, "wt")
  expect_identical(vl$encoding$y$field, "mpg")
  # fill/color aesthetic collapses to the Vega-Lite `color` channel
  expect_identical(vl$encoding$color$field, "cyl")
})

test_that("a colour scale folds into the encoding (range + title)", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, color = gear) |>
    scale_color_manual(values = c("red", "green", "blue"), name = "Gears")
  vl <- spec_to_vegalite(p)
  expect_identical(vl$encoding$color$scale$range, c("red", "green", "blue"))
  expect_identical(vl$encoding$color$title, "Gears")
})

test_that("histogram maps to bar + bin + count", {
  p <- vplot(mtcars) |> mark_histogram(x = mpg)
  vl <- spec_to_vegalite(p)
  expect_identical(vl$mark$type, "bar")
  expect_true(isTRUE(vl$encoding$x$bin))
  expect_identical(vl$encoding$y$aggregate, "count")
})

test_that("facet_wrap maps to the Vega-Lite facet operator", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    facet_wrap(~cyl, ncol = 2L)
  vl <- spec_to_vegalite(p)
  expect_identical(vl$facet$field, "cyl")
  expect_identical(vl$columns, 2L)
  expect_identical(vl$spec$mark$type, "point")
})

test_that("round-trip preserves marks and field encodings", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, color = cyl) |>
    mark_line(x = wt, y = mpg)
  vl <- spec_to_vegalite(p)
  p2 <- spec_from_vegalite(vl, data = mtcars)
  s2 <- as_spec(p2)
  expect_length(s2$layers, 2)
  expect_identical(s2$layers[[1]]$mark, "point")
  expect_identical(s2$layers[[2]]$mark, "line")
  expect_identical(s2$layers[[1]]$encoding$x$field, "wt")
  expect_identical(s2$layers[[1]]$encoding$color$field, "cyl")
})

test_that("importing a hand-written Vega-Lite spec yields a compilable plot", {
  skip_if_not_installed("jsonlite")
  vl <- '{
    "$schema": "https://vega.github.io/schema/vega-lite/v5.json",
    "data": {"values": [
      {"a": 1, "b": 2, "g": "x"},
      {"a": 2, "b": 5, "g": "y"},
      {"a": 3, "b": 4, "g": "x"}
    ]},
    "mark": "point",
    "encoding": {
      "x": {"field": "a", "type": "quantitative"},
      "y": {"field": "b", "type": "quantitative"},
      "color": {"field": "g", "type": "nominal"}
    }
  }'
  p <- spec_from_vegalite(vl)
  expect_s7_class(p, PlotSpec)
  expect_no_error(vellum::as_vellum_scene(p))
})

test_that("unmappable features are reported, not silently dropped", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, effects = list(glow())) |>
    coord_polar()
  expect_warning(spec_to_vegalite(p), "dropped")
})

test_that("spec_to_vegalite can emit JSON", {
  skip_if_not_installed("jsonlite")
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
  json <- spec_to_vegalite(p, json = TRUE)
  expect_match(json, "vega-lite")
})
