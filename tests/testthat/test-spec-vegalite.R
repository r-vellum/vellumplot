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

test_that("spec_to_vegalite() degrades gracefully (never crashes) off the happy path", {
  df <- data.frame(x = 1:6, y = 1:6, g = rep(c("a", "b"), 3))
  # an unmapped mark, aesthetic, and scale transform each warn + drop, not crash
  expect_warning(
    vl <- spec_to_vegalite(vplot(mtcars) |> mark_smooth(x = wt, y = mpg)),
    "dropped"
  )
  expect_equal(vl$mark$type, "point") # unmapped mark falls back to point
  expect_warning(
    spec_to_vegalite(vplot(df) |> mark_line(x = x, y = y, linetype = g)),
    "linetype"
  )
  expect_warning(
    spec_to_vegalite(
      vplot(df) |>
        mark_point(x = x, y = y) |>
        scale_y_continuous(trans = "reverse")
    ),
    "transform"
  )
  # happy path is unchanged (no warning)
  expect_no_warning(spec_to_vegalite(
    vplot(df) |> mark_point(x = x, y = y, color = g)
  ))
  # import likewise degrades an unmapped mark/channel rather than aborting
  vl_in <- list(
    mark = list(type = "trail"),
    encoding = list(
      x = list(field = "a", type = "quantitative"),
      theta = list(field = "b")
    ),
    data = list(values = list(list(a = 1, b = 2)))
  )
  expect_warning(spec_from_vegalite(vl_in), "trail|theta")
})

test_that("free facet scales and resolve_scale() export as Vega-Lite resolve", {
  skip_if_not_installed("jsonlite")
  mt <- mtcars
  mt$gear <- factor(mt$gear)
  p <- vplot(mt) |>
    mark_point(x = wt, y = mpg, color = gear) |>
    facet_wrap(~cyl, scales = "free_y") |>
    resolve_scale(color = "independent")
  vl <- spec_to_vegalite(p)
  # hand-written expectation (validated against the Vega-Lite v5 schema)
  expected <- jsonlite::fromJSON(
    '{
      "scale": {"y": "independent", "color": "independent"},
      "axis": {"y": "independent"},
      "legend": {"color": "independent"}
    }',
    simplifyVector = FALSE
  )
  expect_identical(vl$resolve, expected)
  # the resolve sits on the facet operator, beside `facet` / `spec`
  expect_null(vl$spec$resolve)
  js <- jsonlite::fromJSON(
    spec_to_vegalite(p, json = TRUE),
    simplifyVector = FALSE
  )
  expect_identical(js$resolve, expected)

  # every mapped aesthetic, a grid facet, free x and y; fill shares `color`
  q <- vplot(mt) |>
    mark_point(x = wt, y = mpg, size = hp, shape = gear, alpha = qsec) |>
    facet_grid(am ~ cyl, scales = "free") |>
    resolve_scale(
      size = "independent",
      shape = "independent",
      alpha = "independent",
      fill = "independent"
    )
  r <- spec_to_vegalite(q)$resolve
  expect_setequal(
    names(r$scale),
    c("x", "y", "size", "shape", "opacity", "color")
  )
  expect_true(all(unlist(r$scale) == "independent"))
  expect_setequal(names(r$axis), c("x", "y"))
  expect_setequal(names(r$legend), c("size", "shape", "opacity", "color"))

  # shared (the default) adds nothing; a single panel has no facet, so no resolve
  expect_null(
    spec_to_vegalite(
      vplot(mt) |> mark_point(x = wt, y = mpg) |> facet_wrap(~cyl)
    )$resolve
  )
  expect_null(
    spec_to_vegalite(
      vplot(mt) |>
        mark_point(x = wt, y = mpg) |>
        resolve_scale(y = "independent")
    )$resolve
  )
})

test_that("a facet-level Vega-Lite resolve imports as the resolve lattice", {
  skip_if_not_installed("jsonlite")
  vl <- '{
    "data": {"values": [
      {"a": 1, "b": 2, "g": "x", "f": "p"},
      {"a": 2, "b": 5, "g": "y", "f": "q"},
      {"a": 3, "b": 4, "g": "x", "f": "q"}
    ]},
    "facet": {"field": "f", "type": "nominal"},
    "spec": {
      "mark": "point",
      "encoding": {
        "x": {"field": "a", "type": "quantitative"},
        "y": {"field": "b", "type": "quantitative"},
        "color": {"field": "g", "type": "nominal"}
      }
    },
    "resolve": {
      "scale": {"x": "independent", "color": "independent", "opacity": "shared"},
      "axis": {"x": "independent"},
      "legend": {"color": "independent"}
    }
  }'
  p <- spec_from_vegalite(vl)
  expect_identical(p@resolve, list(x = "independent", color = "independent"))
  expect_no_error(vellum::as_vellum_scene(p))
  # and round-trips through the exporter
  back <- spec_to_vegalite(p)$resolve
  expect_identical(back$scale, list(x = "independent", color = "independent"))

  # an independent guide over a shared scale is reported, not imported
  vl2 <- sub(
    '"axis": {"x": "independent"}',
    '"axis": {"x": "independent", "y": "independent"}',
    vl,
    fixed = TRUE
  )
  expect_warning(p2 <- spec_from_vegalite(vl2), "over a shared scale")
  expect_null(p2@resolve$y)

  # a resolve between layers (no facet) is a dual axis: reported
  layered <- '{
    "data": {"values": [{"a": 1, "b": 2, "c": 3}]},
    "layer": [
      {"mark": "point", "encoding": {"x": {"field": "a"}, "y": {"field": "b"}}},
      {"mark": "line", "encoding": {"x": {"field": "a"}, "y": {"field": "c"}}}
    ],
    "resolve": {"scale": {"y": "independent"}}
  }'
  expect_warning(p3 <- spec_from_vegalite(layered), "between layers")
  expect_length(p3@resolve, 0L)
})

test_that("a position scale that leaves zero out exports scale.zero = false (VP-124)", {
  pt <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
  enc <- spec_to_vegalite(pt)$encoding
  expect_false(enc$x$scale$zero)
  expect_false(enc$y$scale$zero)

  # a bar reaches zero, so Vega-Lite's default already matches
  bar <- vplot(data.frame(g = c("a", "b"), v = c(3, 5))) |>
    mark_bar(x = g, y = v)
  enc <- spec_to_vegalite(bar)$encoding
  expect_null(enc$y$scale$zero)
  expect_null(enc$x$scale$zero)

  # data spanning zero needs nothing either
  sp <- vplot(data.frame(a = c(-1, 2), b = c(-3, 4))) |> mark_point(x = a, y = b)
  expect_null(spec_to_vegalite(sp)$encoding$x$scale$zero)

  # an explicit domain is already honoured by Vega-Lite
  lim <- pt |> scale_x_continuous(limits = c(1, 6))
  enc <- spec_to_vegalite(lim)$encoding
  expect_null(enc$x$scale$zero)
  expect_equal(unlist(enc$x$scale$domain), c(1, 6))
})

test_that("the tooltip channel imports and exports (VP-124)", {
  base <- '{"data": {"values": [{"a": 1, "b": 3, "n": "x"}, {"a": 2, "b": 5, "n": "y"}]},
    "mark": "point",
    "encoding": {"x": {"field": "a", "type": "quantitative"},
                 "y": {"field": "b", "type": "quantitative"}, TT}}'
  with_tt <- function(tt) sub("TT", tt, base, fixed = TRUE)
  tip <- function(p) rlang::quo_get_expr(p@layers[[1]]@interactivity$tooltip)

  # one field: that column
  p1 <- expect_silent(spec_from_vegalite(with_tt('"tooltip": {"field": "n"}')))
  expect_identical(tip(p1), quote(n))

  # an array: a "title: value" line per field
  p2 <- spec_from_vegalite(with_tt(
    '"tooltip": [{"field": "n", "title": "Name"}, {"field": "b"}]'
  ))
  expect_identical(tip(p2), quote(paste0("Name: ", n, "\n", "b: ", b)))

  # `true`: every encoded field
  p3 <- spec_from_vegalite(with_tt('"tooltip": true'))
  expect_identical(tip(p3), quote(paste0("a: ", a, "\n", "b: ", b)))

  # a constant
  p4 <- spec_from_vegalite(with_tt('"tooltip": {"value": "hi"}'))
  expect_identical(tip(p4), "hi")

  # the imported plot renders, and its tooltip goes back out unchanged
  expect_no_error(vellum::as_vellum_scene(p2))
  expect_identical(
    spec_to_vegalite(p2)$encoding$tooltip,
    list(list(field = "n", title = "Name"), list(field = "b"))
  )
  expect_true(spec_to_vegalite(p3)$encoding$tooltip)

  # a vellumplot tooltip exports: a column as is, an expression as its fields
  pt <- vplot(mtcars) |> mark_point(x = wt, y = mpg, tooltip = mpg)
  expect_identical(spec_to_vegalite(pt)$encoding$tooltip, list(field = "mpg"))
  pe <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, tooltip = paste(cyl, gear))
  expect_warning(vl <- spec_to_vegalite(pe), "tooltip expression")
  expect_identical(
    vl$encoding$tooltip,
    list(list(field = "cyl"), list(field = "gear"))
  )
})
