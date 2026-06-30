# Layout introspection: structural viewports carry stable names (so vellum's
# why_size()/render(debug=) work on a compiled plot), and geom layers carry a
# per-layer SVG id.

test_that("structural viewports are named and resolvable via why_size", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, color = factor(cyl)) |>
    facet_wrap(~cyl, ncol = 3) |>
    labs(title = "t")
  scene <- vellum::as_vellum_scene(p)
  for (nm in c(
    "plot",
    "panel-area",
    "panel-1-1",
    "panel-1-3",
    "axis-title-x",
    "axis-title-y",
    "legend",
    "strip-1-1"
  )) {
    w <- vellum::why_size(scene, nm)
    expect_s3_class(w, "vellum_why_size")
    expect_gt(w$width_mm, 0)
    expect_gt(w$height_mm, 0)
  }
})

test_that("facet_grid strips are named by row/col", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    facet_grid(am ~ cyl)
  scene <- vellum::as_vellum_scene(p)
  expect_gt(vellum::why_size(scene, "strip-col-1")$width_mm, 0)
  expect_gt(vellum::why_size(scene, "strip-row-1")$height_mm, 0)
})

test_that("render(debug = TRUE) draws the layout overlay", {
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
  f <- withr::local_tempfile(fileext = ".png")
  expect_no_error(vellum::render(vellum::as_vellum_scene(p), f, debug = TRUE))
  expect_gt(file.info(f)$size, 0)
})

test_that("each geom layer carries a per-layer data-vellum-id in SVG", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, color = factor(cyl)) |>
    mark_smooth(x = wt, y = mpg)
  f <- withr::local_tempfile(fileext = ".svg")
  render_plot(p, f)
  svg <- paste(readLines(f), collapse = "")
  expect_match(svg, "layer-1-point")
  expect_match(svg, "layer-2-smooth")
})

test_that("the layer id is stamped without disturbing raster output", {
  # A plain render still works; id/role is additive metadata only.
  p <- vplot(mtcars) |> mark_bar(x = factor(cyl), fill = factor(cyl))
  f <- withr::local_tempfile(fileext = ".png")
  expect_no_error(render_plot(p, f))
  expect_gt(file.info(f)$size, 0)
})
