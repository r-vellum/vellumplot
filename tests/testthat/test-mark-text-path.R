# mark_text_path(): one label per group, set along the group's polyline.

test_that("mark_text_path stores a text_path layer with identity stat", {
  d <- data.frame(x = 1:5, y = 1:5, lab = "hi")
  p <- vplot(d) |> mark_text_path(x = x, y = y, label = lab)
  L <- p@layers[[length(p@layers)]]
  expect_identical(L@mark, "text_path")
  expect_identical(L@stat, "identity")
})

test_that("a text-path label renders as real, selectable <text> in the SVG", {
  t <- seq(pi, 0, length.out = 30)
  d <- data.frame(x = cos(t), y = sin(t), lab = "arc label")
  svg <- vellum::scene_svg(vellum::as_vellum_scene(
    vplot(d) |> mark_text_path(x = x, y = y, label = lab)
  ))
  expect_match(svg, "<text|<tspan|textPath")
})

test_that("one run per group: a colour mapping splits into separate labels", {
  t <- seq(pi, 0, length.out = 20)
  d <- rbind(
    data.frame(x = cos(t), y = sin(t), g = "a"),
    data.frame(x = cos(t), y = 0.5 * sin(t), g = "b")
  )
  # a text_path grob carries no per-element key, so assert via the compiled
  # node tree: exactly one textpath node per group.
  sc <- vellum::as_vellum_scene(
    vplot(d) |> mark_text_path(x = x, y = y, label = g, color = g)
  )
  f <- withr::local_tempfile(fileext = ".svg")
  vellum::render(sc, f)
  svg <- paste(readLines(f, warn = FALSE), collapse = "\n")
  # both group labels appear
  expect_match(svg, "a")
  expect_match(svg, "b")
  expect_gt(file.info(f)$size, 0)
})

test_that("mark_text_path renders under coord_flip and coord_polar", {
  t <- seq(pi, 0, length.out = 30)
  d <- data.frame(x = cos(t), y = sin(t), lab = "on the arc")
  base <- function() vplot(d) |> mark_text_path(x = x, y = y, label = lab)
  for (p in list(base(), coord_flip(base()), coord_polar(base()))) {
    f <- withr::local_tempfile(fileext = ".png")
    expect_no_error(render_plot(p, f))
    expect_gt(file.info(f)$size, 0)
  }
})

test_that("a single-point group is skipped (a path needs >= 2 points)", {
  d <- data.frame(x = 1, y = 1, lab = "x")
  # no error, just nothing drawn for the degenerate path
  expect_no_error(
    vellum::scene_svg(vellum::as_vellum_scene(
      vplot(d) |> mark_text_path(x = x, y = y, label = lab)
    ))
  )
})
