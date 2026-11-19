# linetype on stroked marks (#30): segment / linerange / rule / errorbar / rug /
# edges / contour previously ignored linetype and always drew solid.

# A dashed line has gaps, so it lays down materially fewer near-black ink pixels
# along its path than the same line drawn solid. Probe a thin horizontal band
# over a full-width horizontal line and compare the two.
# The line at y = 5 (mid data range) lands near row 0.42 of the image once the
# axis gutters are accounted for; probe a band straddling it, clear of the
# white corner points.
horizontal_ink <- function(p) {
  img <- render_px(p)
  count_ink(img, rows = c(0.38, 0.47), cols = c(0.25, 0.75))
}

test_that("mark_segment honours linetype (dashed paints fewer pixels) (#30)", {
  base <- vplot(
    data.frame(x = c(0, 10), y = c(0, 10)),
    width = 4,
    height = 3
  ) |>
    mark_point(x = x, y = y, color = "white")
  solid <- horizontal_ink(
    base |>
      annotate(
        "segment",
        x = 0,
        xend = 10,
        y = 5,
        yend = 5,
        color = "black",
        linewidth = 1.5
      )
  )
  dashed <- horizontal_ink(
    base |>
      annotate(
        "segment",
        x = 0,
        xend = 10,
        y = 5,
        yend = 5,
        color = "black",
        linewidth = 1.5,
        linetype = "dashed"
      )
  )
  expect_gt(solid, 0)
  expect_gt(dashed, 0) # still drawn
  expect_lt(dashed, solid * 0.85) # but with gaps
})

test_that("mark_rule honours linetype (dashed paints fewer pixels) (#30)", {
  base <- vplot(
    data.frame(x = c(0, 10), y = c(0, 10)),
    width = 4,
    height = 3
  ) |>
    mark_point(x = x, y = y, color = "white")
  solid <- horizontal_ink(
    base |> mark_rule(yintercept = 5, color = "black", linewidth = 1.5)
  )
  dashed <- horizontal_ink(
    base |>
      mark_rule(
        yintercept = 5,
        color = "black",
        linewidth = 1.5,
        linetype = "dashed"
      )
  )
  expect_gt(solid, 0)
  expect_gt(dashed, 0)
  expect_lt(dashed, solid * 0.85)
})

test_that("a mapped linetype splits a segment layer into per-type groups (#30)", {
  d <- data.frame(
    x = c(0, 0),
    xend = c(10, 10),
    y = c(3, 7),
    yend = c(3, 7),
    g = c("a", "b")
  )
  p <- vplot(d) |>
    mark_segment(x = x, y = y, xend = xend, yend = yend, linetype = g)
  b <- vellumplot:::.build_panels(p)
  # the linetype scale trains and earns a legend guide
  expect_false(is.null(b$scales$linetype))
  guides <- vellumplot:::.legend_guides(b$scales)
  expect_true(any(vapply(
    guides,
    function(gd) gd$kind == "linetype",
    logical(1)
  )))
})

test_that("every touched stroked mark renders with a linetype set (#30)", {
  df <- data.frame(
    x = 1:6,
    y = 1:6,
    ymin = (1:6) - 1,
    ymax = (1:6) + 1
  )
  expect_no_error(render_px(
    vplot(df) |>
      mark_linerange(x = x, ymin = ymin, ymax = ymax, linetype = "dashed")
  ))
  expect_no_error(render_px(
    vplot(df) |>
      mark_errorbar(x = x, ymin = ymin, ymax = ymax, linetype = "dotted")
  ))
  expect_no_error(render_px(
    vplot(df) |>
      mark_point(x = x, y = y) |>
      mark_rug(x = x, linetype = "dashed")
  ))

  skip_if_not_installed("igraph")
  g <- igraph::make_ring(5)
  expect_no_error(render_px(
    vgraph(g) |> mark_edges(linetype = "dashed") |> mark_nodes()
  ))

  skip_if_not_installed("MASS")
  expect_no_error(render_px(
    vplot(faithful) |>
      mark_contour(x = eruptions, y = waiting, linetype = "dashed")
  ))
})

test_that("a mapped colour / alpha styles each rule, not just the first (VP-19)", {
  r <- data.frame(yi = c(1, 2, 3), xi = c(1, 2, 3), g = c("a", "b", "a"))
  rule_grobs <- function(p) {
    pr <- attr(vellum::as_vellum_scene(p), "vellumplot_provenance")
    Filter(function(e) identical(e$mark, "rule"), pr)
  }
  base <- vplot(data.frame(x = 0:4, y = 0:4)) |> mark_point(x = x, y = y)
  # one grob per colour, each drawing that colour's rows
  h <- rule_grobs(base |> mark_rule(data = r, yintercept = yi, color = g))
  expect_length(h, 2L)
  expect_setequal(lapply(h, function(e) sort(e$rows)), list(c(1L, 3L), 2L))
  v <- rule_grobs(base |> mark_rule(data = r, xintercept = xi, alpha = xi))
  expect_length(v, 3L)
  # a constant style stays one batched grob
  expect_length(
    rule_grobs(base |> mark_rule(data = r, yintercept = yi, color = "red")),
    1L
  )
  # and the colours drawn are the two the legend shows
  p <- base |> mark_rule(data = r, yintercept = yi, color = g)
  cols <- character()
  draw <- .draw
  local_mocked_bindings(
    .draw = function(scene, grob, rows = NULL) {
      if (inherits(grob, "vellum::grob_segments")) {
        cols <<- c(cols, grob@gp@col)
      }
      draw(scene, grob, rows)
    },
    .package = "vellumplot"
  )
  vellum::as_vellum_scene(p)
  sc <- vellumplot:::.build_panels(p)$scales$color
  expect_setequal(unique(cols), sc$map(c("a", "b")))
})
