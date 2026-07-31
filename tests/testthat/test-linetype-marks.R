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
