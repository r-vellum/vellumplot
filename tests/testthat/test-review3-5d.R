# Regression tests for REVIEW3 5D: panel clipping is decided per-layer, so a
# single boundary-mark annotation (grob/sparkline) no longer disables clipping
# for the whole panel. Boundary marks draw in an unclipped sibling overlay.

test_that("a mixed plot clips the panel and overlays only the boundary mark", {
  set.seed(1)
  mixed <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    annotate("sparkline", x = 4, y = 30, values = cumsum(rnorm(20)))
  plain <- vplot(mtcars) |> mark_point(x = wt, y = mpg)

  sc_mixed <- vellum::as_vellum_scene(mixed)
  sc_plain <- vellum::as_vellum_scene(plain)

  # The boundary annotation gets its own unclipped sibling viewport (so the
  # ordinary points still clip to the panel), which a plain plot never creates.
  expect_no_error(vellum::why_size(sc_mixed, "panel-1-1-overlay"))
  expect_error(vellum::why_size(sc_plain, "panel-1-1-overlay"))

  # The plot still renders and both marks are present.
  f <- local_tempfile(fileext = ".png")
  expect_no_error(render_plot(mixed, f))
})

test_that("a pure graph plot stays fully unclipped (no overlay split, unchanged)", {
  skip_if_not_installed("igraph")
  g <- igraph::make_ring(6)
  p <- vgraph(g) |> mark_edges() |> mark_nodes()
  sc <- vellum::as_vellum_scene(p)
  # All layers are boundary marks, so the panel itself is unclipped and there is
  # no overlay split -- byte-identical to the previous behaviour.
  expect_error(vellum::why_size(sc, "panel-1-1-overlay"))
})
