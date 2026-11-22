# VP-73: mark_edge_bundle()'s legend key is a line, at the bundle's opacity.

test_that("an edge bundle's colour key is a line at its default alpha", {
  skip_if_not_installed("igraph")
  skip_if_not_installed("graphlayouts")
  set.seed(1)
  g <- igraph::sample_gnp(20, 0.15)
  igraph::E(g)$k <- rep(c("a", "b"), length.out = igraph::ecount(g))
  key <- function(p) {
    s <- vellumplot:::.build_panels(p)$scales$edge_color
    list(glyph = s$key_glyph, alpha = s$key_alpha)
  }
  p <- vgraph(g, layout = "stress") |> mark_edge_bundle(color = k)
  expect_identical(key(p), list(glyph = "line", alpha = list(alpha = 0.3)))
  # a constant alpha wins over the default
  p2 <- vgraph(g, layout = "stress") |> mark_edge_bundle(color = k, alpha = 0.8)
  expect_identical(key(p2)$alpha, list(alpha = 0.8))
  # plain edges keep a full-opacity line key
  p3 <- vgraph(g, layout = "stress") |> mark_edges(color = k)
  expect_identical(key(p3), list(glyph = "line", alpha = NULL))
})
