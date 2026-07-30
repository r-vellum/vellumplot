# Regression guards for REVIEW3 5J performance rewrites that are NOT covered by
# the scene_model baseline (chord / graph traversal), confirming behaviour is
# preserved by the O(n^2) -> O(n) changes.

test_that("chord per-node totals (tapply) match the per-node sum, incl. sink nodes", {
  # "c" appears only as a target -> its out-total must be 0 (tapply gives NA
  # for an absent group; the fix restores the 0 that sum(numeric(0)) gave).
  lay <- vellumplot:::.chord_layout(
    from = c("a", "b", "a"),
    to = c("b", "c", "c"),
    value = c(1, 2, 3),
    gap = 0.02,
    sort = "input"
  )
  expect_setequal(lay$sectors$node, c("a", "b", "c"))
  expect_true(all(is.finite(c(lay$sectors$theta0, lay$sectors$theta1))))
})

test_that("a dendrogram (BFS head-pointer + preallocated edges) still renders", {
  skip_if_not_installed("igraph")
  hc <- hclust(dist(mtcars[1:8, c("wt", "mpg")]))
  # .hclust_to_igraph must build the same directed tree after the edge-vector
  # preallocation (2 edges per merge, in merge order).
  g <- vellumplot:::.hclust_to_igraph(hc)
  expect_equal(igraph::ecount(g), 2L * nrow(hc$merge))
  p <- vgraph(hc, layout = "dendrogram") |> mark_edges() |> mark_nodes()
  f <- local_tempfile(fileext = ".png")
  expect_no_error(render_plot(p, f))
})
