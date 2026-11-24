# VP-130: edges stop at the rim of a node pie, as they do at a node marker, so
# an arrowhead into a pie is not hidden under it.

edge_caps <- function(p) {
  out <- list()
  walk <- function(x) {
    if (is.list(x)) {
      if (!is.null(x$start_cap)) {
        out[[length(out) + 1L]] <<- rbind(
          x$start_cap$value$v,
          x$end_cap$value$v
        )
      }
      lapply(x, walk)
    }
    invisible()
  }
  walk(vellum::as_scene_spec(as_vellum_scene(p)))
  do.call(cbind, out)
}

pie_graph <- function() {
  g <- igraph::make_ring(4, directed = TRUE)
  igraph::V(g)$a <- c(1, 2, 3, 4)
  igraph::V(g)$b <- c(4, 3, 2, 1)
  g
}

test_that(".graph_caps takes the larger of a node marker and a pie", {
  resolved <- list(
    list(mark = "edges", values = list(), params = list(), n = 1L),
    list(mark = "nodes", values = list(), params = list(size = 2), n = 2L),
    list(
      mark = "node_pie",
      values = list(),
      params = list(size = c(5, 1)),
      stat_params = list(),
      n = 2L
    )
  )
  ed <- data.frame(.from_i = 1L, .to_i = 2L)
  caps <- .graph_caps(resolved, ed, node_n = 2L, list())
  expect_equal(caps$node_r, c(5, 2))
  expect_equal(caps$start_cap, 5)
  expect_equal(caps$end_cap, 2)
})

test_that("edges into node pies stop at the pie rim (VP-130)", {
  skip_if_not_installed("igraph")
  p <- vgraph(pie_graph()) |>
    mark_edges(arrow = TRUE) |>
    mark_node_pie(cols = c("a", "b"), size = 5)
  caps <- edge_caps(p)
  expect_false(is.null(caps))
  # the cap is the pie radius plus the 0.4 mm gap edges leave at a node
  expect_true(all(abs(caps - 5.4) < 1e-6))
})

test_that("a pie over a smaller node marker caps at the pie (VP-130)", {
  skip_if_not_installed("igraph")
  p <- vgraph(pie_graph()) |>
    mark_edges() |>
    mark_nodes(size = 2) |>
    mark_node_pie(cols = c("a", "b"), size = 4)
  expect_true(all(abs(edge_caps(p) - 4.4) < 1e-6))
  # a marker larger than the pie still wins
  p2 <- vgraph(pie_graph()) |>
    mark_edges() |>
    mark_nodes(size = 6) |>
    mark_node_pie(cols = c("a", "b"), size = 4)
  expect_true(all(abs(edge_caps(p2) - 6.4) < 1e-6))
})
