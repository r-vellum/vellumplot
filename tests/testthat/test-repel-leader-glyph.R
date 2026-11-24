# VP-129: a repelled node label's leader ran over the node glyph to the
# vertex centre, so a single-colour pie looked as if it had a radial line. The
# leader now stops at the glyph's rim (plus the 0.4 mm gap edges leave).

leader_graph <- function() {
  skip_if_not_installed("igraph")
  g <- igraph::make_ring(5)
  igraph::V(g)$name <- c(
    "Aaaaaaaa",
    "Bbbbbbbbb",
    "Cccccccc",
    "Ddddddd",
    "Eeeeeeee"
  )
  igraph::V(g)$a <- c(1, 0, 2, 3, 1)
  igraph::V(g)$b <- c(0, 4, 1, 1, 1)
  vgraph(g, width = 3, height = 3) |> mark_edges()
}

# leaders (drawn at the scene root, outside any panel) and how far each one's
# start lies from the nearest vertex centre, in mm. The model gives a segment's
# bounding box, so its start is the box corner nearest a vertex.
leader_starts_mm <- function(p, centres) {
  s <- vellum::as_vellum_scene(p)
  m <- vellum::scene_model(s)$elements
  seg <- m[m$mark == "segment" & is.na(m$panel), ]
  px <- s@dpi / 25.4
  vapply(
    seq_len(nrow(seg)),
    function(i) {
      cx <- c(seg$x0[i], seg$x1[i])
      cy <- c(seg$y0[i], seg$y1[i])
      d <- outer(seq_len(nrow(centres)), 1:4, function(k, j) {
        sqrt(
          (centres$x[k] - cx[(j - 1) %% 2 + 1])^2 +
            (centres$y[k] - cy[(j - 1) %/% 2 + 1])^2
        )
      })
      min(d) / px
    },
    numeric(1)
  )
}

point_centres <- function(p) {
  m <- vellum::scene_model(vellum::as_vellum_scene(p))$elements
  pt <- m[m$mark == "point", ]
  data.frame(x = (pt$x0 + pt$x1) / 2, y = (pt$y0 + pt$y1) / 2)
}

test_that("a repelled label's leader stops at a node marker's rim", {
  p <- leader_graph() |>
    mark_nodes(size = 4) |>
    mark_node_text(repel = TRUE, size = 14)
  d <- leader_starts_mm(p, point_centres(p))
  expect_gt(length(d), 0L)
  expect_equal(d, rep(4.4, length(d)), tolerance = 1e-3)
})

test_that("a repelled label's leader stops at a node pie's rim", {
  # a tiny marker under each pie only to locate the vertices (the pie's fill
  # legend narrows the panel)
  p <- leader_graph() |>
    mark_node_pie(cols = c("a", "b"), size = 5) |>
    mark_nodes(size = 0.01) |>
    mark_node_text(repel = TRUE, size = 14)
  centres <- point_centres(p)
  d <- leader_starts_mm(p, centres)
  expect_gt(length(d), 0L)
  expect_equal(d, rep(5.4, length(d)), tolerance = 1e-3)
})

test_that("leaders of labels with no glyph under them still start at the anchor", {
  p <- leader_graph() |> mark_node_text(repel = TRUE, size = 14)
  ref <- leader_graph() |>
    mark_nodes(size = 0.001) |>
    mark_node_text(repel = TRUE, size = 14)
  d <- leader_starts_mm(p, point_centres(ref))
  expect_true(all(d < 0.01))
})
