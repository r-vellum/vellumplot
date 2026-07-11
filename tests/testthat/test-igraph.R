# Network (igraph) support: layout bridge, two-table construction, edge offsets,
# vgraph() spec defaults (aspect lock + void theme + edge_data), enforced z-order,
# the edge-width scale + legend, and end-to-end compilation.

# --- layout bridge ----------------------------------------------------------

test_that(".graph_layout accepts a matrix, a function, and a name", {
  skip_if_not_installed("igraph")
  g <- igraph::make_ring(5)

  m <- cbind(1:5, 5:1)
  expect_identical(.graph_layout(g, m), m[, 1:2, drop = FALSE])

  f <- function(graph) cbind(seq_len(igraph::vcount(graph)), 0)
  expect_equal(.graph_layout(g, f)[, 1], 1:5)

  skip_if_not_installed("graphlayouts")
  xy <- .graph_layout(g, "stress")
  expect_equal(dim(xy), c(5L, 2L))
  # stress is deterministic: same graph -> same coordinates.
  expect_identical(xy, .graph_layout(g, "stress"))
})

test_that(".graph_layout errors on a bad name or wrong-shape matrix", {
  skip_if_not_installed("igraph")
  g <- igraph::make_ring(4)
  expect_error(.graph_layout(g, "not_a_layout"), "Unknown layout")
  expect_error(.graph_layout(g, cbind(1:3, 3:1)), "N x 2")
})

# --- edge offsets -----------------------------------------------------------

test_that(".edge_offsets spreads parallel/reciprocal edges and flags loops", {
  # reciprocal pair {1,2}: two edges, opposite-signed offsets summing to 0.
  ei <- rbind(c(1, 2), c(2, 1))
  o <- .edge_offsets(ei)
  expect_equal(sort(o$s), c(-0.5, 0.5))
  expect_equal(sum(o$s), 0)
  expect_false(any(o$loop))

  # three parallel edges: centred, evenly spaced -1, 0, 1.
  ei3 <- rbind(c(1, 2), c(1, 2), c(2, 1))
  expect_equal(sort(.edge_offsets(ei3)$s), c(-1, 0, 1))

  # a self-loop is flagged and gets no offset.
  eil <- rbind(c(1, 1), c(1, 2))
  ol <- .edge_offsets(eil)
  expect_identical(ol$loop, c(TRUE, FALSE))
  expect_equal(ol$s[1], 0)

  # a lone edge is not offset.
  expect_equal(.edge_offsets(rbind(c(1, 2)))$s, 0)
})

test_that(".loop_geometry points a self-loop into the largest incident gap", {
  # vertex 1 has neighbours at bearing 0 (right) and pi/2 (up); the widest gap is
  # pi/2 -> 2pi, so a single loop points to its midpoint 5pi/4 (down-left).
  ei <- rbind(c(1, 2), c(1, 3), c(1, 1))
  xy <- rbind(c(0, 0), c(1, 0), c(0, 1))
  lg <- .loop_geometry(ei, xy)
  expect_equal(lg$angle[3], 5 * pi / 4)
  expect_true(is.na(lg$angle[1]) && is.na(lg$angle[2])) # non-loops: no angle
  expect_equal(lg$narrow[3], 1) # wide gap -> full width

  # no other edges -> whole circle free; a lone loop sits at 0.
  lg2 <- .loop_geometry(rbind(c(1, 1)), rbind(c(0, 0)))
  expect_equal(lg2$angle[1], 0)

  # narrowing stays within [0.2, 1].
  expect_true(all(lg$narrow >= 0.2 & lg$narrow <= 1))
})

# --- two-table construction -------------------------------------------------

test_that(".node_table carries name + layout coords in vertex order", {
  skip_if_not_installed("igraph")
  g <- igraph::make_graph(~ a - b - c)
  xy <- cbind(c(0, 1, 2), c(0, 10, 20))
  nt <- .node_table(g, xy)
  expect_identical(nt$name, c("a", "b", "c"))
  expect_identical(nt$x, c(0, 1, 2))
  expect_identical(nt$y, c(0, 10, 20))
})

test_that(".edge_table resolves endpoints by vertex index, not row position", {
  skip_if_not_installed("igraph")
  g <- igraph::make_graph(~ a - b - c) # edges a-b, b-c
  xy <- cbind(c(0, 1, 2), c(0, 10, 20))
  et <- .edge_table(g, xy)
  expect_identical(nrow(et), 2L)
  # first edge a-b: source = a (0,0), target = b (1,10).
  expect_equal(et$x[1], 0)
  expect_equal(et$y[1], 0)
  expect_equal(et$xend[1], 1)
  expect_equal(et$yend[1], 10)
  # a simple graph has no parallel edges -> no offset applied.
  expect_equal(et$xend[2], 2)
  # endpoint vertex indices carried for per-node edge capping.
  expect_identical(et$.from_i, c(1L, 2L))
  expect_identical(et$.to_i, c(2L, 3L))
})

test_that(".graph_caps returns exact per-edge node radii + offsets (mm)", {
  # points_grob `size` is the radius, so node radius == size (sizes 4/8).
  resolved <- list(
    list(mark = "edges", values = list(), params = list(), n = 2L),
    list(mark = "nodes", values = list(), params = list(size = c(4, 8)), n = 2L)
  )
  scales <- list()
  frac <- 1.1 # .EDGE_OFFSET_MM_FRAC

  # two parallel 1->2 edges on opposite canonical sides -> caps by endpoint,
  # offset spread across the larger (8mm) node.
  ed <- data.frame(
    .from_i = c(1L, 1L),
    .to_i = c(2L, 2L),
    .offset_s = c(-0.5, 0.5)
  )
  caps <- .graph_caps(resolved, ed, node_n = 2L, scales)
  expect_equal(caps$node_r, c(4, 8))
  expect_equal(caps$start_cap, c(4, 4)) # source = vertex 1
  expect_equal(caps$end_cap, c(8, 8)) # target = vertex 2
  expect_equal(caps$offset, c(-0.5, 0.5) * 8 * frac)

  # a reversed edge flips the offset sign (vellum offsets along the edge's own
  # normal, so a reciprocal pair must be sign-flipped to separate).
  ed2 <- data.frame(.from_i = 2L, .to_i = 1L, .offset_s = 0.5)
  expect_equal(
    .graph_caps(resolved, ed2, 2L, scales)$offset,
    0.5 * -1 * 8 * frac
  )

  # no nodes layer -> nothing to cap.
  expect_null(.graph_caps(resolved[1], ed, 2L, scales))
})

test_that(".edge_table handles an edgeless graph", {
  skip_if_not_installed("igraph")
  g <- igraph::make_empty_graph(3)
  et <- .edge_table(g, cbind(1:3, 1:3))
  expect_identical(nrow(et), 0L)
  expect_true(all(c("x", "y", "xend", "yend") %in% names(et)))
})

# --- vgraph() spec defaults -------------------------------------------------

test_that("vgraph() locks aspect, voids the theme, and stores the edge table", {
  skip_if_not_installed("igraph")
  skip_if_not_installed("graphlayouts")
  g <- igraph::make_graph("Zachary")
  p <- vgraph(g)
  expect_true(S7::S7_inherits(p, PlotSpec))
  expect_identical(p@coord@kind, "fixed")
  expect_equal(p@coord@ratio, 1)
  expect_true(.is_blank(p@theme[["panel.background"]]))
  expect_true(.is_blank(p@theme[["axis.ticks"]]))
  expect_identical(nrow(p@data), 34L)
  expect_identical(nrow(p@edge_data), 78L)
  # padded position limits are installed as x/y scales.
  aes <- vapply(p@scales, function(s) s@aesthetic, character(1))
  expect_true(all(c("x", "y") %in% aes))
})

test_that("vgraph() errors clearly on a non-graph", {
  skip_if_not_installed("igraph")
  expect_error(vgraph(mtcars), "igraph")
})

# --- marks + enforced z-order -----------------------------------------------

test_that("graph marks default to the right table and carry z bands", {
  skip_if_not_installed("igraph")
  skip_if_not_installed("graphlayouts")
  g <- igraph::make_graph("Zachary")
  p <- vgraph(g) |> mark_edges() |> mark_nodes()
  # mark_edges defaults its data to the edge table.
  expect_identical(p@layers[[1]]@mark, "edges")
  expect_identical(nrow(p@layers[[1]]@data), 78L)
  expect_identical(p@layers[[1]]@z, 1L)
  expect_identical(p@layers[[2]]@z, 2L)
})

test_that("z-order draws edges under nodes under labels regardless of pipe order", {
  skip_if_not_installed("igraph")
  skip_if_not_installed("graphlayouts")
  g <- igraph::make_graph("Zachary")
  # pipe them out of order on purpose.
  p <- vgraph(g) |>
    mark_node_text(label = name) |>
    mark_nodes() |>
    mark_edges()
  svg <- local_tempfile(fileext = ".svg")
  render_plot(p, svg)
  txt <- paste(readLines(svg), collapse = "\n")
  pe <- regexpr("edges", txt)
  pn <- regexpr("layer-.-nodes", txt)
  pt <- regexpr("node_text", txt)
  expect_true(pe > 0 && pn > 0 && pt > 0)
  expect_lt(pe, pn) # edges before nodes
  expect_lt(pn, pt) # nodes before labels
})

# --- edge-width scale + legend ----------------------------------------------

test_that("scale_edge_width declares an edge_width scale", {
  p <- vplot(data.frame(x = 1)) |> scale_edge_width(range = c(1, 5))
  sc <- Filter(function(s) s@aesthetic == "edge_width", p@scales)
  expect_length(sc, 1L)
  expect_identical(sc[[1]]@range, c(1, 5))
})

test_that(".legend_guides emits an edge-width guide when the scale is trained", {
  guides <- .legend_guides(list(edge_width = list(kind = "edge_width")))
  kinds <- vapply(guides, function(g) g$kind, character(1))
  expect_true("edge_width" %in% kinds)
})

# --- end to end -------------------------------------------------------------

test_that("a weighted directed graph compiles and renders", {
  skip_if_not_installed("igraph")
  skip_if_not_installed("graphlayouts")
  el <- matrix(c(1, 2, 2, 1, 2, 3, 3, 3, 1, 1), ncol = 2, byrow = TRUE)
  d <- igraph::graph_from_edgelist(el, directed = TRUE)
  d <- igraph::set_edge_attr(d, "w", value = c(1, 2, 3, 1, 2))
  p <- vgraph(d, layout = "stress") |>
    mark_edges(linewidth = w, color = w, arrow = TRUE) |>
    mark_nodes(size = 6, fill = "tomato") |>
    mark_node_text(label = name) |>
    scale_edge_width(range = c(0.5, 3))
  expect_no_error(vellum::as_vellum_scene(p))
  png <- local_tempfile(fileext = ".png")
  expect_no_error(render_plot(p, png))
  expect_true(file.exists(png))
})
