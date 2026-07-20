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

test_that(".node_table warns when a vertex attribute collides with x/y and layout wins (H15)", {
  skip_if_not_installed("igraph")
  g <- igraph::make_graph(~ a - b - c)
  igraph::V(g)$x <- c(99, 98, 97) # user attr shadows a reserved layout column
  xy <- cbind(c(0, 1, 2), c(0, 10, 20))
  expect_warning(nt <- .node_table(g, xy), "overwritten by the graph layout")
  expect_identical(nt$x, c(0, 1, 2)) # layout coordinates, not 99/98/97
})

test_that(".edge_table warns when an edge attribute collides with x/y/xend/yend (H15)", {
  skip_if_not_installed("igraph")
  g <- igraph::make_graph(~ a - b - c)
  igraph::E(g)$y <- c(5, 6) # user edge attr shadows a reserved layout column
  xy <- cbind(c(0, 1, 2), c(0, 10, 20))
  expect_warning(et <- .edge_table(g, xy), "overwritten by the graph layout")
  expect_equal(et$y, c(0, 10)) # source-endpoint coords, not 5/6
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
  expect_identical(p@layers[[1]]@z, 1L) # edges
  expect_identical(p@layers[[2]]@z, 3L) # nodes (edge labels reserve band 2)
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

# --- N1a: independent edge colour / alpha / line-type scales -----------------

test_that(".rename_edge_aes moves colour/alpha/linetype to edge channels", {
  q <- list(color = 1, fill = 2, alpha = 3, linetype = 4, linewidth = 5, x = 6)
  nm <- names(.rename_edge_aes(q))
  expect_true(all(c("edge_color", "edge_alpha", "edge_linetype") %in% nm))
  # edge width (linewidth) and position channels are left untouched.
  expect_true(all(c("linewidth", "x") %in% nm))
  expect_false(any(c("color", "alpha", "linetype", "fill") %in% nm))
})

test_that("mark_edges scopes colour/alpha/linetype to edge channels", {
  p <- vplot(data.frame(x = 1)) |>
    mark_edges(color = w, alpha = 0.5, linetype = "dashed", linewidth = w)
  L <- p@layers[[1]]
  enc <- names(L@encoding)
  par <- names(L@params)
  expect_true("edge_color" %in% enc) # mapped colour -> edge channel
  expect_false(any(c("color", "fill") %in% enc)) # never the node colour scale
  expect_true("linewidth" %in% enc) # edge-width channel unchanged
  expect_true("edge_alpha" %in% par) # constant -> edge param
  expect_true("edge_linetype" %in% par)
})

test_that(".legend_guides emits independent node and edge colour guides", {
  guides <- .legend_guides(list(
    color = list(kind = "discrete", colors = "red", levels = "a", name = "grp"),
    edge_color = list(kind = "continuous", legend_labels = "1", name = "w"),
    edge_alpha = list(kind = "alpha", legend_labels = "0.5", name = "a"),
    edge_linetype = list(kind = "linetype", levels = "solid", name = "t")
  ))
  kinds <- vapply(guides, function(g) g$kind, character(1))
  expect_true("color_discrete" %in% kinds) # node colour
  expect_true("color_continuous" %in% kinds) # edge colour, own guide
  expect_true("alpha" %in% kinds)
  expect_true("linetype" %in% kinds)
})

test_that("scale_edge_color/alpha/linetype declare edge-scoped scales", {
  p <- vplot(data.frame(x = 1)) |>
    scale_edge_color(palette = "Grays") |>
    scale_edge_alpha(range = c(0.2, 0.9)) |>
    scale_edge_linetype()
  aes <- vapply(p@scales, function(s) s@aesthetic, character(1))
  expect_true(all(
    c("edge_color", "edge_alpha", "edge_linetype") %in% aes
  ))
  expect_identical(scale_edge_colour, scale_edge_color) # British alias
})

# --- N1b: mark_edge_text -----------------------------------------------------

test_that("mark_edge_text adds an edge_text layer at the midpoint, above edges", {
  p <- vplot(data.frame(x = 1)) |> mark_edge_text(label = w)
  L <- p@layers[[1]]
  expect_identical(L@mark, "edge_text")
  expect_identical(L@z, 2L)
  expect_true(all(c("x", "y", "label") %in% names(L@encoding)))
})

test_that("mark_edge_text requires a label mapping", {
  expect_error(
    vplot(data.frame(x = 1)) |> mark_edge_text(),
    "needs a"
  )
})

test_that("mark_edge_text angle = 'along' maps a per-edge rotation", {
  p <- vplot(data.frame(x = 1)) |> mark_edge_text(label = w, angle = "along")
  expect_true("angle" %in% names(p@layers[[1]]@encoding))
})

test_that("graph marks assign fixed z bands regardless of pipe order", {
  skip_if_not_installed("igraph")
  g <- igraph::make_ring(4)
  p <- vgraph(g) |>
    mark_node_text(label = name) |>
    mark_nodes() |>
    mark_edge_text(label = name) |>
    mark_edges()
  z <- vapply(p@layers, function(l) l@z, integer(1))
  m <- vapply(p@layers, function(l) l@mark, character(1))
  expect_identical(z[m == "edges"], 1L)
  expect_identical(z[m == "edge_text"], 2L)
  expect_identical(z[m == "nodes"], 3L)
  expect_identical(z[m == "node_text"], 4L)
})

# --- N1c: arrow spec ---------------------------------------------------------

test_that(".resolve_edge_arrow handles FALSE / TRUE / a vl_arrow spec", {
  expect_null(.resolve_edge_arrow(FALSE))
  expect_null(.resolve_edge_arrow(NULL))
  expect_true(inherits(.resolve_edge_arrow(TRUE), "vellum_arrow"))
  a <- vellum::vl_arrow(ends = "both", type = "open")
  expect_identical(.resolve_edge_arrow(a), a) # custom spec passes through
})

test_that("mark_edges stores the resolved arrow spec", {
  p <- vplot(data.frame(x = 1)) |>
    mark_edges(arrow = vellum::vl_arrow(ends = "both"))
  expect_true(inherits(p@layers[[1]]@stat_params$arrow, "vellum_arrow"))
})

# --- N1 end to end -----------------------------------------------------------

# --- N6: community hulls + pie glyphs ----------------------------------------

test_that(".stat_hull(expand=) grows the hull outward", {
  L <- list(
    values = list(x = c(0, 2, 2, 0, 1), y = c(0, 0, 2, 2, 1)),
    stat_params = list(expand = 0)
  )
  h0 <- .stat_hull(L)
  L$stat_params$expand <- 1
  h1 <- .stat_hull(L)
  expect_gt(diff(range(h1$x)), diff(range(h0$x)))
  expect_gt(diff(range(h1$y)), diff(range(h0$y)))
})

test_that("mark_node_hull draws a hull behind the graph (z = 0)", {
  skip_if_not_installed("igraph")
  skip_if_not_installed("graphlayouts")
  g <- igraph::make_graph("Zachary")
  g <- igraph::set_vertex_attr(
    g,
    "grp",
    value = factor(igraph::membership(igraph::cluster_louvain(g)))
  )
  p <- vgraph(g) |>
    mark_node_hull(fill = grp) |>
    mark_edges(alpha = 0.3) |>
    mark_nodes(size = 4, fill = grp)
  L <- p@layers[[1]]
  expect_identical(L@mark, "hull")
  expect_identical(L@z, 0L) # behind edges (z = 1)
  expect_true("fill" %in% names(L@encoding))
  png <- local_tempfile(fileext = ".png")
  expect_no_error(render_plot(p, png))
  expect_true(file.exists(png))
})

test_that("mark_node_pie builds a node_pie layer and validates cols", {
  skip_if_not_installed("igraph")
  skip_if_not_installed("graphlayouts")
  g <- igraph::make_ring(6)
  for (nm in c("a", "b", "c")) {
    g <- igraph::set_vertex_attr(g, nm, value = runif(6))
  }
  p <- vgraph(g) |>
    mark_edges(alpha = 0.3) |>
    mark_node_pie(cols = c("a", "b", "c"), size = 5)
  L <- p@layers[[2]]
  expect_identical(L@mark, "node_pie")
  expect_identical(dim(L@stat_params$weights), c(6L, 3L))
  expect_identical(L@stat_params$categories, c("a", "b", "c"))
  expect_error(vgraph(g) |> mark_node_pie(cols = "a"), "at least two")
  expect_error(vgraph(g) |> mark_node_pie(cols = c("a", "zzz")), "Unknown pie")
  expect_error(
    vgraph(g) |> mark_node_pie(cols = c("a", "b"), fill = "red"),
    "one colour per column"
  )
})

test_that("pie and donut node glyphs render", {
  skip_if_not_installed("igraph")
  skip_if_not_installed("graphlayouts")
  g <- igraph::make_ring(6)
  for (nm in c("a", "b", "c")) {
    g <- igraph::set_vertex_attr(g, nm, value = runif(6))
  }
  for (inner in c(0, 0.5)) {
    p <- vgraph(g) |>
      mark_edges(alpha = 0.3) |>
      mark_node_pie(cols = c("a", "b", "c"), size = 5, inner = inner)
    expect_no_error(vellum::as_vellum_scene(p))
    png <- local_tempfile(fileext = ".png")
    expect_no_error(render_plot(p, png))
    expect_true(file.exists(png))
  }
})

# --- N4: elbow routing + gradient edges --------------------------------------

test_that(".elbow_points steps along the dominant axis", {
  # dy dominant -> vertical-major (x held, then y): a top-down tree bends down.
  v <- .elbow_points(0, 0, 1, 4)
  expect_equal(v$x, c(0, 0, 1, 1))
  expect_equal(v$y, c(0, 2, 2, 4))
  # dx dominant -> horizontal-major.
  h <- .elbow_points(0, 0, 4, 1)
  expect_equal(h$x, c(0, 2, 2, 4))
  expect_equal(h$y, c(0, 0, 1, 1))
})

test_that("mark_edges(routing=) records the routing and validates it", {
  skip_if_not_installed("igraph")
  p <- vgraph(igraph::make_ring(4)) |> mark_edges(routing = "elbow")
  expect_identical(p@layers[[1]]@stat_params$routing, "elbow")
  # default is straight
  p0 <- vgraph(igraph::make_ring(4)) |> mark_edges()
  expect_identical(p0@layers[[1]]@stat_params$routing, "straight")
  expect_error(
    vgraph(igraph::make_ring(4)) |> mark_edges(routing = "curvy"),
    "should be one of"
  )
})

test_that("mark_edges(gradient=) records the flag; elbow+gradient warns", {
  skip_if_not_installed("igraph")
  p <- vgraph(igraph::make_ring(4)) |> mark_edges(gradient = TRUE)
  expect_true(p@layers[[1]]@stat_params$gradient)
  expect_warning(
    vgraph(igraph::make_ring(4)) |>
      mark_edges(routing = "elbow", gradient = TRUE),
    "ignored"
  )
})

test_that("elbow and gradient edges compile and render", {
  skip_if_not_installed("igraph")
  skip_if_not_installed("graphlayouts")
  tr <- igraph::make_tree(15, children = 2, mode = "out")
  p_elbow <- vgraph(tr, layout = "tree") |>
    mark_edges(routing = "elbow", arrow = TRUE) |>
    mark_nodes(size = 4)
  g <- igraph::sample_gnp(12, 0.3, directed = TRUE)
  p_grad <- vgraph(g) |> mark_edges(gradient = TRUE) |> mark_nodes(size = 4)
  for (p in list(p_elbow, p_grad)) {
    expect_no_error(vellum::as_vellum_scene(p))
    png <- local_tempfile(fileext = ".png")
    expect_no_error(render_plot(p, png))
    expect_true(file.exists(png))
  }
})

# --- N3: data reduction (augment + filters) ---------------------------------

test_that("vgraph(augment=) attaches vertex metrics (opt-in only)", {
  skip_if_not_installed("igraph")
  skip_if_not_installed("graphlayouts")
  g <- igraph::make_star(6, mode = "undirected")
  p <- vgraph(g, augment = TRUE)
  expect_true(all(c("degree", "components") %in% names(p@data)))
  expect_equal(max(p@data$degree), 5) # the hub
  expect_true("betweenness" %in% names(vgraph(g, augment = "betweenness")@data))
  expect_error(vgraph(g, augment = "bogus"), "Unknown")
  # default: nothing attached
  expect_false("degree" %in% names(vgraph(g)@data))
})

test_that("vgraph(filter_edges=) drops edges before layout", {
  skip_if_not_installed("igraph")
  skip_if_not_installed("graphlayouts")
  g <- igraph::make_ring(4)
  g <- igraph::set_edge_attr(g, "w", value = c(1, 2, 3, 4))
  p <- vgraph(g, filter_edges = w >= 3)
  expect_identical(nrow(p@edge_data), 2L)
})

test_that("vgraph(filter_nodes=) drops vertices, can read augment metrics", {
  skip_if_not_installed("igraph")
  skip_if_not_installed("graphlayouts")
  g <- igraph::graph_from_literal(a - b, b - c, c - a, c - d) # d is a pendant
  p <- vgraph(g, augment = "degree", filter_nodes = degree >= 2)
  expect_identical(nrow(p@data), 3L)
  expect_false("d" %in% p@data$name)
})

test_that("vgraph(k_core=) keeps the k-core", {
  skip_if_not_installed("igraph")
  skip_if_not_installed("graphlayouts")
  g <- igraph::graph_from_literal(a - b, b - c, c - a, c - d)
  p <- vgraph(g, k_core = 2) # the triangle is the 2-core; d drops
  expect_identical(nrow(p@data), 3L)
})

test_that("vgraph() errors when filters remove everything or are ill-typed", {
  skip_if_not_installed("igraph")
  skip_if_not_installed("graphlayouts")
  g <- igraph::make_ring(4)
  expect_error(vgraph(g, k_core = 99), "removed every vertex")
  expect_error(vgraph(g, filter_edges = 1:4), "logical")
})

test_that("a reduced graph compiles and renders", {
  skip_if_not_installed("igraph")
  skip_if_not_installed("graphlayouts")
  g <- igraph::make_graph("Zachary")
  p <- vgraph(g, augment = TRUE, k_core = 2) |>
    mark_edges(alpha = 0.3) |>
    mark_nodes(size = degree)
  expect_lt(nrow(p@data), 34L) # the 2-core is a strict subgraph
  png <- local_tempfile(fileext = ".png")
  expect_no_error(render_plot(p, png))
  expect_true(file.exists(png))
})

# --- N2: label quality (repel, halo, radial offset, top-n) -------------------

test_that("mark_node_text(repel=) plumbs repel params", {
  skip_if_not_installed("igraph")
  g <- igraph::make_ring(5)
  p <- vgraph(g) |> mark_node_text(label = name, repel = TRUE)
  prm <- p@layers[[length(p@layers)]]@stat_params$repel
  expect_true(isTRUE(prm$on))
  p0 <- vgraph(g) |> mark_node_text(label = name)
  expect_null(p0@layers[[length(p0@layers)]]@stat_params$repel)
})

test_that("mark_node_text / mark_edge_text accept halo effects", {
  skip_if_not_installed("igraph")
  g <- igraph::make_ring(4)
  # Previously effects were rejected on text marks; now they draw a halo.
  expect_no_error(
    vgraph(g) |> mark_node_text(label = name, effects = list(outline()))
  )
  expect_no_error(
    vgraph(g) |>
      mark_edge_text(label = "e", effects = list(shadow())) |>
      identity()
  )
})

test_that(".node_label_offsets pushes labels radially outward from the centroid", {
  full <- data.frame(x = c(-1, 1, 0, 0), y = c(0, 0, -1, 1)) # centroid (0,0)
  lab <- .node_label_offsets(full, data.frame(x = 1, y = 0), dist = 2)
  # a node due east of the centroid nudges +x by the full dist, 0 in y.
  expect_equal(lab$.node_nudge_x, 2)
  expect_equal(lab$.node_nudge_y, 0)
  # dist = 0 is a no-op (no nudge columns).
  z <- .node_label_offsets(full, data.frame(x = 1, y = 0), dist = 0)
  expect_false(".node_nudge_x" %in% names(z))
})

test_that("mark_node_text(dist=) maps per-row radial nudges", {
  skip_if_not_installed("igraph")
  g <- igraph::make_ring(6)
  p <- vgraph(g) |> mark_node_text(label = name, dist = 3)
  L <- p@layers[[length(p@layers)]]
  expect_true(all(c("nudge_x", "nudge_y") %in% names(L@encoding)))
  expect_true(".node_nudge_x" %in% names(L@data))
})

test_that("mark_node_text(top_n=, by=) keeps only the top-n vertices", {
  skip_if_not_installed("igraph")
  g <- igraph::make_star(8, mode = "undirected") # vertex 1 is the hub
  g <- igraph::set_vertex_attr(g, "deg", value = igraph::degree(g))
  p <- vgraph(g) |> mark_node_text(label = name, top_n = 3, by = deg)
  expect_identical(nrow(p@layers[[length(p@layers)]]@data), 3L)
  expect_error(
    vgraph(g) |> mark_node_text(label = name, top_n = 3),
    "needs a"
  )
})

test_that("label-quality features render end to end", {
  skip_if_not_installed("igraph")
  skip_if_not_installed("graphlayouts")
  g <- igraph::make_graph("Zachary")
  g <- igraph::set_vertex_attr(g, "deg", value = igraph::degree(g))
  p <- vgraph(g) |>
    mark_edges(alpha = 0.3) |>
    mark_nodes(size = deg) |>
    mark_node_text(
      label = name,
      dist = 3,
      top_n = 6,
      by = deg,
      effects = list(outline(color = "white"))
    )
  expect_no_error(vellum::as_vellum_scene(p))
  png <- local_tempfile(fileext = ".png")
  expect_no_error(render_plot(p, png))
  expect_true(file.exists(png))
})

test_that("independent node/edge colour + edge label + arrow spec render", {
  skip_if_not_installed("igraph")
  skip_if_not_installed("graphlayouts")
  el <- matrix(c(1, 2, 2, 3, 3, 1, 1, 4), ncol = 2, byrow = TRUE)
  d <- igraph::graph_from_edgelist(el, directed = TRUE)
  d <- igraph::set_vertex_attr(d, "grp", value = factor(c("a", "a", "b", "b")))
  d <- igraph::set_edge_attr(d, "w", value = c(1, 2, 3, 4))
  p <- vgraph(d) |>
    mark_edges(
      color = w,
      arrow = vellum::vl_arrow(ends = "both", type = "open")
    ) |>
    mark_nodes(fill = grp, size = 5) |>
    mark_edge_text(label = w, angle = "along") |>
    scale_edge_color(palette = "Grays")
  expect_no_error(vellum::as_vellum_scene(p))
  png <- local_tempfile(fileext = ".png")
  expect_no_error(render_plot(p, png))
  expect_true(file.exists(png))
})
