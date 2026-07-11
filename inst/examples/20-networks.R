# Networks: igraph node-link diagrams via vgraph() + mark_edges()/mark_nodes().
# Needs the optional `igraph` and `graphlayouts` packages (both in Suggests) --
# graphlayouts supplies the default stress layout. Skips cleanly if absent.

library(vellumplot)
outdir <- "figures"
dir.create(outdir, showWarnings = FALSE)

if (
  !requireNamespace("igraph", quietly = TRUE) ||
    !requireNamespace("graphlayouts", quietly = TRUE)
) {
  message(
    "20-networks: skipped (install 'igraph' and 'graphlayouts' to run it)"
  )
} else {
  # Zachary's karate club -- the canonical small social network. Precompute the
  # analytical attributes we want to map (vellumplot does not compute centralities).
  g <- igraph::make_graph("Zachary")
  g <- igraph::set_vertex_attr(
    g,
    "community",
    value = as.factor(igraph::cluster_louvain(g)$membership)
  )
  g <- igraph::set_vertex_attr(g, "degree", value = igraph::degree(g))

  # --- 1. The publication-quality default ------------------------------------
  # Stress layout, straight edges under nodes, aspect-locked, no axes -- no
  # tuning. Node size = degree (area-proportional), colour = community.
  vgraph(g, layout = "stress") |>
    mark_edges(alpha = 0.4) |>
    mark_nodes(size = degree, fill = community) |>
    scale_size(range = c(1.5, 6)) |>
    labs(title = "Zachary karate club (stress layout)") |>
    render_plot(file.path(outdir, "20-stress.png"))

  # --- 2. Labelled hubs ------------------------------------------------------
  # Label only the high-degree vertices, so a dense graph is not buried under
  # labels. vgraph() puts the node table (name / x / y / attributes) on the
  # spec, so filtering it and handing the subset to mark_node_text() via the
  # per-layer `data =` keeps the label coordinates aligned.
  gp <- vgraph(g, layout = "stress")
  hubs <- gp@data[gp@data$degree >= 10, ]
  gp |>
    mark_edges(alpha = 0.3) |>
    mark_nodes(size = degree, fill = "grey20") |>
    mark_node_text(data = hubs, label = name, size = 10) |>
    scale_size(range = c(1.5, 6)) |>
    render_plot(file.path(outdir, "20-labelled.png"))

  # --- 3. Alternative layouts ------------------------------------------------
  # Any graphlayouts / igraph layout by name; here a deterministic circle and a
  # backbone layout (emphasises embedded edges to surface group structure).
  vgraph(g, layout = "circle") |>
    mark_edges(alpha = 0.3) |>
    mark_nodes(size = 2.5, fill = community) |>
    labs(title = "Circle layout") |>
    render_plot(file.path(outdir, "20-circle.png"))

  # --- 4. Directed graph with arrows + reciprocal / parallel edges -----------
  # Reciprocal (a->b, b->a) and parallel edges are drawn as straight lines
  # offset off the centre line; self-loops as small loops. Arrowheads mark the
  # target end.
  el <- matrix(
    c(1, 2, 2, 1, 2, 3, 3, 2, 3, 3, 1, 4, 4, 1, 1, 1),
    ncol = 2,
    byrow = TRUE
  )
  d <- igraph::graph_from_edgelist(el, directed = TRUE)
  vgraph(d, layout = "stress") |>
    mark_edges(arrow = TRUE, linewidth = 0.8) |>
    mark_nodes(size = 5, fill = "tomato") |>
    mark_node_text(label = name, size = 9, color = "white") |>
    labs(title = "Directed multigraph (offset edges, self-loops, arrows)") |>
    render_plot(file.path(outdir, "20-directed.png"))

  # --- 5. Weighted edges: width + colour legends -----------------------------
  set.seed(1)
  w <- igraph::sample_gnp(30, 0.12)
  w <- igraph::set_edge_attr(
    w,
    "weight",
    value = runif(igraph::ecount(w), 0.2, 1)
  )
  w <- igraph::set_vertex_attr(w, "degree", value = igraph::degree(w))
  vgraph(w, layout = "stress") |>
    mark_edges(linewidth = weight, color = weight, alpha = 0.9) |>
    mark_nodes(size = degree, fill = "grey20") |>
    scale_edge_width(range = c(0.3, 3.5)) |>
    scale_size(range = c(1.5, 5.5)) |>
    labs(title = "Weighted graph (edge-width + colour legends)") |>
    render_plot(file.path(outdir, "20-weighted.png"))

  # --- 6. At scale: straight edges stay batched ------------------------------
  # Straight edges compile to a handful of grobs regardless of edge count (style
  # grouping), so a large graph still renders fast; sparse stress lays it out.
  # But past a few thousand edges the node-link idiom itself clutters into a
  # hairball -- the honest fixes are backbone layouts, filtering, or edge
  # datashading, not a prettier force layout (see _docs/DESIGN-igraph.md sec 10).
  set.seed(2)
  big <- igraph::sample_pa(2000, m = 2, directed = FALSE)
  xy <- graphlayouts::layout_with_sparse_stress(big, pivots = 100)
  vgraph(big, layout = xy) |>
    mark_edges(alpha = 0.08, linewidth = 0.2) |>
    mark_nodes(size = 0.4, fill = "grey15") |>
    labs(title = "Preferential-attachment graph (2000 nodes)") |>
    render_plot(file.path(outdir, "20-large.png"))

  message("20-networks: wrote figures to ", outdir)
}
