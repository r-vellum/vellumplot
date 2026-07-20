# Start a graph (network) plot

`vgraph()` begins a node-link diagram from an `igraph` graph. It
computes a layout (vertex `x`/`y`), builds a node table and an edge
table, and returns a
[PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md)
with the aspect locked (`coord_equal`) and a void theme (no axes or
gridlines) – publication-quality defaults with no tuning. Add layers
with
[`mark_edges()`](https://r-vellum.github.io/vellumplot/reference/mark_graph.md),
[`mark_nodes()`](https://r-vellum.github.io/vellumplot/reference/mark_graph.md),
and
[`mark_node_text()`](https://r-vellum.github.io/vellumplot/reference/mark_graph.md);
edges default to the edge table and nodes to the node table.

## Usage

``` r
vgraph(
  g,
  layout = "stress",
  ...,
  augment = FALSE,
  filter_nodes = NULL,
  filter_edges = NULL,
  k_core = NULL,
  seed = 42L,
  width = 6,
  height = 4,
  dpi = 96
)
```

## Arguments

- g:

  An `igraph` graph (or a two-column edge-list matrix).

- layout:

  A layout: a name (`"stress"` (default), `"sparse_stress"`,
  `"backbone"`, `"fr"`, `"kk"`, `"circle"`, `"tree"`, `"sugiyama"`,
  ...), a supplied `N x 2` coordinate matrix, or a function `f(g, ...)`
  returning one.

- ...:

  Extra arguments forwarded to the layout function (e.g. `pivots =` for
  sparse stress).

- augment:

  Opt-in vertex metrics to attach as graph attributes (so they can be
  mapped – `mark_nodes(size = degree)` – or used by `filter_nodes`).
  `FALSE` (default) attaches nothing; `TRUE` attaches `degree` and
  `components`; a character vector picks from `"degree"`, `"in_degree"`,
  `"out_degree"`, `"betweenness"`, `"closeness"`, `"eigen"`,
  `"coreness"`, `"components"`, `"community"`. Never computed silently –
  vellum does not guess which centrality you meant.

- filter_nodes, filter_edges:

  Data-masked predicates that keep the vertices / edges for which they
  are `TRUE`, evaluated against the vertex / edge attribute table – e.g.
  `filter_edges = weight > 0.5`, `filter_nodes = degree >= 2` (with
  `augment`). The graph is reduced *before* the layout, so the picture
  is of the subgraph, not a full layout with holes.

- k_core:

  Keep only the k-core: vertices with coreness `>= k_core` (and their
  induced edges). `NULL` (default) keeps everything.

- seed:

  Integer seed for stochastic layouts (`"fr"`, `"drl"`), making the
  figure reproducible. The global RNG stream is restored afterwards.

- width, height:

  Page size in inches.

- dpi:

  Output resolution in dots per inch (see
  [`vplot()`](https://r-vellum.github.io/vellumplot/reference/vplot.md)).

## Value

A
[PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md)
whose data is the node table and whose `edge_data` is the edge table.

## Details

The default layout is stress majorization
([`graphlayouts::layout_with_stress`](https://schochastics.github.io/graphlayouts/reference/layout_stress.html)):
it is deterministic (same graph -\> same picture) and minimizes the
difference between drawn and graph-theoretic distance. Above `10^4`
nodes it falls back to sparse stress automatically. Reciprocal and
parallel edges are drawn as parallel straight lines offset off the
centre line (not curved); self-loops as small loops.

`igraph` (and `graphlayouts`, for its layouts) are optional dependencies
(in `Suggests`); `vgraph()` errors with an install hint if they are not
available.

Reductions apply in a fixed order: `augment` (metrics computed on the
input graph) -\> `filter_edges` -\> `filter_nodes` -\> `k_core`.
Augmented metrics thus reflect the *input* graph, not the post-filter
subgraph.

## See also

[`mark_edges()`](https://r-vellum.github.io/vellumplot/reference/mark_graph.md),
[`mark_nodes()`](https://r-vellum.github.io/vellumplot/reference/mark_graph.md),
[`mark_node_text()`](https://r-vellum.github.io/vellumplot/reference/mark_graph.md)

## Examples

``` r
if (FALSE) { # \dontrun{
g <- igraph::make_graph("Zachary")
vgraph(g, layout = "stress") |>
  mark_edges() |>
  mark_nodes(size = 3)

# attach degree, then plot only the 2-core, sized by degree
vgraph(g, augment = TRUE, k_core = 2) |>
  mark_edges() |>
  mark_nodes(size = degree)
} # }
```
