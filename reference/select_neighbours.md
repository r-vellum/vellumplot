# Highlight a node's graph neighbourhood

A network-aware
[`select_point()`](https://r-vellum.github.io/vellumplot/reference/select_point.md)
preset for
[`vgraph()`](https://r-vellum.github.io/vellumplot/reference/vgraph.md)
plots: pointing at (or clicking) a node selects **its neighbourhood** —
the node, its incident edges, and its adjacent nodes — so a host
spotlights them and dims the rest. Pointing at an edge highlights the
edge and its two endpoint nodes. It builds on the node/edge identity
[`vgraph()`](https://r-vellum.github.io/vellumplot/reference/vgraph.md)
emits for an interactive plot (each node keyed by its vertex `name`,
each edge carrying its endpoint names); the host reconstructs the
adjacency and projects the gesture across it.

## Usage

``` r
select_neighbours(
  plot,
  name = "neighbours",
  on = c("hover", "click"),
  degree = 1L,
  edges = TRUE,
  empty = TRUE
)
```

## Arguments

- plot:

  A
  [PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md),
  normally from
  [`vgraph()`](https://r-vellum.github.io/vellumplot/reference/vgraph.md)
  (piped form), or a selection **name** string (free-standing form).

- name:

  The selection name. Defaults to `"neighbours"`.

- on:

  The gesture: `"hover"` (default) or `"click"`.

- degree:

  How many hops out from the pointed node to include (`1`, the default,
  is the immediate neighbourhood; `2` adds neighbours-of-neighbours).

- edges:

  Whether to include the incident edges in the highlight (`TRUE`,
  default) or only the nodes.

- empty:

  Whether an empty selection matches *all* elements (`TRUE`, default —
  an un-hovered graph shows its full self) or none.

## Value

A
[PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md)
(piped form) or a `SelectionSpec` (free-standing form).

## Details

Like the other selections it is **inert on a static render** and enacted
by a capable host (`vellumwidget`). On its own it spotlights the
neighbourhood; pair it with
[`condition()`](https://r-vellum.github.io/vellumplot/reference/condition.md)
to restyle members explicitly, or
[`filter_by()`](https://r-vellum.github.io/vellumplot/reference/filter_by.md)
to show only the neighbourhood.

## See also

[`select_point()`](https://r-vellum.github.io/vellumplot/reference/select_point.md),
[`condition()`](https://r-vellum.github.io/vellumplot/reference/condition.md),
[`vgraph()`](https://r-vellum.github.io/vellumplot/reference/vgraph.md),
[`mark_edges()`](https://r-vellum.github.io/vellumplot/reference/mark_graph.md)

## Examples

``` r
if (FALSE) { # \dontrun{
g <- igraph::make_graph("Zachary")
vgraph(g) |>
  mark_edges() |>
  mark_nodes(size = 3) |>
  select_neighbours(on = "hover")
} # }
```
