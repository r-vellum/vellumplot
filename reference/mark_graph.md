# Network (graph) marks

Draw a node-link diagram on a
[PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md)
from
[`vgraph()`](https://r-vellum.github.io/vellumplot/reference/vgraph.md).
`mark_edges()` draws the edges (straight lines, batched), `mark_nodes()`
the vertices (points), and `mark_node_text()` the vertex labels. Draw
order is fixed regardless of the order you pipe them: edges under nodes
under labels. Edges default to the edge table
([`vgraph()`](https://r-vellum.github.io/vellumplot/reference/vgraph.md)'s
`edge_data`), nodes and labels to the node table; the
`x`/`y`/`xend`/`yend`/`label`/`name` columns those tables carry are
mapped automatically, so bare `mark_edges() |> mark_nodes()` just works.

## Usage

``` r
mark_edges(
  plot,
  ...,
  color = NULL,
  linewidth = NULL,
  alpha = NULL,
  arrow = FALSE,
  blend = NULL,
  effects = list(),
  sketch = NULL,
  data = NULL
)

mark_nodes(
  plot,
  ...,
  size = NULL,
  shape = NULL,
  fill = NULL,
  color = NULL,
  alpha = NULL,
  blend = NULL,
  effects = list(),
  sketch = NULL,
  data = NULL
)

mark_node_text(
  plot,
  ...,
  label = NULL,
  color = NULL,
  size = NULL,
  alpha = NULL,
  blend = NULL,
  data = NULL
)
```

## Arguments

- plot:

  A
  [PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md),
  normally from
  [`vgraph()`](https://r-vellum.github.io/vellumplot/reference/vgraph.md).

- ...:

  Encodings mapping node/edge attributes to aesthetics. Nodes: `size`,
  `color`/`fill`, `shape`, `alpha`. Edges: `color`, `linewidth`,
  `linetype`, `alpha`. The position channels are supplied by
  [`vgraph()`](https://r-vellum.github.io/vellumplot/reference/vgraph.md)
  and need not be mapped.

- linewidth:

  For `mark_edges()`, the edge width; a constant or (via
  [`scale_edge_width()`](https://r-vellum.github.io/vellumplot/reference/scale_edge_width.md))
  a mapped expression such as `linewidth = weight`.

- arrow:

  For `mark_edges()`, `TRUE` to draw an arrowhead at each edge's target
  end (directed graphs). Edges are capped exactly at each endpoint's
  node boundary (per vertex, at any size/resolution), so the head sits
  on the node edge; self-loops are drawn as teardrop loops sized to the
  node, with the head on the node boundary.

- blend:

  Optional blend mode (see
  [`mark_point()`](https://r-vellum.github.io/vellumplot/reference/mark_point.md)).

- effects:

  A list of layer render effects
  ([`glow()`](https://r-vellum.github.io/vellumplot/reference/glow.md),
  [`outline()`](https://r-vellum.github.io/vellumplot/reference/outline.md),
  [`shadow()`](https://r-vellum.github.io/vellumplot/reference/shadow.md))
  applied to the mark at draw time.

- sketch:

  A
  [`sketch()`](https://r-vellum.github.io/vellumplot/reference/sketch.md)
  spec giving the layer a hand-drawn look, `NA`/`FALSE` to force it
  crisp, or `NULL` (default) to inherit.

- data:

  Optional layer data; overrides the default table.

- size, shape:

  For `mark_nodes()`, the node size (mm) / shape; a constant or a mapped
  expression.

- fill, color, alpha:

  Convenience aesthetics; a constant or a mapped expression. For nodes,
  `fill` (or `color`) is the marker colour.

- label:

  For `mark_node_text()`, the label expression (default the vertex
  `name`).

## Value

The modified
[PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

## Details

These are thin over the point / segment / text marks; `igraph` need not
be installed to use them (only
[`vgraph()`](https://r-vellum.github.io/vellumplot/reference/vgraph.md)
needs it).

## See also

[`vgraph()`](https://r-vellum.github.io/vellumplot/reference/vgraph.md),
[`scale_edge_width()`](https://r-vellum.github.io/vellumplot/reference/scale_edge_width.md)

## Examples

``` r
if (FALSE) { # \dontrun{
g <- igraph::make_graph("Zachary")
vgraph(g) |>
  mark_edges(alpha = 0.5) |>
  mark_nodes(size = 4, fill = "steelblue")
} # }
```
