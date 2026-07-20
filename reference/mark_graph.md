# Network (graph) marks

Draw a node-link diagram on a
[PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md)
from
[`vgraph()`](https://r-vellum.github.io/vellumplot/reference/vgraph.md).
`mark_edges()` draws the edges (straight lines, batched), `mark_nodes()`
the vertices (points), `mark_node_text()` the vertex labels, and
`mark_edge_text()` labels on the edges. Draw order is fixed regardless
of the order you pipe them: edges under edge labels under nodes under
node labels. Edges (and edge labels) default to the edge table
([`vgraph()`](https://r-vellum.github.io/vellumplot/reference/vgraph.md)'s
`edge_data`), nodes and node labels to the node table; the
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
  linetype = NULL,
  arrow = FALSE,
  auto = FALSE,
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

mark_edge_text(
  plot,
  ...,
  label = NULL,
  color = NULL,
  size = NULL,
  alpha = NULL,
  angle = NULL,
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

- linetype:

  For `mark_edges()`, the edge line type; a constant or (via
  [`scale_edge_linetype()`](https://r-vellum.github.io/vellumplot/reference/scale_edge.md))
  a mapped expression.

- arrow:

  For `mark_edges()`, `TRUE` to draw a closed arrowhead at each edge's
  target end (the directed convention), `FALSE`/`NULL` for none, or a
  [`vellum::vl_arrow()`](https://r-vellum.github.io/vellum/reference/vl_arrow.html)
  spec for full control (`ends`, `type`, `length`, `angle`) – e.g.
  `arrow = vellum::vl_arrow(ends = "both", type = "open")`. Edges are
  capped exactly at each endpoint's node boundary (per vertex, at any
  size/resolution), so the head sits on the node edge; self-loops are
  drawn as teardrop loops sized to the node, with the head on the node
  boundary.

- auto:

  For `mark_edges()`, `TRUE` to datashade a large graph's edges as a
  density raster
  ([`vellum::datashade_segments()`](https://r-vellum.github.io/vellum/reference/datashade_lines.html))
  once the edge count exceeds the datashade threshold, instead of
  drawing each edge as a vector segment — the fast, overplotting-honest
  path for hairballs. The device-space refinements of the vector path
  (parallel-edge offsets, node-boundary caps, arrowheads, teardrop
  self-loops) do not apply to the rasterised edges.

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
  expression. `size` is also the label font size for
  `mark_node_text()`/`mark_edge_text()`.

- fill, color, alpha:

  Convenience aesthetics; a constant or a mapped expression. For nodes,
  `fill` (or `color`) is the marker colour; for `mark_edges()`,
  `color`/`alpha` map through the edge scales.

- label:

  For `mark_node_text()`, the label expression (default the vertex
  `name`); for `mark_edge_text()`, the edge label expression (no default
  – map an edge attribute, e.g. `label = weight`).

- angle:

  For `mark_edge_text()`, the label rotation: a constant in degrees, or
  `"along"` to rotate each label along its edge. `NULL` (default) draws
  horizontal labels.

## Value

The modified
[PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

## Details

Edge colour, opacity, and line type train on **their own scales**
([`scale_edge_color()`](https://r-vellum.github.io/vellumplot/reference/scale_edge.md),
[`scale_edge_alpha()`](https://r-vellum.github.io/vellumplot/reference/scale_edge.md),
[`scale_edge_linetype()`](https://r-vellum.github.io/vellumplot/reference/scale_edge.md),
plus
[`scale_edge_width()`](https://r-vellum.github.io/vellumplot/reference/scale_edge_width.md)),
independent of the node colour/alpha/linetype scales – so a plot can map
node fill to a discrete community and edge colour to a continuous weight
without the two legends colliding.

These are thin over the point / segment / text marks; `igraph` need not
be installed to use them (only
[`vgraph()`](https://r-vellum.github.io/vellumplot/reference/vgraph.md)
needs it).

## See also

[`vgraph()`](https://r-vellum.github.io/vellumplot/reference/vgraph.md),
[`scale_edge_width()`](https://r-vellum.github.io/vellumplot/reference/scale_edge_width.md),
[`scale_edge_color()`](https://r-vellum.github.io/vellumplot/reference/scale_edge.md)

## Examples

``` r
if (FALSE) { # \dontrun{
g <- igraph::make_graph("Zachary")
vgraph(g) |>
  mark_edges(alpha = 0.5) |>
  mark_nodes(size = 4, fill = "steelblue")
} # }
```
