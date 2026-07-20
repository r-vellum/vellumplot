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
  routing = c("straight", "elbow"),
  gradient = FALSE,
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
  dist = 0,
  top_n = NULL,
  by = NULL,
  repel = FALSE,
  box_padding = 1,
  point_padding = 1,
  min_segment_length = 2,
  max_overlaps = 10,
  seed = NULL,
  effects = list(),
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
  effects = list(),
  blend = NULL,
  data = NULL
)

mark_node_hull(
  plot,
  ...,
  fill = NULL,
  color = NULL,
  alpha = 0.25,
  expand = 0.08,
  data = NULL
)

mark_node_pie(
  plot,
  cols,
  ...,
  size = 4,
  inner = 0,
  fill = NULL,
  color = "white",
  linewidth = 0.5,
  alpha = NA,
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
  a mapped expression such as `linewidth = weight`. For
  `mark_node_pie()`, the wedge border width.

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

- routing:

  For `mark_edges()`, edge routing: `"straight"` (default) or `"elbow"`
  – orthogonal right-angle steps for tree / DAG / dendrogram layouts
  (still straight segments, no curvature), stepping along whichever axis
  the endpoints are farther apart on. Elbows keep node-boundary caps and
  arrowheads.

- gradient:

  For `mark_edges()`, `TRUE` to fade each (straight) edge from faint at
  its source to opaque at its target – a direction cue that needs no
  arrowhead (igraph's `edge.gradient`). Ignored with
  `routing = "elbow"`.

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

- dist:

  For `mark_node_text()`, a radial offset (mm) pushing each label
  outward from the layout centroid, so labels clear the node markers
  instead of sitting on them. `0` (default) centres the label on the
  vertex.

- top_n, by:

  For `mark_node_text()`, label only the `top_n` vertices with the
  largest `by` (an edge/vertex metric column, e.g. `by = degree`) – the
  idiomatic "label just the hubs" filter. `NULL` (default) labels every
  vertex.

- repel:

  For `mark_node_text()`, `TRUE` to move overlapping labels apart with a
  force-directed layout (ggrepel-style), drawing a thin leader line back
  to each vertex. `box_padding`, `point_padding`, `min_segment_length`,
  `max_overlaps`, and `seed` tune it exactly as in
  [`mark_text()`](https://r-vellum.github.io/vellumplot/reference/mark_text.md).

- box_padding, point_padding, min_segment_length, max_overlaps, seed:

  Repel tuning for `mark_node_text(repel = TRUE)`; see
  [`mark_text()`](https://r-vellum.github.io/vellumplot/reference/mark_text.md).

- angle:

  For `mark_edge_text()`, the label rotation: a constant in degrees, or
  `"along"` to rotate each label along its edge. `NULL` (default) draws
  horizontal labels.

- expand:

  For `mark_node_hull()`, the fraction to grow each hull outward from
  its centroid so it encloses the node markers (default `0.08`).

- cols:

  For `mark_node_pie()`, the compositional columns (a character vector
  of node-table column names, at least two) whose values size each
  node's wedges.

- inner:

  For `mark_node_pie()`, the inner-radius fraction: `0` (default) draws
  a pie, `> 0` a donut (e.g. `0.5`).

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

`mark_node_text()` has the label-legibility tools a dense graph needs:
`repel = TRUE` nudges overlapping labels apart with leader lines, `dist`
pushes labels radially clear of the node markers, `top_n`/`by` label
only the hubs, and `effects = list(outline())` draws a halo so labels
stay readable over edges. `mark_edge_text()` takes the same `effects`.

Two more decorations: `mark_node_hull()` shades communities with a
convex hull behind the graph (grouped by a mapped `fill`, drawn under
the edges), and `mark_node_pie()` replaces node markers with pie / donut
glyphs whose wedges come from a set of compositional columns.

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
