# Spatial and networks

Most of vellumplot maps columns of a data frame to `x` and `y`. Two
important data shapes do not fit that mould: spatial geometries, where
the coordinates live in a geometry column and the aspect ratio matters,
and graphs, where the positions have to be computed by a layout
algorithm first. vellumplot handles both with the same grammar, through
entry points suited to each data type.

## Maps from sf

[`mark_sf()`](https://r-vellum.github.io/vellumplot/reference/mark_sf.md)
draws the geometry column of an `sf` object. Points, lines, and polygons
each render appropriately, and there are no `x`/`y` encodings: the
coordinates come from the geometry. Other aesthetics map feature
attributes as usual, so `fill = AREA` gives you a choropleth. Pair it
with
[`coord_sf()`](https://r-vellum.github.io/vellumplot/reference/coord_sf.md),
which reprojects every layer to a common CRS before training and locks
the map aspect ratio so nothing is stretched.

``` r

nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
vplot(nc) |>
  mark_sf(fill = BIR74) |>
  scale_fill_binned(style = "quantile", n = 6, palette = "Batlow") |>
  coord_sf()
```

![](spatial-and-networks_files/figure-html/unnamed-chunk-2-1.png)

Because
[`mark_sf()`](https://r-vellum.github.io/vellumplot/reference/mark_sf.md)
is an ordinary layer, you can stack it: draw a base layer of boundaries,
then a second
[`mark_sf()`](https://r-vellum.github.io/vellumplot/reference/mark_sf.md)
for highlighted features or point locations on top, and every layer
reprojects together.

## Networks from igraph

[`vgraph()`](https://r-vellum.github.io/vellumplot/reference/vgraph.md)
starts a node-link diagram from an `igraph` graph. It runs a layout
(stress majorization by default, via `graphlayouts`) and produces a spec
whose node and edge tables already carry the `x`, `y`, `xend`, `yend`,
and `name` columns the graph marks need. Then
[`mark_edges()`](https://r-vellum.github.io/vellumplot/reference/mark_graph.md)
and
[`mark_nodes()`](https://r-vellum.github.io/vellumplot/reference/mark_graph.md)
draw it. Draw order is fixed no matter how you pipe them: edges below
nodes below labels.

``` r

g <- igraph::make_graph("Zachary")
g <- igraph::set_vertex_attr(
  g, "grp",
  value = as.factor(igraph::cluster_louvain(g)$membership)
)
g <- igraph::set_vertex_attr(g, "deg", value = igraph::degree(g))

vgraph(g, layout = "stress") |>
  mark_edges(alpha = 0.4) |>
  mark_nodes(size = deg, fill = grp) |>
  scale_size(range = c(2, 9))
```

![](spatial-and-networks_files/figure-html/unnamed-chunk-3-1.png)

The node and edge aesthetics are the same ones you already know.
[`mark_nodes()`](https://r-vellum.github.io/vellumplot/reference/mark_graph.md)
takes `size`, `shape`, `fill`, and `color`;
[`mark_edges()`](https://r-vellum.github.io/vellumplot/reference/mark_graph.md)
takes `color`, `linewidth`, `linetype`, and `alpha`.
[`mark_node_text()`](https://r-vellum.github.io/vellumplot/reference/mark_graph.md)
adds vertex labels.

``` r

vgraph(g, layout = "stress") |>
  mark_edges(color = "grey70") |>
  mark_nodes(fill = grp, size = 6) |>
  mark_node_text(label = name, size = 8)
```

![](spatial-and-networks_files/figure-html/unnamed-chunk-4-1.png)

### Independent edge scales

Edge colour, opacity, line type, and width train on their **own** scales
–
[`scale_edge_color()`](https://r-vellum.github.io/vellumplot/reference/scale_edge.md),
[`scale_edge_alpha()`](https://r-vellum.github.io/vellumplot/reference/scale_edge.md),
[`scale_edge_linetype()`](https://r-vellum.github.io/vellumplot/reference/scale_edge.md),
and
[`scale_edge_width()`](https://r-vellum.github.io/vellumplot/reference/scale_edge_width.md)
– separate from the node colour/alpha/linetype scales. So a figure can
map node fill to a discrete community *and* edge colour to a continuous
edge weight, and each gets its own legend instead of the two collapsing
into one. Map an edge attribute to `color` and it is trained as an edge
scale automatically; call
[`scale_edge_color()`](https://r-vellum.github.io/vellumplot/reference/scale_edge.md)
to choose the palette.

``` r

g <- igraph::set_edge_attr(g, "w", value = runif(igraph::ecount(g)))

vgraph(g, layout = "stress") |>
  mark_edges(color = w, linewidth = w) |>
  mark_nodes(fill = grp, size = deg) |>
  scale_edge_color(palette = "Grays") |>
  scale_edge_width(range = c(0.3, 2.5)) |>
  scale_size(range = c(2, 9))
```

![](spatial-and-networks_files/figure-html/unnamed-chunk-5-1.png)

### Directed edges, arrows, and edge labels

For directed graphs, `arrow = TRUE` draws a closed arrowhead at each
edge’s target end; edges are capped at the node boundary so the head is
never buried under the marker. Pass a
[`vellum::vl_arrow()`](https://r-vellum.github.io/vellum/reference/vl_arrow.html)
for full control over the head (`ends`, `type`, `length`, `angle`).
[`mark_edge_text()`](https://r-vellum.github.io/vellumplot/reference/mark_graph.md)
labels the edges at their midpoints, and `angle = "along"` rotates each
label to follow its edge.

``` r

el <- matrix(c(1, 2, 2, 3, 3, 1, 1, 4, 4, 2), ncol = 2, byrow = TRUE)
d <- igraph::graph_from_edgelist(el, directed = TRUE)
d <- igraph::set_edge_attr(d, "flow", value = c(3, 1, 4, 1, 5))

vgraph(d, layout = "stress") |>
  mark_edges(linewidth = flow, arrow = TRUE) |>
  mark_nodes(size = 8, fill = "steelblue") |>
  mark_edge_text(label = flow, size = 7) |>
  mark_node_text(label = name, size = 8, color = "white") |>
  scale_edge_width(range = c(0.4, 2.5))
```

![](spatial-and-networks_files/figure-html/unnamed-chunk-6-1.png)

### Readable labels

Labelling every vertex of a real network is unreadable, and labels that
sit on the markers or vanish into the edges are worse.
[`mark_node_text()`](https://r-vellum.github.io/vellumplot/reference/mark_graph.md)
has four tools for this:

- `top_n` / `by` label only the most important vertices (e.g. the
  highest-degree hubs) – the common “label a handful, not all 34” move
  as a one-liner.
- `dist` pushes each label radially outward from the layout centre, so
  it clears its node marker.
- `repel = TRUE` nudges any labels that still overlap apart,
  ggrepel-style, with a thin leader line back to each vertex.
- `effects = list(outline())` draws a halo (shadowtext) so a label stays
  legible where it crosses an edge.
  [`mark_edge_text()`](https://r-vellum.github.io/vellumplot/reference/mark_graph.md)
  takes the same `effects`.

``` r

vgraph(g, layout = "stress") |>
  mark_edges(alpha = 0.3) |>
  mark_nodes(size = deg, fill = grp) |>
  mark_node_text(
    label = name,
    top_n = 8,
    by = deg,
    dist = 3,
    effects = list(outline(color = "white"))
  ) |>
  scale_size(range = c(2, 9))
```

![](spatial-and-networks_files/figure-html/unnamed-chunk-7-1.png)

### Choosing a layout

`layout` accepts a name (`"stress"`, `"sparse_stress"`, `"backbone"`,
`"fr"`, `"kk"`, `"circle"`, `"tree"`, `"sugiyama"`, and more), a
supplied N-by-2 coordinate matrix, or a function that returns one.
Stochastic layouts take a `seed` so the figure is reproducible.

``` r

vgraph(g, layout = "circle") |>
  mark_edges(alpha = 0.3) |>
  mark_nodes(fill = grp, size = 5)
```

![](spatial-and-networks_files/figure-html/unnamed-chunk-8-1.png)

Both of these are still ordinary specs. They face the same scales,
themes, and composition tools as any other plot, and they render to a
file with
[`render_plot()`](https://r-vellum.github.io/vellumplot/reference/render_plot.md).
For per-feature interactivity (tooltips and data keys on map features or
nodes), see the interactivity notes on
[`mark_sf()`](https://r-vellum.github.io/vellumplot/reference/mark_sf.md)
and the mark reference pages.
