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
takes `color`, `linewidth` (scaled by
[`scale_edge_width()`](https://r-vellum.github.io/vellumplot/reference/scale_edge_width.md)),
and `alpha`, plus `arrow = TRUE` for directed graphs.
[`mark_node_text()`](https://r-vellum.github.io/vellumplot/reference/mark_graph.md)
adds vertex labels.

``` r

vgraph(g, layout = "stress") |>
  mark_edges(color = "grey70") |>
  mark_nodes(fill = grp, size = 6) |>
  mark_node_text(label = name, size = 8)
```

![](spatial-and-networks_files/figure-html/unnamed-chunk-4-1.png)

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

![](spatial-and-networks_files/figure-html/unnamed-chunk-5-1.png)

Both of these are still ordinary specs. They face the same scales,
themes, and composition tools as any other plot, and they render to a
file with
[`render_plot()`](https://r-vellum.github.io/vellumplot/reference/render_plot.md).
For per-feature interactivity (tooltips and data keys on map features or
nodes), see the interactivity notes on
[`mark_sf()`](https://r-vellum.github.io/vellumplot/reference/mark_sf.md)
and the mark reference pages.
