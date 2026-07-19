# Flows and hierarchies

Some plot types don’t map columns to `x` and `y` at all — their geometry
comes from a *layout* computed from the data’s structure.
[`vgraph()`](https://r-vellum.github.io/vellumplot/reference/vgraph.md)
(see [Spatial and
networks](https://r-vellum.github.io/vellumplot/articles/spatial-and-networks.md))
is one; this article covers **flow diagrams**. Like
[`vgraph()`](https://r-vellum.github.io/vellumplot/reference/vgraph.md),
these are whole plot types with their own constructor, an axis-free
panel, and a layout run in R before anything is drawn.

## Sankey diagrams

[`vsankey()`](https://r-vellum.github.io/vellumplot/reference/vsankey.md)
draws a layered flow diagram from a **flow list**: one row per flow,
with a `from` node, a `to` node, and a `value` that sets the ribbon’s
width.

``` r

flows <- data.frame(
  from  = c("Coal", "Gas", "Coal", "Solar", "Grid", "Grid"),
  to    = c("Grid", "Grid", "Export", "Grid", "Homes", "Industry"),
  value = c(30, 20, 10, 15, 40, 25)
)
vsankey(flows, from, to, value)
```

![](flows-and-hierarchies_files/figure-html/unnamed-chunk-2-1.png)

Nodes are the union of the `from` and `to` values; a node that appears
as both a target and a source (here, `Grid`) makes the diagram
multi-stage. Columns are placed by a longest-path layering of the flows,
each node’s height is proportional to the larger of its total in- and
out-flow, and ribbon widths are proportional to `value` — so the diagram
reads as a conserved flow left to right.

The flows must form a directed acyclic graph (a node cannot reach
itself), and `value`s must be positive. Turn node labels off with
`label = FALSE`:

``` r

vsankey(flows, from, to, value, label = FALSE)
```

![](flows-and-hierarchies_files/figure-html/unnamed-chunk-3-1.png)

[`vsankey()`](https://r-vellum.github.io/vellumplot/reference/vsankey.md)
returns an ordinary \[PlotSpec\], so it renders with
[`render_plot()`](https://r-vellum.github.io/vellumplot/reference/render_plot.md)
/ [`print()`](https://rdrr.io/r/base/print.html) like any other plot.
Under the hood it adds a single
[`mark_sankey()`](https://r-vellum.github.io/vellumplot/reference/vsankey.md)
layer to an axis-free, free-aspect panel;
[`mark_sankey()`](https://r-vellum.github.io/vellumplot/reference/vsankey.md)
is exported too, for building a flow layer on a plot you have set up
yourself.
