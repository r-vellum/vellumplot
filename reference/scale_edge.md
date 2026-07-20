# Edge colour / alpha / line-type scales

Declare the scale for a mapped edge aesthetic on a
[`vgraph()`](https://r-vellum.github.io/vellumplot/reference/vgraph.md)
plot – the colour (`mark_edges(color = )`), opacity (`alpha = `), or
line type (`linetype = `) of the edges. These are the edge counterparts
of
[`scale_color_continuous()`](https://r-vellum.github.io/vellumplot/reference/scale_color_continuous.md)
/
[`scale_alpha()`](https://r-vellum.github.io/vellumplot/reference/scale_alpha.md)
/
[`scale_linetype()`](https://r-vellum.github.io/vellumplot/reference/scale_linetype.md):
an edge aesthetic is trained and legended **independently of the node
scales**, so a figure can map, say, node fill to a discrete community
*and* edge colour to a continuous weight without the two collapsing into
one legend. Each draws its own legend automatically.

## Usage

``` r
scale_edge_color(
  plot,
  palette = NULL,
  breaks = NULL,
  labels = NULL,
  name = NULL,
  midpoint = NULL
)

scale_edge_colour(
  plot,
  palette = NULL,
  breaks = NULL,
  labels = NULL,
  name = NULL,
  midpoint = NULL
)

scale_edge_alpha(plot, range = NULL, limits = NULL, breaks = NULL, name = NULL)

scale_edge_linetype(plot, values = NULL, name = NULL)
```

## Arguments

- plot:

  A
  [PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md),
  normally from
  [`vgraph()`](https://r-vellum.github.io/vellumplot/reference/vgraph.md).

- palette:

  For `scale_edge_color()`, a vector of colours or a single palette name
  (as in
  [`scale_color_continuous()`](https://r-vellum.github.io/vellumplot/reference/scale_color_continuous.md));
  `NULL` for a default.

- name:

  Legend title, or `NULL` to derive from the encoding.

- midpoint:

  For `scale_edge_color()`, the data value placed at the ramp's midpoint
  (a diverging scale); `NULL` for an ordinary ramp.

- range:

  For `scale_edge_alpha()`, the output opacity range `c(min, max)`, or
  `NULL` for the default.

- limits, breaks, labels:

  Data limits, explicit legend breaks, and explicit labels, or `NULL` to
  derive from the data.

- values:

  For `scale_edge_linetype()`, a character vector of line types (as in
  [`scale_linetype()`](https://r-vellum.github.io/vellumplot/reference/scale_linetype.md));
  `NULL` for the default palette.

## Value

The modified
[PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

## Details

`scale_edge_color()` infers discrete vs continuous from the mapped data
(a factor/character trains a discrete swatch legend; a number a colour
bar), the same as the node colour scale. `scale_edge_colour()` is a
British-spelling alias.

## See also

[`mark_edges()`](https://r-vellum.github.io/vellumplot/reference/mark_graph.md),
[`scale_edge_width()`](https://r-vellum.github.io/vellumplot/reference/scale_edge_width.md),
[`vgraph()`](https://r-vellum.github.io/vellumplot/reference/vgraph.md)

## Examples

``` r
if (FALSE) { # \dontrun{
g <- igraph::make_graph("Zachary")
g <- igraph::set_edge_attr(g, "w", value = runif(igraph::ecount(g)))
vgraph(g) |>
  mark_edges(color = w) |>
  mark_nodes(fill = factor(igraph::membership(igraph::cluster_louvain(g)))) |>
  scale_edge_color(palette = "Grays")
} # }
```
