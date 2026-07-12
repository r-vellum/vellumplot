# Edge-width scale

Declare the scale for a mapped edge `linewidth` aesthetic (e.g.
`mark_edges(linewidth = weight)` on a
[`vgraph()`](https://r-vellum.github.io/vellumplot/reference/vgraph.md)
plot). The data range is rescaled to a line-width range and an
edge-width legend is drawn automatically.

## Usage

``` r
scale_edge_width(plot, range = NULL, limits = NULL, breaks = NULL, name = NULL)
```

## Arguments

- plot:

  A
  [PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

- range:

  Output line-width range `c(min, max)`, or `NULL` for the default
  `c(0.3, 3)`.

- limits:

  Data limits `c(min, max)`, or `NULL` to train from the data.

- breaks:

  Explicit legend breaks, or `NULL`.

- name:

  Legend title, or `NULL` to derive from the encoding.

## Value

The modified
[PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

## See also

[`mark_edges()`](https://r-vellum.github.io/vellumplot/reference/mark_graph.md),
[`vgraph()`](https://r-vellum.github.io/vellumplot/reference/vgraph.md)

## Examples

``` r
if (FALSE) { # \dontrun{
g <- igraph::sample_gnp(20, 0.2)
g <- igraph::set_edge_attr(g, "w", value = runif(igraph::ecount(g)))
vgraph(g) |> mark_edges(linewidth = w) |> mark_nodes() |> scale_edge_width(range = c(0.3, 4))
} # }
```
