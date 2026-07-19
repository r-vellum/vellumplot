# Sankey (flow) diagram

`vsankey()` draws a layered flow diagram from a *flow list* — one row
per flow with a `from` node, a `to` node, and a `value` (the ribbon
width). Nodes are the union of `from`/`to`; a node that is both a source
and a target makes the diagram multi-stage. Like
[`vgraph()`](https://r-vellum.github.io/vellumplot/reference/vgraph.md),
it returns a ready
[PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md)
with an axis-free, aspect-free panel; `mark_sankey()` is the layer it
adds and can be used directly on a plot you have set up yourself.

## Usage

``` r
vsankey(data, from, to, value, label = TRUE, width = 8, height = 5, dpi = 96)

mark_sankey(plot, from, to, value, label = TRUE)
```

## Arguments

- data:

  A data frame of flows.

- from, to, value:

  Columns (tidy-eval): the source node, target node, and flow value.

- label:

  Draw node labels? Default `TRUE`.

- width, height, dpi:

  Page size (inches) and resolution.

- plot:

  A
  [PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

## Value

A
[PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md)
(`vsankey()`) or the modified plot (`mark_sankey()`).

## Details

The flows must form a DAG (no cycles). Nodes within a column are ordered
to minimise ribbon crossings (a deterministic Sugiyama barycenter
sweep), and ribbons are stacked to meet each node in matching order.
Nodes are coloured from the built-in qualitative palette.

## Examples

``` r
flows <- data.frame(
  from = c("A", "A", "B", "C", "C"),
  to = c("B", "C", "D", "D", "E"),
  value = c(4, 6, 4, 4, 2)
)
vsankey(flows, from, to, value)
```
