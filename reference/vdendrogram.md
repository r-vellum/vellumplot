# Dendrogram from a clustering

`vdendrogram()` turns a base `hclust`/`dendrogram` (e.g. from
[`stats::hclust()`](https://rdrr.io/r/stats/hclust.html)) into a ready
node-link dendrogram: leaves spread along a line, each merge drawn at
its height, joined by right-angle brackets. It is a thin preset over
`vgraph(<clustering>, layout = "dendrogram")` plus bracket edges and
leaf labels; for full control, use that path and add marks yourself.

## Usage

``` r
vdendrogram(
  x,
  direction = c("down", "up", "left", "right"),
  k = NULL,
  labels = TRUE,
  label_size = 5,
  width = 7,
  height = 5,
  dpi = 96
)
```

## Arguments

- x:

  An `hclust` or `dendrogram` object.

- direction:

  Growth direction (leaves opposite the root): `"down"` (default; root
  at top), `"up"`, `"left"`, or `"right"`.

- k:

  Optionally cut the tree into `k` clusters
  ([`stats::cutree()`](https://rdrr.io/r/stats/cutree.html)) and colour
  branches and leaf labels by cluster; branches above the cut stay
  neutral. `NULL` (default) draws a single-colour tree.

- labels:

  Draw the leaf labels? Default `TRUE`.

- label_size:

  Leaf-label font size (points). Default `5`; lower it for a tree with
  many leaves.

- width, height, dpi:

  Page size (inches) and resolution.

## Value

A
[PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

## See also

[`vgraph()`](https://r-vellum.github.io/vellumplot/reference/vgraph.md)
for the general node-link path (`layout = "dendrogram"` or
`"unrooted"`).

## Examples

``` r
hc <- hclust(dist(USArrests))
vdendrogram(hc, k = 3)
```
