# A widget-ready provenance payload (click-to-source)

`provenance_payload()` returns a JSON-serializable structure mapping
each rendered element's stable id (`data-vellum-id`, the join key an SVG
/ widget carries) to the source data rows that produced it — the enabler
for a host (e.g. vellumwidget) to answer "which rows made this element?"
on click, without re-running the grammar. With `values = TRUE` the
referenced data rows are inlined so the host can display them directly.

## Usage

``` r
provenance_payload(x, values = FALSE)
```

## Arguments

- x:

  A
  [PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md)
  or composition.

- values:

  If `TRUE`, inline the source data rows (row-wise records) so the host
  needs no separate data access. Default `FALSE` (ids + row indices
  only).

## Value

A list with `fields` (column names), `elements` (a list of
`list(id, rows)` records), and — when `values = TRUE` — `data` (row-wise
records of the plot's data).

## See also

[`provenance_join()`](https://r-vellum.github.io/vellumplot/reference/provenance_join.md),
[`plot_provenance()`](https://r-vellum.github.io/vellumplot/reference/plot_provenance.md)

## Examples

``` r
p <- vplot(mtcars) |> mark_point(x = wt, y = mpg, color = factor(cyl))
pl <- provenance_payload(p)
pl$elements[[1]]$id
#> [1] "layer-1-point-g1"
```
