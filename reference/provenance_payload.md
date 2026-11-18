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

A list with

- `fields`:

  the plot data's column names.

- `elements`:

  a list of `list(id, rows, source)` records.

- `sources`:

  for a
  [PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md),
  one record per table an element's rows refer to, named by `source`:
  `list(fields)`, plus `data` (row-wise records) when `values = TRUE`.

- `data`:

  when `values = TRUE`, row-wise records of the plot's data (the
  `"plot"` source, kept at the top level for existing hosts).

## Details

Row indices are 1-based and refer to the table named by the element's
`source`: `"plot"` for the plot's data, or `"layer:<i>"` for the data
passed to layer `i`'s own `data =` argument. In a faceted plot they are
rows of that whole table, not of the panel's subset.

## See also

[`provenance_join()`](https://r-vellum.github.io/vellumplot/reference/provenance_join.md),
[`plot_provenance()`](https://r-vellum.github.io/vellumplot/reference/plot_provenance.md)

## Examples

``` r
p <- vplot(mtcars) |> mark_point(x = wt, y = mpg, color = factor(cyl))
pl <- provenance_payload(p)
pl$elements[[1]]$id
#> [1] "layer-1-point-g1"
pl$elements[[1]]$source
#> [1] "plot"
```
