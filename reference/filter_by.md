# Filter a view by a selection

Show only the rows in `selection`, hiding the rest (a display-tier hide,
not a re-aggregation). Point a second view's `filter_by()` at a
selection whose gesture lives in a first view and you get
**cross-filtering**: brush one panel, a linked panel redraws to just
those rows.

## Usage

``` r
filter_by(plot, selection)
```

## Arguments

- plot:

  A
  [PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

- selection:

  A `SelectionSpec`, or a selection name string.

## Value

The
[PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md),
with the filter registered.

## See also

[`select_interval()`](https://r-vellum.github.io/vellumplot/reference/select_point.md),
[`add_selection()`](https://r-vellum.github.io/vellumplot/reference/add_selection.md)
