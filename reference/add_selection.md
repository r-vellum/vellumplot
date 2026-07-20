# Register a free-standing selection on a plot

Attach a `SelectionSpec` (from the free-standing form of
[`select_point()`](https://r-vellum.github.io/vellumplot/reference/select_point.md)
/
[`select_interval()`](https://r-vellum.github.io/vellumplot/reference/select_point.md))
to a plot, marking that the selection's gesture is active on this plot's
marks. Use it when a selection is defined once and shared across views
in a composition (define + `add_selection()` on one plot,
[`filter_by()`](https://r-vellum.github.io/vellumplot/reference/filter_by.md)
on another).

## Usage

``` r
add_selection(plot, selection)
```

## Arguments

- plot:

  A
  [PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

- selection:

  A `SelectionSpec`.

## Value

The
[PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md),
with the selection registered.

## See also

[`select_point()`](https://r-vellum.github.io/vellumplot/reference/select_point.md),
[`filter_by()`](https://r-vellum.github.io/vellumplot/reference/filter_by.md)
