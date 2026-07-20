# Bind a panel's view to a selection (overview + detail)

Make the `aes` view of this plot's panel follow an interval
[selection](https://r-vellum.github.io/vellumplot/reference/select_point.md)
defined on another (overview) plot: dragging the overview brush pans and
zooms this (detail) panel to the selected range. This is a display-tier
view change (pan/zoom), not a scale retrain.

## Usage

``` r
bind_scale(plot, selection, aes = c("x", "y"))
```

## Arguments

- plot:

  A
  [PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md)
  (the detail view).

- selection:

  A `SelectionSpec` (an interval selection), or a name string.

- aes:

  The axis to bind, `"x"` (default) or `"y"`.

## Value

The
[PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md),
with the bind registered.

## See also

[`select_interval()`](https://r-vellum.github.io/vellumplot/reference/select_point.md)
