# The interaction model of a compiled plot

Returns the serialisable declaration block a host needs to enact a
plot's declarative interactivity (selections, conditional encodings,
filters, and scale-domain binds), or `NULL` when the plot declares none.
This is the plot-level companion to the per-element metadata carried on
the compiled scene; `vellumwidget::as_widget()` reads it to wire
gestures to reactions.

## Usage

``` r
interaction_model(x)
```

## Arguments

- x:

  A
  [PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md)
  or a plot composition.

## Value

A named list (`selections`, `conditions`, `filters`, `binds`), or `NULL`
if there is no declared interactivity.
