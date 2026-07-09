# Overlay a plot as an inset

Place `plot` as an inset over `base`, positioned by fractional
coordinates (0–1) of the chosen reference box. Returns a 1-cell
`PlotComposition` carrying the inset (compose it further or render
directly).

## Usage

``` r
inset(
  base,
  plot,
  left = 0.6,
  bottom = 0.6,
  right = 0.98,
  top = 0.98,
  align_to = c("panel", "plot", "full"),
  on_top = TRUE
)
```

## Arguments

- base:

  A
  [PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md)
  or `PlotComposition` to draw underneath.

- plot:

  The
  [PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md)
  to overlay.

- left, bottom, right, top:

  Inset position as fractions (0–1).

- align_to:

  Reference box: `"panel"`, `"plot"`, or `"full"` (currently the whole
  `base` cell).

- on_top:

  Draw the inset above (`TRUE`) or below the base.

## Value

A `PlotComposition`.

## Examples

``` r
a <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
b <- vplot(mtcars) |> mark_histogram(x = mpg, bins = 8)
inset(a, b, left = 0.55, bottom = 0.55, right = 0.98, top = 0.98)
```
