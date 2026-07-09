# Resolve scales as shared or independent across panels

The scale-resolution lattice (after Vega-Lite): for each aesthetic,
choose whether faceted panels share one trained scale or train
independent ones. Position scales default to `"shared"`;
`facet_*(scales = "free_*")` is sugar for setting `x`/`y` to
`"independent"`.

## Usage

``` r
resolve_scale(plot, ...)
```

## Arguments

- plot:

  A
  [PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

- ...:

  Named arguments mapping an aesthetic (`x`, `y`, ...) to `"shared"` or
  `"independent"`.

## Value

The modified
[PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

## Examples

``` r
vplot(mtcars) |> mark_point(x = wt, y = mpg) |> facet_wrap(~cyl) |>
  resolve_scale(y = "independent")
```
