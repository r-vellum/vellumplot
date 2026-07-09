# Alpha (opacity) scale

Declare the scale for a mapped `alpha` aesthetic: data values map to an
opacity `range` in `[0, 1]`. An alpha legend is drawn automatically.

## Usage

``` r
scale_alpha(plot, range = c(0.1, 1), limits = NULL, breaks = NULL, name = NULL)

scale_alpha_continuous(
  plot,
  range = c(0.1, 1),
  limits = NULL,
  breaks = NULL,
  name = NULL
)
```

## Arguments

- plot:

  A
  [PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

- range:

  Numeric length-2 output opacity range (default `c(0.1, 1)`).

- limits:

  Numeric length-2 data domain, or `NULL` to train from the data.

- breaks:

  Explicit legend breaks, or `NULL`.

- name:

  Legend title, or `NULL` to derive from the encoding.

## Value

The modified
[PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

## Examples

``` r
vplot(mtcars) |> mark_point(x = wt, y = mpg, alpha = hp) |> scale_alpha(range = c(0.2, 1))
```
