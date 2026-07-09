# Shape scale

Declare the scale for a mapped (discrete) `shape` aesthetic. Levels
cycle through a default set of marker shapes unless `values` is given. A
shape legend is drawn automatically.

## Usage

``` r
scale_shape(plot, values = NULL, name = NULL)
```

## Arguments

- plot:

  A
  [PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

- values:

  Character vector of shapes (each one of `"circle"`, `"square"`,
  `"triangle"`, `"diamond"`, `"plus"`, `"cross"`), or `NULL` for the
  default.

- name:

  Legend title, or `NULL` to derive from the encoding.

## Value

The modified
[PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

## Examples

``` r
vplot(mtcars) |> mark_point(x = wt, y = mpg, shape = factor(cyl))
```
