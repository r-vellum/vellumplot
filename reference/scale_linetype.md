# Line-type scale

Declare the scale for a mapped (discrete) `linetype` aesthetic. Levels
cycle through a default set of line types unless `values` is given. A
line-type legend is drawn automatically. Applies to line-like marks
([`mark_line()`](https://r-vellum.github.io/vellumplot/reference/mark_point.md),
[`mark_step()`](https://r-vellum.github.io/vellumplot/reference/mark_area.md)).

## Usage

``` r
scale_linetype(plot, values = NULL, name = NULL)

scale_linetype_discrete(plot, values = NULL, name = NULL)
```

## Arguments

- plot:

  A
  [PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

- values:

  Character vector of line types (each one of `"solid"`, `"dashed"`,
  `"dotted"`, `"dotdash"`, `"longdash"`, `"twodash"`), or `NULL` for the
  default set.

- name:

  Legend title, or `NULL` to derive from the encoding.

## Value

The modified
[PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

## Examples

``` r
df <- data.frame(x = rep(1:10, 2), y = rnorm(20), g = rep(c("a", "b"), each = 10))
vplot(df) |> mark_line(x = x, y = y, linetype = g)
```
