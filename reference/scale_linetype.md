# Line-type scale

Declare the scale for a mapped (discrete) `linetype` aesthetic. Levels
cycle through a default set of line types unless `values` is given. A
line-type legend is drawn automatically. Applies to line-like marks
([`mark_line()`](https://r-vellum.github.io/vellumplot/reference/mark_point.md),
[`mark_step()`](https://r-vellum.github.io/vellumplot/reference/mark_area.md)).

## Usage

``` r
scale_linetype(
  plot,
  values = NULL,
  name = NULL,
  breaks = NULL,
  labels = NULL,
  limits = NULL,
  na.value = NULL
)

scale_linetype_manual(
  plot,
  values,
  breaks = NULL,
  labels = NULL,
  name = NULL,
  limits = NULL,
  na.value = NULL
)

scale_linetype_discrete(
  plot,
  values = NULL,
  name = NULL,
  breaks = NULL,
  labels = NULL,
  limits = NULL,
  na.value = NULL
)
```

## Arguments

- plot:

  A
  [PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

- values:

  Character vector of line types (each one of `"solid"`, `"dashed"`,
  `"dotted"`, `"dotdash"`, `"longdash"`, `"twodash"`), or `NULL` for the
  default set. For `scale_linetype_manual()` `values` is required, and
  may also be ggplot2's numeric line types (`1` solid, `2` dashed, ...
  `6` twodash). Named values are matched to levels by name.

- name:

  Legend title, or `NULL` to derive from the encoding.

- breaks, labels, limits:

  As for
  [`scale_shape()`](https://r-vellum.github.io/vellumplot/reference/scale_shape.md).

- na.value:

  The line type drawn for missing values, or `NULL`.

## Value

The modified
[PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

## Examples

``` r
df <- data.frame(x = rep(1:10, 2), y = rnorm(20), g = rep(c("a", "b"), each = 10))
vplot(df) |> mark_line(x = x, y = y, linetype = g)

vplot(df) |>
  mark_line(x = x, y = y, linetype = g) |>
  scale_linetype_manual(values = c(a = "dotted", b = "solid"))
```
