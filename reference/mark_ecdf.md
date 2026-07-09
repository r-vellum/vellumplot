# Distribution marks

Marks that summarise the distribution of a variable. `mark_ecdf()` draws
the empirical cumulative distribution of `x` as a step; `mark_rug()`
draws marginal ticks at each datum; `mark_qq()` draws a
quantile-quantile plot of a `sample` against a theoretical distribution,
with `mark_qq_line()` adding the reference line. All respect a mapped
`color`/`fill` grouping.

## Usage

``` r
mark_ecdf(plot, ..., blend = NULL, data = NULL)

mark_rug(plot, ..., sides = "bl", length = 0.03, blend = NULL, data = NULL)

mark_qq(plot, ..., distribution = "qnorm", blend = NULL, data = NULL)

mark_qq_line(plot, ..., distribution = "qnorm", blend = NULL, data = NULL)
```

## Arguments

- plot:

  A
  [PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

- ...:

  Encodings. `mark_ecdf()` needs `x`; `mark_qq()`/`mark_qq_line()` need
  `sample`; `mark_rug()` takes `x` and/or `y`.

- blend, data:

  Standard layer arguments (see
  [`mark_point()`](https://r-vellum.github.io/vellumplot/reference/mark_point.md)).

- sides:

  Which edges `mark_rug()` draws ticks on: any of `"b"` (bottom), `"l"`
  (left), `"t"` (top), `"r"` (right); default `"bl"`.

- length:

  Rug tick length as a fraction of the panel (default `0.03`).

- distribution:

  Quantile function of the reference distribution for `mark_qq()` /
  `mark_qq_line()` (default
  [stats::qnorm](https://rdrr.io/r/stats/Normal.html)).

## Value

The modified
[PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

## Examples

``` r
vplot(mtcars) |> mark_ecdf(x = mpg)

vplot(mtcars) |> mark_point(x = wt, y = mpg) |> mark_rug()

vplot(mtcars) |> mark_qq(sample = mpg) |> mark_qq_line(sample = mpg)
```
