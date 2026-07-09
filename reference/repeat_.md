# Repeat a view across fields

Replicate a plot, re-pointing one or more encodings at a different data
field each time, and arrange the copies as a composition (like
[`concat()`](https://r-vellum.github.io/vellumplot/reference/concat.md),
so each sub-plot keeps its own scales and axes). Supply each aesthetic
as a character vector of column names; all vectors must be the same
length `N`, and are zipped to produce `N` sub-plots.

## Usage

``` r
repeat_(plot, ..., ncol = NULL, nrow = NULL)
```

## Arguments

- plot:

  A
  [PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md);
  the repeated aesthetic(s) are set on **every** layer (added if not
  already mapped). In a multi-layer plot every layer therefore receives
  the re-pointed field, so any layer carrying its own `data` must
  contain the named field columns.

- ...:

  Named aesthetics, each a character vector of field names, e.g.
  `x = c("wt", "hp", "disp")`.

- ncol, nrow:

  Grid dimensions (passed to
  [`concat()`](https://r-vellum.github.io/vellumplot/reference/concat.md)).

## Value

A `PlotComposition`.

## Examples

``` r
repeat_(vplot(mtcars) |> mark_point(y = mpg), x = c("wt", "hp", "disp"))
```
