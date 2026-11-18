# A 2-D legend for a bivariate scale

`guide_bivariate()` draws a bivariate colour scale as a square grid of
swatches: the `x` variable's classes run left to right along the bottom,
the `y` variable's classes bottom to top up the side, each axis with its
title and an arrow pointing towards "more". It is the default guide of
[`scale_fill_bivariate()`](https://r-vellum.github.io/vellumplot/reference/scale_fill_bivariate.md);
use it through
[`guides()`](https://r-vellum.github.io/vellumplot/reference/guides.md)
to retitle the axes or resize the swatches without respelling the scale.
Asking for it on a fill that maps a
[`bi_class()`](https://r-vellum.github.io/vellumplot/reference/bi_class.md)-style
column makes that scale bivariate.

## Usage

``` r
guide_bivariate(title = NULL, xlab = NULL, ylab = NULL, key.size = NULL)
```

## Arguments

- title:

  A title above the grid, or `NULL` (default) for none.

- xlab, ylab:

  The axis titles, or `NULL` to keep the scale's (or the variable names
  recorded by
  [`bi_class()`](https://r-vellum.github.io/vellumplot/reference/bi_class.md)).

- key.size:

  Side of one swatch, in millimetres. `NULL` (default) uses 1.5 x the
  theme's `legend.key.size`.

## Value

A guide specification for
[`guides()`](https://r-vellum.github.io/vellumplot/reference/guides.md).

## See also

[`scale_fill_bivariate()`](https://r-vellum.github.io/vellumplot/reference/scale_fill_bivariate.md)

Other bivariate:
[`bi_class()`](https://r-vellum.github.io/vellumplot/reference/bi_class.md),
[`bi_pal()`](https://r-vellum.github.io/vellumplot/reference/bi_pal.md),
[`scale_fill_bivariate()`](https://r-vellum.github.io/vellumplot/reference/scale_fill_bivariate.md)

## Examples

``` r
d <- bi_class(mtcars, hp, wt)
vplot(d) |>
  mark_point(x = hp, y = wt, color = bi_class, size = 2) |>
  guides(color = guide_bivariate(xlab = "More power", ylab = "More weight"))
```
