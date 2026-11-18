# Bivariate colour scales

`scale_fill_bivariate()` colours a
[`bi_class()`](https://r-vellum.github.io/vellumplot/reference/bi_class.md)
column with a bivariate palette and draws its legend as a `dim` x `dim`
grid of swatches, the `x` variable along the bottom and the `y` variable
up the side (see
[`guide_bivariate()`](https://r-vellum.github.io/vellumplot/reference/guide_bivariate.md)).
It is the scale half of a bivariate choropleth; it works on any mark
with a fill
([`mark_sf()`](https://r-vellum.github.io/vellumplot/reference/mark_sf.md)
polygons, tiles, bars, points), and `scale_color_bivariate()` does the
same for `color`.

## Usage

``` r
scale_fill_bivariate(
  plot,
  palette = "DkBlue",
  dim = NULL,
  xlab = NULL,
  ylab = NULL,
  name = NULL,
  na_value = "grey80"
)

scale_color_bivariate(
  plot,
  palette = "DkBlue",
  dim = NULL,
  xlab = NULL,
  ylab = NULL,
  name = NULL,
  na_value = "grey80"
)

scale_colour_bivariate(
  plot,
  palette = "DkBlue",
  dim = NULL,
  xlab = NULL,
  ylab = NULL,
  name = NULL,
  na_value = "grey80"
)
```

## Arguments

- plot:

  A
  [PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

- palette:

  A palette name for
  [`bi_pal()`](https://r-vellum.github.io/vellumplot/reference/bi_pal.md)
  (`"DkBlue"`, `"GrPink"`, `"DkViolet"`, `"Brown"`), or your own `dim^2`
  colours, named by class (`"1-1"`, `"2-1"`, ...) or given in that
  order.

- dim:

  Classes per variable (`2`, `3` or `4`). `NULL` (default) takes it from
  the
  [`bi_class()`](https://r-vellum.github.io/vellumplot/reference/bi_class.md)
  column, falling back to `3`.

- xlab, ylab:

  The legend's axis titles. `NULL` uses the variable names recorded by
  [`bi_class()`](https://r-vellum.github.io/vellumplot/reference/bi_class.md).

- name:

  A title above the legend grid. `NULL` (default) draws none — the axis
  titles already say what the colours mean.
  [`labs()`](https://r-vellum.github.io/vellumplot/reference/labs.md)
  also sets it.

- na_value:

  Fill for features with a missing class. Default `"grey80"`.

## Value

The modified
[PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

## Details

A column made by
[`bi_class()`](https://r-vellum.github.io/vellumplot/reference/bi_class.md)
is recognised on its own, so the scale is only needed to change the
palette or the axis titles. The class count is read from that column
too. A plain character or factor column of `"x-y"` class labels also
works with an explicit `dim`.

Hover and selection work as for any discrete colour scale: on an
interactive plot each legend swatch is keyed to the features in its
class.

## See also

[`bi_class()`](https://r-vellum.github.io/vellumplot/reference/bi_class.md),
[`guide_bivariate()`](https://r-vellum.github.io/vellumplot/reference/guide_bivariate.md),
[`bi_pal()`](https://r-vellum.github.io/vellumplot/reference/bi_pal.md)

Other bivariate:
[`bi_class()`](https://r-vellum.github.io/vellumplot/reference/bi_class.md),
[`bi_pal()`](https://r-vellum.github.io/vellumplot/reference/bi_pal.md),
[`guide_bivariate()`](https://r-vellum.github.io/vellumplot/reference/guide_bivariate.md)

## Examples

``` r
d <- bi_class(mtcars, hp, wt)
vplot(d) |>
  mark_point(x = hp, y = wt, color = bi_class, size = 2) |>
  scale_color_bivariate(palette = "GrPink", xlab = "Power", ylab = "Weight")


if (requireNamespace("sf", quietly = TRUE)) {
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  nc <- bi_class(nc, BIR74, SID74)
  vplot(nc) |>
    mark_sf(fill = bi_class, color = "white", linewidth = 0.2) |>
    scale_fill_bivariate(xlab = "Births", ylab = "SIDS cases")
}
```
