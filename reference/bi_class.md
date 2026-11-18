# Classify two variables for a bivariate map

`bi_class()` bins two numeric columns into `dim` classes each and adds
one column holding the pair of classes as `"x-y"` (`"1-1"` is low on
both, `"3-3"` high on both at `dim = 3`). Map a fill to that column and
add
[`scale_fill_bivariate()`](https://r-vellum.github.io/vellumplot/reference/scale_fill_bivariate.md)
to colour it with a bivariate palette and draw the 2-D legend. The
design follows the `biscale` package's `bi_class()`.

## Usage

``` r
bi_class(data, x, y, dim = 3, style = "quantile", name = "bi_class")
```

## Arguments

- data:

  A data frame or `sf` object.

- x, y:

  The two numeric variables (bare column names). `x` runs along the
  legend's horizontal axis, `y` up its vertical one.

- dim:

  Classes per variable: `2`, `3` (default) or `4`.

- style:

  How to choose the class breaks: `"quantile"` (default), `"equal"`,
  `"pretty"`, or any
  [`classInt::classIntervals()`](https://r-spatial.github.io/classInt/reference/classIntervals.html)
  style (needs the `classInt` package) — the same styles as
  [`scale_fill_binned()`](https://r-vellum.github.io/vellumplot/reference/scale_fill_binned.md).

- name:

  Name of the new column (default `"bi_class"`).

## Value

`data` with a factor column `name` whose levels are all `dim^2` classes
in order. Rows where `x` or `y` is missing get `NA`.

## Details

Classes are computed over the whole table, so a faceted map shares one
set of breaks across its panels. `bi_class()` works on a plain data
frame or an `sf` table (the geometry is kept). It records the two
variable names on the new column, which
[`scale_fill_bivariate()`](https://r-vellum.github.io/vellumplot/reference/scale_fill_bivariate.md)
uses as the legend's axis titles when you don't give your own.

## See also

[`scale_fill_bivariate()`](https://r-vellum.github.io/vellumplot/reference/scale_fill_bivariate.md),
[`bi_pal()`](https://r-vellum.github.io/vellumplot/reference/bi_pal.md)

Other bivariate:
[`bi_pal()`](https://r-vellum.github.io/vellumplot/reference/bi_pal.md),
[`guide_bivariate()`](https://r-vellum.github.io/vellumplot/reference/guide_bivariate.md),
[`scale_fill_bivariate()`](https://r-vellum.github.io/vellumplot/reference/scale_fill_bivariate.md)

## Examples

``` r
d <- bi_class(mtcars, hp, wt)
table(d$bi_class)
#> 
#> 1-1 2-1 3-1 1-2 2-2 3-2 1-3 2-3 3-3 
#>   8   2   1   3   5   2   0   1  10 
```
