# Bivariate palettes

`bi_pal()` returns the colours of a bivariate palette: a `dim` x `dim`
matrix of colours, one per pair of classes, flattened to a vector named
by class (`"1-1"`, `"2-1"`, ..., `"3-3"`; the first number is the `x`
class, the second the `y` class). It is what
[`scale_fill_bivariate()`](https://r-vellum.github.io/vellumplot/reference/scale_fill_bivariate.md)
colours each class with.

## Usage

``` r
bi_pal(palette = "DkBlue", dim = 3)
```

## Arguments

- palette:

  A palette name: `"DkBlue"` (default), `"GrPink"`, `"DkViolet"` or
  `"Brown"`.

- dim:

  Classes per variable: `2`, `3` (default) or `4`.

## Value

A named character vector of `dim^2` colours.

## Details

The built-in palettes are Joshua Stevens' four 3x3 schemes; for
`dim = 2` and `dim = 4` they are interpolated in Lab space from the 3x3
grid, so the corner colours are the same at every size. The lightest
corner (`"1-1"`) is low on both variables, the darkest (`"3-3"` at
`dim = 3`) high on both.

## See also

[`bi_class()`](https://r-vellum.github.io/vellumplot/reference/bi_class.md),
[`scale_fill_bivariate()`](https://r-vellum.github.io/vellumplot/reference/scale_fill_bivariate.md)

Other bivariate:
[`bi_class()`](https://r-vellum.github.io/vellumplot/reference/bi_class.md),
[`guide_bivariate()`](https://r-vellum.github.io/vellumplot/reference/guide_bivariate.md),
[`scale_fill_bivariate()`](https://r-vellum.github.io/vellumplot/reference/scale_fill_bivariate.md)

## Examples

``` r
bi_pal("GrPink")
#>       1-1       2-1       3-1       1-2       2-2       3-2       1-3       2-3 
#> "#e8e8e8" "#e4acac" "#c85a5a" "#b0d5df" "#ad9ea5" "#985356" "#64acbe" "#627f8c" 
#>       3-3 
#> "#574249" 
bi_pal("DkBlue", dim = 2)
#>       1-1       2-1       1-2       2-2 
#> "#E8E8E8" "#6C83B5" "#73AE80" "#2A5A5B" 
```
