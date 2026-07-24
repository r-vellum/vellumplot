# Custom tiling-pattern fill

A thin re-export of
[`vellum::vl_pattern()`](https://r-vellum.github.io/vellum/reference/vl_pattern.html)
for building a pattern fill from an arbitrary tile grob, when the
ready-made
[`pattern_stripe()`](https://r-vellum.github.io/vellumplot/reference/patterns.md)
family does not fit. Like a gradient, the result is an unscaled `fill`
*value*.

## Usage

``` r
vl_pattern(
  grob,
  width = 0.1,
  height = 0.1,
  x = 0.5,
  y = 0.5,
  units = "npc",
  extend = "repeat"
)
```

## Arguments

- grob, width, height, x, y, units, extend:

  See
  [`vellum::vl_pattern()`](https://r-vellum.github.io/vellum/reference/vl_pattern.html).

## Value

A `vellum_pattern` object usable as a `fill` value.

## See also

[`pattern_stripe()`](https://r-vellum.github.io/vellumplot/reference/patterns.md),
[`linear_gradient()`](https://r-vellum.github.io/vellumplot/reference/gradients.md)

## Examples

``` r
dots <- vellum::circle_grob(r = 0.25, gp = vellum::vl_gpar(fill = "grey30"))
vl_pattern(dots, width = 0.08, height = 0.08)
#> $grob
#> <vellum::grob_circle>
#>  @ name  : NULL
#>  @ gp    : <vellum::vl_gpar>
#>  .. @ col       : NULL
#>  .. @ fill      : chr "grey30"
#>  .. @ lwd       : NULL
#>  .. @ alpha     : NULL
#>  .. @ lty       : NULL
#>  .. @ lineend   : NULL
#>  .. @ linejoin  : NULL
#>  .. @ linemitre : NULL
#>  .. @ fontfamily: NULL
#>  .. @ fontface  : NULL
#>  .. @ fontsize  : NULL
#>  .. @ lineheight: NULL
#>  @ vp    : NULL
#>  @ id    : NULL
#>  @ role  : NULL
#>  @ keys  : NULL
#>  @ meta  : NULL
#>  @ x     : unit [1:1] 0.5npc
#>  @ y     : unit [1:1] 0.5npc
#>  @ r     : unit [1:1] 0.25npc
#>  @ sketch: NULL
#> 
#> $width
#> [1] 0.08
#> 
#> $height
#> [1] 0.08
#> 
#> $x
#> [1] 0.5
#> 
#> $y
#> [1] 0.5
#> 
#> $units
#> [1] "npc"
#> 
#> $extend
#> [1] "repeat"
#> 
#> attr(,"class")
#> [1] "vellum_pattern"
```
