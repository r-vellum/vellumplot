# Custom tiling-pattern fill

`vl_pattern()` is
[`vellum::vl_pattern()`](https://r-vellum.github.io/vellum/reference/vl_pattern.html),
re-exported, for building a pattern fill from an arbitrary tile grob
when the ready-made
[`pattern_stripe()`](https://r-vellum.github.io/vellumplot/reference/patterns.md)
family does not fit. Like a gradient, the result (a `vellum_pattern`
object) is an unscaled `fill` *value*. It takes the tile `grob`, the
tile `width` / `height` and anchor `x` / `y` in `units`, and `extend`;
see
[`vellum::vl_pattern()`](https://r-vellum.github.io/vellum/reference/vl_pattern.html)
for the full reference.

## See also

[`pattern_stripe()`](https://r-vellum.github.io/vellumplot/reference/patterns.md),
[`linear_gradient()`](https://r-vellum.github.io/vellumplot/reference/gradients.md)

## Examples

``` r
dots <- vellum::circle_grob(r = 0.25, gp = vellum::vl_gpar(fill = "grey30"))
vl_pattern(dots, width = 0.08, height = 0.08)
#> <vellum_pattern> cell 0.08 x 0.08 "npc", extend = "repeat"
```
