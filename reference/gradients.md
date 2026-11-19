# Gradient fill paints

`linear_gradient()` and `radial_gradient()` are
[`vellum::linear_gradient()`](https://r-vellum.github.io/vellum/reference/gradients.html)
and
[`vellum::radial_gradient()`](https://r-vellum.github.io/vellum/reference/gradients.html),
re-exported. A gradient is an unscaled *value* for the `fill` aesthetic:
pass it directly, e.g.
`mark_area(x = t, y = y, fill = linear_gradient(c("#00e5ff", "#00e5ff00")))`,
and the filled region (area / ribbon / bar) is painted with the paint as
a single grob. Use `"transparent"` (or an `"#RRGGBB00"` colour) as a
stop to fade out — the "glow fade under a line" look. A gradient cannot
be *mapped* to a data column (it is one paint per region).

## Details

Both take `colours` and optional `stops`. A linear gradient's
`x1`/`y1`/`x2`/`y2` (in `units`, `"npc"` by default) set its direction;
a radial gradient takes `cx`/`cy`/`r` plus the optional focal circle
`fx`/`fy`/`fr`. Both also take `extend` and `interpolation` (`"srgb"`
default, or `"oklab"` / `"oklch"` to blend the stops perceptually —
`"oklch"` additionally preserves chroma by rotating hue). Each returns a
`vellum_gradient` object usable as a `fill` value. See
[`vellum::linear_gradient()`](https://r-vellum.github.io/vellum/reference/gradients.html)
/
[`vellum::radial_gradient()`](https://r-vellum.github.io/vellum/reference/gradients.html)
for the full reference.

## See also

[`glow()`](https://r-vellum.github.io/vellumplot/reference/glow.md),
[`theme_cyberpunk()`](https://r-vellum.github.io/vellumplot/reference/theme_gray.md)

## Examples

``` r
df <- data.frame(x = 1:20, y = cumsum(abs(rnorm(20))))
vplot(df) |>
  mark_area(x = x, y = y, fill = linear_gradient(c("#00e5ff", "#00e5ff00"),
    x1 = 0, y1 = 1, x2 = 0, y2 = 0))
```
