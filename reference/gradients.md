# Gradient fill paints

Thin re-exports of
[`vellum::linear_gradient()`](https://r-vellum.github.io/vellum/reference/gradients.html)
and
[`vellum::radial_gradient()`](https://r-vellum.github.io/vellum/reference/gradients.html).
A gradient is an unscaled *value* for the `fill` aesthetic: pass it
directly, e.g.
`mark_area(x = t, y = y, fill = linear_gradient(c("#00e5ff", "#00e5ff00")))`,
and the filled region (area / ribbon / bar) is painted with the paint as
a single grob. Use `"transparent"` (or an `"#RRGGBB00"` colour) as a
stop to fade out — the "glow fade under a line" look. The gradient's
`x1`/`y1`/`x2`/`y2` (in `units`, `"npc"` by default) set its direction.
A gradient cannot be *mapped* to a data column (it is one paint per
region).

## Usage

``` r
linear_gradient(colours, stops = NULL, ...)

radial_gradient(colours, stops = NULL, ...)
```

## Arguments

- colours, stops:

  See
  [`vellum::linear_gradient()`](https://r-vellum.github.io/vellum/reference/gradients.html)
  /
  [`vellum::radial_gradient()`](https://r-vellum.github.io/vellum/reference/gradients.html).

- ...:

  Further gradient arguments passed to the vellum constructor:
  `x1`/`y1`/`x2`/`y2` (linear), `cx`/`cy`/`r` (radial), `units`,
  `extend`, and `interpolation` (`"srgb"` default, or `"oklab"` /
  `"oklch"` to blend the stops perceptually — `"oklch"` additionally
  preserves chroma by rotating hue). See
  [`vellum::linear_gradient()`](https://r-vellum.github.io/vellum/reference/gradients.html)
  /
  [`vellum::radial_gradient()`](https://r-vellum.github.io/vellum/reference/gradients.html).

## Value

A `vellum_gradient` object usable as a `fill` value.

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
