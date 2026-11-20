# Outline (halo) layer effect

Draws one opaque, wider copy of a stroked or point mark beneath the
crisp original in a contrasting colour, so the mark stays legible over a
busy or dark backdrop (the "sticker" look). Applies to the same marks as
[`glow()`](https://r-vellum.github.io/vellumplot/reference/glow.md).

## Usage

``` r
outline(size = 1, color = NULL, alpha = 1)
```

## Arguments

- size:

  Halo width per side, in millimetres.

- color:

  Outline colour. `NULL` (the default) is the theme's paper colour
  ([`element_geom()`](https://r-vellum.github.io/vellumplot/reference/element_geom.md)):
  white on a light theme, the dark canvas on a dark one such as
  [`theme_cyberpunk()`](https://r-vellum.github.io/vellumplot/reference/theme_gray.md).

- alpha:

  Outline opacity.

## Value

An `OutlineSpec` object for a mark's `effects` list.

## See also

[`glow()`](https://r-vellum.github.io/vellumplot/reference/glow.md),
[`shadow()`](https://r-vellum.github.io/vellumplot/reference/shadow.md)

## Examples

``` r
vplot(mtcars) |>
  mark_point(x = wt, y = mpg, color = factor(cyl), size = 3,
    effects = list(outline()))
```
