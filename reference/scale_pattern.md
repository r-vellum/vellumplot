# Pattern (texture) scale

Map a discrete variable to fill **textures** – the greyscale- and
colour-vision-safe companion of a fill palette – via a filled mark's
`pattern` aesthetic: `mark_bar(x = g, y = n, pattern = series)`. Each
level takes a distinct
[`pattern_stripe()`](https://r-vellum.github.io/vellumplot/reference/patterns.md)-family
texture. Applies to the filled marks (bar, area, tile, rect, boxplot,
violin, hull).

## Usage

``` r
scale_pattern(plot, values = NULL, name = NULL)
```

## Arguments

- plot:

  A
  [PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

- values:

  Optional textures, one per level: a character vector of builder names
  (`"stripe"`, `"crosshatch"`, `"grid"`, `"dot"`, `"checker"`), or a
  list of `vellum_pattern` objects (e.g. from
  [`pattern_stripe()`](https://r-vellum.github.io/vellumplot/reference/patterns.md)).
  `NULL` (default) cycles a built-in palette of distinct textures.

- name:

  Optional legend title.

## Value

The modified
[PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

## See also

[`pattern_stripe()`](https://r-vellum.github.io/vellumplot/reference/patterns.md),
[`scale_shape()`](https://r-vellum.github.io/vellumplot/reference/scale_shape.md)

## Examples

``` r
df <- data.frame(g = c("a", "b", "c"), n = c(3, 5, 2))
vplot(df) |> mark_bar(x = g, y = n, pattern = g) |> scale_pattern()
```
