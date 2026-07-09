# Neon glow layer effect

A render effect for stroked and point marks, in the spirit of
[mplcyberpunk](https://github.com/dhaitz/mplcyberpunk): the mark is
drawn as several widened, low-opacity copies composited additively (a
`"screen"` blend) beneath the crisp original, producing a soft neon
halo. Pass it to a mark's `effects` argument, e.g.
`mark_line(..., effects = list(glow()))`. Pairs naturally with
[`theme_cyberpunk()`](https://r-vellum.github.io/vellumplot/reference/theme_gray.md).

## Usage

``` r
glow(size = 6, layers = 6L, alpha = 0.12, blend = "screen", color = NULL)
```

## Arguments

- size:

  Extra visual spread, in millimetres, added to the stroke width (or
  point diameter) at the outermost copy.

- layers:

  Number of stacked halo copies.

- alpha:

  Opacity of each copy (they accumulate toward the centre).

- blend:

  Blend mode compositing the halo copies, typically `"screen"` or
  `"lighten"` (any CSS `mix-blend-mode` name).

- color:

  Halo colour, or `NULL` (default) to inherit the mark's own resolved
  colour — the usual neon look.

## Value

A `GlowSpec` object for a mark's `effects` list.

## Details

The glow is applied per style group, so a colour-mapped multi-series
line glows each series in its own hue. It applies to
[`mark_point()`](https://r-vellum.github.io/vellumplot/reference/mark_point.md),
[`mark_line()`](https://r-vellum.github.io/vellumplot/reference/mark_point.md),
[`mark_step()`](https://r-vellum.github.io/vellumplot/reference/mark_area.md),
[`mark_rule()`](https://r-vellum.github.io/vellumplot/reference/mark_point.md),
[`mark_segment()`](https://r-vellum.github.io/vellumplot/reference/mark_segment.md),
[`mark_edges()`](https://r-vellum.github.io/vellumplot/reference/mark_graph.md),
and
[`mark_nodes()`](https://r-vellum.github.io/vellumplot/reference/mark_graph.md);
other marks reject it with an error.

## See also

[`outline()`](https://r-vellum.github.io/vellumplot/reference/outline.md),
[`shadow()`](https://r-vellum.github.io/vellumplot/reference/shadow.md),
[`theme_cyberpunk()`](https://r-vellum.github.io/vellumplot/reference/theme_gray.md)

## Examples

``` r
df <- data.frame(x = 1:20, y = cumsum(rnorm(20)))
vplot(df) |>
  mark_line(x = x, y = y, color = "#00e5ff", effects = list(glow())) |>
  theme_cyberpunk()
```
