# Neon glow layer effect

A render effect for stroked, point, and text marks, in the spirit of
[mplcyberpunk](https://github.com/dhaitz/mplcyberpunk): a widened copy
of the mark is drawn beneath the crisp original and softened with a
**real Gaussian blur** (`vellum`'s `vl_viewport(blur=)`), composited
additively (a `"screen"` blend) into a soft neon halo. Pass it to a
mark's `effects` argument, e.g.
`mark_line(..., effects = list(glow()))`. Pairs naturally with
[`theme_cyberpunk()`](https://r-vellum.github.io/vellumplot/reference/theme_gray.md).

## Usage

``` r
glow(size = 6, layers = 6L, alpha = 0.12, blend = "screen", color = NULL)
```

## Arguments

- size:

  Halo spread in millimetres: the blur radius (and, for stroked/point
  marks, how much the copy is widened before blurring).

- layers, blend:

  Retained for compatibility and the neon look: `layers` scales the halo
  opacity (it no longer stacks copies – one blurred layer replaces
  them), and `blend` (typically `"screen"`/`"lighten"`) composites the
  halo.

- alpha:

  Base halo opacity (scaled by `layers`).

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
[`mark_nodes()`](https://r-vellum.github.io/vellumplot/reference/mark_graph.md),
and text marks
([`mark_text()`](https://r-vellum.github.io/vellumplot/reference/mark_text.md)
/
[`mark_label()`](https://r-vellum.github.io/vellumplot/reference/mark_text.md));
other marks reject it.

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
