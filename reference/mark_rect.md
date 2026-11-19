# Rectangle mark

`mark_rect()` draws one rectangle per row, spanning `xmin`..`xmax` by
`ymin`..`ymax` (ggplot2's `geom_rect()`). Unlike
[`mark_tile()`](https://r-vellum.github.io/vellumplot/reference/mark_tile.md)
(a centre and a size), it takes the corner bounds, so an infinite bound
reaches the panel edge: `xmin = -Inf, xmax = Inf` shades a full-width
band. `fill` fills (default `"grey35"`) and `color` outlines; a `color`
given without any `fill` fills the rectangle instead, as for
[`mark_tile()`](https://r-vellum.github.io/vellumplot/reference/mark_tile.md).
[`annotate()`](https://r-vellum.github.io/vellumplot/reference/annotate.md)`("rect", ...)`
draws a one-off rectangle.

## Usage

``` r
mark_rect(
  plot,
  ...,
  position = "identity",
  blend = NULL,
  sketch = NULL,
  data = NULL
)
```

## Arguments

- plot:

  A
  [PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md)
  (from
  [`vplot()`](https://r-vellum.github.io/vellumplot/reference/vplot.md)).

- ...:

  Encodings (tidy-eval): `xmin`, `xmax`, `ymin`, `ymax` (+ `fill`,
  `color`, `alpha`, `linewidth`, `pattern`).

- position:

  Position adjustment: a string — `"identity"` (default), `"nudge"`,
  `"stack"`, `"fill"`, `"dodge"`, `"dodge2"`, `"jitter"`,
  `"jitterdodge"`, `"sina"` — or a parameterised
  [position](https://r-vellum.github.io/vellumplot/reference/position.md)
  object such as
  [`position_dodge()`](https://r-vellum.github.io/vellumplot/reference/position.md)
  or
  [`position_stack()`](https://r-vellum.github.io/vellumplot/reference/position.md).
  Every mark takes `"identity"` and `"nudge"`. `"stack"`/`"fill"` work
  on bars, areas, points, text and labels (so a label can sit on its
  stacked bar); `"dodge"`/`"dodge2"` on bars, points, text, labels,
  error bars, line/point ranges, cross-bars, box plots, violins and
  slab-intervals (so error bars dodge over dodged bars); `"jitter"` on
  points, text and labels; `"jitterdodge"` and `"sina"` on points. A
  position a mark does not support is an error naming the ones it does.

- blend:

  Optional blend mode for compositing this layer against what is already
  drawn beneath it (the panel and earlier layers), one of the CSS
  `mix-blend-mode` names, e.g. `"multiply"`, `"screen"`, `"darken"`. The
  whole layer composites as one isolated group (not per element).

- sketch:

  A
  [`sketch()`](https://r-vellum.github.io/vellumplot/reference/sketch.md)
  spec giving this layer a hand-drawn look (wobbly outlines, hachure
  fills), `NA`/`FALSE` to force it crisp (overriding a plot-wide
  [`theme_sketch()`](https://r-vellum.github.io/vellumplot/reference/theme_sketch.md)),
  or `NULL` (default) to inherit. Geometry marks accept it; text,
  raster, hex and datashade marks do not.

- data:

  Optional layer data frame; overrides the plot data for this layer.

## Value

The modified
[PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

## Examples

``` r
recs <- data.frame(
  start = c(1, 4, 7), end = c(3, 5, 10),
  party = c("A", "B", "A")
)
vplot(recs) |>
  mark_rect(xmin = start, xmax = end, ymin = 0, ymax = 1, fill = party)
```
