# Segment, curve and spoke marks

`mark_segment()` draws a straight line from `(x, y)` to `(xend, yend)`
per row. `mark_curve()` bends that line into an arc (ggplot2's
`geom_curve()`). `mark_spoke()` draws a segment from `(x, y)` in the
direction `angle` (radians) with length `radius`, both in data units
(ggplot2's `geom_spoke()`): a vector field.

## Usage

``` r
mark_segment(
  plot,
  ...,
  position = "identity",
  auto = FALSE,
  blend = NULL,
  effects = list(),
  sketch = NULL,
  data = NULL
)

mark_curve(
  plot,
  ...,
  curvature = 0.5,
  angle = 90,
  ncp = 5,
  position = "identity",
  blend = NULL,
  effects = list(),
  sketch = NULL,
  data = NULL
)

mark_spoke(
  plot,
  ...,
  position = "identity",
  blend = NULL,
  effects = list(),
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

  Encodings (tidy-eval): `x`, `y`, `xend`, `yend` (+ `color`,
  `linewidth`, `linetype`, `alpha`); `mark_spoke()` takes `angle` and
  `radius` in place of `xend`, `yend`. A mapped `linewidth` gives each
  segment its own width – see
  [`scale_linewidth()`](https://r-vellum.github.io/vellumplot/reference/scale_linewidth.md).

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

- auto:

  For
  [`mark_point()`](https://r-vellum.github.io/vellumplot/reference/mark_point.md),
  [`mark_line()`](https://r-vellum.github.io/vellumplot/reference/mark_point.md),
  [`mark_step()`](https://r-vellum.github.io/vellumplot/reference/mark_area.md),
  `mark_segment()`, and
  [`mark_edges()`](https://r-vellum.github.io/vellumplot/reference/mark_graph.md),
  when `TRUE` and the layer has very many rows, automatically render it
  as a datashaded density raster (see
  [`mark_datashade()`](https://r-vellum.github.io/vellumplot/reference/mark_datashade.md))
  instead of individual vector marks: points bin into a density grid
  ([`vellum::datashade()`](https://r-vellum.github.io/vellum/reference/datashade.html)),
  dense lines/steps rasterise as connected polylines
  ([`vellum::datashade_lines()`](https://r-vellum.github.io/vellum/reference/datashade_lines.html)),
  and segments/edges as independent segments
  ([`vellum::datashade_segments()`](https://r-vellum.github.io/vellum/reference/datashade_lines.html)).
  The datashaded line/segment output is `dynspread`-ed so thin marks
  stay visible (see the `spread` argument of
  [`mark_datashade()`](https://r-vellum.github.io/vellumplot/reference/mark_datashade.md)).
  The fallback is skipped under a warped coordinate system
  ([`coord_polar()`](https://r-vellum.github.io/vellumplot/reference/coord_polar.md)
  /
  [`coord_trans()`](https://r-vellum.github.io/vellumplot/reference/coord_trans.md)),
  which draws the vector marks instead.

- blend:

  Optional blend mode for compositing this layer against what is already
  drawn beneath it (the panel and earlier layers), one of the CSS
  `mix-blend-mode` names, e.g. `"multiply"`, `"screen"`, `"darken"`. The
  whole layer composites as one isolated group (not per element).

- effects:

  A list of layer render effects applied to the mark at draw time —
  [`glow()`](https://r-vellum.github.io/vellumplot/reference/glow.md),
  [`outline()`](https://r-vellum.github.io/vellumplot/reference/outline.md),
  and
  [`shadow()`](https://r-vellum.github.io/vellumplot/reference/shadow.md).
  Available on stroked and point marks.

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

- curvature:

  For `mark_curve()`, the amount of bend: `0` is straight, `1` a half
  circle; negative values bend the other way (ggplot2's convention:
  positive bends to the right, travelling from start to end).

- angle:

  For `mark_curve()`, where the apex of the bend sits, in degrees
  between 0 and 180: `90` (default) is symmetric, smaller values move it
  towards the start and larger towards the end.

- ncp:

  For `mark_curve()`, the number of control points (more give a smoother
  curve; default `5`).

## Value

The modified
[PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

## Details

A curve bends in the panel's own frame (its x and y spans scaled to the
same length), so on a panel that is not square the arc stretches with
the panel; ggplot2 bends in device space instead.

## See also

[`scale_linewidth()`](https://r-vellum.github.io/vellumplot/reference/scale_linewidth.md)

## Examples

``` r
d <- data.frame(x = 1, y = 1, xend = 5, yend = 4)
vplot(d) |> mark_segment(x = x, y = y, xend = xend, yend = yend)

vplot(d) |> mark_curve(x = x, y = y, xend = xend, yend = yend)


# a vector field
g <- expand.grid(x = 1:8, y = 1:8)
g$angle <- atan2(g$y - 4.5, g$x - 4.5)
vplot(g) |> mark_spoke(x = x, y = y, angle = angle, radius = 0.4)
```
