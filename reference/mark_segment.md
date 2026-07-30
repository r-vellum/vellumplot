# Segment mark

`mark_segment()` draws a straight line from `(x, y)` to `(xend, yend)`
per row.

## Usage

``` r
mark_segment(
  plot,
  ...,
  auto = FALSE,
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
  `linewidth`, `linetype`, `alpha`).

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
  ([`vellum::datashade()`](https://r-vellum.github.io/vellum//reference/datashade.html)),
  dense lines/steps rasterise as connected polylines
  ([`vellum::datashade_lines()`](https://r-vellum.github.io/vellum//reference/datashade_lines.html)),
  and segments/edges as independent segments
  ([`vellum::datashade_segments()`](https://r-vellum.github.io/vellum//reference/datashade_lines.html)).
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

## Value

The modified
[PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

## Examples

``` r
d <- data.frame(x = 1, y = 1, xend = 5, yend = 4)
vplot(d) |> mark_segment(x = x, y = y, xend = xend, yend = yend)
```
