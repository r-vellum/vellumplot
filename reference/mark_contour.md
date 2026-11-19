# 2-D density contours

`mark_contour()` draws iso-density contour **lines** of a 2-D point
cloud (`x`, `y`); `mark_contour_filled()` fills the bands between them.
By default the field is a kernel density estimate (needs the MASS
package); map a `z` aesthetic to instead contour a supplied surface over
a regular `x`/`y` grid. Contours are coloured by level automatically —
`mark_contour()` maps `color = after_stat(level)`,
`mark_contour_filled()` maps `fill`. Contour tracing is done by the
engine
([`vellum::vl_contour()`](https://r-vellum.github.io/vellum/reference/vl_contour.html)),
so no extra package is needed for the lines; a density field
additionally needs MASS.

## Usage

``` r
mark_contour(
  plot,
  ...,
  position = "identity",
  bins = 10,
  binwidth = NULL,
  breaks = NULL,
  n = 100,
  blend = NULL,
  sketch = NULL,
  data = NULL
)

mark_contour_filled(
  plot,
  ...,
  position = "identity",
  bins = 10,
  binwidth = NULL,
  breaks = NULL,
  n = 100,
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

  Encodings (tidy-eval): `x`, `y` (+ optional `z` surface, `color` /
  `fill`, `linewidth`, `linetype`).

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

- bins:

  Target number of contour levels (when neither `breaks` nor `binwidth`
  is given).

- binwidth:

  Spacing between contour levels, or `NULL`.

- breaks:

  Explicit contour levels, or `NULL` to derive from `bins` / `binwidth`.

- n:

  Density-estimate grid resolution per axis (KDE mode only).

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

## Details

As in ggplot2, each filled band is coloured by its lower level, and the
lowest band – from the highest break at or below the field's minimum
(e.g. `0` for a density) up to the first inner level – covers the whole
grid, so the field's low-lying parts are painted rather than left as
background.

## Examples

``` r
vplot(faithful) |>
  mark_point(x = eruptions, y = waiting) |>
  mark_contour(x = eruptions, y = waiting)

vplot(faithful) |> mark_contour_filled(x = eruptions, y = waiting)
```
