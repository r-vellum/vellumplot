# Boxplot, error bar, and summary marks

`mark_boxplot()` draws a box-and-whisker per `x` category from the raw
`y` values (box = Q1-Q3, median line, 1.5\*IQR whiskers, outlier
points). `mark_errorbar()` draws vertical bars from `ymin` to `ymax`
with horizontal caps; `mark_linerange()` omits the caps.
`mark_summary()` aggregates `y` per `x` with `fun` (default mean) and
draws the result as points.

## Usage

``` r
mark_boxplot(
  plot,
  ...,
  position = "identity",
  blend = NULL,
  sketch = NULL,
  data = NULL
)

mark_errorbar(
  plot,
  ...,
  position = "identity",
  width = 0.5,
  blend = NULL,
  sketch = NULL,
  data = NULL
)

mark_linerange(
  plot,
  ...,
  position = "identity",
  blend = NULL,
  sketch = NULL,
  data = NULL
)

mark_summary(
  plot,
  ...,
  position = "identity",
  fun = mean,
  blend = NULL,
  sketch = NULL,
  data = NULL
)

mark_pointrange(
  plot,
  ...,
  position = "identity",
  blend = NULL,
  sketch = NULL,
  data = NULL
)

mark_crossbar(
  plot,
  ...,
  position = "identity",
  width = 0.5,
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

  Encodings (tidy-eval): `x`, `y` for boxplot/summary; `x`, `ymin`,
  `ymax` for errorbar/linerange; plus `color`/`fill` (and `linetype` for
  errorbar/linerange).

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

- width:

  For `mark_errorbar()`, the cap width as a fraction of the band.

- fun:

  For `mark_summary()`, the aggregation function (default `mean`).

## Value

The modified
[PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

## Details

`mark_pointrange()` draws a point at `(x, y)` with a vertical line from
`ymin` to `ymax` (an identity mark — supply the summary values yourself,
e.g. from a model). `mark_crossbar()` draws a box spanning
`ymin`..`ymax` with a horizontal line at `y`. Both are the identity
counterparts of the aggregating
[`mark_interval()`](https://r-vellum.github.io/vellumplot/reference/mark_halfeye.md).

## Examples

``` r
vplot(mtcars) |> mark_boxplot(x = factor(cyl), y = mpg)
```
