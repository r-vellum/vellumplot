# Quantile regression lines

`mark_quantile()` fits a quantile regression of `y` on `x` per group for
each of `quantiles` and draws the fitted lines (ggplot2's
`geom_quantile()`). The fit is
[`quantreg::rq()`](https://rdrr.io/pkg/quantreg/man/rq.html) of
`formula` (default `y ~ x`), predicted on 100 points across the group's
range of `x`. Map `color = after_stat(factor(quantile))` (or `linetype`)
to tell the lines apart. Needs the quantreg package.

## Usage

``` r
mark_quantile(
  plot,
  ...,
  quantiles = c(0.25, 0.5, 0.75),
  formula = NULL,
  method.args = list(),
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

  Encodings (tidy-eval): `x`, `y` (+ `color`, `group`, `linewidth`,
  `linetype`, `alpha`).

- quantiles:

  The conditional quantiles to fit (default `c(0.25, 0.5, 0.75)`).

- formula:

  The model formula in `x` and `y` (default `y ~ x`), e.g.
  `y ~ splines::ns(x, 3)` for a curve.

- method.args:

  A list of further arguments to
  [`quantreg::rq()`](https://rdrr.io/pkg/quantreg/man/rq.html).

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

## See also

[`mark_smooth()`](https://r-vellum.github.io/vellumplot/reference/mark_histogram.md)
with `method = "rq"` for one quantile with the smoother's options.

## Examples

``` r
# \donttest{
if (requireNamespace("quantreg", quietly = TRUE)) {
  vplot(mtcars) |>
    mark_point(x = disp, y = mpg) |>
    mark_quantile(x = disp, y = mpg)
}

# }
```
