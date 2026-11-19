# Uncertainty marks (slab + interval)

Marks for visualising a distribution's shape *and* its summary intervals
together, in the style of the ggdist package — a natural fit for
posterior draws or bootstrap samples (one row per draw, grouped by a
categorical `x`).

## Usage

``` r
mark_halfeye(
  plot,
  ...,
  position = "identity",
  .width = c(0.66, 0.95),
  point = "median",
  adjust = 1,
  scale = 0.9,
  orientation = NA,
  blend = NULL,
  sketch = NULL,
  data = NULL
)

mark_interval(
  plot,
  ...,
  position = "identity",
  .width = c(0.66, 0.95),
  point = "median",
  orientation = NA,
  blend = NULL,
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

  Encodings (tidy-eval): a categorical `x` and the sample `y` (+
  `color`/`fill`), or a categorical `y` and the sample `x` for
  horizontal intervals.

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

- .width:

  Interval probabilities, widest last (default `c(0.66, 0.95)`).

- point:

  Central summary: `"median"` (default) or `"mean"`.

- adjust:

  `mark_halfeye()` density bandwidth multiplier.

- scale:

  Slab width as a fraction of the category band (`mark_halfeye()`).

- orientation:

  `NA` (default) to infer it from the data (horizontal when `y` is
  discrete and `x` is not, as in ggplot2), `"x"` for vertical intervals
  along a discrete `x` (the slab to the right of each), or `"y"` for
  horizontal intervals along `y` (the slab above each).

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

- `mark_halfeye()` draws, per category, a one-sided density "slab" (a
  half violin) with a **point-interval** at its base: the median (or
  mean) as a point, a thick bar for the inner interval and a thin bar
  for the outer one.

- `mark_interval()` draws the point-interval alone (no slab).

Intervals are equal-tailed quantile intervals at the `.width`
probabilities (widest drawn thinnest). Supply many draws per `x` (e.g.
posterior samples).

## Examples

``` r
set.seed(1)
draws <- data.frame(
  grp = rep(c("a", "b", "c"), each = 400),
  val = rnorm(1200, rep(c(0, 1, 2), each = 400))
)
vplot(draws) |> mark_halfeye(x = grp, y = val)

vplot(draws) |> mark_interval(x = grp, y = val)

# horizontal: the discrete variable on y
vplot(draws) |> mark_halfeye(x = val, y = grp)
```
