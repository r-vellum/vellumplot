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
  .width = c(0.66, 0.95),
  point = "median",
  adjust = 1,
  scale = 0.9,
  blend = NULL,
  sketch = NULL,
  data = NULL
)

mark_interval(
  plot,
  ...,
  .width = c(0.66, 0.95),
  point = "median",
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
  `color`/`fill`).

- .width:

  Interval probabilities, widest last (default `c(0.66, 0.95)`).

- point:

  Central summary: `"median"` (default) or `"mean"`.

- adjust:

  `mark_halfeye()` density bandwidth multiplier.

- scale:

  Slab width as a fraction of the category band (`mark_halfeye()`).

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

- `mark_halfeye()` draws, per `x` category, a one-sided density "slab"
  (a half violin) with a **point-interval** at its base: the median (or
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
```
