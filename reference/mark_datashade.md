# Datashade a large point cloud

For data too dense to draw one marker each (overplotted, up to millions
of points), `mark_datashade()` bins the points into a canvas-sized grid
in one pass and colours each cell by density (via
[`vellum::datashade()`](https://r-vellum.github.io/vellum/reference/datashade.html)),
drawing a single raster that fills the panel. Cost is decoupled from
point count and overplotting.

## Usage

``` r
mark_datashade(
  plot,
  ...,
  width = 400,
  height = 300,
  colors = NULL,
  how = "eq_hist",
  span = NULL,
  clip = NULL,
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

  Encodings; `x` and `y` are required. Map `color`/`fill` to a discrete
  variable for categorical (`count_cat`) shading.

- width, height:

  Aggregation grid size in cells (output raster pixels).

- colors:

  Two or more colours forming the low-to-high density ramp (plain
  density shading only; ignored when a `color`/`fill` aesthetic is
  mapped).

- how:

  Density-to-colour mapping (and, categorically, per-cell opacity):
  `"eq_hist"` (default), `"log"`, `"cbrt"`, or `"linear"`.

- span, clip:

  Optional density-range clamping passed to
  [`vellum::datashade()`](https://r-vellum.github.io/vellum/reference/datashade.html):
  `span = c(lo, hi)` absolute limits, or `clip = c(0.01, 0.99)`
  percentiles, so a few extreme cells don't flatten the rest. Both
  default `NULL`.

- blend:

  Optional blend mode for compositing this layer against what is already
  drawn beneath it (the panel and earlier layers), one of the CSS
  `mix-blend-mode` names, e.g. `"multiply"`, `"screen"`, `"darken"`. The
  whole layer composites as one isolated group (not per element).

- data:

  Optional layer data frame; overrides the plot data for this layer.

## Value

The modified
[PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

## Details

Map a discrete `color` (or `fill`) aesthetic to shade **categorically**
(datashader's `count_cat`): each category is aggregated separately and
every cell is coloured by the count-weighted average of the category
hues it holds, with opacity from its total density — so you see which
category dominates where, and where they mix, with a colour legend. The
category hues come from the usual discrete colour scale (so
[`scale_color_manual()`](https://r-vellum.github.io/vellumplot/reference/scale_color_continuous.md)
etc. apply). Without a mapped colour, cell colour encodes plain density
via the `colors` ramp.

## Examples

``` r
n <- 1e5
d <- data.frame(x = rnorm(n), y = rnorm(n), g = sample(c("a", "b"), n, TRUE))
vplot(d) |> mark_datashade(x = x, y = y)

# categorical: colour by group
vplot(d) |> mark_datashade(x = x, y = y, color = g)
```
