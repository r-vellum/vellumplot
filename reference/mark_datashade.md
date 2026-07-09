# Datashade a large point cloud

For data too dense to draw one marker each (overplotted, up to millions
of points), `mark_datashade()` bins the points into a canvas-sized grid
in one pass and colours each cell by density (via
[`vellum::datashade()`](https://r-vellum.github.io/vellum/reference/datashade.html)),
drawing a single raster that fills the panel. Cost is decoupled from
point count and overplotting. Per-point colour/size aesthetics do not
apply; cell colour encodes density.

## Usage

``` r
mark_datashade(
  plot,
  ...,
  width = 400,
  height = 300,
  colors = NULL,
  how = "eq_hist",
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

  Encodings; `x` and `y` are required.

- width, height:

  Aggregation grid size in cells (output raster pixels).

- colors:

  Two or more colours forming the low-to-high density ramp. For an
  additive per-category overlay, ramp from a transparent/black low end
  to the category hue and composite with `blend = "screen"` (see
  details).

- how:

  Density-to-colour mapping: `"eq_hist"` (default), `"log"`, `"cbrt"`,
  or `"linear"`.

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

Categorical shading (à la datashader's `count_cat`) has no single-call
form, but is reproduced by stacking one datashade layer per category —
each with a `colors` ramp from black to its hue — composited with
`blend = "screen"`, so overlapping densities mix additively.

## Examples

``` r
n <- 1e5
d <- data.frame(x = rnorm(n), y = rnorm(n))
vplot(d) |> mark_datashade(x = x, y = y)
```
