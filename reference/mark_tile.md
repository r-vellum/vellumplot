# Heatmap marks

`mark_tile()` draws a rectangular tile at each `(x, y)` coloured by
`fill`; `mark_raster()` draws the same as one raster image (a fast path
requiring a complete regular grid). `mark_bin2d()` bins continuous
`x`/`y` into a grid and colours each cell by count. `mark_density()`
draws a 1-D kernel density of `x` as a filled curve. On all of them a
`color` given alongside a `fill` (or the default count fill of
`mark_bin2d()` / `mark_hex()`) outlines the shapes; a `color` alone
fills them. Unmapped, tiles, rasters and densities fill with
`col_mix(ink, paper, 0.2)` from the theme's
[`element_geom()`](https://r-vellum.github.io/vellumplot/reference/element_geom.md)
(`"grey20"` on a light theme), the fill of ggplot2's `geom_tile()` and
`geom_area()`. (ggplot2's `geom_density()` draws an unfilled outline;
`mark_density()` keeps its filled curve, filled like an area.)

## Usage

``` r
mark_tile(
  plot,
  ...,
  position = "identity",
  blend = NULL,
  sketch = NULL,
  data = NULL
)

mark_raster(plot, ..., position = "identity", blend = NULL, data = NULL)

mark_bin2d(
  plot,
  ...,
  position = "identity",
  bins = 30,
  blend = NULL,
  data = NULL
)

mark_density(
  plot,
  ...,
  position = "identity",
  adjust = 1,
  trim = FALSE,
  blend = NULL,
  sketch = NULL,
  data = NULL
)

mark_hex(
  plot,
  ...,
  position = "identity",
  bins = 30,
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

  Encodings (tidy-eval): `x`, `y`, `fill` for tile/raster; `x`, `y` for
  bin2d; `x` (+ `fill`/`color`) for density.

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

- bins:

  Number of bins per axis for `mark_bin2d()` / hex columns for
  `mark_hex()`.

- adjust:

  Bandwidth multiplier for `mark_density()`.

- trim:

  For `mark_density()`, `FALSE` (default) estimates every group's
  density over the range of the whole layer's `x`, as ggplot2 does;
  `TRUE` trims each group's curve to the range of its own data. The
  curve never runs past the data, so the axis is not padded by the
  kernel's tails.

## Value

The modified
[PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

## Examples

``` r
d <- expand.grid(x = 1:5, y = 1:5)
d$z <- d$x * d$y
vplot(d) |> mark_tile(x = x, y = y, fill = z)
```
