# Heatmap marks

`mark_tile()` draws a rectangular tile at each `(x, y)` coloured by
`fill`; `mark_raster()` draws the same as one raster image (a fast path
requiring a complete regular grid). `mark_bin2d()` bins continuous
`x`/`y` into a grid and colours each cell by count. `mark_density()`
draws a 1-D kernel density of `x` as a filled curve.

## Usage

``` r
mark_tile(plot, ..., blend = NULL, sketch = NULL, data = NULL)

mark_raster(plot, ..., blend = NULL, data = NULL)

mark_bin2d(plot, ..., bins = 30, blend = NULL, data = NULL)

mark_density(plot, ..., adjust = 1, blend = NULL, sketch = NULL, data = NULL)

mark_hex(plot, ..., bins = 30, blend = NULL, data = NULL)
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

## Value

The modified
[PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

## Examples

``` r
d <- expand.grid(x = 1:5, y = 1:5)
d$z <- d$x * d$y
vplot(d) |> mark_tile(x = x, y = y, fill = z)
```
