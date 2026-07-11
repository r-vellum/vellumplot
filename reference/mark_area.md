# Area, ribbon, and step marks

`mark_area()` fills the region between a `y` line and the zero baseline;
`mark_ribbon()` fills between `ymin` and `ymax`; `mark_step()` draws a
staircase line. All connect points in `x` order.

## Usage

``` r
mark_area(
  plot,
  ...,
  position = "stack",
  blend = NULL,
  sketch = NULL,
  data = NULL
)

mark_ribbon(plot, ..., blend = NULL, sketch = NULL, data = NULL)

mark_step(
  plot,
  ...,
  direction = "hv",
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

  Encodings (tidy-eval): `x` and `y` for area/step; `x`, `ymin`, `ymax`
  for ribbon; plus `color`/`fill`/`alpha`.

- position:

  For `mark_area()`, how areas sharing an `x` combine when
  `fill`/`color` is mapped: `"stack"` (default) stacks them into a band,
  `"fill"` normalises each `x` to 1, `"identity"` overlays them from the
  zero baseline. With no fill mapping all three are equivalent (a single
  area).

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

- direction:

  For `mark_step()`, `"hv"` (horizontal then vertical, default) or
  `"vh"`.

- effects:

  A list of layer render effects applied to the mark at draw time —
  [`glow()`](https://r-vellum.github.io/vellumplot/reference/glow.md),
  [`outline()`](https://r-vellum.github.io/vellumplot/reference/outline.md),
  and
  [`shadow()`](https://r-vellum.github.io/vellumplot/reference/shadow.md).
  Available on stroked and point marks.

## Value

The modified
[PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

## Examples

``` r
vplot(pressure) |> mark_area(x = temperature, y = pressure)
```
