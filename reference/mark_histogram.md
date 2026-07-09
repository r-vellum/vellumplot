# Statistical marks

Marks that apply a statistical transform before drawing.
`mark_histogram()` bins a continuous `x` and draws the per-bin counts as
bars. `mark_smooth()` fits a model (`"lm"` for now) of `y` on `x` and
draws the fitted line, with a confidence ribbon when `se = TRUE`.

## Usage

``` r
mark_histogram(
  plot,
  ...,
  bins = 30,
  position = "stack",
  blend = NULL,
  sketch = NULL,
  data = NULL
)

mark_smooth(
  plot,
  ...,
  method = "lm",
  se = TRUE,
  level = 0.95,
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

  Encodings (tidy-eval), e.g. `x`, `y`, `color`/`fill`.

- bins:

  Number of histogram bins.

- position:

  Position adjustment for the histogram bars (`"stack"`, `"dodge"`,
  `"fill"`).

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

- method:

  Smoothing method; `"lm"` (linear) for now.

- se:

  Draw a confidence ribbon around the smooth?

- level:

  Confidence level for the ribbon.

## Value

The modified
[PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

## Examples

``` r
vplot(mtcars) |> mark_histogram(x = mpg, bins = 10)

vplot(mtcars) |> mark_point(x = wt, y = mpg) |> mark_smooth(x = wt, y = mpg)
```
