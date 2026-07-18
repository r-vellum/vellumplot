# Statistical marks

Marks that apply a statistical transform before drawing.
`mark_histogram()` bins a continuous `x` and draws the per-bin counts as
bars. `mark_smooth()` fits a model of `y` on `x` (per group) and draws
the fitted line, with a confidence ribbon when `se = TRUE`.

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
  method = "auto",
  formula = NULL,
  span = 0.75,
  se = TRUE,
  level = 0.95,
  method.args = list(),
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

  Smoothing method: one of `"auto"`, `"lm"`, `"loess"`, `"glm"`,
  `"gam"`, `"rq"`.

- formula:

  Model formula in terms of `x` and `y` (e.g. `y ~ poly(x, 2)`,
  `y ~ s(x)` for `gam`). Defaults to `y ~ x` (`y ~ s(x)` for `gam`).

- span:

  `loess` neighbourhood size (larger = smoother).

- se:

  Draw a confidence ribbon around the smooth? Ignored for `"rq"`.

- level:

  Confidence level for the ribbon.

- method.args:

  Extra arguments to the fitting function, e.g.
  `list(family = binomial())` for `glm`, or `list(tau = 0.9)` for `rq`.

## Value

The modified
[PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

## Details

`mark_smooth()` supports several `method`s:

- `"auto"` (default) picks `"loess"` for small groups (\< 1000 points)
  and `"gam"` for large ones.

- `"lm"` / `"glm"` — linear and generalised linear fits (`glm` takes a
  `family` via `method.args`, e.g.
  [`binomial()`](https://rdrr.io/r/stats/family.html) for logistic).

- `"loess"` — local regression, controlled by `span`.

- `"gam"` — a generalised additive model with a smooth term (default
  `y ~ s(x)`); needs the mgcv package.

- `"rq"` — quantile regression at a single `method.args$tau` (default
  the median); needs the quantreg package and draws the fitted line only
  (no confidence ribbon). For several quantiles, add one layer per
  `tau`.

## Examples

``` r
vplot(mtcars) |> mark_histogram(x = mpg, bins = 10)

vplot(mtcars) |> mark_point(x = wt, y = mpg) |> mark_smooth(x = wt, y = mpg)

# local regression with a wider neighbourhood
vplot(mtcars) |>
  mark_point(x = wt, y = mpg) |>
  mark_smooth(x = wt, y = mpg, method = "loess", span = 0.9)
```
