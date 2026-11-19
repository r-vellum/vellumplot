# 2-D density estimate

`mark_density_2d()` draws the contour lines of a 2-D kernel density
estimate of (`x`, `y`) and `mark_density_2d_filled()` fills the bands
between them, with the semantics of ggplot2's `geom_density_2d()` and
`geom_density_2d_filled()`: the density is
[`MASS::kde2d()`](https://rdrr.io/pkg/MASS/man/kde2d.html) on an `n` x
`n` grid over the data range with bandwidths `h` (default
[`MASS::bandwidth.nrd()`](https://rdrr.io/pkg/MASS/man/bandwidth.nrd.html)
per axis) times `adjust`, and the levels are ~`bins` pretty breaks of
the density over every group. Unlike
[`mark_contour()`](https://r-vellum.github.io/vellumplot/reference/mark_contour.md),
the lines are not coloured by level unless you map
`color = after_stat(level)`; the filled bands map
`fill = after_stat(level)`. A discrete `color`/`fill` (or `group`)
estimates one density per group. Needs the MASS package.

## Usage

``` r
mark_density_2d(
  plot,
  ...,
  position = "identity",
  contour_var = "density",
  bins = NULL,
  binwidth = NULL,
  breaks = NULL,
  n = 100,
  h = NULL,
  adjust = c(1, 1),
  blend = NULL,
  sketch = NULL,
  data = NULL
)

mark_density_2d_filled(
  plot,
  ...,
  position = "identity",
  contour_var = "density",
  bins = NULL,
  binwidth = NULL,
  breaks = NULL,
  n = 100,
  h = NULL,
  adjust = c(1, 1),
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

  Encodings (tidy-eval): `x`, `y` (+ `color`/`fill`, `group`,
  `linewidth`, `linetype`, `alpha`).

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

- contour_var:

  The variable to contour: `"density"` (default), `"ndensity"` (density
  scaled to a maximum of 1 per group) or `"count"` (density times the
  number of observations in the group).

- bins:

  Target number of contour levels (when neither `breaks` nor `binwidth`
  is given).

- binwidth:

  Spacing between contour levels, or `NULL`.

- breaks:

  Explicit contour levels, or `NULL` to derive from `bins` / `binwidth`.

- n:

  Number of grid points per axis.

- h:

  Bandwidth (the standard deviation of the kernel) per axis: a number or
  a pair; `NULL` (default) uses
  [`MASS::bandwidth.nrd()`](https://rdrr.io/pkg/MASS/man/bandwidth.nrd.html).

- adjust:

  Multiplier(s) for the bandwidth, a number or a pair (default
  `c(1, 1)`).

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

## See also

[`mark_contour()`](https://r-vellum.github.io/vellumplot/reference/mark_contour.md)
to contour a supplied `z` surface.

## Examples

``` r
vplot(faithful) |>
  mark_point(x = eruptions, y = waiting) |>
  mark_density_2d(x = eruptions, y = waiting)

vplot(faithful) |> mark_density_2d_filled(x = eruptions, y = waiting)
```
