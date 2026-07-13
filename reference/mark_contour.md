# 2-D density contours

`mark_contour()` draws iso-density contour **lines** of a 2-D point
cloud (`x`, `y`); `mark_contour_filled()` fills the bands between them.
By default the field is a kernel density estimate (needs the MASS
package); map a `z` aesthetic to instead contour a supplied surface over
a regular `x`/`y` grid. Contours are coloured by level automatically —
`mark_contour()` maps `color = after_stat(level)`,
`mark_contour_filled()` maps `fill`. Requires the isoband package.

## Usage

``` r
mark_contour(
  plot,
  ...,
  bins = 10,
  binwidth = NULL,
  breaks = NULL,
  n = 100,
  blend = NULL,
  data = NULL
)

mark_contour_filled(
  plot,
  ...,
  bins = 10,
  binwidth = NULL,
  breaks = NULL,
  n = 100,
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

  Encodings (tidy-eval): `x`, `y` (+ optional `z` surface, `color` /
  `fill`, `linewidth`, `linetype`).

- bins:

  Target number of contour levels (when neither `breaks` nor `binwidth`
  is given).

- binwidth:

  Spacing between contour levels, or `NULL`.

- breaks:

  Explicit contour levels, or `NULL` to derive from `bins` / `binwidth`.

- n:

  Density-estimate grid resolution per axis (KDE mode only).

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

## Examples

``` r
vplot(faithful) |>
  mark_point(x = eruptions, y = waiting) |>
  mark_contour(x = eruptions, y = waiting)

vplot(faithful) |> mark_contour_filled(x = eruptions, y = waiting)
```
