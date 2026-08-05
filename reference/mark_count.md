# Count overlapping points

`mark_count()` collapses coincident `(x, y)` observations to one point
sized by how many fall there (ggplot2's `geom_count()` / `stat_sum()`) —
the honest way to show overplotting on a discrete or rounded grid.
`size` defaults to `after_stat(n)` (the overlap count); map it to
`after_stat(prop)` for the proportion instead.

## Usage

``` r
mark_count(plot, ..., blend = NULL, sketch = NULL, data = NULL)
```

## Arguments

- plot:

  A
  [PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md)
  (from
  [`vplot()`](https://r-vellum.github.io/vellumplot/reference/vplot.md)).

- ...:

  Encodings (tidy-eval): `x`, `y` (+ `color`/`fill` to count within
  groups, and `size` to override the default `after_stat(n)`).

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

## Examples

``` r
vplot(mpg <- data.frame(cyl = mtcars$cyl, gear = mtcars$gear)) |>
  mark_count(x = cyl, y = gear)
```
