# Clip or mask a plot to a geometry

Restrict where a plot's marks show, by a shape rather than the panel
rectangle. `clip_to()` is a **hard** clip: only marks inside `region`
are drawn (or, with `invert = TRUE`, only those outside) – the way to
fill a raster / hexbin / tile heatmap into a country outline (a textured
choropleth). `set_mask()` is a **soft** mask: a radial luminance ramp
that fades the panel out towards its edges (a vignette / spotlight).

## Usage

``` r
clip_to(plot, region, invert = FALSE)

set_mask(plot, region = NULL, type = c("luminance", "alpha"), feather = 0.35)
```

## Arguments

- plot:

  A
  [PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

- region:

  The clip geometry: an `sf` object (its polygons), or a `data.frame` /
  matrix with `x` / `y` columns (and an optional `group` column for
  several rings). For `set_mask()`, `NULL` (default) fades the whole
  panel.

- invert:

  For `clip_to()`, `TRUE` keeps the marks *outside* `region` (punching
  the shape out as a hole) instead of inside.

- type:

  For `set_mask()`, how the mask's pixels set coverage: `"luminance"`
  (default – brightness, white shows / black hides) or `"alpha"`.

- feather:

  For `set_mask()`, the soft-edge fraction of the radial ramp in
  `[0, 0.9]`: `0` a hard disc, larger a gentler fade. Default `0.35`.

## Value

The modified
[PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

## Details

Both attach to the plot and resolve at render into an isolated masked
layer
([`vellum::as_mask()`](https://r-vellum.github.io/vellum/reference/as_mask.html)),
so the static output is unchanged when neither is set. Cartesian
coordinates only (not polar or a nonlinear
[`coord_trans()`](https://r-vellum.github.io/vellumplot/reference/coord_trans.md)).

A smooth *feathered* edge on an arbitrary polygon needs a blur the
renderer does not provide yet, so `clip_to()` is hard-edged; use
`set_mask()` for a soft (radial) fade.

## Examples

``` r
if (FALSE) { # \dontrun{
# clip a raster heatmap to a country outline
vplot(grid) |> mark_raster(x = x, y = y, fill = z) |> clip_to(country_sf)

# a vignette
vplot(mtcars) |> mark_point(x = wt, y = mpg) |> set_mask(feather = 0.5)
} # }
```
