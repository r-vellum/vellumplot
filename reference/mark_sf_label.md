# Label sf features at their interior point

`mark_sf_label()` draws one text label per `sf` feature, placed at the
feature's **interior point**
([`sf::st_point_on_surface()`](https://r-spatial.github.io/sf/reference/geos_unary.html),
guaranteed to fall inside each polygon — unlike a centroid, which can
land in a bay or a hole). The label points are reprojected through the
*same* `coord_sf` CRS as
[`mark_sf()`](https://r-vellum.github.io/vellumplot/reference/mark_sf.md),
so they always land where the geometry is drawn. Layer it over a
[`mark_sf()`](https://r-vellum.github.io/vellumplot/reference/mark_sf.md)
choropleth to name the regions; `repel = TRUE` (the default) pushes
crowded labels apart.

## Usage

``` r
mark_sf_label(
  plot,
  ...,
  size = NULL,
  repel = TRUE,
  box_padding = 1,
  min_segment_length = 2,
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

  Encodings (tidy-eval): the `label` to draw at each feature, plus an
  optional `color` / `size`. The position comes from the geometry, so
  `x` / `y` are not mapped.

- size:

  Font size in points.

- repel:

  Move overlapping labels apart (force-directed, ggrepel-style), with
  leader lines to the points? Single cartesian panel only.

- box_padding:

  Extra space (mm) kept around each label box during repulsion.

- min_segment_length:

  Shortest leader line (mm) worth drawing; a label that barely moved
  gets none.

- blend:

  Optional blend mode for compositing this layer against what is already
  drawn beneath it (the panel and earlier layers), one of the CSS
  `mix-blend-mode` names, e.g. `"multiply"`, `"screen"`, `"darken"`. The
  whole layer composites as one isolated group (not per element).

- data:

  An `sf` data frame (defaults to the plot's). Its geometry supplies the
  label positions.

## Value

The modified
[PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

## See also

[`mark_sf()`](https://r-vellum.github.io/vellumplot/reference/mark_sf.md),
[`mark_text()`](https://r-vellum.github.io/vellumplot/reference/mark_text.md)

## Examples

``` r
if (requireNamespace("sf", quietly = TRUE)) {
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  vplot(nc) |>
    mark_sf(fill = AREA) |>
    mark_sf_label(label = NAME, size = 5)
}
```
