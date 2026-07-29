# Map decorations

Cartographic furniture for
[`coord_sf()`](https://r-vellum.github.io/vellumplot/reference/coord_sf.md)
maps. `mark_scalebar()` draws a segmented distance bar; `mark_compass()`
draws a north arrow. Both are fixed-position decorations pinned to a
panel corner (they take no data aesthetics). Graticule lines (meridians
and parallels) are added through
[`coord_sf()`](https://r-vellum.github.io/vellumplot/reference/coord_sf.md)'s
`graticule` argument, not a mark, so they render behind the map.

## Usage

``` r
mark_scalebar(
  plot,
  distance = NULL,
  unit = "km",
  position = "bottomleft",
  segments = 4L,
  height = 2.5,
  pad = 4,
  text_size = 7,
  color = "black",
  fill = "white",
  data = NULL
)

mark_compass(
  plot,
  position = "topright",
  size = 10,
  pad = 4,
  rotation = 0,
  color = "black",
  fill = "black",
  text = TRUE,
  data = NULL
)
```

## Arguments

- plot:

  A
  [PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

- distance:

  Bar length in `unit`. `NULL` (default) picks a round number near a
  quarter of the panel width.

- unit:

  Distance unit for the label and `distance`: `"km"` (default), `"m"`,
  `"mi"`, or `"ft"`.

- position:

  Panel corner to anchor the decoration: `"bottomleft"` (scale bar
  default), `"topright"` (compass default), `"bottomright"`, or
  `"topleft"` (the abbreviations `"bl"`/`"br"`/`"tl"`/ `"tr"` also
  work).

- segments:

  Number of alternating segments in the bar (default `4`).

- height:

  Bar thickness, in millimetres.

- pad:

  Inset from the panel edge, in millimetres.

- text_size:

  Label font size (points).

- color, fill:

  Stroke and fill colours. For the scale bar the segments alternate
  `color` and `fill`; for the compass the arrow is `fill` with a `color`
  outline.

- data:

  Optional layer data (rarely needed — decorations are not data-driven).

- size:

  Glyph height, in millimetres.

- rotation:

  Extra clockwise rotation of the arrow, in degrees (e.g. to point at
  grid north on a rotated layout).

- text:

  Draw the `"N"` label above the arrow (default `TRUE`).

## Value

The modified
[PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

## See also

[`coord_sf()`](https://r-vellum.github.io/vellumplot/reference/coord_sf.md),
[`mark_sf()`](https://r-vellum.github.io/vellumplot/reference/mark_sf.md)

## Examples

``` r
if (FALSE) { # \dontrun{
nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
vplot(nc) |>
  mark_sf(fill = BIR74) |>
  coord_sf(crs = 3857, graticule = TRUE) |>
  mark_scalebar() |>
  mark_compass()
} # }
```
