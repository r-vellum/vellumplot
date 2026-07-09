# Draw simple-feature (sf) geometries

`mark_sf()` draws the geometry column of an `sf` object as a map layer:
`POINT`/`MULTIPOINT` render as points, `LINESTRING`/`MULTILINESTRING` as
polylines, and `POLYGON`/`MULTIPOLYGON` as filled paths (holes cut with
the even-odd rule, so ring winding need not be canonical). Coordinates
come from the geometry, so there are no `x`/`y` encodings; other
aesthetics map feature attributes as usual, e.g. `fill = AREA` for a
choropleth. Pair with
[`coord_sf()`](https://r-vellum.github.io/vellumplot/reference/coord_sf.md)
to reproject and lock the map aspect ratio.

## Usage

``` r
mark_sf(
  plot,
  ...,
  fill = NULL,
  color = NULL,
  alpha = NULL,
  linewidth = NULL,
  size = NULL,
  na_value = "grey80",
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

  Encodings mapping feature attributes to aesthetics: `fill`, `color`,
  `alpha`, `linewidth`, `size`. A geometry column is not encoded — it is
  read from the data.

- fill, color, alpha, linewidth, size:

  Convenience aesthetic arguments; a constant or a mapped expression.

- na_value:

  Fill colour for features whose mapped `fill`/`color` value is `NA`
  (drawn as a distinct legend swatch). Default `"grey80"`.

- blend:

  Optional blend mode (see
  [`mark_point()`](https://r-vellum.github.io/vellumplot/reference/mark_point.md)).

- data:

  Optional layer data (an `sf` object); overrides the plot data.

## Value

The modified
[PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

## Details

`sf` is an optional dependency (in `Suggests`); `mark_sf()` errors with
an install hint if it is not available.

## See also

[`coord_sf()`](https://r-vellum.github.io/vellumplot/reference/coord_sf.md),
[`scale_fill_binned()`](https://r-vellum.github.io/vellumplot/reference/scale_fill_binned.md)

## Examples

``` r
if (FALSE) { # \dontrun{
nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
vplot(nc) |> mark_sf(fill = BIR74) |> coord_sf()
} # }
```
