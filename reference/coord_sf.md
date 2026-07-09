# Map coordinate system

`coord_sf()` is the coordinate system for
[`mark_sf()`](https://r-vellum.github.io/vellumplot/reference/mark_sf.md)
maps. Before scale training it reprojects every `sf` layer to a common
CRS (via
[`sf::st_transform()`](https://r-spatial.github.io/sf/reference/st_transform.html)),
so the renderer only ever sees projected Cartesian coordinates, and it
locks the panel aspect ratio so the map is not stretched: `1` for a
projected CRS, and the equirectangular correction `1/cos(mean_latitude)`
for unprojected longitude/latitude data.

## Usage

``` r
coord_sf(plot, crs = NULL, xlim = NULL, ylim = NULL)
```

## Arguments

- plot:

  A
  [PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

- crs:

  Target coordinate reference system to project all layers into
  (anything
  [`sf::st_crs()`](https://r-spatial.github.io/sf/reference/st_crs.html)
  accepts, e.g. an EPSG code, `"OGC:CRS84"`, or a proj/WKT string).
  `NULL` (default) uses the CRS of the first `sf` layer. For guaranteed
  longitude/latitude order, use `"OGC:CRS84"` rather than `EPSG:4326`.
  For choropleths, prefer an equal-area CRS.

- xlim, ylim:

  Length-2 view-window limits in the target CRS, or `NULL`.

## Value

The modified
[PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

## Details

`sf` is an optional dependency (in `Suggests`); `coord_sf()` errors with
an install hint if it is not available at render time.

## See also

[`mark_sf()`](https://r-vellum.github.io/vellumplot/reference/mark_sf.md)

## Examples

``` r
if (FALSE) { # \dontrun{
nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
vplot(nc) |> mark_sf(fill = BIR74) |> coord_sf(crs = "OGC:CRS84")
} # }
```
