# Spatial maps: sf geometries, choropleths, projection, aspect lock.
# Unlike the other scripts, this one needs the optional `sf` package (and
# `classInt` for classed breaks) -- both are in Suggests. It skips cleanly if
# they are absent.

library(vellumplot)
outdir <- "figures"
dir.create(outdir, showWarnings = FALSE)

if (!requireNamespace("sf", quietly = TRUE)) {
  message("18-spatial-maps: skipped (install the 'sf' package to run it)")
} else {
  # North Carolina counties -- the canonical sf demo dataset (lon/lat, NAD27).
  nc <- sf::st_read(
    system.file("shape/nc.shp", package = "sf"),
    quiet = TRUE
  )

  # --- 1. Plain polygon map ---------------------------------------------------
  # mark_sf() with no fill draws the default grey polygons; coord_sf() is added
  # automatically, locking the aspect ratio (the map is not stretched).
  vplot(nc) |>
    mark_sf() |>
    labs(title = "North Carolina counties") |>
    render_plot(file.path(outdir, "18-polygons.png"))

  # --- 2. Continuous choropleth ----------------------------------------------
  # Map a feature attribute to fill: an unclassed Batlow ramp (the default).
  vplot(nc) |>
    mark_sf(fill = BIR74) |>
    labs(title = "Births, 1974 (continuous fill)", fill = "BIR74") |>
    render_plot(file.path(outdir, "18-choropleth-continuous.png"))

  # --- 3. Classed choropleth + NA swatch -------------------------------------
  # Classed (binned) fills read back to a value range far more reliably. Blank
  # one county to show the distinct NA legend swatch.
  nc_na <- nc
  nc_na$SID74[1] <- NA
  vplot(nc_na) |>
    mark_sf(fill = SID74) |>
    scale_fill_binned(style = "quantile", n = 5, name = "SID74") |>
    labs(title = "SIDS deaths, 1974 (quantile classes)") |>
    render_plot(file.path(outdir, "18-choropleth-classed.png"))

  # --- 4. Reprojection --------------------------------------------------------
  # coord_sf(crs=) reprojects before drawing. An equal-area projection (NC state
  # plane, EPSG:32119) is the correct choice for a choropleth.
  vplot(nc) |>
    mark_sf(fill = BIR74) |>
    coord_sf(crs = 32119) |>
    labs(title = "Equal-area projection (EPSG:32119)", fill = "BIR74") |>
    render_plot(file.path(outdir, "18-projected.png"))

  # --- 5. Cartographic furniture ---------------------------------------------
  # coord_sf(graticule = TRUE) draws meridians/parallels (reprojected, so they
  # curve under a conic projection); mark_scalebar() and mark_compass() add a
  # distance bar and a north arrow, each pinned to a panel corner.
  vplot(nc) |>
    mark_sf(fill = BIR74, color = "white", linewidth = 0.2) |>
    coord_sf(crs = 5070, graticule = TRUE) |>
    mark_scalebar(unit = "km", position = "bottomleft") |>
    mark_compass(position = "topright") |>
    labs(title = "Decorated map (graticule, scale bar, compass)", fill = "BIR74") |>
    render_plot(file.path(outdir, "18-decorations.png"))

  message("18-spatial-maps: wrote 5 figures to ", outdir)
}
