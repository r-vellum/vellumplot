# Merged choropleth regions: mark_sf(merge = TRUE) dissolves adjacent features
# that share a fill into one crisp region (no internal seams, exact in PDF).

library(vellumplot)
if (!requireNamespace("sf", quietly = TRUE)) {
  stop("this example needs the 'sf' package")
}
outdir <- "figures"
dir.create(outdir, showWarnings = FALSE)

nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
# group the counties into four east-west regions, so adjacent counties share a
# fill value
nc$region <- cut(
  sf::st_coordinates(sf::st_centroid(sf::st_geometry(nc)))[, 1],
  4,
  labels = c("west", "midwest", "mideast", "east")
)

# Plain: every county border is drawn, subdividing each colour block.
render_plot(
  vplot(nc) |>
    mark_sf(fill = region) |>
    coord_sf() |>
    labs(title = "Counties (internal borders)"),
  file.path(outdir, "33-plain.png")
)

# Merged: each region dissolves to one outline.
render_plot(
  vplot(nc) |>
    mark_sf(fill = region, merge = TRUE, color = "white") |>
    coord_sf() |>
    labs(title = "Merged regions"),
  file.path(outdir, "33-merged.png")
)

message("33-merged-regions: wrote plain + merged choropleths to ", outdir)
