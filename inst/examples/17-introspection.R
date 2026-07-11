# Layout introspection and semantic output (developer-facing).
# Uses vellum's layout-debug tools on a compiled plot, and shows the per-layer
# ids vellumplot stamps into SVG. Requires vellum (>= 0.0.0.9001).

library(vellumplot)
library(vellum)
outdir <- "figures"
dir.create(outdir, showWarnings = FALSE)

peng <- na.omit(datasets::penguins)

p <- vplot(peng, width = 8, height = 3) |>
  mark_point(x = bill_len, y = flipper_len, color = species) |>
  facet_wrap(~species, ncol = 3) |>
  labs(title = "Layout debug demo", x = "Bill (mm)", y = "Flipper (mm)")

# Compile to a vellum scene once; both tools operate on the compiled scene.
scene <- as_vellum_scene(p)

# --- render(debug = TRUE): overlay every named viewport region ---------------
# Outlines and labels panels, axis gutters, strips, the legend, and titles on
# top of the normal render -- a picture of the track skeleton the layout built.
render(scene, file.path(outdir, "17-debug-overlay.png"), debug = TRUE)

# --- why_size(): why is a region the size it is? ----------------------------
# vellumplot names its structural viewports, so they are queryable by name:
#   plot, panel-area, panel-<r>-<c>, axis-x-<col>, axis-y-<row>,
#   axis-title-x, axis-title-y, strip-<r>-<c> (wrap) / strip-col|row-<n> (grid),
#   legend.
for (nm in c("panel-1-1", "panel-1-3", "legend", "axis-title-x")) {
  w <- why_size(scene, nm)
  # The print method may not auto-dispatch; read fields directly.
  message(sprintf(
    "%-13s %5.1f x %5.1f mm  <- %s",
    nm,
    w$width_mm,
    w$height_mm,
    w$determined_by
  ))
}

# --- per-layer SVG identity --------------------------------------------------
# Each geom layer is stamped with id = "layer-<i>-<mark>", emitted by the SVG
# backend as data-vellum-id on the layer's <g> -- a stable selector for tests,
# accessibility, or interactivity. (Raster/PDF ignore it; it is pure metadata.)
p2 <- vplot(peng) |>
  mark_point(x = bill_len, y = body_mass, color = species) |>
  mark_smooth(x = bill_len, y = body_mass)
svg <- file.path(outdir, "17-ids.svg")
render_plot(p2, svg)
ids <- unique(regmatches(
  paste(readLines(svg), collapse = ""),
  gregexpr("data-vellum-id=\"[^\"]+\"", paste(readLines(svg), collapse = ""))
)[[1]])
message("SVG layer ids: ", paste(ids, collapse = ", "))

message("17-introspection: wrote debug overlay + ids svg to ", outdir)
