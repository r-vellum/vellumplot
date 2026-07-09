# The size and shape aesthetics, and their scales.

library(vellumplot)
outdir <- "figures"
dir.create(outdir, showWarnings = FALSE)

peng <- na.omit(datasets::penguins)

# --- size (bubble chart) ----------------------------------------------------
# A mapped continuous size; scale_size sets the output range in mm.
vplot(peng) |>
  mark_point(x = bill_len, y = flipper_len, size = body_mass, alpha = 0.5) |>
  scale_size(range = c(0.5, 4), name = "Mass (g)") |>
  labs(title = "Bubble chart (size = body mass)") |>
  render_plot(file.path(outdir, "06-size.png"))

# --- shape ------------------------------------------------------------------
# A discrete shape aesthetic cycles through marker shapes; restrict / reorder
# them with scale_shape(values = ).
vplot(peng) |>
  mark_point(x = bill_len, y = flipper_len, shape = species, size = 1.8) |>
  scale_shape(values = c("circle", "triangle", "square")) |>
  labs(title = "Shape encodes species") |>
  render_plot(file.path(outdir, "06-shape.png"))

# --- size + shape + colour together -----------------------------------------
# Four channels at once: x, y, colour (continuous), size (continuous),
# shape (discrete) -> three stacked legends. Three guides (one a bubble legend
# whose keys are sized to the largest point) need vertical room, so the figure
# is a little taller than the default.
vplot(peng, width = 6.5, height = 5.5) |>
  mark_point(
    x = bill_len, y = bill_dep,
    color = flipper_len, size = body_mass, shape = species,
    alpha = 0.8
  ) |>
  scale_color_continuous(palette = "plasma") |>
  scale_size(range = c(0.5, 3.5)) |>
  labs(title = "Colour + size + shape") |>
  render_plot(file.path(outdir, "06-combined.png"))

# --- merged legend ----------------------------------------------------------
# Mapping one variable to two aesthetics draws a single legend whose keys carry
# both encodings: here `species` drives colour and shape, so the guide shows a
# coloured shape per species instead of two stacked legends. (Give one scale a
# different name= to keep them separate.)
vplot(peng) |>
  mark_point(x = bill_len, y = flipper_len, color = species, shape = species) |>
  labs(title = "Merged colour + shape legend") |>
  render_plot(file.path(outdir, "06-merged.png"))

message("06-size-shape-scales: wrote 4 figures to ", outdir)
