# The size, shape and line-width aesthetics, and their scales.

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
    x = bill_len,
    y = bill_dep,
    color = flipper_len,
    size = body_mass,
    shape = species,
    alpha = 0.8
  ) |>
  scale_color_continuous(palette = "plasma") |>
  scale_size(range = c(0.5, 3.5)) |>
  labs(title = "Colour + size + shape") |>
  render_plot(file.path(outdir, "06-combined.png"))

# --- SVG icon markers -------------------------------------------------------
# `shape` also accepts an SVG icon -- a path `d` string (what icon sets ship) or
# a `.svg` file -- drawn as a crisp vector marker via vellum's svg_grob(). A
# literal `d` is a constant marker; map `shape` to a variable and give
# scale_shape(values = ) one icon per level to vary it (the legend shows the
# icons). `size` scales the icon.
heart <- "M12 21s-7-4.35-9.5-8.5C1 9 2.5 5 6 5c2 0 3.5 1.5 4 2.5C10.5 6.5 12 5 14 5c3.5 0 5 4 3.5 7.5C19 16.65 12 21 12 21z"
star <- "M12 2l3 7h7l-5.5 4.5 2 7-6.5-4.5-6.5 4.5 2-7L2 9h7z"
drop <- "M12 2s7 8 7 12a7 7 0 0 1-14 0c0-4 7-12 7-12z"

vplot(peng) |>
  mark_point(
    x = bill_len,
    y = flipper_len,
    shape = species,
    color = species,
    size = 1.6
  ) |>
  scale_shape(values = c(Adelie = heart, Chinstrap = star, Gentoo = drop)) |>
  labs(title = "SVG icon markers (shape = a path d per species)") |>
  render_plot(file.path(outdir, "06-svg-markers.png"))

# --- line width -------------------------------------------------------------
# A mapped `linewidth` on a line: each segment is drawn at one width (the mean
# of its two endpoint values), so the stroke thickens where the value is larger.
# The width steps at each vertex -- that is inherent to per-segment width.
vplot(datasets::pressure) |>
  mark_line(x = temperature, y = pressure, linewidth = pressure) |>
  scale_linewidth(range = c(0.5, 6), name = "Pressure") |>
  labs(title = "Variable-width line (linewidth = pressure)") |>
  render_plot(file.path(outdir, "06-linewidth.png"))

# --- merged legend ----------------------------------------------------------
# Mapping one variable to two aesthetics draws a single legend whose keys carry
# both encodings: here `species` drives colour and shape, so the guide shows a
# coloured shape per species instead of two stacked legends. (Give one scale a
# different name= to keep them separate.)
vplot(peng) |>
  mark_point(x = bill_len, y = flipper_len, color = species, shape = species) |>
  labs(title = "Merged colour + shape legend") |>
  render_plot(file.path(outdir, "06-merged.png"))

message("06-size-shape-scales: wrote 6 figures to ", outdir)
