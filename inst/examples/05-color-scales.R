# Colour and fill scales: continuous ramps, discrete palettes, manual, gradient.

library(quill)
outdir <- "figures"
dir.create(outdir, showWarnings = FALSE)

peng <- na.omit(datasets::penguins)

# --- continuous colour ramp -------------------------------------------------
# A mapped continuous variable gets a perceptual ramp + a colour-bar legend.
# `palette` accepts any grDevices::hcl.colors() name.
vplot(peng) |>
  mark_point(x = bill_len, y = bill_dep, color = body_mass, size = 1.4) |>
  scale_color_continuous(palette = "Batlow", name = "Mass (g)") |>
  labs(title = "Continuous colour (Batlow)") |>
  render_plot(file.path(outdir, "05-continuous.png"))

# --- discrete qualitative palette -------------------------------------------
vplot(peng) |>
  mark_point(x = bill_len, y = bill_dep, color = species, size = 1.4) |>
  scale_color_discrete(palette = "Set 2") |>
  labs(title = "Discrete colour (Set 2)") |>
  render_plot(file.path(outdir, "05-discrete.png"))

# --- manual colours ---------------------------------------------------------
# Named values match data levels by name; unmatched levels fall back to grey.
vplot(peng) |>
  mark_point(x = bill_len, y = bill_dep, color = species, size = 1.4) |>
  scale_color_manual(values = c(
    Adelie = "#ff6f00", Chinstrap = "#9c27b0", Gentoo = "#00897b"
  )) |>
  labs(title = "Manual colours by name") |>
  render_plot(file.path(outdir, "05-manual.png"))

# --- two-point gradient -----------------------------------------------------
vplot(peng) |>
  mark_point(x = bill_len, y = flipper_len, color = body_mass, size = 1.4) |>
  scale_color_gradient(low = "#ffeda0", high = "#bd0026", name = "Mass") |>
  labs(title = "Two-point gradient (low -> high)") |>
  render_plot(file.path(outdir, "05-gradient.png"))

# --- fill scale on bars -----------------------------------------------------
# color and fill share one scale machinery; use the fill_* variants for filled
# geometries.
vplot(peng) |>
  mark_bar(x = island, fill = island) |>
  scale_fill_discrete(palette = "Dark 2") |>
  labs(title = "Fill scale on bars") |>
  render_plot(file.path(outdir, "05-fill.png"))

message("05-color-scales: wrote 5 figures to ", outdir)
