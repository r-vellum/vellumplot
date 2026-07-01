# Position adjustments: jitter for points; stack / dodge / fill for bars.

library(quill)
outdir <- "figures"
dir.create(outdir, showWarnings = FALSE)

peng <- na.omit(datasets::penguins)

# --- jitter -----------------------------------------------------------------
# Scatter discrete x so overlapping points separate; seed makes it repeatable.
vplot(peng) |>
  mark_point(
    x = species, y = body_mass, color = species,
    position = "jitter", seed = 42, alpha = 0.6
  ) |>
  labs(title = "Jittered points over a discrete axis", y = "Body mass (g)") |>
  render_plot(file.path(outdir, "11-jitter.png"))

# --- stacked bars (default) -------------------------------------------------
vplot(peng) |>
  mark_bar(x = island, fill = species) |>
  labs(title = "Stacked bars (position = \"stack\")") |>
  render_plot(file.path(outdir, "11-stack.png"))

# --- dodged bars ------------------------------------------------------------
vplot(peng) |>
  mark_bar(x = island, fill = species, position = "dodge") |>
  labs(title = "Side-by-side bars (position = \"dodge\")") |>
  render_plot(file.path(outdir, "11-dodge.png"))

# --- filled bars (normalised to 1) ------------------------------------------
vplot(peng) |>
  mark_bar(x = island, fill = species, position = "fill") |>
  labs(title = "Proportions (position = \"fill\")", y = "proportion") |>
  render_plot(file.path(outdir, "11-fill.png"))

message("11-positions: wrote 4 figures to ", outdir)
