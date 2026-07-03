# Layer effects beyond glow: outline and shadow for stroked / point marks.
# Effects are passed per layer via `effects = list(...)` and compose in order.

library(quill)
outdir <- "figures"
dir.create(outdir, showWarnings = FALSE)

set.seed(1)

# --- 1. outline: a contrasting halo for legibility --------------------------
# A white halo makes coloured points pop off a busy / mid-tone panel.
vplot(datasets::iris) |>
  mark_point(
    x = Petal.Length,
    y = Petal.Width,
    color = Species,
    size = 3,
    effects = list(outline(size = 0.8, color = "white"))
  ) |>
  labs(title = "Outlined points", color = NULL) |>
  render_plot(file.path(outdir, "22-outline.png"))

# --- 2. shadow: a soft drop shadow under a line -----------------------------
line <- data.frame(x = 1:40, y = cumsum(rnorm(40)))
vplot(line) |>
  mark_line(x = x, y = y, effects = list(shadow())) |>
  mark_line(x = x, y = y, color = "#1f6feb") |>
  labs(title = "Line with a drop shadow") |>
  render_plot(file.path(outdir, "22-shadow.png"))

# --- 3. composing effects: outline under glow -------------------------------
vplot(line) |>
  mark_line(
    x = x,
    y = y,
    color = "#00e5ff",
    effects = list(outline(color = "black", size = 1.2), glow())
  ) |>
  labs(title = "Outline + glow (composed)") |>
  theme_cyberpunk() |>
  render_plot(file.path(outdir, "22-composed.png"))

message("22-layer-effects: wrote 3 figures to ", outdir)
