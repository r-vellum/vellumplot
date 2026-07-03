# Layer effects beyond glow: outline, shadow, sketch (+ theme_sketch), and the
# inner glow / inner shadow for filled marks. Effects are passed per layer via
# `effects = list(...)` and compose in order.

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

# --- 3. sketch: hand-drawn wobble + theme_sketch ----------------------------
# sketch() densifies and perturbs the path; theme_sketch() sets a paper canvas.
waves <- do.call(
  rbind,
  lapply(1:3, function(k) {
    data.frame(
      x = seq(0, 10, 0.5),
      y = sin(seq(0, 10, 0.5)) + k,
      g = LETTERS[k]
    )
  })
)
vplot(waves) |>
  mark_line(x = x, y = y, color = g, effects = list(sketch())) |>
  labs(title = "Hand-drawn lines", color = NULL) |>
  theme_sketch() |>
  render_plot(file.path(outdir, "22-sketch.png"))

# --- 4. inner glow on a filled area -----------------------------------------
area <- data.frame(x = 1:60, y = 20 + cumsum(rnorm(60, 0.1, 1)))
vplot(area) |>
  mark_area(
    x = x,
    y = y,
    fill = "#0a2a43",
    effects = list(inner_glow(color = "#43c6ff", size = 3))
  ) |>
  labs(title = "Inner glow (masked fill)") |>
  render_plot(file.path(outdir, "22-inner-glow.png"))

# --- 5. inner shadow on a ribbon --------------------------------------------
rib <- data.frame(x = 1:40, lo = (1:40) * 0.3, hi = (1:40) * 0.3 + 10)
vplot(rib) |>
  mark_ribbon(
    x = x,
    ymin = lo,
    ymax = hi,
    fill = "#d8c7a6",
    effects = list(inner_shadow(size = 4))
  ) |>
  labs(title = "Inner shadow (masked fill)") |>
  render_plot(file.path(outdir, "22-inner-shadow.png"))

# --- 6. composing effects: outline under glow -------------------------------
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

message("22-layer-effects: wrote 6 figures to ", outdir)
