# Cyberpunk effects: theme_cyberpunk() + glow() layer effect + gradient fills.
# The three compose: theme_cyberpunk() sets the dark canvas and neon palette;
# glow() fans a stroked/point mark into a soft halo; linear_gradient() fills an
# area/bar with a paint that fades to transparent.

library(vellumplot)
outdir <- "figures"
dir.create(outdir, showWarnings = FALSE)

set.seed(1)

# --- 1. glowing multi-series lines ------------------------------------------
# color maps to the neon default palette; each series glows in its own hue.
series <- do.call(
  rbind,
  lapply(c("signal", "noise", "trend"), function(g) {
    data.frame(t = 1:60, y = cumsum(rnorm(60, 0, 1)), g = g)
  })
)
vplot(series) |>
  mark_line(x = t, y = y, color = g, effects = list(glow())) |>
  labs(title = "Glowing series", color = NULL) |>
  theme_cyberpunk() |>
  render_plot(file.path(outdir, "21-glow-lines.png"))

# --- 2. gradient area under a glowing line ----------------------------------
# The classic mplcyberpunk look: a bright line with a gradient fill fading to
# transparent beneath it. The gradient direction is set by the paint's y1/y2.
area <- data.frame(x = 1:80, y = 20 + cumsum(rnorm(80, 0.05, 1)))
vplot(area) |>
  mark_area(
    x = x,
    y = y,
    fill = linear_gradient(
      c("#08F7FE", "#08F7FE00"),
      x1 = 0,
      y1 = 1,
      x2 = 0,
      y2 = 0
    )
  ) |>
  mark_line(x = x, y = y, color = "#08F7FE", effects = list(glow(size = 5))) |>
  labs(title = "Gradient fill under a neon line") |>
  theme_cyberpunk() |>
  render_plot(file.path(outdir, "21-gradient-line.png"))

# --- 3. gradient bars -------------------------------------------------------
# A gradient fill paints every bar with one paint, fading up from transparent.
bars <- data.frame(
  cat = c("alpha", "beta", "gamma", "delta", "epsilon"),
  n = c(4, 9, 6, 11, 7)
)
vplot(bars) |>
  mark_bar(
    x = cat,
    y = n,
    fill = linear_gradient(
      c("#FE53BB00", "#FE53BB"),
      x1 = 0,
      y1 = 0,
      x2 = 0,
      y2 = 1
    )
  ) |>
  labs(title = "Gradient bars") |>
  theme_cyberpunk() |>
  render_plot(file.path(outdir, "21-gradient-bars.png"))

# --- 4. glowing scatter -----------------------------------------------------
# glow() on points: each marker gets a neon halo. Colour uses the neon default.
vplot(datasets::iris) |>
  mark_point(
    x = Petal.Length,
    y = Petal.Width,
    color = Species,
    size = 2.5,
    effects = list(glow(size = 4))
  ) |>
  labs(title = "Glowing scatter", color = NULL) |>
  theme_cyberpunk() |>
  render_plot(file.path(outdir, "21-glow-points.png"))

message("21-cyberpunk-effects: wrote 4 figures to ", outdir)
