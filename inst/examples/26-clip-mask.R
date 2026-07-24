# Clip / mask to a geometry: restrict a plot's marks to a shape (clip_to) or fade
# the panel with a soft radial mask (set_mask). Cartesian coordinates only.

library(vellumplot)
outdir <- "figures"
dir.create(outdir, showWarnings = FALSE)

# A field to clip.
grid <- expand.grid(x = 1:24, y = 1:24)
grid$z <- with(grid, sin(x / 4) + cos(y / 4))

# --- 1. hard clip to a polygon region ---------------------------------------
# region as a data frame of x/y vertices (an sf object works too).
diamond <- data.frame(x = c(12, 22, 12, 2), y = c(2, 12, 22, 12))
vplot(grid) |>
  mark_tile(x = x, y = y, fill = z) |>
  clip_to(diamond) |>
  labs(title = "Clipped to a diamond") |>
  render_plot(file.path(outdir, "26-clip.png"))

# --- 2. invert: punch the shape out as a hole -------------------------------
vplot(grid) |>
  mark_tile(x = x, y = y, fill = z) |>
  clip_to(diamond, invert = TRUE) |>
  labs(title = "Region punched out (invert)") |>
  render_plot(file.path(outdir, "26-clip-invert.png"))

# --- 3. a soft radial vignette ----------------------------------------------
vplot(grid) |>
  mark_tile(x = x, y = y, fill = z) |>
  set_mask(feather = 0.5) |>
  labs(title = "Vignette (set_mask)") |>
  render_plot(file.path(outdir, "26-vignette.png"))
