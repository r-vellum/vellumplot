# Coordinate systems: cartesian zoom, flipped axes, fixed aspect ratio.
# (Polar / pie / donut have their own script, 09.)

library(quill)
outdir <- "figures"
dir.create(outdir, showWarnings = FALSE)

# --- coord_cartesian: zoom without dropping data ----------------------------
# Unlike scale limits, a coord zoom clips the *view*: the smooth still sees all
# the data, we just look at a window of it.
vplot(mtcars) |>
  mark_point(x = wt, y = mpg) |>
  mark_smooth(x = wt, y = mpg, se = TRUE) |>
  coord_cartesian(xlim = c(2, 4), ylim = c(15, 30)) |>
  labs(title = "coord_cartesian zoom (smooth fitted on all data)") |>
  render_plot(file.path(outdir, "08-cartesian-zoom.png"))

# --- coord_flip: horizontal bars --------------------------------------------
vplot(mtcars) |>
  mark_bar(x = factor(cyl), fill = factor(cyl)) |>
  coord_flip() |>
  labs(title = "Flipped bars", x = "Cylinders") |>
  render_plot(file.path(outdir, "08-flip.png"))

# --- coord_fixed / coord_equal: locked aspect -------------------------------
# One unit on y occupies `ratio` times the device length of one unit on x.
vplot(iris) |>
  mark_point(x = Petal.Length, y = Petal.Width, color = Species) |>
  coord_fixed(ratio = 1) |>
  labs(title = "coord_fixed(ratio = 1): equal data units") |>
  render_plot(file.path(outdir, "08-fixed.png"))

message("08-coordinates: wrote 3 figures to ", outdir)
