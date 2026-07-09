# Polar coordinates and their shortcuts: rose/coxcomb, radar, pie, donut.

library(vellumplot)
outdir <- "figures"
dir.create(outdir, showWarnings = FALSE)

peng <- na.omit(datasets::penguins)

# --- wind rose / coxcomb ----------------------------------------------------
# theta = "x": the categorical x becomes the angle, bar height the radius.
vplot(mtcars) |>
  mark_bar(x = factor(cyl), fill = factor(cyl)) |>
  coord_polar(theta = "x") |>
  labs(title = "Coxcomb (bars in polar space)", fill = "cyl") |>
  render_plot(file.path(outdir, "09-coxcomb.png"))

# --- radar / spider ---------------------------------------------------------
# A closed line traced around the angular axis.
radar <- data.frame(
  axis = factor(c("speed", "power", "range", "comfort", "price"),
                levels = c("speed", "power", "range", "comfort", "price")),
  value = c(8, 6, 7, 5, 9)
)
vplot(radar) |>
  mark_line(x = axis, y = value, color = "#2b8cbe") |>
  mark_point(x = axis, y = value, size = 2, color = "#2b8cbe") |>
  coord_polar(theta = "x") |>
  labs(title = "Radar chart") |>
  render_plot(file.path(outdir, "09-radar.png"))

# --- pie (mark_pie shortcut) ------------------------------------------------
# Each value is a wedge; the shortcut sets a polar theta = "y" coordinate.
counts <- as.data.frame(table(species = peng$species))
vplot(counts) |>
  mark_pie(value = Freq, fill = species) |>
  labs(title = "Penguins per species (pie)") |>
  render_plot(file.path(outdir, "09-pie.png"))

# --- donut ------------------------------------------------------------------
vplot(counts) |>
  mark_donut(value = Freq, fill = species, hole = 0.6) |>
  labs(title = "Same data as a donut") |>
  render_plot(file.path(outdir, "09-donut.png"))

# --- bullseye: a stacked bar in polar y -------------------------------------
# coord_polar(theta = "y") on a single stacked bar gives a part-of-whole ring;
# direction = -1 winds counter-clockwise.
vplot(counts) |>
  mark_bar(x = factor(1), y = Freq, fill = species, position = "fill") |>
  coord_polar(theta = "y", direction = -1) |>
  labs(title = "Normalised ring (position = fill, polar y)") |>
  render_plot(file.path(outdir, "09-ring.png"))

message("09-polar-pie-donut: wrote 5 figures to ", outdir)
