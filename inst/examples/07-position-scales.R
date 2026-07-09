# Position scales: limits, transforms, custom breaks/labels, discrete order.

library(vellumplot)
outdir <- "figures"
dir.create(outdir, showWarnings = FALSE)

peng <- na.omit(datasets::penguins)

# --- explicit limits --------------------------------------------------------
# A scale limit sets the data domain (and the trained range it draws).
vplot(mtcars) |>
  mark_point(x = wt, y = mpg) |>
  scale_x_continuous(limits = c(0, 6)) |>
  scale_y_continuous(limits = c(0, 40)) |>
  labs(title = "Axes forced to start at zero") |>
  render_plot(file.path(outdir, "07-limits.png"))

# --- log10 transform --------------------------------------------------------
# Transforms apply on the scale; breaks land in transformed space.
vplot(mtcars) |>
  mark_point(x = wt, y = hp) |>
  scale_y_continuous(trans = "log10") |>
  labs(title = "Log10 y axis", y = "Horsepower (log10)") |>
  render_plot(file.path(outdir, "07-log10.png"))

# --- sqrt and reverse transforms --------------------------------------------
vplot(mtcars) |>
  mark_point(x = wt, y = disp) |>
  scale_y_continuous(trans = "sqrt") |>
  labs(title = "Square-root y axis") |>
  render_plot(file.path(outdir, "07-sqrt.png"))

vplot(mtcars) |>
  mark_point(x = wt, y = mpg) |>
  scale_x_continuous(trans = "reverse") |>
  labs(title = "Reversed x axis") |>
  render_plot(file.path(outdir, "07-reverse.png"))

# --- custom breaks and labels -----------------------------------------------
vplot(mtcars) |>
  mark_point(x = wt, y = mpg) |>
  scale_x_continuous(
    breaks = c(2, 3, 4, 5),
    labels = c("2k", "3k", "4k", "5k"),
    name = "Weight"
  ) |>
  labs(title = "Custom breaks and labels") |>
  render_plot(file.path(outdir, "07-breaks.png"))

# --- discrete order via limits ----------------------------------------------
# Reorder (and subset) categorical axis levels by passing limits.
vplot(peng) |>
  mark_boxplot(x = species, y = body_mass, fill = species) |>
  scale_x_discrete(limits = c("Gentoo", "Adelie", "Chinstrap")) |>
  labs(title = "Reordered discrete x axis", y = "Body mass (g)") |>
  render_plot(file.path(outdir, "07-discrete-order.png"))

message("07-position-scales: wrote 6 figures to ", outdir)
