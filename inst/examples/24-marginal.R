# Marginal plots: distributions of x and y along the panel edges (add_marginal),
# the vellumplot analogue of ggExtra::ggMarginal().

library(vellumplot)
outdir <- "figures"
dir.create(outdir, showWarnings = FALSE)

peng <- na.omit(datasets::penguins)

# --- density margins --------------------------------------------------------
# A top density of x and a right density of y, each sharing the scatter's axis.
vplot(faithful) |>
  mark_point(x = eruptions, y = waiting) |>
  add_marginal() |>
  labs(title = "Old Faithful", x = "Eruption (min)", y = "Waiting (min)") |>
  render_plot(file.path(outdir, "24-marginal-density.png"))

# --- histogram margins ------------------------------------------------------
# type = "histogram" bins each variable; sides selects which edges to draw.
vplot(faithful) |>
  mark_point(x = eruptions, y = waiting) |>
  add_marginal(type = "histogram", bins = 20) |>
  render_plot(file.path(outdir, "24-marginal-histogram.png"))

# --- grouped margins --------------------------------------------------------
# group = TRUE splits each marginal by the scatter's discrete colour mapping.
vplot(peng) |>
  mark_point(x = bill_len, y = bill_dep, color = species) |>
  add_marginal(group = TRUE) |>
  labs(title = "Penguin bills", x = "Bill length (mm)", y = "Bill depth (mm)") |>
  render_plot(file.path(outdir, "24-marginal-grouped.png"))
