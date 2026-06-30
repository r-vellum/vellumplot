# Faceting: small multiples with facet_wrap / facet_grid, and scale resolution.

library(vellumplot)
outdir <- "figures"
dir.create(outdir, showWarnings = FALSE)

peng <- na.omit(datasets::penguins)

# --- facet_wrap -------------------------------------------------------------
# One panel per level of a variable, wrapped into a ribbon of ncol columns.
vplot(peng, width = 8, height = 3) |>
  mark_point(x = bill_len, y = flipper_len, color = species) |>
  facet_wrap(~species, ncol = 3) |>
  labs(title = "facet_wrap(~species)") |>
  render_plot(file.path(outdir, "10-wrap.png"))

# --- facet_grid -------------------------------------------------------------
# A 2-D grid: rows ~ cols.
vplot(peng, width = 8, height = 5) |>
  mark_point(x = bill_len, y = body_mass, color = species) |>
  facet_grid(sex ~ species) |>
  labs(title = "facet_grid(sex ~ species)") |>
  render_plot(file.path(outdir, "10-grid.png"))

# --- free scales ------------------------------------------------------------
# By default position scales are shared so axes align. Free them per panel
# when the ranges differ a lot.
vplot(mtcars, width = 8, height = 3) |>
  mark_point(x = wt, y = mpg) |>
  facet_wrap(~cyl, scales = "free", ncol = 3) |>
  labs(title = "facet_wrap(scales = \"free\")") |>
  render_plot(file.path(outdir, "10-free.png"))

# --- resolve_scale (fine-grained control) -----------------------------------
# Equivalent to scales = "free_y" but spelled out per aesthetic.
vplot(mtcars, width = 8, height = 3) |>
  mark_point(x = wt, y = mpg) |>
  facet_wrap(~cyl, ncol = 3) |>
  resolve_scale(y = "independent") |>
  labs(title = "resolve_scale(y = \"independent\")") |>
  render_plot(file.path(outdir, "10-resolve.png"))

message("10-facets: wrote 4 figures to ", outdir)
