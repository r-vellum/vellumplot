# Composing several plots into one figure: concat / hconcat / vconcat,
# wrap_plots, design layouts, spacers, insets, figure annotation, and repeat_.

library(vellumplot)
outdir <- "figures"
dir.create(outdir, showWarnings = FALSE)

peng <- na.omit(datasets::penguins)

p_scatter <- vplot(peng) |>
  mark_point(x = bill_len, y = flipper_len, color = species)
p_hist <- vplot(peng) |>
  mark_histogram(x = body_mass, bins = 20, fill = species)
p_box <- vplot(peng) |>
  mark_boxplot(x = species, y = bill_dep, fill = species)

# --- side by side / stacked -------------------------------------------------
# Identical legends are collected into one by default (guides = "collect").
hconcat(p_scatter, p_hist) |>
  render_plot(file.path(outdir, "15-hconcat.png"))

vconcat(p_scatter, p_box) |>
  render_plot(file.path(outdir, "15-vconcat.png"))

# --- a grid (concat / wrap_plots) -------------------------------------------
concat(p_scatter, p_hist, p_box, ncol = 2) |>
  render_plot(file.path(outdir, "15-grid.png"))

wrap_plots(list(p_scatter, p_hist, p_box, p_scatter), ncol = 2) |>
  render_plot(file.path(outdir, "15-wrap-plots.png"))

# --- explicit spanning layout via a design string --------------------------
# Distinct letters bind to the plots in alphabetical order; "#" is an empty
# cell. Here A spans the top row, B and C share the bottom.
concat(
  p_scatter,
  p_hist,
  p_box,
  design = "
    AA
    BC
  "
) |>
  render_plot(file.path(outdir, "15-design.png"))

# --- spacers ----------------------------------------------------------------
concat(p_scatter, plot_spacer(), p_box, plot_spacer(), ncol = 2) |>
  render_plot(file.path(outdir, "15-spacer.png"))

# --- inset ------------------------------------------------------------------
# Drop a small plot into the corner of a larger one (panel-relative coords).
overview <- vplot(peng) |>
  mark_point(x = bill_len, y = flipper_len, color = species, alpha = 0.6)
mini <- vplot(peng) |>
  mark_histogram(x = body_mass, bins = 15) |>
  theme_minimal() |>
  theme(legend.position = "none")
inset(overview, mini, left = 0.62, bottom = 0.08, right = 0.98, top = 0.45) |>
  render_plot(file.path(outdir, "15-inset.png"))

# --- figure-level annotation with auto tags ---------------------------------
concat(p_scatter, p_hist, p_box, ncol = 3, width = 9, height = 3) |>
  compose_annotation(
    title = "Palmer penguins at a glance",
    caption = "Composed with vellumplot",
    tag_levels = "A"
  ) |>
  render_plot(file.path(outdir, "15-annotation-tags.png"))

# --- repeat_: one view across several fields --------------------------------
# Re-point an encoding at each of several columns and lay the copies out.
repeat_(
  vplot(peng) |> mark_point(y = body_mass, color = species),
  x = c("bill_len", "bill_dep", "flipper_len"),
  ncol = 3
) |>
  render_plot(file.path(outdir, "15-repeat.png"))

message("15-composition: wrote 9 figures to ", outdir)
