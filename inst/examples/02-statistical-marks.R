# Statistical marks: geometries that transform the data before drawing.
# histogram, density, smooth, boxplot, summary, errorbar, linerange.

library(quill)
outdir <- "figures"
dir.create(outdir, showWarnings = FALSE)

peng <- na.omit(datasets::penguins)

# --- histogram --------------------------------------------------------------
# Bins a continuous x and draws per-bin counts. Mapping fill stacks the groups.
vplot(peng) |>
  mark_histogram(x = body_mass, bins = 25, fill = species) |>
  labs(title = "Body mass distribution", x = "Body mass (g)", y = "count") |>
  render_plot(file.path(outdir, "02-histogram.png"))

# --- density ----------------------------------------------------------------
# A 1-D kernel density as a filled curve; adjust scales the bandwidth.
vplot(faithful) |>
  mark_density(x = eruptions, fill = "#3182bd", alpha = 0.6, adjust = 0.8) |>
  labs(title = "Old Faithful eruption density", x = "Eruption (min)") |>
  render_plot(file.path(outdir, "02-density.png"))

# --- smooth -----------------------------------------------------------------
# A linear fit with a 95% confidence ribbon over the raw points.
vplot(peng) |>
  mark_point(x = bill_len, y = body_mass, color = species, alpha = 0.5) |>
  mark_smooth(x = bill_len, y = body_mass, method = "lm", se = TRUE) |>
  labs(title = "Linear smooth with confidence band") |>
  render_plot(file.path(outdir, "02-smooth.png"))

# --- boxplot ----------------------------------------------------------------
# Box (Q1-Q3), median, 1.5*IQR whiskers and outliers, per x category.
vplot(peng) |>
  mark_boxplot(x = species, y = flipper_len, fill = species) |>
  labs(title = "Flipper length by species", y = "Flipper (mm)") |>
  render_plot(file.path(outdir, "02-boxplot.png"))

# --- summary ----------------------------------------------------------------
# Aggregate y per x with a function (default mean) and draw the result.
vplot(peng) |>
  mark_point(x = species, y = body_mass, color = species, alpha = 0.25) |>
  mark_summary(x = species, y = body_mass, fun = median, size = 2.5) |>
  labs(title = "Raw points with median summary", y = "Body mass (g)") |>
  render_plot(file.path(outdir, "02-summary.png"))

# --- error bars / line ranges ----------------------------------------------
# Pre-compute mean +/- sd, then draw a ranged interval. errorbar adds caps;
# linerange omits them.
agg <- do.call(rbind, lapply(split(peng, peng$species), function(d) {
  data.frame(
    species = d$species[1],
    mean = mean(d$body_mass),
    ymin = mean(d$body_mass) - sd(d$body_mass),
    ymax = mean(d$body_mass) + sd(d$body_mass)
  )
}))
vplot(agg) |>
  mark_errorbar(x = species, ymin = ymin, ymax = ymax, width = 0.4) |>
  mark_point(x = species, y = mean, size = 1.8) |>
  labs(title = "Mean +/- SD error bars", y = "Body mass (g)") |>
  render_plot(file.path(outdir, "02-errorbar.png"))

vplot(agg) |>
  mark_linerange(x = species, ymin = ymin, ymax = ymax) |>
  mark_point(x = species, y = mean, size = 1.8, color = "#c0392b") |>
  labs(title = "Line ranges (no caps)", y = "Body mass (g)") |>
  render_plot(file.path(outdir, "02-linerange.png"))

message("02-statistical-marks: wrote 7 figures to ", outdir)
