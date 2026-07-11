# Basic marks: the building-block geometries.
# point, line, rule, segment, bar, step.

library(vellumplot)
outdir <- "figures"
dir.create(outdir, showWarnings = FALSE)

peng <- na.omit(datasets::penguins)

# --- points -----------------------------------------------------------------
# The canonical scatterplot. A constant aesthetic (size = 1.4) is a number;
# a mapped aesthetic (color = species) is a bare column name.
vplot(peng) |>
  mark_point(x = bill_len, y = flipper_len, color = species, size = 1.4) |>
  labs(
    title = "Bill length vs flipper length",
    x = "Bill (mm)",
    y = "Flipper (mm)"
  ) |>
  render_plot(file.path(outdir, "01-points.png"))

# --- lines ------------------------------------------------------------------
# Points are connected in x order. Mapping colour splits the data into one
# line per group (here one trajectory per chick's diet).
cw <- aggregate(weight ~ Time + Diet, ChickWeight, mean)
cw$Diet <- factor(cw$Diet)
vplot(cw) |>
  mark_line(x = Time, y = weight, color = Diet) |>
  labs(title = "Mean chick weight over time", x = "Days", y = "Weight (g)") |>
  render_plot(file.path(outdir, "01-lines.png"))

# --- reference rules --------------------------------------------------------
# mark_rule draws full-width / full-height reference lines from a mapped
# intercept (here horizontal rules at each group mean).
means <- aggregate(flipper_len ~ species, peng, mean)
vplot(peng) |>
  mark_point(x = bill_len, y = flipper_len, color = species, alpha = 0.5) |>
  mark_rule(y = flipper_len, color = species, data = means) |>
  labs(title = "Group means as reference rules") |>
  render_plot(file.path(outdir, "01-rules.png"))

# --- segments ---------------------------------------------------------------
# A line per row from (x, y) to (xend, yend). A lollipop chart: stems from the
# axis up to each value.
mt <- data.frame(car = rownames(mtcars), mpg = mtcars$mpg)
mt <- mt[order(mt$mpg), ][1:12, ]
mt$i <- seq_len(nrow(mt))
vplot(mt) |>
  mark_segment(x = i, y = 0, xend = i, yend = mpg) |>
  mark_point(x = i, y = mpg, size = 1.8, color = "#2b6cb0") |>
  labs(title = "Lollipops via segment + point", x = "rank", y = "mpg") |>
  render_plot(file.path(outdir, "01-segments.png"))

# --- bars -------------------------------------------------------------------
# With no y, mark_bar counts rows per category. With y it uses the values.
vplot(peng) |>
  mark_bar(x = species, fill = species) |>
  labs(title = "Counts per species (count stat)") |>
  render_plot(file.path(outdir, "01-bars-count.png"))

vplot(means) |>
  mark_bar(x = species, y = flipper_len, fill = species) |>
  labs(title = "Mean flipper length (value bars)", y = "Flipper (mm)") |>
  render_plot(file.path(outdir, "01-bars-value.png"))

# --- steps ------------------------------------------------------------------
# A staircase line, e.g. for a cumulative or piecewise-constant series.
ed <- data.frame(x = 1:10, y = cumsum(c(2, 1, 3, -1, 2, 4, -2, 1, 3, 1)))
vplot(ed) |>
  mark_step(x = x, y = y) |>
  labs(title = "Cumulative series as a step line") |>
  render_plot(file.path(outdir, "01-step.png"))

message("01-basic-marks: wrote 8 figures to ", outdir)
