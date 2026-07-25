# Sparklines: compact, axis-free "word-sized" charts of a single series, sized in
# physical units (mm). A vsparkline() is a plain PlotSpec -- render it, or inset()
# it into a figure.

library(vellumplot)
outdir <- "figures"
dir.create(outdir, showWarnings = FALSE)

set.seed(1)

# --- 1. the three shapes ----------------------------------------------------
render_plot(
  vsparkline(cumsum(rnorm(40)), width = 60, height = 16),
  file.path(outdir, "27-line.png"),
  dpi = 300
)
render_plot(
  vsparkline(
    rpois(24, 6),
    type = "bar",
    color = "steelblue",
    width = 60,
    height = 16
  ),
  file.path(outdir, "27-bar.png"),
  dpi = 300
)
render_plot(
  vsparkline(
    sample(c(-1, 1), 30, replace = TRUE),
    type = "winloss",
    width = 60,
    height = 16
  ),
  file.path(outdir, "27-winloss.png"),
  dpi = 300
)

# --- 2. a sparkline floated onto a plot -------------------------------------
# It is a PlotSpec, so it composes: a tiny trend indicator in the corner.
main <- vplot(mtcars) |>
  mark_point(x = wt, y = mpg) |>
  labs(title = "Weight vs mileage")
spark <- vsparkline(cumsum(rnorm(40)), points = "last")
inset(main, spark, left = 0.62, bottom = 0.9, right = 0.98, top = 0.99) |>
  render_plot(file.path(outdir, "27-inset.png"))
