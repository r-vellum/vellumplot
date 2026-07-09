# Filled marks between lines and baselines: area, ribbon, step directions.

library(vellumplot)
outdir <- "figures"
dir.create(outdir, showWarnings = FALSE)

# --- area (fill to the zero baseline) ---------------------------------------
vplot(pressure) |>
  mark_area(x = temperature, y = pressure, fill = "#2c7fb8", alpha = 0.7) |>
  mark_line(x = temperature, y = pressure) |>
  labs(title = "Vapour pressure (area to baseline)", x = "Temp (C)", y = "Pressure") |>
  render_plot(file.path(outdir, "04-area.png"))

# --- ribbon (fill between ymin and ymax) ------------------------------------
# A band that tracks a rolling mean +/- a margin.
set.seed(7)
t <- seq(0, 12, length.out = 120)
y <- sin(t) + cumsum(rnorm(120, 0, 0.08))
rb <- data.frame(t = t, y = y, lo = y - 0.4, hi = y + 0.4)
vplot(rb) |>
  mark_ribbon(x = t, ymin = lo, ymax = hi, fill = "#fdae6b", alpha = 0.6) |>
  mark_line(x = t, y = y, color = "#e6550d") |>
  labs(title = "Uncertainty band (ribbon + line)", x = "time", y = "signal") |>
  render_plot(file.path(outdir, "04-ribbon.png"))

# --- step directions --------------------------------------------------------
# "hv" goes horizontal-then-vertical; "vh" vertical-then-horizontal.
sd <- data.frame(x = 1:8, y = c(1, 3, 2, 5, 4, 6, 5, 7))
vplot(sd) |>
  mark_step(x = x, y = y, direction = "hv", color = "#3182bd") |>
  mark_step(x = x, y = y, direction = "vh", color = "#de2d26") |>
  mark_point(x = x, y = y, size = 1.4) |>
  labs(title = "Step: hv (blue) vs vh (red)") |>
  render_plot(file.path(outdir, "04-step-directions.png"))

message("04-area-ribbon-step: wrote 3 figures to ", outdir)
