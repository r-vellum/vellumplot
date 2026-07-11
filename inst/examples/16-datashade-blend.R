# Big data and compositing: datashading dense clouds, the auto fast-path, and
# blend modes for overlapping layers.

library(vellumplot)
outdir <- "figures"
dir.create(outdir, showWarnings = FALSE)

# --- datashade a million points ---------------------------------------------
# mark_datashade bins points into a canvas-sized grid in one pass and colours
# each cell by density. Cost is decoupled from the point count, so millions of
# rows render fast and overplotting becomes information instead of a smear.
set.seed(1)
n <- 1e6
big <- data.frame(
  x = c(rnorm(n / 2, -2), rnorm(n / 2, 2)),
  y = c(rnorm(n / 2), rnorm(n / 2, 1.5))
)
vplot(big) |>
  mark_datashade(x = x, y = y, how = "eq_hist") |>
  labs(title = "1,000,000 points, datashaded") |>
  render_plot(file.path(outdir, "16-datashade.png"))

# A custom density ramp + a different density-to-colour mapping.
vplot(big) |>
  mark_datashade(
    x = x,
    y = y,
    colors = c("#0d0887", "#cc4778", "#f0f921"),
    how = "log"
  ) |>
  labs(title = "Datashade with a custom ramp (how = \"log\")") |>
  render_plot(file.path(outdir, "16-datashade-ramp.png"))

# --- auto: let mark_point switch to datashading when it's dense -------------
# With auto = TRUE a normal point layer renders as a density raster once the
# row count gets large -- one call that scales from dozens to millions.
vplot(big) |>
  mark_point(x = x, y = y, auto = TRUE) |>
  labs(title = "mark_point(auto = TRUE) on dense data") |>
  render_plot(file.path(outdir, "16-auto.png"))

# --- blend modes ------------------------------------------------------------
# Each layer composites as one isolated group using a CSS mix-blend-mode.
# "multiply" darkens where translucent clouds overlap.
set.seed(2)
m <- 4000
two <- function(cx, cy) data.frame(x = rnorm(m, cx, 0.8), y = rnorm(m, cy, 0.8))
vplot(two(-0.6, 0)) |>
  mark_point(x = x, y = y, color = "#e41a1c", alpha = 0.4) |>
  mark_point(
    x = x,
    y = y,
    color = "#377eb8",
    alpha = 0.4,
    blend = "multiply",
    data = two(0.6, 0)
  ) |>
  labs(title = "Two clouds, blend = \"multiply\"") |>
  render_plot(file.path(outdir, "16-blend.png"))

message("16-datashade-blend: wrote 5 figures to ", outdir)
