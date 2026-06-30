# 2-D density and heatmaps: tile, raster, bin2d, hex.

library(vellumplot)
outdir <- "figures"
dir.create(outdir, showWarnings = FALSE)

# --- tile (categorical / coarse grid) ---------------------------------------
# One rectangle per (x, y) coloured by fill. Good for correlation matrices.
cm <- cor(mtcars[, c("mpg", "disp", "hp", "drat", "wt", "qsec")])
hm <- expand.grid(v1 = rownames(cm), v2 = colnames(cm))
hm$cor <- as.vector(cm)
vplot(hm) |>
  mark_tile(x = v1, y = v2, fill = cor) |>
  scale_fill_gradient(low = "#b2182b", high = "#2166ac") |>
  labs(title = "Correlation matrix (tiles)", x = NULL, y = NULL) |>
  render_plot(file.path(outdir, "03-tile.png"))

# --- raster (fast path for a complete regular grid) -------------------------
# A smooth field rendered as a single raster image.
g <- expand.grid(x = seq(-3, 3, length.out = 120), y = seq(-3, 3, length.out = 120))
g$z <- with(g, sin(x * 1.5) * cos(y * 1.5) * exp(-(x^2 + y^2) / 8))
vplot(g) |>
  mark_raster(x = x, y = y, fill = z) |>
  scale_fill_continuous(palette = "viridis") |>
  labs(title = "Continuous field (raster)") |>
  render_plot(file.path(outdir, "03-raster.png"))

# --- bin2d (rectangular binning of a point cloud) ---------------------------
# fill defaults to after_stat(count): cell colour encodes how many points fell
# in each rectangular bin.
set.seed(1)
n <- 20000
df <- data.frame(x = rnorm(n), y = rnorm(n) + rnorm(n, 0, 0.4))
vplot(df) |>
  mark_bin2d(x = x, y = y, bins = 40) |>
  scale_fill_continuous(palette = "inferno") |>
  labs(title = "20k points binned into a grid (bin2d)") |>
  render_plot(file.path(outdir, "03-bin2d.png"))

# --- hex (hexagonal binning) ------------------------------------------------
vplot(df) |>
  mark_hex(x = x, y = y, bins = 30) |>
  scale_fill_continuous(palette = "viridis") |>
  labs(title = "Same cloud, hexagonal bins (hex)") |>
  render_plot(file.path(outdir, "03-hex.png"))

message("03-heatmaps-2d: wrote 4 figures to ", outdir)
