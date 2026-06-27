# Compile a plot and read its pixels back via vellum's public raster accessor
# (no temp file). `scene_raster()` returns an integer array [channel, x, y] in
# 0:255 with a top-left origin; reshape to the [y, x, channel] 0..1 layout the
# probes below expect (row 1 = top, matching a read-back PNG).
render_px <- function(plot) {
  arr <- vellum::scene_raster(plot)
  aperm(arr, c(3, 2, 1)) / 255
}

# Count pixels whose RGB is within `tol` of a target (each channel 0..1).
count_near <- function(img, rgb, tol = 0.06) {
  r <- img[,, 1]
  g <- img[,, 2]
  b <- img[,, 3]
  sum(abs(r - rgb[1]) < tol & abs(g - rgb[2]) < tol & abs(b - rgb[3]) < tol)
}

# Does a rectangular region (fractional bounds) contain near-black ink?
has_ink <- function(img, rows, cols, thresh = 0.3) {
  H <- dim(img)[1]
  W <- dim(img)[2]
  rr <- seq(max(1, floor(rows[1] * H)), min(H, ceiling(rows[2] * H)))
  cc <- seq(max(1, floor(cols[1] * W)), min(W, ceiling(cols[2] * W)))
  sub <- img[rr, cc, 1:3, drop = FALSE]
  any(rowSums(sub <= thresh, dims = 2) == 3)
}
