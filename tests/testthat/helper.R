# Render a plot to a temp PNG and read it back as an [height, width, channels]
# array of 0..1 values (the only way to verify output without a raster
# accessor; mirrors vellum's own pixel-probing tests).
render_png <- function(plot) {
  skip_if_not_installed("png")
  f <- withr::local_tempfile(fileext = ".png", .local_envir = parent.frame())
  render_plot(plot, f)
  png::readPNG(f)
}

# Count pixels whose RGB is within `tol` of a target (each channel 0..1).
count_near <- function(img, rgb, tol = 0.06) {
  r <- img[, , 1]
  g <- img[, , 2]
  b <- img[, , 3]
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
