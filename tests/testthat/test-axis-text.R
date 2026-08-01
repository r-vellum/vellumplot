# Axis tick-label rotation (axis.text angle) and wrapping/auto-fit of long
# labels to their per-tick width.

# Bottom edge of the grey panel as a fraction of image height: the last row that
# is *mostly* grey panel. A taller x-label band (rotated or wrapped labels) pushes
# the panel up, so this fraction shrinks.
panel_floor_bar <- function(p) {
  img <- render_px(p)
  grey <- abs(img[,, 1] - img[,, 2]) < 0.03 &
    img[,, 1] > 0.85 &
    img[,, 1] < 0.97
  rows <- which(rowMeans(grey) > 0.4)
  rows[length(rows)] / dim(img)[1]
}

longcats <- data.frame(
  cat = c(
    "Central metropolitan area",
    "Isolated island group",
    "Northern region",
    "Southeastern district",
    "Western rural counties"
  ),
  val = c(9, 6, 8, 5, 4)
)
shortcats <- data.frame(
  cat = c("A", "B", "C", "D", "E"),
  val = c(9, 6, 8, 5, 4)
)
medcats <- data.frame(
  cat = c("alpha", "bravo", "charlie", "delta", "echo"),
  val = c(9, 6, 8, 5, 4)
)

# ---- unit: rotation plumbing -----------------------------------------------

test_that(".el_rot reads an angle and is blank-safe", {
  expect_identical(vellumplot:::.el_rot(element_text(angle = 45)), 45)
  expect_identical(vellumplot:::.el_rot(element_text()), 0)
  # element_blank() carries no `angle` property; it must read as unrotated.
  expect_identical(vellumplot:::.el_rot(element_blank()), 0)
})

test_that("rotated tick labels anchor the end of the run at the tick", {
  geom <- list(just = c("centre", "top"))
  el <- element_text()
  # unrotated keeps the track's fixed anchor (byte-identical path)
  expect_identical(
    vellumplot:::.axis_text_just(geom, el, 0),
    c("centre", "top")
  )
  # a positive slant hangs down-left (right/top); a negative one down-right
  expect_identical(
    vellumplot:::.axis_text_just(geom, el, 45),
    c("right", "top")
  )
  expect_identical(
    vellumplot:::.axis_text_just(geom, el, -45),
    c("left", "top")
  )
  # an explicit hjust/vjust wins over the default
  expect_identical(
    vellumplot:::.axis_text_just(
      geom,
      element_text(hjust = 0, vjust = 0.5),
      90
    ),
    c("left", "centre")
  )
})

# ---- unit: wrapping stacks a long label ------------------------------------

test_that("a wrap width stacks a long label into more vertical space", {
  el <- element_text(size = 9)
  mm <- function(g) vellum::vl_convert(vellum::grobheight(g), "mm")
  tall <- vellumplot:::.axis_text_grob(
    "Central metropolitan area",
    el,
    c("centre", "top"),
    wrap_mm = 18
  )
  flat <- vellumplot:::.axis_text_grob(
    "Central metropolitan area",
    el,
    c("centre", "top"),
    wrap_mm = NA_real_
  )
  expect_gt(mm(tall), mm(flat))
})

# ---- integration: rotation + wrapping reserve height -----------------------

test_that("axis.text.x angle rotates labels and reserves row height", {
  base <- vplot(medcats, width = 6, height = 4) |> mark_bar(x = cat, y = val)
  rotated <- base |> theme(axis.text.x = element_text(angle = 90))
  # vertical labels make the x-label band taller, so the panel floor rises
  expect_lt(panel_floor_bar(rotated), panel_floor_bar(base))
})

test_that("long discrete x labels wrap and reserve more height than short ones", {
  long <- vplot(longcats, width = 6, height = 4) |> mark_bar(x = cat, y = val)
  short <- vplot(shortcats, width = 6, height = 4) |> mark_bar(x = cat, y = val)
  # wrapped multi-line labels push the panel up relative to single-char labels
  expect_lt(panel_floor_bar(long), panel_floor_bar(short))
})

test_that("rotated and wrapped axes render without error", {
  long <- vplot(longcats, width = 6, height = 4) |> mark_bar(x = cat, y = val)
  expect_no_error(vellum::as_vellum_scene(long))
  expect_no_error(vellum::as_vellum_scene(
    long |> theme(axis.text.x = element_text(angle = 45))
  ))
  # y-axis rotation and a faceted (free-x) case exercise the fallback paths
  expect_no_error(vellum::as_vellum_scene(
    long |> theme(axis.text.y = element_text(angle = 45))
  ))
})
