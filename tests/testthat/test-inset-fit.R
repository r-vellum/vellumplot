# VP-120: a fixed-aspect plot placed with inset() / vellum::scene_inset() kept
# the full inset box, framing its letterboxed panel with empty bands; the box
# now shrinks to the plot's own shape.

inset_panel <- function(x) {
  m <- vellum::scene_model(vellum::as_vellum_scene(x))$panels
  m[m$name == "panel-1-1", ]
}

test_that("an aspect-locked plot declares its content box for scene_inset()", {
  fixed <- vplot(mtcars) |> mark_point(x = wt, y = mpg) |> coord_fixed(ratio = 0.2)
  cf <- attr(vellum::as_vellum_scene(fixed), "vellum_content_fit")
  expect_length(cf, 4L)
  expect_true(all(is.finite(cf)))
  # the null weights are the panel's data spans (x-range : ratio * y-range)
  expect_gt(cf[3], 0)
  expect_gt(cf[4], 0)
  free <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
  expect_null(attr(vellum::as_vellum_scene(free), "vellum_content_fit"))
})

test_that("inset() fits a fixed-aspect plot to its box, keeping the near edges", {
  base <- vplot(mtcars) |> mark_point(x = hp, y = qsec)
  # a wide box for a tall plot: the box narrows, keeping its right edge
  tall <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    coord_fixed(ratio = 0.5) |>
    theme(axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank())
  comp <- inset(base, tall, left = 0.3, bottom = 0.5, right = 0.98, top = 0.98,
    align_to = "full")
  s <- vellum::as_vellum_scene(comp)
  dpi <- s@dpi
  W <- 6 * dpi
  H <- 4 * dpi
  margin_px <- 5.5 / 25.4 * dpi
  p <- inset_panel(comp)
  # the panel reaches the box's right edge less the plot margin ...
  expect_equal(p$px1, 0.98 * W - margin_px, tolerance = 1.5)
  # ... and fills the box's height, so its aspect is the locked one
  expect_equal(p$py0, (1 - 0.98) * H + margin_px, tolerance = 1.5)
  asp <- (p$px1 - p$px0) / (p$py1 - p$py0)
  dx <- diff(range(p$xscale_lo, p$xscale_hi))
  dy <- diff(range(p$yscale_lo, p$yscale_hi))
  expect_equal(asp, dx / (0.5 * dy), tolerance = 0.01)
  # a free-aspect inset keeps the whole box
  free <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
  q <- inset_panel(inset(base, free, left = 0.3, bottom = 0.5, right = 0.98, top = 0.98,
    align_to = "full"))
  expect_gt(q$px1 - q$px0, 2 * (p$px1 - p$px0))
})

test_that("inset() fits a coord_sf() map and passes a shadow on", {
  skip_if_not_installed("sf")
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  map <- vplot(nc) |> mark_sf() |> coord_sf() |> theme_void()
  base <- vplot(mtcars) |> mark_point(x = hp, y = qsec)
  comp <- inset(base, map, left = 0.5, bottom = 0.5, right = 0.98, top = 0.98,
    align_to = "full",
    shadow = vellum::vl_shadow(dx = 2, dy = 2, blur = 4))
  s <- vellum::as_vellum_scene(comp)
  box <- vellum::why_size(s, "inset-1")
  # NC is wide: the box keeps its full width (0.48 * 6 in) and gets shorter
  expect_equal(box$width_mm, 0.48 * 6 * 25.4, tolerance = 0.01)
  expect_lt(box$height_mm, 0.48 * 4 * 25.4 - 5)
  expect_no_error(vellum::scene_raster(s))
})

test_that("vellum::scene_inset() fits a fixed-aspect vellumplot guest", {
  skip_if_not("fit" %in% names(formals(vellum::scene_inset)))
  host <- vellum::vl_scene(6, 4)
  guest <- vplot(mtcars, width = 2, height = 2) |>
    mark_point(x = wt, y = mpg) |>
    coord_fixed(ratio = 0.02)
  s <- vellum::scene_inset(host, guest, x = 0.5, y = 0.5, width = 0.5, height = 0.5)
  vp <- vellum:::.node_children(vellum:::.materialize(s))[[1]]@vp
  npc <- function(u) vctrs_field(u, "value")
  # wide content (x-range 3.9 vs 0.02 * 23.5): the region keeps its width only
  expect_equal(npc(vp@width), 0.5)
  expect_lt(npc(vp@height), 0.5)
})
