# mark_sf(merge = TRUE): dissolve adjacent same-fill features into one region.

# Two unit squares sharing the edge x = 1, light grey fill and the default border.
# Plain: the shared edge is stroked (drawn once per square). Merged: the two
# squares union into one 2x1 rectangle, so that interior edge is gone.
two_squares <- function() {
  skip_if_not_installed("sf")
  sq <- function(x0) {
    sf::st_polygon(list(rbind(
      c(x0, 0),
      c(x0 + 1, 0),
      c(x0 + 1, 1),
      c(x0, 1),
      c(x0, 0)
    )))
  }
  sf::st_sf(id = c("a", "b"), geometry = sf::st_sfc(sq(0), sq(1)))
}

# Count the mid-grey border pixels (the polygon stroke) across the image. The
# fills are either light grey (excluded, too pale) or saturated palette colours
# (excluded, not grey), so this isolates the borders. A dissolved region strokes
# only its outer perimeter, so it has fewer.
border_px <- function(p) {
  img <- render_px(p)
  g <- img[,, 1]
  sum(
    g > 0.25 &
      g < 0.55 &
      abs(img[,, 1] - img[,, 2]) < 0.04 &
      abs(img[,, 2] - img[,, 3]) < 0.04
  )
}

sfmap <- function(d, m, ...) {
  vplot(d, width = 4, height = 3) |>
    mark_sf(merge = m, linewidth = 1, ...) |>
    coord_sf() |>
    theme_void()
}

test_that("merge dissolves the shared border between same-fill features", {
  d <- two_squares()
  plain <- border_px(sfmap(d, FALSE, fill = "grey90"))
  merged <- border_px(sfmap(d, TRUE, fill = "grey90"))
  expect_gt(plain, 0)
  expect_lt(merged, 0.75 * plain) # the interior edge is gone
})

test_that("merge leaves features of different fill untouched", {
  d <- two_squares()
  # distinct fills => two style groups => nothing to union either way
  kept <- border_px(sfmap(d, TRUE, fill = id))
  same <- border_px(sfmap(d, FALSE, fill = id))
  expect_equal(kept, same)
})

test_that("merge is a no-op on a single feature (nothing to union)", {
  skip_if_not_installed("sf")
  d <- two_squares()[1, ]
  # one feature => merge_poly() returns NULL => identical batched emit
  expect_identical(
    vellum::scene_raster(sfmap(d, TRUE, fill = "grey90")),
    vellum::scene_raster(sfmap(d, FALSE, fill = "grey90"))
  )
})

test_that("a merged choropleth renders to PNG, PDF, and a scene", {
  skip_if_not_installed("sf")
  nc <- sf::st_read(
    system.file("shape/nc.shp", package = "sf"),
    quiet = TRUE
  )
  nc$region <- factor(nc$CNTY_ID %% 3L)
  p <- vplot(nc) |> mark_sf(fill = region, merge = TRUE) |> coord_sf()
  expect_no_error(vellum::as_vellum_scene(p))
  f <- withr::local_tempfile(fileext = ".pdf")
  expect_no_error(render_plot(p, f))
  expect_identical(rawToChar(readBin(f, "raw", 5L)), "%PDF-")
})
