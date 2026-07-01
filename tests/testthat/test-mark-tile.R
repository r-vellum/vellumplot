# Heatmaps + 2-D bin / 1-D density.

train <- function(p) {
  quill:::.train_scales(p, quill:::.resolve_layers(p))
}
resolve1 <- function(p) quill:::.resolve_layers(p)[[1]]

d <- expand.grid(x = 1:6, y = 1:6)
d$z <- d$x * d$y

test_that("constructors set mark/stat", {
  expect_identical(
    (vplot(d) |> mark_tile(x = x, y = y, fill = z))@layers[[1]]@mark,
    "tile"
  )
  expect_identical(
    (vplot(d) |> mark_raster(x = x, y = y, fill = z))@layers[[1]]@mark,
    "raster"
  )
  bl <- (vplot(faithful) |> mark_bin2d(x = waiting, y = eruptions))@layers[[1]]
  expect_identical(bl@mark, "tile")
  expect_identical(bl@stat, "bin2d")
  dl <- (vplot(mtcars) |> mark_density(x = mpg))@layers[[1]]
  expect_identical(dl@mark, "area")
  expect_identical(dl@stat, "density")
})

test_that("stat bin2d produces non-empty cells with count + width/height", {
  r <- resolve1(
    vplot(faithful) |> mark_bin2d(x = waiting, y = eruptions, bins = 10)
  )
  expect_gt(r$n, 0)
  expect_true(is.numeric(r$values$fill)) # after_stat(count)
  expect_true(!is.null(r$values$width) && !is.null(r$values$height))
})

test_that("stat density produces a dense (x, y) curve, y >= 0", {
  r <- resolve1(vplot(mtcars) |> mark_density(x = mpg))
  expect_gt(r$n, 50)
  expect_true(all(r$values$y >= 0))
})

test_that("density forces the y axis through zero", {
  sc <- train(vplot(mtcars) |> mark_density(x = mpg))
  expect_lte(sc$y$domain[1], 0)
})

test_that("tile / raster / bin2d / density render (tile/bin2d also flipped)", {
  renders <- list(
    function() vplot(d) |> mark_tile(x = x, y = y, fill = z),
    function() vplot(d) |> mark_raster(x = x, y = y, fill = z),
    function() vplot(faithful) |> mark_bin2d(x = waiting, y = eruptions),
    function() vplot(mtcars) |> mark_density(x = mpg),
    function() vplot(d) |> mark_tile(x = x, y = y, fill = z) |> coord_flip(),
    function() {
      vplot(faithful) |> mark_bin2d(x = waiting, y = eruptions) |> coord_flip()
    }
  )
  for (mk in renders) {
    f <- withr::local_tempfile(fileext = ".png")
    render_plot(mk(), f)
    expect_gt(file.info(f)$size, 0)
  }
})

test_that("mark_raster errors on an irregular grid and under coord_flip", {
  irreg <- vplot(mtcars) |> mark_raster(x = wt, y = mpg, fill = hp)
  expect_error(
    render_plot(irreg, withr::local_tempfile(fileext = ".png")),
    "regular grid"
  )
  expect_error(
    render_plot(
      vplot(d) |> mark_raster(x = x, y = y, fill = z) |> coord_flip(),
      withr::local_tempfile(fileext = ".png")
    ),
    "coord_flip"
  )
})

test_that("tile heatmap fills the panel with coloured cells", {
  img <- render_px(vplot(d) |> mark_tile(x = x, y = y, fill = z))
  # very little grey panel remains (tiles cover it)
  panel <- count_near(img, c(0.92, 0.92, 0.92))
  expect_lt(panel, 0.2 * prod(dim(img)[1:2]))
})
