# Regression tests for REVIEW3 5E: an sf sub-plot is aspect-locked and needs
# projection + graticule that the aligned composition path never runs, so it now
# forces the composition onto the independent layout (each sub-plot drawn via
# .draw_plot, which projects sf correctly).

test_that("a plain composition still uses the aligned path", {
  comp <- concat(
    vplot(mtcars) |> mark_point(x = wt, y = mpg),
    vplot(mtcars) |> mark_point(x = hp, y = mpg)
  )
  expect_true(vellumplot:::.comp_alignable(comp))
})

test_that("an explicit coord_sf sub-plot forces the independent (projected) path", {
  skip_if_not_installed("sf")
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  m <- vplot(nc) |> mark_sf() |> coord_sf(crs = 3857, graticule = TRUE)
  s <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
  comp <- concat(m, s)
  expect_false(vellumplot:::.comp_alignable(comp))
  f <- local_tempfile(fileext = ".png")
  expect_no_error(render_plot(comp, f))
})

test_that("an sf layer alone (auto-adopted coord) also excludes alignment", {
  skip_if_not_installed("sf")
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  m <- vplot(nc) |> mark_sf()
  s <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
  expect_false(vellumplot:::.comp_alignable(concat(m, s)))
})
