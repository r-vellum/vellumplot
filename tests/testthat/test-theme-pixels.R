# Commit-2 new ink: ticks, axis lines, minor gridlines. Each element controls
# pixels iff blanking it changes the rendered image.

diff_px <- function(p1, p2) {
  sum(vellum::scene_raster(p1) != vellum::scene_raster(p2))
}

base <- function() vplot(mtcars) |> mark_point(x = wt, y = mpg)

test_that("axis ticks draw ink (gray has ticks)", {
  expect_gt(diff_px(base(), base() |> theme(axis.ticks = element_blank())), 0)
})

test_that("minor gridlines draw ink", {
  expect_gt(
    diff_px(base(), base() |> theme(panel.grid.minor = element_blank())),
    0
  )
})

test_that("blanking major gridlines removes ink", {
  expect_gt(
    diff_px(base(), base() |> theme(panel.grid.major = element_blank())),
    0
  )
})

test_that("axis lines draw ink (theme_classic)", {
  cl <- base() |> theme_classic()
  expect_gt(diff_px(cl, cl |> theme(axis.line = element_blank())), 0)
})

test_that("theme_void removes the panel background", {
  img_gray <- render_px(base())
  img_void <- render_px(base() |> theme_void())
  # the grey92 panel fill is gone under void
  expect_gt(count_near(img_gray, c(0.92, 0.92, 0.92)), 1000)
  expect_lt(count_near(img_void, c(0.92, 0.92, 0.92)), 1000)
})
