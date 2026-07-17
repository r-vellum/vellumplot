# The `scales` panel-meta convention: vellumplot attaches a per-axis scale
# descriptor to each cartesian data panel's viewport `meta`, surfaced by vellum as
# `scene_model()$panels$meta[[i]]$scales` and read by a host (vellumwidget) to map
# device pixels back to data values. See `.panel_scales_meta()` / `.axis_scale_desc()`.

# The scales descriptor of the "panel-1-1" data panel, or NULL.
panel_scales <- function(p) {
  m <- vellum::scene_model(vellum::as_vellum_scene(p))$panels
  i <- which(m$name == "panel-1-1")
  if (!length(i)) return(NULL)
  m$meta[[i]]$scales
}

test_that("a continuous cartesian panel carries x/y scale descriptors", {
  df <- data.frame(wt = mtcars$wt, mpg = mtcars$mpg)
  s <- panel_scales(vplot(df) |> mark_point(x = wt, y = mpg))
  expect_true(isTRUE(s$cartesian))
  expect_equal(s$x$type, "continuous")
  expect_equal(s$x$transform, "identity")
  expect_false(s$x$discrete)
  # data range is the (untransformed) data extent; native is the 5%-expanded domain
  expect_equal(c(s$x$data_lo, s$x$data_hi), range(df$wt))
  expect_true(s$x$native_lo < min(df$wt) && s$x$native_hi > max(df$wt))
  expect_null(s$x$time_unit)
  expect_equal(s$y$type, "continuous")
})

test_that("a log10 axis reports transform='log10' with a native (log) domain", {
  s <- panel_scales(
    vplot(data.frame(a = c(1, 10, 100, 1000), y = 1:4)) |>
      mark_point(x = a, y = y) |>
      scale_x_continuous(trans = "log10")
  )
  expect_equal(s$x$type, "continuous")
  expect_equal(s$x$transform, "log10")
  # native domain is in log space (~log10(1)=0 to log10(1000)=3, expanded)
  expect_true(s$x$native_lo < 0.5 && s$x$native_hi > 2.5)
})

test_that("a Date axis reports type='date' + time_unit='day' with epoch-day values", {
  df <- data.frame(d = as.Date("2020-01-01") + c(0, 100, 200, 300), y = 1:4)
  s <- panel_scales(vplot(df) |> mark_point(x = d, y = y))
  expect_equal(s$x$type, "date")
  expect_equal(s$x$time_unit, "day")
  # values are as.numeric(Date) = days since 1970
  expect_true(s$x$data_lo == as.numeric(min(df$d)))
})

test_that("a POSIXct axis reports type='datetime' + time_unit='second'", {
  df <- data.frame(
    t = as.POSIXct("2020-01-01 00:00", tz = "UTC") + c(0, 3600, 7200),
    y = 1:3
  )
  s <- panel_scales(vplot(df) |> mark_point(x = t, y = y))
  expect_equal(s$x$type, "datetime")
  expect_equal(s$x$time_unit, "second")
})

test_that("a discrete axis reports type='discrete' with band width and level labels", {
  s <- panel_scales(
    vplot(data.frame(g = factor(c("a", "b", "c")), y = 1:3)) |> mark_point(x = g, y = y)
  )
  expect_equal(s$x$type, "discrete")
  expect_true(s$x$discrete)
  expect_equal(s$x$band_width, 1)
  expect_equal(s$x$labels, c("a", "b", "c"))
})
