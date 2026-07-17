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

# --- continuous colorbar filter metadata (interactive visualMap) ---

test_that("a continuous colour scale attaches per-mark filter_value + a colorbar descriptor", {
  df <- data.frame(x = 1:5, y = 1:5, z = c(10, 20, 30, 40, 50), id = letters[1:5])
  m <- vellum::scene_model(vellum::as_vellum_scene(
    vplot(df) |> mark_point(x = x, y = y, color = z, data_id = id)
  ))
  e <- m$elements
  # per-mark value on the continuous colour scale
  i <- which(e$key == "c")
  expect_equal(e$meta[[i]]$filter_value, 30)
  # a colorbar descriptor rides on the gradient-bar rect (unkeyed), with its bbox
  cb_i <- which(vapply(e$meta, function(md) !is.null(md$colorbar), logical(1)))
  expect_length(cb_i, 1L)
  cb <- e$meta[[cb_i]]$colorbar
  expect_equal(cb$aesthetic, "color")
  expect_equal(c(cb$lo, cb$hi), range(df$z))
  expect_true(cb$orientation %in% c("v", "h"))
  expect_true(e$x1[cb_i] > e$x0[cb_i] && e$y1[cb_i] > e$y0[cb_i]) # a real rect
})

test_that("a discrete colour scale attaches no colorbar descriptor / filter_value", {
  df <- data.frame(x = 1:3, y = 1:3, g = factor(c("a", "b", "c")), id = letters[1:3])
  m <- vellum::scene_model(vellum::as_vellum_scene(
    vplot(df) |> mark_point(x = x, y = y, color = g, data_id = id)
  ))
  e <- m$elements
  has_cb <- any(vapply(e$meta, function(md) !is.null(md$colorbar), logical(1)))
  has_fv <- any(vapply(e$meta, function(md) !is.null(md$filter_value), logical(1)))
  expect_false(has_cb)
  expect_false(has_fv)
})
