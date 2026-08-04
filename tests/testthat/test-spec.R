# Spec-IR tests: building the spec is pure data manipulation, no rendering.

test_that("vplot() builds an empty PlotSpec carrying the data and page size", {
  p <- vplot(mtcars, width = 5, height = 3)
  expect_s3_class(p, "vellumplot::PlotSpec")
  expect_identical(p@data, mtcars)
  expect_equal(p@width, 5)
  expect_equal(p@height, 3)
  expect_length(p@layers, 0)
})

test_that("vplot() rejects non-data-frame input", {
  expect_error(vplot(1:10), "data frame")
})

test_that("mark_point() appends a layer with the right mark and channels", {
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg, color = hp)
  expect_length(p@layers, 1)
  L <- p@layers[[1]]
  expect_identical(L@mark, "point")
  expect_named(L@encoding, c("x", "y", "color"))
  expect_identical(vellumplot:::.channel_label(L@encoding$x), "wt")
})

test_that("scalar aesthetics become params, mapped ones become channels", {
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg, size = 3, color = "red")
  L <- p@layers[[1]]
  expect_named(L@encoding, c("x", "y"))
  expect_equal(L@params$size, 3)
  expect_equal(L@params$color, "red")
})

test_that("a positional literal is a constant coordinate channel, not a param", {
  # `y = 0` (a segment baseline) must train the position scale, so it stays a
  # channel; a style literal like `color` remains a param.
  d <- data.frame(i = 1:3, v = c(2, 5, 9))
  p <- vplot(d) |>
    mark_segment(x = i, y = 0, xend = i, yend = v, color = "red")
  L <- p@layers[[1]]
  expect_named(L@encoding, c("x", "xend", "yend", "y"), ignore.order = TRUE)
  expect_null(L@params$y)
  expect_equal(L@params$color, "red")

  # It resolves to a constant vector recycled over the layer's rows.
  r <- vellumplot:::.resolve_layers(p)[[1]]
  expect_equal(r$values$y, 0)
  expect_identical(r$types$y, "quantitative")
})

test_that("a constant width/height literal stays a param, not a coordinate", {
  # `width`/`height` are geometry but not coordinates: they never train a
  # position scale and their meaning is mark-specific (a data-unit band width on
  # a bar vs a physical box size on a label). So a constant stays in `params`,
  # where each emitter reads it -- it is deliberately not a `.POSITION_AES`
  # channel (vellumplot#3).
  p <- vplot(data.frame(x = c("a", "b"), y = c(1, 2))) |>
    mark_bar(x = x, y = y, width = 0.5)
  L <- p@layers[[1]]
  expect_equal(L@params$width, 0.5)
  expect_false("width" %in% names(L@encoding))

  # The param survives resolution rather than being dropped, so the emitter can
  # read it (a mapped `width = col` would instead be a channel in `values`).
  r <- vellumplot:::.resolve_layers(p)[[1]]
  expect_equal(r$params$width, 0.5)
  expect_null(r$values$width)
})

test_that("multiple marks stack into multiple layers", {
  p <- vplot(mtcars) |>
    mark_line(x = wt, y = mpg) |>
    mark_point(x = wt, y = mpg)
  expect_length(p@layers, 2)
  expect_identical(
    vapply(p@layers, function(L) L@mark, character(1)),
    c("line", "point")
  )
})

test_that("scale_*() appends and overrides last-wins per aesthetic", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    scale_x_continuous(limits = c(0, 6)) |>
    scale_x_continuous(limits = c(1, 5))
  xs <- Filter(function(s) s@aesthetic == "x", p@scales)
  expect_length(xs, 1)
  expect_equal(xs[[1]]@domain, c(1, 5))
})

test_that("channel type is inferred at resolve time", {
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg, color = factor(cyl))
  r <- vellumplot:::.resolve_layers(p)[[1]]
  expect_identical(r$types$x, "quantitative")
  expect_identical(r$types$color, "nominal")
})

test_that("summary() shows a readable spec tree", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, color = hp) |>
    scale_color_continuous()
  out <- cli::cli_fmt(summary(p))
  expect_true(any(grepl("PlotSpec", out)))
  expect_true(any(grepl("mark_point", out)))
  expect_true(any(grepl("color", out)))
})

test_that("print() draws the plot and returns the spec invisibly", {
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
  f <- local_tempfile(fileext = ".png")
  grDevices::png(f, width = 400, height = 300)
  on.exit(grDevices::dev.off(), add = TRUE)
  out <- withVisible(print(p))
  expect_false(out$visible)
  expect_identical(out$value, p)
  grDevices::dev.off()
  on.exit()
  expect_gt(file.info(f)$size, 0) # something was drawn into the device
})

test_that("the spec round-trips through serialize()", {
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg, color = hp)
  p2 <- unserialize(serialize(p, NULL))
  expect_length(p2@layers, 1)
  expect_identical(
    vellumplot:::.channel_label(p2@layers[[1]]@encoding$y),
    "mpg"
  )
})
