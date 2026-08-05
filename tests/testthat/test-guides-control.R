# guides(): per-scale legend control (guide = "none" / guide_legend()).

df <- data.frame(wt = mtcars$wt, mpg = mtcars$mpg, cyl = factor(mtcars$cyl))

test_that("guides(color = 'none') drops the colour legend", {
  p <- vplot(df) |>
    mark_point(x = wt, y = mpg, color = cyl) |>
    guides(color = "none")
  expect_length(
    vellumplot:::.legend_guides(vellumplot:::.build_panels(p)$scales),
    0L
  )
})

test_that("guide_none() hides a legend even when no scale is declared", {
  p <- vplot(df) |>
    mark_point(x = wt, y = mpg, color = cyl) |>
    guides(color = guide_none())
  expect_length(
    vellumplot:::.legend_guides(vellumplot:::.build_panels(p)$scales),
    0L
  )
  # the colour mapping still applies (marks are still coloured)
  b <- vellumplot:::.build_panels(p)
  expect_false(is.null(b$scales$color))
})

test_that("guide_legend(reverse=) reverses the key order only", {
  p <- vplot(df) |>
    mark_point(x = wt, y = mpg, color = cyl) |>
    guides(color = guide_legend(reverse = TRUE))
  b <- vellumplot:::.build_panels(p)
  expect_identical(b$scales$color$levels, rev(c("4", "6", "8")))
  # the data -> colour mapping is unchanged (still maps "4" to its trained colour)
  base <- vellumplot:::.build_panels(
    vplot(df) |> mark_point(x = wt, y = mpg, color = cyl)
  )
  expect_identical(b$scales$color$map("4"), base$scales$color$map("4"))
})

test_that("guide_legend(reverse=) flips a continuous colourbar (H31)", {
  dc <- data.frame(wt = mtcars$wt, mpg = mtcars$mpg, hp = mtcars$hp)
  p <- vplot(dc) |>
    mark_point(x = wt, y = mpg, color = hp) |>
    guides(color = guide_legend(reverse = TRUE))
  b <- vellumplot:::.build_panels(p)
  # the drawer flag is set; value<->label pairing is untouched (no array reverse)
  expect_true(isTRUE(b$scales$color$reverse_bar))
  # and the bar actually renders reversed (previously an invisible no-op)
  base <- vplot(dc) |> mark_point(x = wt, y = mpg, color = hp)
  expect_false(identical(
    vellum::scene_raster(p),
    vellum::scene_raster(base)
  ))
})

test_that("guide_legend(reverse=) on a binned colour scale keeps breaks aligned (H31)", {
  dc <- data.frame(wt = mtcars$wt, mpg = mtcars$mpg, hp = mtcars$hp)
  p <- vplot(dc) |>
    mark_point(x = wt, y = mpg, color = hp) |>
    scale_color_binned() |>
    guides(color = guide_legend(reverse = TRUE))
  sc <- vellumplot:::.build_panels(p)$scales$color
  # n colours/labels, n+1 boundaries: breaks are NOT reversed into a desync
  expect_equal(length(sc$breaks), length(sc$colors) + 1L)
  base <- vplot(dc) |>
    mark_point(x = wt, y = mpg, color = hp) |>
    scale_color_binned()
  bc <- vellumplot:::.build_panels(base)$scales$color
  expect_identical(sc$colors, rev(bc$colors)) # swatches reversed
  expect_identical(sc$breaks, bc$breaks) # boundaries unchanged
})

test_that("guide_legend(title=) overrides the legend title", {
  p <- vplot(df) |>
    mark_point(x = wt, y = mpg, color = cyl) |>
    guides(color = guide_legend(title = "Cylinders"))
  expect_identical(vellumplot:::.build_panels(p)$scales$color$name, "Cylinders")
})

test_that("guides() applies to size/shape/alpha/linetype and requires named args", {
  p <- vplot(df) |>
    mark_point(x = wt, y = mpg, size = mpg) |>
    guides(size = "none")
  expect_length(
    vellumplot:::.legend_guides(vellumplot:::.build_panels(p)$scales),
    0L
  )
  expect_error(
    vplot(df) |> mark_point(x = wt, y = mpg) |> guides("none"),
    "named"
  )
})

test_that("guided plots render", {
  f <- local_tempfile(fileext = ".png")
  p <- vplot(df) |>
    mark_point(x = wt, y = mpg, color = cyl) |>
    guides(color = guide_legend(reverse = TRUE))
  expect_no_error(render_plot(p, f))
})

test_that("guide_legend(override.aes=) forces key aesthetics without touching marks", {
  base <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, color = factor(cyl), alpha = 0.15, size = 0.6)
  p <- base |>
    guides(color = guide_legend(override.aes = list(size = 5, alpha = 1)))
  b <- vellumplot:::.build_panels(p)
  # the override rides the trained scale for the key drawers to read
  expect_equal(b$scales$color$override_aes, list(size = 5, alpha = 1))
  # the marks are unaffected: the data alpha/size are unchanged
  expect_null(
    base |> (\(x) vellumplot:::.build_panels(x)$scales$color$override_aes)()
  )
  # renders for colour, fill, and size legends
  expect_no_error(plot_svg(p))
  expect_no_error(plot_svg(
    vplot(mtcars) |>
      mark_boxplot(x = factor(cyl), y = mpg, fill = factor(cyl)) |>
      guides(fill = guide_legend(override.aes = list(alpha = 0.5)))
  ))
  expect_no_error(plot_svg(
    vplot(mtcars) |>
      mark_point(x = wt, y = mpg, size = hp) |>
      guides(size = guide_legend(override.aes = list(color = "steelblue")))
  ))
})

test_that("an override size grows the key cell so large keys do not overflow", {
  m <- list(key = 4)
  g_plain <- list(kind = "color_discrete", sc = list())
  g_big <- list(
    kind = "color_discrete",
    sc = list(override_aes = list(size = 5))
  )
  expect_equal(vellumplot:::.guide_key_d(g_plain, m), 4) # default
  expect_equal(vellumplot:::.guide_key_d(g_big, m), 10) # 2 * size
})

test_that("override.aes validates and canonicalises the British spelling", {
  expect_error(guide_legend(override.aes = list(5)), "named list")
  expect_error(guide_legend(override.aes = 5), "named list")
  g <- guide_legend(override.aes = list(colour = "red"))
  expect_identical(names(g[["override.aes"]]), "color")
})

test_that("override.aes round-trips through a spec", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, color = factor(cyl)) |>
    guides(color = guide_legend(override.aes = list(size = 5, alpha = 1)))
  q <- from_spec(as_spec(p))
  gd <- Filter(function(s) !is.null(s@guide), q@scales)[[1]]@guide
  expect_equal(gd[["override.aes"]], list(size = 5, alpha = 1))
})
