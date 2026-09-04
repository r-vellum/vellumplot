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

test_that("guide_colourbar() carries bar geometry + tick/label options", {
  base <- vplot(mtcars) |> mark_point(x = wt, y = mpg, color = hp)
  p <- base |>
    guides(
      color = guide_colourbar(
        barwidth = 8,
        barheight = 60,
        ticks = FALSE,
        ticks.colour = "black",
        label.position = "left"
      )
    )
  cl <- vellumplot:::.build_panels(p)$scales$color
  expect_equal(cl$bar_width, 8)
  expect_equal(cl$bar_height, 60)
  expect_false(cl$bar_ticks)
  expect_identical(cl$bar_ticks_colour, "black")
  expect_identical(cl$bar_label_pos, "left")
  expect_no_error(plot_svg(p))
  # a horizontal colour bar still renders (honours thickness + ticks)
  expect_no_error(plot_svg(
    base |>
      guides(color = guide_colourbar(barheight = 6, ticks = FALSE)) |>
      theme(legend.position = "bottom")
  ))
})

test_that("barwidth/barheight resize the colour-bar measurement", {
  base <- vplot(mtcars) |> mark_point(x = wt, y = mpg, color = hp)
  g_def <- list(kind = "color_continuous", sc = list(legend_labels = 1:5))
  g_big <- list(
    kind = "color_continuous",
    sc = list(legend_labels = 1:5, bar_width = 20, bar_height = 90)
  )
  m <- vellumplot:::.legend_metrics(vellumplot:::.resolve_theme(
    vellumplot:::.theme_of(base)
  ))
  # barheight fixes the bar length; barwidth widens the reserved column
  expect_equal(vellumplot:::.bar_len_mm(g_big, m), 90)
  expect_gt(
    vellumplot:::.guide_col_width(g_big, m),
    vellumplot:::.guide_col_width(g_def, m)
  )
})

test_that("guide_colourbar(n.breaks =) re-derives the bar's ticks", {
  base <- vplot(mtcars) |> mark_point(x = wt, y = mpg, color = hp)
  ticks <- function(p) vellumplot:::.build_panels(p)$scales$color$legend_breaks
  auto <- ticks(base)
  few <- ticks(base |> guides(color = guide_colourbar(n.breaks = 3)))
  many <- ticks(base |> guides(color = guide_colourbar(n.breaks = 10)))
  # A target, not a promise -- so assert the ordering, not the exact counts.
  expect_lt(length(few), length(auto))
  expect_gt(length(many), length(auto))
  # Re-derived, not thinned: a higher count is its own round-number set over the
  # range (60, 90, ...), not a superset of the automatic one -- which thinning
  # could never produce, and which is the point of asking `scales` again.
  expect_false(all(many %in% auto))
  # Every tick still lands inside the bar's range, whatever the count.
  rng <- vellumplot:::.build_panels(base)$scales$color$range
  for (t in list(few, many)) {
    expect_true(all(t >= rng[1] - 1e-9 & t <= rng[2] + 1e-9))
  }
  # The floor of two never yields a bar with no labels at all.
  expect_gt(
    length(ticks(base |> guides(color = guide_colourbar(n.breaks = 2)))),
    0L
  )
  # The labels follow the breaks; a stale pairing would show as a length mismatch.
  cl <- vellumplot:::.build_panels(
    base |> guides(color = guide_colourbar(n.breaks = 3))
  )$scales$color
  expect_length(cl$legend_labels, length(cl$legend_breaks))
  expect_no_error(plot_svg(
    base |>
      guides(
        color = guide_colourbar(n.breaks = 4)
      )
  ))
})

test_that("an explicit breaks= outranks n.breaks", {
  base <- vplot(mtcars) |> mark_point(x = wt, y = mpg, color = hp)
  ticks <- function(p) vellumplot:::.build_panels(p)$scales$color$legend_breaks
  b <- c(100, 200, 300)
  # Both spellings of "which values get a tick" win over a count: `breaks=` names
  # them, and `labels=` is paired index-wise with whatever `breaks=` resolved to.
  expect_equal(
    ticks(
      base |>
        scale_color_continuous(breaks = b) |>
        guides(color = guide_colourbar(n.breaks = 9))
    ),
    b
  )
  # `labels=` alone is paired index-wise with the automatic breaks, so it pins
  # them just as firmly: re-deriving would silently mislabel the bar. The pairing
  # happens BEFORE the off-range breaks are dropped, so the labels are one per
  # derived break, not one per drawn tick.
  rng <- vellumplot:::.build_panels(base)$scales$color$range
  lab <- paste0(scales::breaks_extended()(rng), "hp")
  auto <- ticks(base)
  cl <- vellumplot:::.build_panels(
    base |>
      scale_color_continuous(labels = lab) |>
      guides(color = guide_colourbar(n.breaks = 9))
  )$scales$color
  expect_equal(cl$legend_breaks, auto)
  expect_identical(cl$legend_labels, lab[lab %in% paste0(auto, "hp")])
})

test_that("n.breaks is ignored where a bar has no derived ticks", {
  base <- vplot(mtcars) |> mark_point(x = wt, y = mpg, color = hp)
  # A binned colour scale's ticks are its bin boundaries, not a derived break
  # set; asking for a count there is a no-op rather than an error.
  b <- base |> scale_color_binned(n = 4)
  expect_equal(
    vellumplot:::.build_panels(
      b |> guides(color = guide_colourbar(n.breaks = 9))
    )$scales$color$breaks,
    vellumplot:::.build_panels(b)$scales$color$breaks
  )
  expect_no_error(plot_svg(
    b |> guides(color = guide_coloursteps()) # takes no n.breaks at all
  ))
  expect_false("n.breaks" %in% names(formals(guide_coloursteps)))
})

test_that("n.breaks is validated", {
  expect_error(guide_colourbar(n.breaks = 1), "whole number")
  expect_error(guide_colourbar(n.breaks = 3.5), "whole number")
  expect_error(guide_colourbar(n.breaks = c(3, 4)), "whole number")
  expect_error(guide_colourbar(n.breaks = "many"), "whole number")
  expect_null(guide_colourbar()$n_breaks)
  expect_identical(guide_colourbar(n.breaks = 5)$n_breaks, 5L)
})

test_that("guide_colourbar() validates and round-trips", {
  expect_error(guide_colourbar(barwidth = -1), "positive")
  expect_error(guide_colourbar(barheight = c(1, 2)), "positive")
  expect_error(guide_colourbar(label.position = "sideways"), "label.position")
  expect_identical(guide_colorbar, guide_colourbar) # US alias
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, color = hp) |>
    guides(color = guide_colourbar(barwidth = 8, ticks = FALSE))
  gd <- Filter(function(s) !is.null(s@guide), from_spec(as_spec(p))@scales)[[
    1
  ]]@guide
  expect_identical(gd$kind, "colourbar")
  expect_equal(gd$bar_width, 8)
  expect_false(gd$ticks)
})

test_that("guide_coloursteps() renders a binned scale as a segmented bar", {
  b <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, color = hp) |>
    scale_color_binned()
  p <- b |> guides(color = guide_coloursteps(barheight = 50))
  cl <- vellumplot:::.build_panels(p)$scales$color
  expect_true(isTRUE(cl$stepped)) # flagged for the stepped drawer
  expect_equal(cl$bar_height, 50)
  # routed to the stepped-bar guide, not the discrete swatches
  guides_list <- vellumplot:::.legend_guides(
    vellumplot:::.build_panels(p)$scales
  )
  expect_identical(guides_list[[1]]$kind, "color_steps")
  # the bar labels the numeric bin boundaries (breaks), not the interval strings
  expect_equal(
    vellumplot:::.guide_labels(guides_list[[1]]),
    format(cl$breaks, trim = TRUE)
  )
  expect_no_error(plot_svg(p)) # vertical
  expect_no_error(plot_svg(p |> theme(legend.position = "bottom"))) # horizontal
  expect_identical(guide_colorsteps, guide_coloursteps) # US alias
})

test_that("guide_coloursteps() round-trips and is graceful off a binned scale", {
  b <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, color = hp) |>
    scale_color_binned()
  gd <- Filter(
    function(s) !is.null(s@guide),
    from_spec(as_spec(
      b |> guides(color = guide_coloursteps(ticks = TRUE))
    ))@scales
  )[[1]]@guide
  expect_identical(gd$kind, "coloursteps")
  expect_true(gd$ticks)
  # on a non-binned (continuous) scale it does not force a stepped bar -> no crash
  expect_no_error(plot_svg(
    vplot(mtcars) |>
      mark_point(x = wt, y = mpg, color = hp) |>
      guides(color = guide_coloursteps())
  ))
})

test_that("guide_legend(nested = TRUE) draws a size scale as concentric circles", {
  base <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, size = hp) |>
    scale_size(range = c(2, 12))
  p <- base |> guides(size = guide_legend(nested = TRUE))
  cl <- vellumplot:::.build_panels(p)$scales$size
  expect_true(isTRUE(cl$nested))
  # routed to the bubble drawer, not the stacked size keys
  gl <- vellumplot:::.legend_guides(vellumplot:::.build_panels(p)$scales)
  expect_identical(gl[[1]]$kind, "size_nested")
  expect_no_error(plot_svg(p)) # vertical (nested circles)
  expect_no_error(plot_svg(p |> theme(legend.position = "bottom"))) # horizontal (stacked fallback)
  # round-trips; nested is off by default
  gd <- Filter(function(s) !is.null(s@guide), from_spec(as_spec(p))@scales)[[
    1
  ]]@guide
  expect_true(isTRUE(gd$nested))
  expect_false(isTRUE(guide_legend()$nested))
})

test_that("guide fixes: coloursteps reverse, horizontal measurement, NA, label.position", {
  b <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, color = hp) |>
    scale_color_binned()
  # coloursteps(reverse=TRUE) flips colours AND breaks together (no desync)
  cl0 <- vellumplot:::.build_panels(b)$scales$color
  cl <- vellumplot:::.build_panels(
    b |> guides(color = guide_coloursteps(reverse = TRUE))
  )$scales$color
  expect_identical(cl$colors, rev(cl0$colors))
  expect_identical(cl$breaks, rev(cl0$breaks))
  # horizontal coloursteps + horizontal colourbar(barheight/barwidth) render
  expect_no_error(plot_svg(
    b |>
      guides(color = guide_coloursteps()) |>
      theme(legend.position = "bottom")
  ))
  cbar <- vplot(mtcars) |> mark_point(x = wt, y = mpg, color = hp)
  expect_no_error(plot_svg(
    cbar |>
      guides(color = guide_colourbar(barheight = 12, barwidth = 80)) |>
      theme(legend.position = "bottom")
  ))
  # a binned colour with NA renders under coloursteps (NA swatch), v + h
  d <- mtcars
  d$hp[c(1, 5, 10)] <- NA
  bn <- vplot(d) |>
    mark_point(x = wt, y = mpg, color = hp) |>
    scale_color_binned() |>
    guides(color = guide_coloursteps())
  expect_no_error(plot_svg(bn))
  expect_no_error(plot_svg(bn |> theme(legend.position = "bottom")))
  # label.position is right/left only (top/bottom never worked)
  expect_error(guide_colourbar(label.position = "top"), "right.*left|left")
  expect_no_error(guide_colourbar(label.position = "left"))
})

test_that("override.aes survives a merged colour+shape guide", {
  set.seed(1)
  d <- data.frame(x = rnorm(20), y = rnorm(20), g = rep(c("a", "b"), 10))
  p <- vplot(d) |>
    mark_point(x = x, y = y, color = g, shape = g) |>
    guides(color = guide_legend(override.aes = list(size = 6)))
  gl <- vellumplot:::.legend_guides(vellumplot:::.build_panels(p)$scales)
  expect_identical(gl[[1]]$kind, "merged")
  expect_equal(gl[[1]]$sc$override_aes, list(size = 6))
})
