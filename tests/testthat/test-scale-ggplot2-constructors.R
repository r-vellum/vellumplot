# The ggplot2-named scale constructors (VP-43): each trains the same breaks,
# values and colours as its ggplot2 namesake. The comparisons run only where
# ggplot2 is installed.

train <- function(p) .train_scales(p, .resolve_layers(p))

# Build a ggplot in an environment that cannot see vellumplot: ggplot2 looks
# its default scales up from the plot's environment, and would otherwise find
# vellumplot's `scale_x_continuous()` and friends. Variables of the calling
# test (and `mt`) are copied in. ggplot2 also resolves a scale's guide *name*
# from the search path (where vellumplot's `guide_legend()` masks its own), so
# every legend is replaced by a guide *object*: `guide_none()`, except for
# the aesthetics in `binned`, which get ggplot2's bins guide -- a binned
# scale only settles its "nice" limits when a guide asks for its breaks, so
# without one its values would not be those of the drawn plot.
gg <- function(expr, binned = character()) {
  env <- new.env(parent = baseenv())
  env$mt <- mt
  list2env(as.list(parent.frame()), env)
  aes_names <- c("colour", "fill", "shape", "size", "alpha", "linetype")
  guides <- lapply(aes_names, function(a) {
    if (a %in% binned) ggplot2::guide_bins() else ggplot2::guide_none()
  })
  names(guides) <- aes_names
  eval(substitute(expr), env) + do.call(ggplot2::guides, guides)
}

gg_data <- function(p) ggplot2::layer_data(p)

# The largest perceptual difference (CIEDE2000) between two colour vectors.
# Ramps are blended in Oklab here and in Lab by ggplot2, and the viridis maps
# come from grDevices::hcl.colors() rather than viridisLite, so colours from a
# ramp match closely rather than exactly.
max_colour_diff <- function(a, b) {
  da <- farver::decode_colour(a)
  db <- farver::decode_colour(b)
  max(vapply(
    seq_len(nrow(da)),
    function(i) {
      farver::compare_colour(
        da[i, , drop = FALSE],
        db[i, , drop = FALSE],
        "rgb",
        method = "cie2000"
      )[1, 1]
    },
    numeric(1)
  ))
}

hex <- function(x) toupper(substr(x, 1, 7))

mt <- transform(mtcars, fcyl = factor(cyl), fam = factor(am))

# --- discrete manual scales --------------------------------------------------

test_that("scale_shape_manual() matches ggplot2, numeric codes included", {
  skip_if_not_installed("ggplot2")
  vals <- c("4" = 16, "6" = 17, "8" = 15)
  p <- vplot(mt) |>
    mark_point(x = wt, y = mpg, shape = fcyl) |>
    scale_shape_manual(values = vals, labels = c("four", "six", "eight"))
  sc <- train(p)$shape
  g <- gg_data(gg(
    ggplot2::ggplot(mt, ggplot2::aes(wt, mpg, shape = fcyl)) +
      ggplot2::geom_point() +
      ggplot2::scale_shape_manual(values = vals)
  ))
  expect_identical(sc$map(mt$fcyl), .pch_to_shape(g$shape))
  expect_identical(sc$shapes, c("circle", "triangle", "square"))
  expect_identical(.guide_labels(list(kind = "shape", sc = sc)), c("four", "six", "eight"))
  expect_no_error(vellum::as_vellum_scene(p))
})

test_that("scale_shape_manual() honours breaks, limits and na.value", {
  p <- vplot(mt) |>
    mark_point(x = wt, y = mpg, shape = fcyl) |>
    scale_shape_manual(
      values = c("square", "triangle"),
      limits = c("4", "6"),
      breaks = "6",
      na.value = "cross"
    )
  sc <- train(p)$shape
  expect_identical(sc$levels, "6")
  expect_identical(sc$map(c("4", "6", "8")), c("square", "triangle", "cross"))
  expect_error(
    vplot(mt) |> scale_shape_manual(values = "blob"),
    "Unknown shape"
  )
})

test_that("scale_linetype_manual() matches ggplot2", {
  skip_if_not_installed("ggplot2")
  p <- vplot(mt) |>
    mark_line(x = wt, y = mpg, linetype = fam) |>
    scale_linetype_manual(values = c("1" = "dotted", "0" = "solid"))
  sc <- train(p)$linetype
  g <- gg_data(gg(
    ggplot2::ggplot(mt, ggplot2::aes(wt, mpg, linetype = fam)) +
      ggplot2::geom_point() +
      ggplot2::scale_linetype_manual(values = c("1" = "dotted", "0" = "solid"))
  ))
  expect_identical(sc$map(mt$fam), g$linetype)
  # numeric line types, as ggplot2 takes them
  p2 <- vplot(mt) |>
    mark_line(x = wt, y = mpg, linetype = fam) |>
    scale_linetype_manual(values = c(2, 3))
  expect_identical(train(p2)$linetype$linetypes, c("dashed", "dotted"))
  expect_no_error(vellum::as_vellum_scene(p))
})

test_that("scale_size_manual() matches ggplot2 and does not warn", {
  skip_if_not_installed("ggplot2")
  p <- vplot(mt) |>
    mark_point(x = wt, y = mpg, size = fcyl) |>
    scale_size_manual(values = c(2, 4, 8))
  expect_no_warning(sc <- train(p)$size)
  g <- gg_data(gg(
    ggplot2::ggplot(mt, ggplot2::aes(wt, mpg, size = fcyl)) +
      ggplot2::geom_point() +
      ggplot2::scale_size_manual(values = c(2, 4, 8))
  ))
  expect_equal(sc$map(mt$fcyl), g$size)
  expect_equal(sc$legend_sizes, c(2, 4, 8))
  expect_error(
    vplot(mt) |> scale_size_manual(values = c("a", "b")),
    "must be numeric"
  )
})

test_that("scale_alpha_manual() matches ggplot2", {
  skip_if_not_installed("ggplot2")
  p <- vplot(mt) |>
    mark_point(x = wt, y = mpg, alpha = fcyl) |>
    scale_alpha_manual(values = c("8" = 1, "6" = 0.5, "4" = 0.2))
  sc <- train(p)$alpha
  g <- gg_data(gg(
    ggplot2::ggplot(mt, ggplot2::aes(wt, mpg, alpha = fcyl)) +
      ggplot2::geom_point() +
      ggplot2::scale_alpha_manual(values = c("8" = 1, "6" = 0.5, "4" = 0.2))
  ))
  expect_equal(sc$map(mt$fcyl), g$alpha)
  expect_error(
    vplot(mt) |> scale_alpha_manual(values = c(0.5, 2)),
    "must lie in"
  )
})

test_that("scale_alpha_ordinal() / scale_size_ordinal() match ggplot2", {
  skip_if_not_installed("ggplot2")
  p <- vplot(mt) |>
    mark_point(x = wt, y = mpg, alpha = fcyl, size = fcyl) |>
    scale_alpha_ordinal(range = c(0.1, 1)) |>
    scale_size_ordinal(range = c(2, 6))
  expect_no_warning(sc <- train(p))
  g <- gg_data(gg(
    ggplot2::ggplot(mt, ggplot2::aes(wt, mpg, alpha = fcyl, size = fcyl)) +
      ggplot2::geom_point() +
      ggplot2::scale_alpha_ordinal(range = c(0.1, 1)) +
      ggplot2::scale_size_ordinal(range = c(2, 6))
  ))
  expect_equal(sc$alpha$map(mt$fcyl), g$alpha)
  expect_equal(sc$size$map(mt$fcyl), g$size)
})

test_that("scale_alpha_discrete() / scale_size_discrete() warn like ggplot2", {
  p <- vplot(mt) |>
    mark_point(x = wt, y = mpg, alpha = fcyl) |>
    scale_alpha_discrete()
  expect_warning(sc <- train(p)$alpha, class = "vellumplot_discrete_sized_aes")
  expect_equal(sc$legend_alphas, c(0.1, 0.55, 1))
  p2 <- vplot(mt) |>
    mark_point(x = wt, y = mpg, size = fcyl) |>
    scale_size_discrete()
  expect_warning(train(p2), class = "vellumplot_discrete_sized_aes")
})

test_that("scale_color_manual() takes breaks, limits and na.value", {
  p <- vplot(mt) |>
    mark_point(x = wt, y = mpg, color = fcyl) |>
    scale_color_manual(
      values = c("red", "blue"),
      limits = c("4", "6"),
      breaks = "6",
      labels = function(x) paste("cyl", x),
      na.value = "black"
    )
  sc <- train(p)$color
  expect_identical(sc$levels, "6")
  expect_identical(sc$labels, "cyl 6")
  expect_identical(sc$map(c("4", "6", "8")), c("red", "blue", "black"))
})

# --- hue / grey / ordinal ----------------------------------------------------

test_that("scale_color_hue() / scale_fill_hue() match ggplot2", {
  skip_if_not_installed("ggplot2")
  p <- vplot(mt) |>
    mark_point(x = wt, y = mpg, color = fcyl) |>
    scale_color_hue(l = 45, direction = -1)
  g <- gg_data(gg(
    ggplot2::ggplot(mt, ggplot2::aes(wt, mpg, colour = fcyl)) +
      ggplot2::geom_point() +
      ggplot2::scale_colour_hue(l = 45, direction = -1)
  ))
  expect_identical(hex(train(p)$color$map(mt$fcyl)), hex(g$colour))
  pf <- vplot(mt) |>
    mark_point(x = wt, y = mpg, fill = fcyl) |>
    scale_fill_hue()
  expect_identical(train(pf)$fill$colors, scales::pal_hue()(3))
  expect_identical(
    train(vplot(mt) |> mark_point(x = wt, y = mpg, color = fcyl) |> scale_colour_hue())$color$colors,
    scales::pal_hue()(3)
  )
  expect_no_error(vellum::as_vellum_scene(p))
})

test_that("scale_color_grey() / scale_fill_grey() match ggplot2", {
  skip_if_not_installed("ggplot2")
  p <- vplot(mt) |>
    mark_point(x = wt, y = mpg, fill = fcyl) |>
    scale_fill_grey(start = 0.1, end = 0.9)
  g <- gg_data(gg(
    ggplot2::ggplot(mt, ggplot2::aes(wt, mpg, fill = fcyl)) +
      ggplot2::geom_point() +
      ggplot2::scale_fill_grey(start = 0.1, end = 0.9)
  ))
  expect_identical(hex(train(p)$fill$map(mt$fcyl)), hex(g$fill))
  pc <- vplot(mt) |>
    mark_point(x = wt, y = mpg, color = fcyl) |>
    scale_color_grey()
  expect_identical(train(pc)$color$colors, scales::pal_grey()(3))
  expect_identical(train(pc)$color$na_value, "red")
  expect_error(vplot(mt) |> scale_color_grey(start = 2), "in \\[0, 1\\]")
})

test_that("scale_color_ordinal() / scale_fill_ordinal() match ggplot2", {
  skip_if_not_installed("ggplot2")
  of <- factor(mt$cyl, ordered = TRUE)
  d <- transform(mt, ocyl = of)
  p <- vplot(d) |>
    mark_point(x = wt, y = mpg, color = ocyl) |>
    scale_color_ordinal()
  g <- gg_data(gg(
    ggplot2::ggplot(d, ggplot2::aes(wt, mpg, colour = ocyl)) +
      ggplot2::geom_point() +
      ggplot2::scale_colour_ordinal()
  ))
  expect_lt(max_colour_diff(train(p)$color$map(d$ocyl), g$colour), 10)
  pf <- vplot(d) |>
    mark_point(x = wt, y = mpg, fill = ocyl) |>
    scale_fill_ordinal()
  expect_identical(train(pf)$fill$colors, train(p)$color$colors)
})

# --- binned colour scales -----------------------------------------------------

gg_binned <- function(aes_name, scale) {
  p <- gg(
    ggplot2::ggplot(mt, ggplot2::aes(wt, mpg, colour = hp, fill = hp)) +
      ggplot2::geom_point() +
      scale,
    binned = c("colour", "fill")
  )
  b <- ggplot2::ggplot_build(p)
  sc <- b$plot$scales$get_scales(aes_name)
  list(
    data = ggplot2::layer_data(p),
    breaks = sort(unique(c(sc$get_limits(), sc$get_breaks())))
  )
}

test_that("scale_color_steps2() / scale_fill_steps2() match ggplot2", {
  skip_if_not_installed("ggplot2")
  p <- vplot(mt) |>
    mark_point(x = wt, y = mpg, color = hp) |>
    scale_color_steps2(midpoint = 150)
  sc <- train(p)$color
  g <- gg_binned("colour", ggplot2::scale_colour_steps2(midpoint = 150))
  expect_equal(sc$breaks, g$breaks)
  expect_lt(max_colour_diff(sc$map(mt$hp), g$data$colour), 6)
  pf <- vplot(mt) |>
    mark_point(x = wt, y = mpg, fill = hp) |>
    scale_fill_steps2(midpoint = 150, n.breaks = 3)
  gf <- gg_binned("fill", ggplot2::scale_fill_steps2(midpoint = 150, n.breaks = 3))
  expect_equal(train(pf)$fill$breaks, gf$breaks)
  expect_no_error(vellum::as_vellum_scene(p))
})

test_that("scale_color_stepsn() / scale_fill_stepsn() match ggplot2", {
  skip_if_not_installed("ggplot2")
  cols <- c("#2166AC", "#F7F7F7", "#B2182B")
  p <- vplot(mt) |>
    mark_point(x = wt, y = mpg, fill = hp) |>
    scale_fill_stepsn(colours = cols, limits = c(0, 400), breaks = c(100, 200, 300))
  sc <- train(p)$fill
  g <- gg_binned(
    "fill",
    ggplot2::scale_fill_stepsn(colours = cols, limits = c(0, 400), breaks = c(100, 200, 300))
  )
  expect_equal(sc$breaks, c(0, 100, 200, 300, 400))
  expect_equal(sc$breaks, g$breaks)
  expect_lt(max_colour_diff(sc$map(mt$hp), g$data$fill), 6)
  pc <- vplot(mt) |>
    mark_point(x = wt, y = mpg, color = hp) |>
    scale_colour_stepsn(colours = cols)
  expect_equal(train(pc)$color$breaks, gg_binned("colour", ggplot2::scale_colour_stepsn(colours = cols))$breaks)
})

test_that("scale_color_fermenter() / scale_fill_fermenter() match ggplot2", {
  skip_if_not_installed("ggplot2")
  p <- vplot(mt) |>
    mark_point(x = wt, y = mpg, color = hp) |>
    scale_color_fermenter(palette = "Greens")
  sc <- train(p)$color
  g <- gg_binned("colour", ggplot2::scale_colour_fermenter(palette = "Greens"))
  expect_equal(sc$breaks, g$breaks)
  expect_identical(hex(sc$map(mt$hp)), hex(g$data$colour))
  pf <- vplot(mt) |>
    mark_point(x = wt, y = mpg, fill = hp) |>
    scale_fill_fermenter(palette = 2, type = "div", direction = 1)
  gf <- gg_binned("fill", ggplot2::scale_fill_fermenter(palette = 2, type = "div", direction = 1))
  expect_identical(hex(train(pf)$fill$map(mt$hp)), hex(gf$data$fill))
})

test_that("scale_color_viridis_b() / scale_fill_viridis_b() match ggplot2", {
  skip_if_not_installed("ggplot2")
  p <- vplot(mt) |>
    mark_point(x = wt, y = mpg, color = hp) |>
    scale_color_viridis_b()
  sc <- train(p)$color
  g <- gg_binned("colour", ggplot2::scale_colour_viridis_b())
  expect_equal(sc$breaks, g$breaks)
  expect_lt(max_colour_diff(sc$map(mt$hp), g$data$colour), 10)
  pf <- vplot(mt) |>
    mark_point(x = wt, y = mpg, fill = hp) |>
    scale_fill_viridis_b(direction = -1)
  gf <- gg_binned("fill", ggplot2::scale_fill_viridis_b(direction = -1))
  expect_lt(max_colour_diff(train(pf)$fill$map(mt$hp), gf$data$fill), 10)
})

test_that("the ggplot2-named binned scales label right-closed classes", {
  p <- vplot(mt) |>
    mark_point(x = wt, y = mpg, color = hp) |>
    scale_color_steps2(breaks = c(100, 200))
  sc <- train(p)$color
  expect_identical(sc$labels, c("[52, 100]", "(100, 200]", "(200, 335]"))
  # a value on a boundary falls in the class below, as ggplot2's cut(right = TRUE)
  expect_identical(sc$map(100), sc$map(60))
})

# --- size scales ----------------------------------------------------------------

test_that("scale_size_binned() matches ggplot2", {
  skip_if_not_installed("ggplot2")
  p <- vplot(mt) |>
    mark_point(x = wt, y = mpg, size = hp) |>
    scale_size_binned(range = c(1, 6))
  sc <- train(p)$size
  g <- gg_data(gg(
    ggplot2::ggplot(mt, ggplot2::aes(wt, mpg, size = hp)) +
      ggplot2::geom_point() +
      ggplot2::scale_size_binned(range = c(1, 6)),
    binned = "size"
  ))
  expect_equal(sc$map(mt$hp), g$size)
  expect_length(sc$legend_sizes, 6L)
  expect_no_error(vellum::as_vellum_scene(p))
})

test_that("scale_radius() matches ggplot2", {
  skip_if_not_installed("ggplot2")
  p <- vplot(mt) |>
    mark_point(x = wt, y = mpg, size = hp) |>
    scale_radius()
  g <- gg_data(gg(
    ggplot2::ggplot(mt, ggplot2::aes(wt, mpg, size = hp)) +
      ggplot2::geom_point() +
      ggplot2::scale_radius()
  ))
  expect_equal(train(p)$size$map(mt$hp), g$size)
})

# --- position transform shortcuts ----------------------------------------------

test_that("scale_x_log10() / scale_y_log10() match ggplot2", {
  skip_if_not_installed("ggplot2")
  p <- vplot(mt) |> mark_point(x = hp, y = disp) |> scale_x_log10() |> scale_y_log10()
  sc <- train(p)
  g <- gg_data(gg(
    ggplot2::ggplot(mt, ggplot2::aes(hp, disp)) +
      ggplot2::geom_point() +
      ggplot2::scale_x_log10() +
      ggplot2::scale_y_log10()
  ))
  expect_equal(sc$x$map(mt$hp), g$x)
  expect_equal(sc$y$map(mt$disp), g$y)
})

test_that("scale_x_sqrt() / scale_y_sqrt() match ggplot2", {
  skip_if_not_installed("ggplot2")
  p <- vplot(mt) |> mark_point(x = hp, y = disp) |> scale_x_sqrt() |> scale_y_sqrt()
  sc <- train(p)
  g <- gg_data(gg(
    ggplot2::ggplot(mt, ggplot2::aes(hp, disp)) +
      ggplot2::geom_point() +
      ggplot2::scale_x_sqrt() +
      ggplot2::scale_y_sqrt()
  ))
  expect_equal(sc$x$map(mt$hp), g$x)
  expect_equal(sc$y$map(mt$disp), g$y)
})

test_that("scale_x_reverse() / scale_y_reverse() flip the axis like ggplot2", {
  skip_if_not_installed("ggplot2")
  p <- vplot(mt) |>
    mark_point(x = hp, y = disp) |>
    scale_x_reverse(name = "HP") |>
    scale_y_reverse()
  sc <- train(p)
  b <- ggplot2::ggplot_build(gg(
    ggplot2::ggplot(mt, ggplot2::aes(hp, disp)) +
      ggplot2::geom_point() +
      ggplot2::scale_x_reverse() +
      ggplot2::scale_y_reverse()
  ))
  gx <- b$layout$panel_params[[1]]$x$continuous_range
  # ggplot2 negates the data; vellumplot keeps it and flips the domain
  expect_equal(sc$x$domain, -gx)
  expect_gt(sc$y$domain[1], sc$y$domain[2])
  expect_identical(sc$x$name, "HP")
  expect_identical(
    train(vplot(mt) |> mark_point(x = hp, y = disp) |> scale_x_continuous(trans = "reverse"))$x$domain,
    sc$x$domain
  )
})

# --- the older binned scales, aligned with ggplot2 (VP-49) ---------------------

test_that("scale_*_steps() matches ggplot2's breaks and colours", {
  skip_if_not_installed("ggplot2")
  p <- vplot(mt) |>
    mark_point(x = wt, y = mpg, color = hp) |>
    scale_color_steps()
  sc <- train(p)$color
  g <- gg_binned("colour", ggplot2::scale_colour_steps())
  expect_equal(sc$breaks, g$breaks)
  expect_identical(hex(sc$map(mt$hp)), hex(g$data$colour))
  pf <- vplot(mt) |>
    mark_point(x = wt, y = mpg, fill = hp) |>
    scale_fill_steps(low = "white", high = "darkred", n.breaks = 3)
  gf <- gg_binned(
    "fill",
    ggplot2::scale_fill_steps(low = "white", high = "darkred", n.breaks = 3)
  )
  expect_equal(train(pf)$fill$breaks, gf$breaks)
  expect_lt(max_colour_diff(train(pf)$fill$map(mt$hp), gf$data$fill), 10)
  expect_identical(scale_colour_steps, scale_color_steps)
})

test_that("scale_*_binned() bins like ggplot2 by default", {
  skip_if_not_installed("ggplot2")
  p <- vplot(mt) |>
    mark_point(x = wt, y = mpg, color = hp) |>
    scale_color_binned(palette = c("#132B43", "#56B1F7"))
  sc <- train(p)$color
  g <- gg_binned("colour", ggplot2::scale_colour_binned())
  expect_equal(sc$breaks, g$breaks)
  expect_identical(hex(sc$map(mt$hp)), hex(g$data$colour))
  expect_identical(sc$labels[1:2], c("[50, 100]", "(100, 150]"))
  # the classic classification styles are still there
  q <- train(
    vplot(mt) |>
      mark_point(x = wt, y = mpg, fill = hp) |>
      scale_fill_binned(style = "quantile", n = 4)
  )$fill
  expect_equal(q$breaks, unname(stats::quantile(mt$hp, 0:4 / 4)))
  expect_match(q$labels[1], "^\\[")
})

test_that("scale_*_stepsn() takes the colours' positions in values", {
  skip_if_not_installed("ggplot2")
  cols <- c("#2166AC", "#F7F7F7", "#B2182B")
  p <- vplot(mt) |>
    mark_point(x = wt, y = mpg, color = hp) |>
    scale_color_stepsn(colours = cols, values = c(0, 0.3, 1))
  sc <- train(p)$color
  g <- gg_binned(
    "colour",
    ggplot2::scale_colour_stepsn(colours = cols, values = c(0, 0.3, 1))
  )
  expect_equal(sc$breaks, g$breaks)
  expect_lt(max_colour_diff(sc$map(mt$hp), g$data$colour), 6)
  even <- train(
    vplot(mt) |>
      mark_point(x = wt, y = mpg, fill = hp) |>
      scale_fill_stepsn(colours = cols)
  )$fill
  expect_false(identical(even$colors, sc$colors))
  expect_error(
    vplot(mt) |> scale_fill_stepsn(colours = cols, values = c(0, 1)),
    "values"
  )
  expect_error(
    vplot(mt) |> scale_color_stepsn(colours = cols, values = c(0, 0.8, 0.5)),
    "values"
  )
})

test_that("manual size and alpha scales refuse a numeric variable", {
  p <- vplot(mt) |>
    mark_point(x = wt, y = mpg, size = cyl) |>
    scale_size_manual(values = c(1, 2, 3))
  expect_error(train(p), "Continuous values supplied to a discrete")
  pa <- vplot(mt) |>
    mark_point(x = wt, y = mpg, alpha = cyl) |>
    scale_alpha_manual(values = c(0.2, 0.5, 1))
  expect_error(train(pa), "Continuous values supplied to a discrete")
  if (rlang::is_installed("ggplot2")) {
    expect_error(
      ggplot2::layer_data(gg(
        ggplot2::ggplot(mt, ggplot2::aes(wt, mpg, size = cyl)) +
          ggplot2::geom_point() +
          ggplot2::scale_size_manual(values = c(1, 2, 3))
      )),
      "Continuous value"
    )
  }
  # a factor is what they are for
  ok <- vplot(mt) |>
    mark_point(x = wt, y = mpg, size = fcyl) |>
    scale_size_manual(values = c(1, 2, 3))
  expect_identical(train(ok)$size$legend_sizes, c(1, 2, 3))
})
