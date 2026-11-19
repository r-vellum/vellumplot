# ggplot2 parity for scales and legends (VP-11, F-10).

panels <- function(p) .build_panels(p)

# --- VP-11 ---------------------------------------------------------------

test_that("coord_cartesian(xlim) zooms a discrete axis in level positions", {
  d <- data.frame(g = letters[1:5], v = 1:5)
  p <- vplot(d) |>
    mark_bar(x = g, y = v, stat = "identity") |>
    coord_cartesian(xlim = c(2, 4))
  x <- panels(p)$scales$x
  expect_equal(x$domain, c(1.5, 4.5))
  expect_identical(x$labels, c("b", "c", "d"))
  expect_identical(x$levels, letters[1:5])
  expect_no_error(vellum::as_vellum_scene(p))
})

test_that("a discrete colour scale on a numeric variable errors", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, color = hp) |>
    scale_color_manual(values = c("red", "blue"))
  expect_error(panels(p), "Continuous values supplied to a discrete")
  p2 <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, color = factor(cyl)) |>
    scale_color_manual(values = c("red", "blue", "green"))
  expect_no_error(panels(p2))
})

test_that("infinite positions sit on the panel edge", {
  d <- data.frame(x = c(1, 2, 3, Inf, -Inf), y = c(1, 2, 3, 2, Inf))
  sc <- panels(vplot(d) |> mark_point(x = x, y = y))$scales
  dom <- sc$x$domain
  expect_equal(sc$x$map(c(Inf, -Inf)), c(max(dom), min(dom)))
  expect_equal(sc$y$map(Inf), max(sc$y$domain))
  # the data range ignores them
  expect_equal(sc$x$data_range, c(1, 3))
  # a reversed axis mirrors them: Inf is on the left, where the (descending)
  # domain starts, as under ggplot2's scale_x_reverse()
  rv <- panels(
    vplot(d) |>
      mark_point(x = x, y = y) |>
      scale_x_continuous(trans = "reverse")
  )$scales$x
  expect_equal(rv$map(Inf), rv$domain[1])
  # a value a transform cannot show (0 on log10) is still dropped, not moved
  lg <- panels(
    vplot(data.frame(x = c(0, 1, 10), y = 1:3)) |>
      mark_point(x = x, y = y) |>
      scale_x_continuous(trans = "log10")
  )$scales$x
  expect_false(is.finite(lg$map(0)))
})

test_that("an all-missing continuous aesthetic draws no legend", {
  d <- data.frame(x = 1:3, y = 1:3, z = NA_real_)
  b <- panels(vplot(d) |> mark_point(x = x, y = y, color = z, size = z))
  expect_true(b$scales$color$no_guide)
  expect_true(b$scales$size$no_guide)
  expect_length(.legend_guides(b$scales), 0L)
  # with limits there is something to show
  b2 <- panels(
    vplot(d) |>
      mark_point(x = x, y = y, color = z) |>
      scale_color_continuous(limits = c(0, 10))
  )
  expect_length(.legend_guides(b2$scales), 1L)
})

# --- F-10: discrete position scales --------------------------------------

test_that("scale_x_discrete() takes breaks, labels and drop", {
  d <- data.frame(
    g = factor(c("a", "b", "c"), levels = c("a", "b", "c", "z")),
    v = 1:3
  )
  base <- vplot(d) |> mark_point(x = g, y = v)
  # unused levels drop by default, as in ggplot2
  expect_identical(panels(base)$scales$x$levels, c("a", "b", "c"))
  kept <- panels(base |> scale_x_discrete(drop = FALSE))$scales$x
  expect_identical(kept$levels, c("a", "b", "c", "z"))
  # breaks pick the ticks
  br <- panels(base |> scale_x_discrete(breaks = c("a", "c")))$scales$x
  expect_equal(br$breaks, c(1, 3))
  expect_identical(br$labels, c("a", "c"))
  # labels: a vector, a named vector, or a function
  lab <- panels(base |> scale_x_discrete(labels = c("A", "B", "C")))$scales$x
  expect_identical(lab$labels, c("A", "B", "C"))
  nm <- panels(base |> scale_x_discrete(labels = c(b = "Bee")))$scales$x
  expect_identical(nm$labels, c("a", "Bee", "c"))
  fn <- panels(base |> scale_x_discrete(labels = toupper))$scales$x
  expect_identical(fn$labels, c("A", "B", "C"))
  both <- panels(
    base |> scale_x_discrete(breaks = c("a", "c"), labels = c("first", "last"))
  )$scales$x
  expect_identical(both$labels, c("first", "last"))
  expect_error(
    panels(base |> scale_x_discrete(labels = c("A", "B"))),
    "one entry per break"
  )
  expect_error(scale_y_discrete(base, drop = NA), "drop")
})

# --- F-10: oob ------------------------------------------------------------

test_that("oob censors, squishes or keeps out-of-limits positions", {
  base <- vplot(data.frame(x = c(1, 5, 10), y = 1:3)) |>
    mark_point(x = x, y = y)
  map_x <- function(oob) {
    panels(base |> scale_x_continuous(limits = c(2, 8), oob = oob))$scales$x$map
  }
  expect_equal(map_x("censor")(c(1, 5, 10)), c(NA, 5, NA))
  expect_equal(map_x("squish")(c(1, 5, 10)), c(2, 5, 8))
  expect_equal(map_x("keep")(c(1, 5, 10)), c(1, 5, 10))
  expect_equal(map_x(scales::oob_squish)(c(1, 5, 10)), c(2, 5, 8))
  # the default keeps them (drawn, clipped by the panel)
  expect_equal(
    panels(base |> scale_x_continuous(limits = c(2, 8)))$scales$x$map(1),
    1
  )
  # Inf is never out of bounds: it still goes to the edge
  expect_equal(
    map_x("censor")(Inf),
    max(panels(base |> scale_x_continuous(limits = c(2, 8)))$scales$x$domain)
  )
  expect_error(scale_x_continuous(base, oob = "clip"), "oob")
  censored <- base |> scale_x_continuous(limits = c(2, 8), oob = "censor")
  expect_no_error(vellum::as_vellum_scene(censored))
})

test_that("oob on colour and size scales", {
  d <- data.frame(x = 1:3, y = 1:3, v = c(0, 5, 10))
  col <- function(oob) {
    panels(
      vplot(d) |>
        mark_point(x = x, y = y, color = v) |>
        scale_color_continuous(
          limits = c(2, 8),
          oob = oob,
          palette = c("black", "white")
        )
    )$scales$color
  }
  cc <- col("censor")
  expect_identical(cc$map(0), cc$na_value)
  sq <- col("squish")
  expect_identical(sq$map(0), sq$map(2))
  sz <- function(oob) {
    panels(
      vplot(d) |>
        mark_point(x = x, y = y, size = v) |>
        scale_size(limits = c(2, 8), range = c(1, 4), oob = oob)
    )$scales$size
  }
  expect_true(is.na(sz("censor")$map(0)))
  expect_equal(sz("squish")$map(10), 4)
  expect_equal(sz(NULL)$map(10), 5) # extrapolated, as before
})

test_that("oob round-trips through the spec serializer", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    scale_x_continuous(limits = c(2, 4), oob = scales::oob_squish) |>
    scale_y_discrete(drop = FALSE)
  s <- p@scales[[1]]
  expect_identical(s@oob, "squish")
  expect_identical(.scale_from_ir(.scale_to_ir(s))@oob, "squish")
})

# --- F-10: inside legends -------------------------------------------------

test_that("legend.position = 'inside' / c(x, y) places the legend over the panels", {
  base <- vplot(mtcars) |> mark_point(x = wt, y = mpg, color = factor(cyl))
  rt <- function(p) .resolve_theme(p@theme)
  inside <- base |>
    theme(legend.position = "inside", legend.justification = c(1, 1))
  expect_equal(.legend_inside(rt(inside)), list(pos = c(1, 1), just = c(1, 1)))
  old <- base |> theme(legend.position = c(0.1, 0.9))
  expect_equal(
    .legend_inside(rt(old)),
    list(pos = c(0.1, 0.9), just = c(0.1, 0.9))
  )
  words <- base |>
    theme(
      legend.position = "inside",
      legend.position.inside = c(0.5, 0.5),
      legend.justification = c("left", "top")
    )
  expect_equal(
    .legend_inside(rt(words)),
    list(pos = c(0.5, 0.5), just = c(0, 1))
  )
  expect_null(.legend_inside(rt(base)))
  # the legend takes no track: the panel is as wide as with no legend
  svg <- plot_svg(inside)
  expect_match(svg, "legend-inside", fixed = TRUE)
  expect_error(theme(base, legend.position = c(2, NA)), "legend.position")
  expect_error(theme(base, legend.justification = "middle"), "justification")
})
