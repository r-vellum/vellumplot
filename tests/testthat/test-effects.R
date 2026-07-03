# Layer effects (glow), gradient fill paints, and theme_cyberpunk.

train <- function(p) {
  quill:::.train_scales(p, quill:::.resolve_layers(p))
}

line_df <- data.frame(x = 1:20, y = cumsum(c(0, rep(1, 19))))

# --- glow() constructor -----------------------------------------------------

test_that("glow() builds a GlowSpec with defaults and validates arguments", {
  g <- glow()
  expect_true(S7::S7_inherits(g, quill:::GlowSpec))
  expect_identical(g@layers, 6L)
  expect_identical(g@blend, "screen")
  expect_null(g@color)

  expect_error(glow(size = 0), "positive")
  expect_error(glow(size = -2), "positive")
  expect_error(glow(layers = 0), "positive integer")
  expect_error(glow(alpha = 1.5), "\\[0, 1\\]")
  expect_error(glow(color = c("a", "b")), "single colour")
  expect_error(glow(blend = "glow"), "blend")
})

# --- attaching effects to layers --------------------------------------------

test_that("effects ride the layer for stroked / point marks", {
  p <- vplot(line_df) |>
    mark_line(x = x, y = y, effects = list(glow(size = 5, layers = 8)))
  e <- p@layers[[1]]@effects[[1]]
  expect_true(S7::S7_inherits(e, quill:::GlowSpec))
  expect_identical(e@layers, 8L)
})

test_that("glow on an unsupported mark errors", {
  # via the reserved-argument guard (mark_bar has no effects arg)
  expect_error(
    vplot(line_df) |> mark_bar(x = x, y = y, effects = list(glow())),
    "not an aesthetic"
  )
  # and directly, via the mark validation
  expect_error(
    quill:::.check_effects(list(glow()), "bar"),
    "stroked and point"
  )
})

test_that("non-effect entries in effects error", {
  expect_error(
    vplot(line_df) |> mark_line(x = x, y = y, effects = list("nope")),
    "effect constructor"
  )
})

test_that("a glowing line renders a halo (more lit pixels than the plain line)", {
  mk <- function(fx) {
    vplot(line_df) |>
      mark_line(x = x, y = y, color = "#08F7FE", effects = fx) |>
      theme_cyberpunk()
  }
  lit <- function(p) sum(render_px(p)[,, 2] > 0.25) # green channel = cyan halo
  expect_gt(lit(mk(list(glow()))), lit(mk(list())))
})

# --- gradient fill paints ---------------------------------------------------

test_that("linear_gradient() is captured as a fill value, not a data channel", {
  g <- linear_gradient(c("#08F7FE", "#08F7FE00"))
  expect_true(inherits(g, "vellum_gradient"))

  p <- vplot(line_df) |> mark_area(x = x, y = y, fill = g)
  expect_true(inherits(p@layers[[1]]@params$fill, "vellum_gradient"))
  expect_null(p@layers[[1]]@encoding$fill)
  expect_no_error(render_px(p))
})

test_that("gradient fill works on area, ribbon, and bar", {
  g <- linear_gradient(
    c("#FE53BB00", "#FE53BB"),
    x1 = 0,
    y1 = 0,
    x2 = 0,
    y2 = 1
  )
  bars <- data.frame(cat = c("a", "b", "c"), n = c(2, 5, 3))
  rib <- data.frame(x = 1:10, lo = 1:10, hi = (1:10) + 3)
  expect_no_error(render_px(vplot(bars) |> mark_bar(x = cat, y = n, fill = g)))
  expect_no_error(render_px(
    vplot(rib) |> mark_ribbon(x = x, ymin = lo, ymax = hi, fill = g)
  ))
})

test_that("gradient fill on a polar bar errors clearly", {
  bars <- data.frame(cat = c("a", "b", "c"), n = c(2, 5, 3))
  g <- linear_gradient(c("#000000", "#ffffff"))
  expect_error(
    render_px(
      vplot(bars) |> mark_bar(x = cat, y = n, fill = g) |> coord_polar()
    ),
    "polar"
  )
})

# --- theme_cyberpunk --------------------------------------------------------

test_that("theme_cyberpunk sets a dark canvas and neon palette defaults", {
  p <- vplot(line_df) |> mark_line(x = x, y = y) |> theme_cyberpunk()
  expect_identical(p@theme$panel.background@fill, "#0d0f18")
  expect_identical(p@theme[["palette"]], quill:::.NEON_QUAL)
})

test_that("theme_cyberpunk supplies the default palette, overridable by a scale", {
  bars <- data.frame(cat = c("a", "b", "c"), n = c(2, 5, 3))
  neon <- vplot(bars) |>
    mark_bar(x = cat, y = n, fill = cat) |>
    theme_cyberpunk()
  expect_identical(train(neon)$color$map("a"), quill:::.NEON_QUAL[[1]])

  # an explicit scale palette still wins over the theme default
  overridden <- neon |> scale_fill_discrete(palette = "Blues")
  expect_false(identical(
    train(overridden)$color$map("a"),
    quill:::.NEON_QUAL[[1]]
  ))

  # without the theme, the default palette is unchanged
  plain <- vplot(bars) |> mark_bar(x = cat, y = n, fill = cat)
  expect_false(identical(train(plain)$color$map("a"), quill:::.NEON_QUAL[[1]]))
})
