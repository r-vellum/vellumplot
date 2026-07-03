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
  # a filled mark takes effects=, but glow does not apply to it
  expect_error(
    vplot(line_df) |> mark_bar(x = x, y = y, effects = list(glow())),
    "does not apply"
  )
  expect_error(
    quill:::.check_effects(list(glow()), "bar"),
    "does not apply"
  )
  # a mark with no effects= arg catches it via the reserved-argument guard
  expect_error(
    vplot(line_df) |> mark_tile(x = x, y = y, effects = list(glow())),
    "not an aesthetic"
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

# --- outline() --------------------------------------------------------------

test_that("outline() builds an OutlineSpec, attaches, and renders", {
  o <- outline(size = 2, color = "black")
  expect_true(S7::S7_inherits(o, quill:::OutlineSpec))
  expect_error(outline(size = -1), "positive")
  expect_error(outline(color = c("a", "b")), "single colour")

  p <- vplot(line_df) |>
    mark_point(x = x, y = y, size = 3, effects = list(outline()))
  expect_true(S7::S7_inherits(p@layers[[1]]@effects[[1]], quill:::OutlineSpec))
  expect_no_error(render_px(p))
})

# --- shadow() ---------------------------------------------------------------

test_that("shadow() builds a ShadowSpec and renders on a line", {
  s <- shadow(x = 0.01, y = -0.01)
  expect_true(S7::S7_inherits(s, quill:::ShadowSpec))
  expect_error(shadow(alpha = 2), "\\[0, 1\\]")
  expect_error(shadow(spread = -1), "non-negative")

  p <- vplot(line_df) |> mark_line(x = x, y = y, effects = list(shadow()))
  expect_no_error(render_px(p))
})

# --- sketch() ---------------------------------------------------------------

test_that("sketch() perturbs and densifies a path deterministically", {
  sk <- sketch(amount = 0.02, seed = 3)
  expect_true(S7::S7_inherits(sk, quill:::SketchSpec))
  scales <- list(x = list(domain = c(0, 10)), y = list(domain = c(0, 10)))
  a <- quill:::.sketch_native(scales, c(0, 10), c(0, 10), sk)
  b <- quill:::.sketch_native(scales, c(0, 10), c(0, 10), sk)
  expect_gt(length(a$x), 2) # densified
  expect_identical(a, b) # reproducible given the seed
  # a straight segment is actually bent
  expect_false(isTRUE(all.equal(a$y, seq(0, 10, length.out = length(a$y)))))
})

test_that("sketch renders and applies only to path marks", {
  p <- vplot(line_df) |>
    mark_line(x = x, y = y, effects = list(sketch())) |>
    theme_sketch()
  expect_no_error(render_px(p))
  expect_error(
    vplot(line_df) |> mark_point(x = x, y = y, effects = list(sketch())),
    "does not apply"
  )
})

# --- inner glow / shadow ----------------------------------------------------

test_that("inner_glow / inner_shadow build InnerSpec and render on fills", {
  ig <- inner_glow()
  is_ <- inner_shadow()
  expect_true(S7::S7_inherits(ig, quill:::InnerSpec))
  expect_false(ig@dark)
  expect_true(is_@dark)

  ar <- data.frame(x = 1:20, y = cumsum(abs(rnorm(20))))
  rb <- data.frame(x = 1:20, lo = 1:20, hi = (1:20) + 5)
  expect_no_error(render_px(
    vplot(ar) |> mark_area(x = x, y = y, fill = "#0a2a43", effects = list(ig))
  ))
  expect_no_error(render_px(
    vplot(rb) |> mark_ribbon(x = x, ymin = lo, ymax = hi, effects = list(is_))
  ))
  # inner effects do not apply to stroked marks
  expect_error(
    vplot(line_df) |> mark_line(x = x, y = y, effects = list(inner_glow())),
    "does not apply"
  )
})

# --- composition + theme_sketch ---------------------------------------------

test_that("effects compose in one layer", {
  p <- vplot(line_df) |>
    mark_line(
      x = x,
      y = y,
      color = "#00e5ff",
      effects = list(outline(color = "black"), glow())
    ) |>
    theme_cyberpunk()
  expect_no_error(render_px(p))
})

test_that("theme_sketch sets a paper canvas with no minor grid", {
  p <- vplot(line_df) |> mark_line(x = x, y = y) |> theme_sketch()
  expect_identical(p@theme$panel.background@fill, "#fbf7ee")
  expect_true(quill:::.is_blank(p@theme$panel.grid.minor))
})
