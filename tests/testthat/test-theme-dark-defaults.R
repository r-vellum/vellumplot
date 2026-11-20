# VP-65: defaults that fell back to grey50 disappeared into theme_dark()'s
# grey50 panel. A hex or waffle fill that close to the panel now takes the
# area fill col_mix(ink, paper, 0.2); light themes keep grey50. The colourbar
# frame follows the theme's ink and paper.

# Every fill value in the compiled scene (gpar fills and per-shape fills).
dd_fills <- function(p) {
  s <- unlist(vellum::as_scene_spec(vellum::as_vellum_scene(p)))
  tolower(s[grepl("fill(\\.v[0-9]*)?$", names(s)) & !grepl("_t$", names(s))])
}

# Every stroke value in the compiled scene.
dd_cols <- function(p) {
  s <- unlist(vellum::as_scene_spec(vellum::as_vellum_scene(p)))
  tolower(s[grepl("gp\\.col(\\.v)?$", names(s))])
}

dd_hex <- vplot(mtcars) |> mark_hex(x = wt, y = mpg, fill = NULL, bins = 5)
dd_ink <- "#c7d0f0"
dd_paper <- "#0d0f18"

test_that("an unfilled mark_hex() stays visible on theme_dark()", {
  # light themes: ggplot2's grey50, as before
  for (th in list(identity, theme_gray, theme_bw, theme_minimal)) {
    f <- dd_fills(th(dd_hex))
    expect_true("grey50" %in% f)
    expect_false("grey20" %in% f)
  }
  # theme_dark: the panel is grey50, so the hexes take grey20
  f <- dd_fills(theme_dark(dd_hex))
  expect_true("grey20" %in% f)
  expect_identical(sum(f == "grey50"), 1L) # the panel background alone
  # theme_cyberpunk: its mid grey already stands off the dark panel
  f <- dd_fills(theme_cyberpunk(dd_hex))
  expect_true(tolower(vellumplot:::.col_mix(dd_ink, dd_paper, 0.5)) %in% f)
})

test_that("an explicit or themed hex fill still wins on theme_dark()", {
  p <- vplot(mtcars) |> mark_hex(x = wt, y = mpg, fill = "grey50", bins = 5)
  expect_gt(sum(dd_fills(theme_dark(p)) == "grey50"), 1L)
  q <- theme_dark(dd_hex) |> theme(geom = element_geom(fill = "grey52"))
  expect_true("grey52" %in% dd_fills(q))
  expect_false("grey20" %in% dd_fills(q))
})

test_that("the waffle's fallback fill stays visible on theme_dark()", {
  # vwaffle() always maps fill; the grey is drawn only without a fill scale
  bare <- vellumplot:::.add_layer(
    vellumplot::PlotSpec(data = data.frame(k = factor(rep("a", 4)))),
    "waffle",
    rlang::quos(),
    const_params = list(rows = 2L, flip = FALSE, pad = 0.1)
  )
  expect_true("grey50" %in% dd_fills(bare))
  expect_false("grey20" %in% dd_fills(bare))
  f <- dd_fills(theme_dark(bare))
  expect_true("grey20" %in% f)
  # a mapped waffle draws its palette on either theme
  w <- vwaffle(data.frame(k = c("a", "b"), n = c(3, 7)), k, n)
  expect_identical(
    setdiff(dd_fills(theme_dark(w)), "grey50"),
    setdiff(dd_fills(theme_gray(w)), c("grey92", "grey50"))
  )
})

test_that("the colourbar frame follows the theme's ink and paper", {
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg, color = hp)
  b <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, color = hp) |>
    guides(color = guide_coloursteps())
  for (q in list(p, b)) {
    # light: grey50 as before
    expect_true("grey50" %in% dd_cols(q))
    # cyberpunk: no hard-coded grey50 left in the scene
    expect_false("grey50" %in% dd_cols(theme_cyberpunk(q)))
    expect_true(
      tolower(vellumplot:::.col_mix(dd_ink, dd_paper, 0.5)) %in%
        dd_cols(theme_cyberpunk(q))
    )
  }
})

test_that(".contrast_ratio() matches the WCAG formula", {
  expect_equal(vellumplot:::.contrast_ratio("black", "white"), 21)
  expect_equal(vellumplot:::.contrast_ratio("grey50", "grey50"), 1)
})
