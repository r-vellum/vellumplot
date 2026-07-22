# Regression tests for the review-remediation Phase 3: degenerate-input
# hardening and doc/robustness fixes.

# --- B6: ShadowSpec defaults match shadow() (mm, not the old npc numbers) ---

test_that("ShadowSpec class defaults are the mm offsets shadow() uses", {
  s <- vellumplot:::ShadowSpec()
  expect_equal(s@x, 0.5)
  expect_equal(s@y, -0.5)
  # and the constructor still builds a usable effect
  eff <- shadow()
  expect_equal(eff@x, 0.5)
})

# --- B7: a degenerate (single-value) position domain does not collapse ------

panel_px_w <- function(p) {
  m <- vellum::scene_model(vellum::as_vellum_scene(p))
  pan <- m$panels[m$panels$name == "panel-1-1", ]
  pan$px1 - pan$px0
}

test_that("a single distinct x/y under coord_fixed keeps a finite panel", {
  px <- panel_px_w(
    vplot(data.frame(x = c(1, 1, 1), y = c(1, 2, 3))) |>
      mark_point(x = x, y = y) |>
      coord_fixed()
  )
  expect_true(is.finite(px) && px > 0)

  py <- panel_px_w(
    vplot(data.frame(x = c(1, 2, 3), y = c(5, 5, 5))) |>
      mark_point(x = x, y = y) |>
      coord_fixed()
  )
  expect_true(is.finite(py) && py > 0)
})

# --- B8: a pole-centred geographic map has a bounded aspect -----------------

test_that("map aspect stays finite and bounded at the poles", {
  skip_if_not_installed("sf")
  skip_if_not_installed("vctrs")
  rt <- vellumplot:::.resolve_theme(vellumplot:::.theme_default())
  g <- sf::st_sf(
    geometry = sf::st_sfc(
      sf::st_point(c(0, 90)),
      sf::st_point(c(2, 90)),
      crs = 4326
    )
  )
  spec <- vplot(g) |> mark_sf() |> coord_sf()
  proj <- vellumplot:::.project_sf_data(spec)
  built <- vellumplot:::.build_panels(proj$spec)
  built$sf_geographic <- proj$geographic
  lay <- vellumplot:::.build_layout(
    built,
    list(),
    list(),
    rt,
    FALSE,
    proj$spec@coord
  )
  h <- lay$heights[lay$panel_row]
  val <- vctrs::field(h, "value")
  # unclamped this would be ~1e16 (1 / cos(90 deg)); clamped it is modest.
  expect_true(is.finite(val) && val < 1e6)
})

# --- B9: a rolling window needs a positive integer k ------------------------

test_that("window stats reject a non-positive / non-integer k", {
  d <- data.frame(x = 1:6, y = c(1, 3, 2, 5, 4, 6))
  win <- function(k) {
    vellum::as_vellum_scene(
      vplot(d) |> mark_line(x = x, y = y, window = list(op = "mean", k = k))
    )
  }
  expect_error(win(0), "positive integer")
  expect_error(win(-2), "positive integer")
  expect_error(win(2.5), "positive integer")
  expect_no_error(win(3))
})

# --- B12: a computed-constant if_false is carried; a column is not ----------

conds <- function(p) interaction_model(p)$conditions

test_that("a negative-literal / computed-constant if_false is a constant", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, color = condition("s", factor(cyl), -0.5)) |>
    select_point("s")
  expect_equal(conds(p)[[1]]$if_false, -0.5)

  p2 <- vplot(mtcars) |>
    mark_point(
      x = wt,
      y = mpg,
      color = condition("s", factor(cyl), grey(0.5))
    ) |>
    select_point("s")
  expect_equal(conds(p2)[[1]]$if_false, grey(0.5))
})

test_that("a per-row column if_false is NOT treated as a constant", {
  d <- data.frame(x = 1:4, y = 1:4, g = c("a", "b", "a", "b"), col = "grey")
  p <- vplot(d) |>
    mark_point(x = x, y = y, color = condition("s", g, col)) |>
    select_point("s")
  expect_null(conds(p)[[1]]$if_false)
})

# --- B16: .layer_panel_idx falls back only when the facet var is absent -----

test_that("a layer whose data lacks the facet variable draws on every panel", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    facet_wrap(~cyl) |>
    mark_rule(yintercept = 20, data = data.frame(yintercept = 20))
  expect_no_error(vellum::as_vellum_scene(p))
})

test_that(".layer_panel_idx still subsets when the facet variable is present", {
  facet <- facet_wrap(vplot(mtcars) |> mark_point(x = wt, y = mpg), ~cyl)@facet
  panel <- list(lvl = list(wrap = "4"))
  idx <- vellumplot:::.layer_panel_idx(facet, mtcars, panel)
  expect_equal(idx, which(mtcars$cyl == 4))
})

# --- B17: collinear circle centres do not produce NaN coordinates -----------

test_that("a circlepack of equal collinear leaves has finite coordinates", {
  d <- data.frame(
    id = c("r", "a", "b", "c"),
    parent = c(NA, "r", "r", "r"),
    value = c(NA, 1, 1, 1)
  )
  m <- vellum::scene_model(
    vellum::as_vellum_scene(vhierarchy(
      d,
      id,
      parent,
      value,
      type = "circlepack"
    ))
  )
  el <- m$elements
  expect_false(any(is.nan(el$x) | is.nan(el$y)))
  expect_true(all(is.finite(el$x) & is.finite(el$y)))
})

# --- T7: a pathologically deep tree aborts clearly --------------------------

test_that("an over-deep hierarchy aborts instead of overflowing the stack", {
  n <- vellumplot:::.MAX_TREE_DEPTH + 2L
  dd <- data.frame(
    id = as.character(seq_len(n)),
    parent = c(NA, as.character(seq_len(n - 1L))),
    value = c(rep(NA_real_, n - 1L), 1)
  )
  expect_error(
    vellum::as_vellum_scene(vhierarchy(dd, id, parent, value)),
    "too deep"
  )
})

test_that("an over-deep dendrogram aborts instead of overflowing the stack", {
  skip_if_not_installed("igraph")
  n <- vellumplot:::.MAX_TREE_DEPTH + 2L
  g <- igraph::make_tree(n, children = 1L) # a path (each node one child)
  expect_error(
    vellum::as_vellum_scene(vgraph(g, layout = "dendrogram")),
    "too deep"
  )
})
