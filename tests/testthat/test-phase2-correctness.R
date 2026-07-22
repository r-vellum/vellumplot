# Regression tests for the review-remediation Phase 2: silent wrong-output
# correctness fixes.

# --- B2: mark_point(auto=) must not datashade under a warped coord ----------

marks_of <- function(p) {
  m <- vellum::scene_model(vellum::as_vellum_scene(p))
  table(m$elements$mark)
}

test_that("auto point datashades under cartesian but not under polar/trans", {
  n <- vellumplot:::.DATASHADE_AUTO + 5000L
  df <- data.frame(x = runif(n), y = runif(n))

  # cartesian: the datashade fallback fires -> the cloud becomes a raster, so no
  # per-point "point" elements are emitted.
  cart <- marks_of(vplot(df) |> mark_point(x = x, y = y, auto = TRUE))
  expect_false("point" %in% names(cart))

  # polar: datashade bins in linear space and would misplace, so the guard skips
  # it and the vector path draws every point.
  pol <- marks_of(
    vplot(df) |> mark_point(x = x, y = y, auto = TRUE) |> coord_polar()
  )
  expect_equal(unname(pol["point"]), n)

  # coord_trans is warped too -> vector path. (Strictly-positive data so the
  # log10 transform's domain is satisfied.)
  dfp <- data.frame(x = runif(n, 1, 10), y = runif(n, 1, 10))
  tr <- marks_of(
    vplot(dfp) |>
      mark_point(x = x, y = y, auto = TRUE) |>
      coord_trans(x = "log10")
  )
  expect_equal(unname(tr["point"]), n)
})

# --- B3: a sankey column too wide for its gaps aborts clearly ---------------

test_that("a fat sankey column aborts instead of drawing inverted geometry", {
  flows <- data.frame(from = paste0("s", 1:60), to = "t", value = 1)
  expect_error(
    vellum::as_vellum_scene(vsankey(flows, from, to, value)),
    "too many nodes"
  )
})

test_that("an ordinary sankey still renders", {
  flows <- data.frame(
    from = c("A", "A", "B", "C"),
    to = c("B", "C", "D", "D"),
    value = c(4, 6, 4, 4)
  )
  f <- local_tempfile(fileext = ".png")
  expect_no_error(render_plot(vsankey(flows, from, to, value), f))
})

# --- B5: a stat realigns both colour and fill ------------------------------

test_that(".merge_stat realigns both colour and fill to the aggregated groups", {
  L <- list(
    after = list(),
    types = list(),
    values = list(
      x = 1:3,
      color = c("a", "a", "b"),
      fill = c("a", "a", "b")
    )
  )
  sdf <- data.frame(x = c("a", "b"), y = c(1.5, 3), group = c("a", "b"))
  out <- vellumplot:::.merge_stat(L, sdf)
  expect_equal(out$values$color, sdf$group)
  expect_equal(out$values$fill, sdf$group)
})

# --- B10: aggregate drops NA-summary categories with a warning, consistently -

test_that("mark_bar(stat) drops NA-summary categories with a warning", {
  d <- data.frame(cat = c("a", "a", "b", "b", "c"), y = c(1, 2, 3, NA, 5))
  L <- list(
    values = list(x = d$cat, y = d$y),
    stat_params = list(fun = "mean"),
    n = nrow(d)
  )
  expect_warning(
    out <- vellumplot:::.stat_aggregate(L),
    "missing summary value"
  )
  # "b" (mean of 3, NA -> NA) dropped; "a" and "c" kept.
  expect_equal(as.character(out$x), c("a", "c"))
})

test_that("grouped and ungrouped aggregate treat NA identically", {
  d <- data.frame(
    cat = c("a", "a", "b", "b"),
    y = c(1, 2, 3, NA),
    g = c("x", "x", "x", "x")
  )
  ung <- list(
    values = list(x = d$cat, y = d$y),
    stat_params = list(fun = "mean"),
    n = nrow(d)
  )
  grp <- list(
    values = list(x = d$cat, y = d$y, color = d$g),
    stat_params = list(fun = "mean"),
    n = nrow(d)
  )
  suppressWarnings({
    u <- vellumplot:::.stat_aggregate(ung)
    g <- vellumplot:::.stat_aggregate(grp)
  })
  # both feed `fun` the NA -> both drop "b"; the surviving x categories match.
  expect_equal(as.character(u$x), as.character(g$x))
  expect_equal(as.character(u$x), "a")
})

# --- B15: a grouping aesthetic shorter than the data is recycled ------------

test_that(".layer_group recycles a constant grouping to the row count", {
  L <- list(values = list(x = c(1, 2, 3), color = "red"), n = 3)
  expect_length(vellumplot:::.layer_group(L), 3L)
  # an already-full-length group is untouched
  L2 <- list(values = list(x = 1:3, fill = c("a", "b", "c")), n = 3)
  expect_equal(vellumplot:::.layer_group(L2), c("a", "b", "c"))
})
