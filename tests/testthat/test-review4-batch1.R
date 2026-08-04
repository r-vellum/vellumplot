# Regression tests for the REVIEW4 Batch 1 correctness fixes.

panels <- function(p) vellumplot:::.build_panels(p)
compile <- function(p) vellumplot:::.compile_plot(p)

# CP1 -----------------------------------------------------------------------
test_that("a structurally-empty facet_grid cell renders instead of aborting", {
  df <- data.frame(
    v = rnorm(60),
    r = rep(c("A", "B"), 30),
    c = rep(c("x", "y", "z"), each = 20)
  )
  df <- df[!(df$r == "B" & df$c == "x"), ] # leave the (B, x) cell empty
  p <- vplot(df) |> mark_histogram(x = v) |> facet_grid(r ~ c)
  expect_no_error(compile(p))
})

# CP2 / SC1 -----------------------------------------------------------------
test_that("one-sided limits pin one end and train the other from data", {
  lo <- panels(vplot(mtcars) |> mark_point(x = wt, y = mpg) |> ylim(0, NA))
  yr <- lo$scales$y$data_range
  expect_equal(yr[1], 0)
  expect_equal(yr[2], max(mtcars$mpg))

  hi <- panels(vplot(mtcars) |> mark_point(x = wt, y = mpg) |> xlim(NA, 5))
  xr <- hi$scales$x$data_range
  expect_equal(xr[1], min(mtcars$wt))
  expect_equal(xr[2], 5)
})

# CP3 -----------------------------------------------------------------------
test_that("pooling a factor and a character keeps every category", {
  expect_setequal(
    vellumplot:::.cat_levels(list(factor(c("a", "b")), c("a", "b", "c"))),
    c("a", "b", "c")
  )
})

# CP5 -----------------------------------------------------------------------
test_that("numeric categories drawn discretely order numerically", {
  expect_identical(
    vellumplot:::.cat_levels(c(1, 2, 10, 10, 2)),
    c("1", "2", "10")
  )
})

# CM6 -----------------------------------------------------------------------
test_that("a pie (theta = value) renders with a constant/short x", {
  d <- data.frame(cat = c("a", "b", "c"), val = c(3, 5, 2))
  expect_no_error(compile(vplot(d) |> mark_pie(value = val, fill = cat)))
  expect_no_error(compile(vplot(d) |> mark_donut(value = val, fill = cat)))
})

# GF1 -----------------------------------------------------------------------
test_that("a disconnected dendrogram layout fails with a clear message", {
  skip_if_not_installed("igraph")
  g <- igraph::add_edges(
    igraph::make_empty_graph(5, directed = FALSE),
    c(1, 2, 2, 3)
  )
  expect_error(compile(vgraph(g, layout = "dendrogram")), "connected tree")
})

# GF6 -----------------------------------------------------------------------
test_that("vtable renders a blank cell for a too-short sparkline series", {
  d <- data.frame(g = c("a", "b"))
  d$s <- list(c(1, 2, 3, 2), numeric(0)) # second series is empty
  expect_no_error(as_vellum_scene(vtable(d, spark = "s")))
})

# GF2 -----------------------------------------------------------------------
test_that("a non-wrapping reveal animation reaches its final keyframe", {
  sched <- vellumplot:::.anim_schedule(
    k = 4,
    nframes = 10,
    easing = "linear",
    seg_weights = rep(1, 3),
    state_length = 0,
    wrap = FALSE
  )
  # last frame lands on the final keyframe (seg K-1 at fraction 1)
  expect_equal(sched$seg[10], 3)
  expect_equal(sched$frac[10], 1)
})

test_that("a wrapping animation still stops short of duplicating the start", {
  sched <- vellumplot:::.anim_schedule(
    k = 4,
    nframes = 10,
    easing = "linear",
    seg_weights = rep(1, 4),
    state_length = 0,
    wrap = TRUE
  )
  expect_lt(sched$frac[10], 1)
})

# MK1 -----------------------------------------------------------------------
test_that("graph marks accept layer effects", {
  skip_if_not_installed("igraph")
  g <- igraph::make_ring(5)
  expect_no_error(vgraph(g) |> mark_edge_bundle(effects = list(shadow())))
  expect_no_error(
    vgraph(g) |>
      mark_flow_map(root = 1, effects = list(glow()))
  )
})

# SR1 -----------------------------------------------------------------------
test_that("a facet on a non-syntactic column name round-trips", {
  d <- data.frame(x = 1:6, y = 1:6, grp = rep(c("p", "q"), 3))
  names(d)[3] <- "my group"
  p <- vplot(d) |>
    mark_point(x = x, y = y) |>
    facet_wrap(~`my group`)
  expect_no_error(from_spec(as_spec(p)))
})
