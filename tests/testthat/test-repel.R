# Label repulsion (mark_text / mark_label repel = TRUE): the engine placement
# solver applied as a single post-compile pass. Coordinate-agnostic -- faceted,
# polar and warped panels are solved together, no per-panel restriction.

# The moved label boxes of a compiled repel spec, in device px. `vl_place()` on
# the already-placed scene returns each label's box at its final position (the
# residual displacement is ~0), which is what these tests inspect.
repel_boxes <- function(spec) {
  sc <- vellum::as_vellum_scene(spec)
  labs <- grep("^repel:", vellum::node_names(sc), value = TRUE)
  vellum::vl_place(sc, labels = labs)
}

n_segments <- function(spec) {
  el <- vellum::scene_model(vellum::as_vellum_scene(spec))$elements
  sum(el$mark == "segment")
}

# Count overlapping pairs among a set of device-px boxes.
overlapping_pairs <- function(b) {
  if (nrow(b) < 2L) {
    return(0L)
  }
  p <- utils::combn(nrow(b), 2L)
  sum(apply(p, 2, function(ij) {
    i <- ij[1]
    j <- ij[2]
    !(b$x1[i] <= b$x0[j] |
      b$x0[i] >= b$x1[j] |
      b$y1[i] <= b$y0[j] |
      b$y0[i] >= b$y1[j])
  }))
}

test_that("repel = TRUE records the repel flag; FALSE records none", {
  on <- (vplot(mtcars) |>
    mark_text(x = wt, y = mpg, label = "a", repel = TRUE))@layers[[1]]
  expect_true(isTRUE(on@stat_params$repel$on))
  off <- (vplot(mtcars) |>
    mark_text(x = wt, y = mpg, label = "a"))@layers[[1]]
  expect_null(off@stat_params$repel)
})

test_that("co-located labels are separated by the solver", {
  # five labels on the exact same point must not stay stacked
  d <- data.frame(x = rep(2, 5), y = rep(3, 5), lab = paste0("lab", 1:5))
  b <- repel_boxes(
    vplot(d) |>
      mark_point(x = x, y = y) |>
      mark_text(x = x, y = y, label = lab, repel = TRUE)
  )
  expect_equal(nrow(b), 5L)
  expect_equal(overlapping_pairs(b), 0L)
})

test_that("repel is deterministic (same spec compiles to the same placement)", {
  d <- data.frame(x = rep(1, 5), y = rep(1, 5), lab = letters[1:5])
  sp <- vplot(d) |>
    mark_point(x = x, y = y) |>
    mark_text(x = x, y = y, label = lab, repel = TRUE)
  a <- repel_boxes(sp)
  b <- repel_boxes(sp)
  expect_equal(a$x0, b$x0)
  expect_equal(a$y0, b$y0)
})

test_that("moved labels get leader lines", {
  d <- data.frame(x = rep(1, 6), y = rep(1, 6), lab = paste0("x", 1:6))
  base <- vplot(d) |>
    mark_point(x = x, y = y) |>
    mark_text(x = x, y = y, label = lab)
  repelled <- vplot(d) |>
    mark_point(x = x, y = y) |>
    mark_text(x = x, y = y, label = lab, repel = TRUE)
  # the co-located labels all move, so each contributes a leader segment
  expect_gt(n_segments(repelled), n_segments(base))
})

test_that("displaced labels stay within the panel", {
  set.seed(1)
  d <- data.frame(x = rnorm(20), y = rnorm(20), lab = paste0("p", 1:20))
  sp <- vplot(d, width = 6, height = 5) |>
    mark_point(x = x, y = y) |>
    mark_text(x = x, y = y, label = lab, repel = TRUE, size = 4)
  sc <- vellum::as_vellum_scene(sp)
  m <- vellum::scene_model(sc)
  panel <- m$panels[grepl("^panel-", m$panels$name), , drop = FALSE][1, ]
  labs <- grep("^repel:", vellum::node_names(sc), value = TRUE)
  b <- vellum::vl_place(sc, labels = labs)
  # every label box centre lies inside the panel rect (a small tolerance for the
  # label that is pinned at the panel edge)
  cx <- (b$x0 + b$x1) / 2
  cy <- (b$y0 + b$y1) / 2
  tol <- 2
  expect_true(all(cx >= panel$x0 - tol & cx <= panel$x1 + tol))
  expect_true(all(cy >= panel$y0 - tol & cy <= panel$y1 + tol))
})

test_that("repel works under facets (previously an error)", {
  d <- data.frame(
    x = 1:6,
    y = 1:6,
    lab = letters[1:6],
    g = rep(c("a", "b"), 3)
  )
  sp <- vplot(d) |>
    mark_point(x = x, y = y) |>
    mark_text(x = x, y = y, label = lab, repel = TRUE) |>
    facet_wrap(~g)
  expect_no_error(sc <- vellum::as_vellum_scene(sp))
  # labels are named per panel, so both facet panels carry their own repel nodes
  labs <- grep("^repel:", vellum::node_names(sc), value = TRUE)
  panels <- unique(sub("^repel:([^:]+):.*$", "\\1", labs))
  expect_gte(length(panels), 2L)
})

test_that("repel works under coord_polar (previously an error)", {
  d <- data.frame(x = rep(2, 5), y = rep(3, 5), lab = paste0("p", 1:5))
  sp <- vplot(d, width = 6, height = 6) |>
    mark_point(x = x, y = y) |>
    mark_text(x = x, y = y, label = lab, repel = TRUE) |>
    coord_polar()
  f <- local_tempfile(fileext = ".png")
  expect_no_error(render_plot(sp, f))
  expect_gt(file.info(f)$size, 0)
})

test_that("repelled text and label marks render", {
  set.seed(1)
  d <- data.frame(x = rnorm(12), y = rnorm(12), lab = paste0("i", 1:12))
  f1 <- local_tempfile(fileext = ".png")
  render_plot(
    vplot(d) |>
      mark_point(x = x, y = y) |>
      mark_text(x = x, y = y, label = lab, repel = TRUE),
    f1
  )
  expect_gt(file.info(f1)$size, 0)
  f2 <- local_tempfile(fileext = ".png")
  render_plot(
    vplot(d) |>
      mark_point(x = x, y = y) |>
      mark_label(x = x, y = y, label = lab, repel = TRUE),
    f2
  )
  expect_gt(file.info(f2)$size, 0)
})
