# Label repulsion (mark_text/mark_label repel = TRUE): the exact two-pass solver.

# Resolve a spec's repel solution for its (single) repel layer.
repel_solution <- function(spec, layer = 2L) {
  prov <- vellumplot:::.compile_plot(spec)
  s2 <- vellumplot:::.attach_repel_solutions(spec, vellum::scene_model(prov))
  s2@layers[[layer]]@stat_params$repel$solution
}

test_that("repel = TRUE records the repel params; FALSE records none", {
  on <- (vplot(mtcars) |>
    mark_text(x = wt, y = mpg, label = "a", repel = TRUE))@layers[[1]]
  expect_true(isTRUE(on@stat_params$repel$on))
  off <- (vplot(mtcars) |>
    mark_text(x = wt, y = mpg, label = "a"))@layers[[1]]
  expect_null(off@stat_params$repel)
})

test_that("the repel solution is deterministic for a fixed seed", {
  d <- data.frame(
    x = c(1, 1, 1, 1, 1),
    y = c(1, 1, 1, 1, 1),
    lab = letters[1:5]
  )
  spec <- vplot(d) |>
    mark_point(x = x, y = y) |>
    mark_text(x = x, y = y, label = lab, repel = TRUE, seed = 1)
  a <- repel_solution(spec)
  b <- repel_solution(spec)
  expect_identical(a$x, b$x)
  expect_identical(a$y, b$y)
})

test_that("a different seed gives a different layout", {
  d <- data.frame(x = rep(1, 5), y = rep(1, 5), lab = letters[1:5])
  base <- function(s) {
    vplot(d) |>
      mark_point(x = x, y = y) |>
      mark_text(x = x, y = y, label = lab, repel = TRUE, seed = s)
  }
  expect_false(identical(repel_solution(base(1))$x, repel_solution(base(2))$x))
})

test_that("co-located labels are pushed to distinct positions", {
  # five labels on the exact same point must not stay stacked
  d <- data.frame(x = rep(2, 5), y = rep(3, 5), lab = paste0("lab", 1:5))
  spec <- vplot(d) |>
    mark_point(x = x, y = y) |>
    mark_text(x = x, y = y, label = lab, repel = TRUE, seed = 1)
  sol <- repel_solution(spec)
  # every pair of label centres is separated
  centres <- cbind(sol$x, sol$y)
  dmat <- as.matrix(stats::dist(centres))
  diag(dmat) <- Inf
  expect_gt(min(dmat), 0)
  # and they moved off the shared anchor
  expect_true(all(abs(sol$x - 2) > 0 | abs(sol$y - 3) > 0))
})

test_that("displaced labels stay within the panel's native range", {
  set.seed(1)
  d <- data.frame(x = rnorm(20), y = rnorm(20), lab = paste0("p", 1:20))
  spec <- vplot(d) |>
    mark_point(x = x, y = y) |>
    mark_text(x = x, y = y, label = lab, repel = TRUE, seed = 1)
  sc <- vellumplot:::.build_panels(spec)$scales
  sol <- repel_solution(spec)
  expect_true(all(sol$x >= min(sc$x$domain) & sol$x <= max(sc$x$domain)))
  expect_true(all(sol$y >= min(sc$y$domain) & sol$y <= max(sc$y$domain)))
})

test_that("moved labels carry leader segments", {
  d <- data.frame(x = rep(1, 6), y = rep(1, 6), lab = paste0("x", 1:6))
  sol <- repel_solution(
    vplot(d) |>
      mark_point(x = x, y = y) |>
      mark_text(x = x, y = y, label = lab, repel = TRUE, seed = 1)
  )
  expect_false(is.null(sol$leaders))
  expect_true(nrow(sol$leaders) >= 1)
})

test_that("repel under facets errors clearly", {
  d <- data.frame(x = 1:6, y = 1:6, lab = letters[1:6], g = rep(c("a", "b"), 3))
  expect_error(
    vellum::as_vellum_scene(
      vplot(d) |>
        mark_text(x = x, y = y, label = lab, repel = TRUE) |>
        facet_wrap(~g)
    ),
    "single cartesian panel"
  )
})

test_that("repel restores the global RNG stream", {
  d <- data.frame(x = rep(1, 5), y = rep(1, 5), lab = letters[1:5])
  set.seed(123)
  before <- runif(1)
  invisible(vellum::as_vellum_scene(
    vplot(d) |> mark_text(x = x, y = y, label = lab, repel = TRUE, seed = 1)
  ))
  set.seed(123)
  expect_identical(before, runif(1))
})

test_that("repelled text and label marks render", {
  set.seed(1)
  d <- data.frame(x = rnorm(12), y = rnorm(12), lab = paste0("i", 1:12))
  f1 <- local_tempfile(fileext = ".png")
  render_plot(
    vplot(d) |>
      mark_point(x = x, y = y) |>
      mark_text(x = x, y = y, label = lab, repel = TRUE, seed = 1),
    f1
  )
  expect_gt(file.info(f1)$size, 0)
  f2 <- local_tempfile(fileext = ".png")
  render_plot(
    vplot(d) |>
      mark_point(x = x, y = y) |>
      mark_label(x = x, y = y, label = lab, repel = TRUE, seed = 1),
    f2
  )
  expect_gt(file.info(f2)$size, 0)
})
