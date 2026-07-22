# Regression tests for the review-remediation Phase 1: input validation turns
# cryptic low-level failures / silent misbehaviour into clear errors.

# --- B1: continuous scale limits / range must be length-2 -------------------

test_that("scale_x/y_continuous(limits=) rejects a non-length-2 vector", {
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
  expect_error(
    scale_x_continuous(p, limits = c(1, 2, 3)),
    "length-2"
  )
  expect_error(
    scale_y_continuous(p, limits = 5),
    "length-2"
  )
})

test_that("valid length-2 continuous limits still work", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    scale_x_continuous(limits = c(0, 6))
  expect_no_error(vellum::as_vellum_scene(p))
})

test_that("scale_size/alpha/edge_width limits and range are validated", {
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg, size = hp)
  expect_error(scale_size(p, limits = 5), "length-2")
  expect_error(scale_size(p, range = c(1, 2, 3)), "length-2")
  expect_error(scale_size(p, limits = c("a", "b")), "must be numeric")
  expect_error(scale_alpha(p, range = 0.5), "length-2")
  expect_error(scale_edge_width(p, limits = 1), "length-2")
  expect_error(scale_edge_alpha(p, range = c(1, 2, 3)), "length-2")
})

test_that("scale_size_area validates limits and max_size", {
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg, size = hp)
  expect_error(scale_size_area(p, limits = c(1, 2, 3)), "length-2")
  expect_error(scale_size_area(p, max_size = c(1, 2)), "single positive")
  expect_error(scale_size_area(p, max_size = -1), "single positive")
})

test_that("the shortcut path (xlim) already-validated behaviour is unchanged", {
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
  expect_error(xlim(p, 1, 2, 3), "length-2")
})

# --- B4: add_marginal() must reject a factor x/y ----------------------------

test_that("add_marginal() errors on a factor mapping instead of using codes", {
  d <- data.frame(x = factor(c("a", "b", "c")), y = 1:3)
  p <- vplot(d) |> mark_point(x = x, y = y) |> add_marginal()
  expect_error(vellum::as_vellum_scene(p), "numeric")
})

test_that("add_marginal() still works for genuinely numeric x/y", {
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg) |> add_marginal()
  expect_no_error(vellum::as_vellum_scene(p))
})

# --- B13: selection flags and vgraph() dimensions ---------------------------

test_that("select_point()/select_interval() reject non-logical flags", {
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
  expect_error(select_point(p, "s", toggle = "yes"), "TRUE.*FALSE")
  expect_error(select_point(p, "s", empty = 1), "TRUE.*FALSE")
  expect_error(select_interval(p, "s", empty = NA), "TRUE.*FALSE")
})

test_that("valid selection flags are accepted", {
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
  expect_no_error(select_point(p, "s", toggle = FALSE, empty = FALSE))
})

test_that("vgraph() validates width/height like its siblings", {
  skip_if_not_installed("igraph")
  g <- igraph::make_ring(5)
  expect_error(vgraph(g, width = -1), "positive")
  expect_error(vgraph(g, height = c(1, 2)), "single positive")
})

# --- B18: area() rejects non-numeric / non-integer cell indices -------------

test_that("area() gives a clear error for a non-numeric cell index", {
  expect_error(area("a", 1), "positive integer")
  expect_error(area(1, 1, b = 2.5), "positive integer")
  expect_error(area(0, 1), "positive integer")
})

test_that("valid area() still builds", {
  expect_equal(area(1, 1, 2, 3), list(t = 1, l = 1, b = 2, r = 3))
})
