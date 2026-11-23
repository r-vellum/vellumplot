# VP-121: mark_rule() under coord_polar() / coord_radial() used to emit
# cartesian segments in data units, which fell outside the [-1, 1] polar panel
# and were clipped away, so the layer drew nothing.

polar_rule_base <- function() {
  d <- data.frame(m = 1:12, v = c(3, 4, 6, 8, 9, 11, 12, 11, 9, 7, 5, 3))
  vplot(d) |>
    mark_bar(x = m, y = v, fill = "grey80") |>
    theme(panel.background = element_rect(fill = "white"))
}

red_px <- function(p) count_near(render_px(p), c(1, 0, 0), tol = 0.15)

test_that("a constant-y rule draws a circle under coord_polar()", {
  p <- polar_rule_base() |>
    mark_rule(yintercept = 6, color = "red", linewidth = 2) |>
    coord_polar()
  expect_gt(red_px(p), 200)
  # it is a closed ring around the centre: red on both sides of the panel
  img <- render_px(p)
  mid <- dim(img)[2] / 2
  red <- abs(img[,, 1] - 1) < 0.15 & img[,, 2] < 0.15 & img[,, 3] < 0.15
  cols <- which(colSums(red) > 0)
  expect_true(any(cols < mid) && any(cols > mid))
})

test_that("a constant-x rule draws a spoke under coord_polar()", {
  p <- polar_rule_base() |>
    mark_rule(xintercept = 4, color = "red", linewidth = 2) |>
    coord_polar()
  n <- red_px(p)
  expect_gt(n, 50)
  # a spoke is far smaller than a ring
  ring <- red_px(
    polar_rule_base() |>
      mark_rule(yintercept = 6, color = "red", linewidth = 2) |>
      coord_polar()
  )
  expect_lt(n, ring)
})

test_that("rules follow coord_radial()'s partial sweep and theta = 'y'", {
  half <- polar_rule_base() |>
    mark_rule(yintercept = 8, color = "red", linewidth = 2) |>
    coord_radial(start = -pi / 2, end = pi / 2, inner_radius = 0.3)
  img <- render_px(half)
  red <- abs(img[,, 1] - 1) < 0.15 & img[,, 2] < 0.15 & img[,, 3] < 0.15
  expect_gt(sum(red), 100)
  # an arc over the top half only: no red in the bottom third of the image
  rows <- which(rowSums(red) > 0)
  expect_lt(max(rows), dim(img)[1] * 0.67)

  ty <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, color = "grey50") |>
    mark_rule(yintercept = 20, color = "red", linewidth = 2) |>
    coord_polar(theta = "y")
  expect_gt(red_px(ty), 50)
})

test_that("an intercept outside its scale's domain draws nothing", {
  p <- polar_rule_base() |>
    mark_rule(yintercept = 6, color = "red") |>
    coord_polar() |>
    scale_y_continuous(limits = c(0, 12))
  q <- polar_rule_base() |>
    mark_rule(yintercept = 60, color = "red") |>
    coord_polar() |>
    scale_y_continuous(limits = c(0, 12))
  expect_gt(red_px(p), 0)
  expect_equal(red_px(q), 0)
})
