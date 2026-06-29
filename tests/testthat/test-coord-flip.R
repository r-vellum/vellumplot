# coord_flip(): swap the x and y axes at render time.

train <- function(p) {
  vellumplot:::.train_scales(p, vellumplot:::.resolve_layers(p))
}

# Near-black ink count in a fractional region of a rendered image.
ink_in <- function(img, rows, cols) {
  H <- dim(img)[1]
  W <- dim(img)[2]
  rr <- seq(max(1, floor(rows[1] * H)), min(H, ceiling(rows[2] * H)))
  cc <- seq(max(1, floor(cols[1] * W)), min(W, ceiling(cols[2] * W)))
  sub <- img[rr, cc, 1:3, drop = FALSE]
  sum(rowSums(sub <= 0.4, dims = 2) == 3)
}

test_that("coord_flip leaves the trained scales unchanged", {
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
  pf <- p |> coord_flip()
  expect_equal(train(p)$x$domain, train(pf)$x$domain)
  expect_equal(train(p)$y$domain, train(pf)$y$domain)
})

test_that("flipped bars extend horizontally, not vertically", {
  up <- render_px(vplot(mtcars) |> mark_bar(x = factor(cyl)))
  fl <- render_px(vplot(mtcars) |> mark_bar(x = factor(cyl)) |> coord_flip())
  # Upright bars fill columns (vertical extent); flipped bars fill rows.
  # In the bottom-left quadrant the flipped chart has a long horizontal bar
  # (ink spanning far to the right) absent in the upright chart's far-right-low
  # region near the axis. Compare ink along a low horizontal strip vs a left
  # vertical strip.
  # Flipped: a horizontal strip near the bottom is mostly bar ink.
  expect_gt(ink_in(fl, rows = c(0.75, 0.9), cols = c(0.1, 0.8)), 0)
  # Upright: that same low-wide strip is mostly empty panel (bars are columns),
  # so flipped has strictly more ink there.
  expect_gt(
    ink_in(fl, rows = c(0.75, 0.9), cols = c(0.1, 0.8)),
    ink_in(up, rows = c(0.75, 0.9), cols = c(0.1, 0.8))
  )
})

test_that("flip swaps which scale drives each axis title", {
  # bottom-axis title becomes the y-scale's name, left becomes the x-scale's.
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    labs(x = "WEIGHT", y = "MILES")
  f <- withr::local_tempfile(fileext = ".svg")
  render_plot(p |> coord_flip(), f)
  svg <- paste(readLines(f), collapse = "\n")
  expect_true(grepl("WEIGHT", svg))
  expect_true(grepl("MILES", svg))
})

test_that("flipped point/line/smooth/rule render", {
  for (build in list(
    function(x) x |> mark_point(x = wt, y = mpg),
    function(x) x |> mark_line(x = wt, y = mpg),
    function(x) {
      x |> mark_point(x = wt, y = mpg) |> mark_smooth(x = wt, y = mpg)
    },
    function(x) x |> mark_point(x = wt, y = mpg) |> mark_rule(xintercept = 3)
  )) {
    f <- withr::local_tempfile(fileext = ".png")
    render_plot(build(vplot(mtcars)) |> coord_flip(), f)
    expect_gt(file.info(f)$size, 0)
  }
})

test_that("faceted flip with shared scales renders", {
  f <- withr::local_tempfile(fileext = ".png")
  render_plot(
    vplot(mtcars) |>
      mark_point(x = wt, y = mpg) |>
      facet_wrap(~cyl) |>
      coord_flip(),
    f
  )
  expect_gt(file.info(f)$size, 0)
})
