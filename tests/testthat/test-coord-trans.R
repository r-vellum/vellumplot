# coord_trans(): a nonlinear *display* remap. The scale trains normally (breaks at
# data values); coord_trans warps only where things are drawn, keeping the axis
# labels at their original data values. Distinct from scale_*(trans=). Phase 5.

svg_of <- function(p) {
  f <- local_tempfile(fileext = ".svg")
  render_plot(p, f)
  gsub("vl[0-9]+", "vlID", paste(readLines(f), collapse = "\n")) # drop a11y id counter
}

test_that("coord_trans() renders point + line under a log-y display", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    mark_line(x = wt, y = mpg) |>
    coord_trans(y = "log10")
  expect_no_error(svg_of(p))
})

test_that("a bar/area is rejected under a nonlinear coord_trans y (H22)", {
  # A zero-baseline mark has no defined baseline on a log axis; refuse it with a
  # clear message rather than aborting deep in the domain guard.
  db <- data.frame(g = c("a", "b", "c"), n = c(100, 200, 300))
  expect_error(
    svg_of(vplot(db) |> mark_bar(x = g, y = n) |> coord_trans(y = "log10")),
    "zero baseline"
  )
  expect_error(
    svg_of(vplot(db) |> mark_area(x = seq_along(g), y = n) |> coord_trans(y = "log10")),
    "zero baseline"
  )
  # an x-only warp leaves the y baseline linear, so bars are still allowed
  expect_no_error(
    svg_of(vplot(db) |> mark_bar(x = seq_along(g), y = n) |> coord_trans(x = "sqrt"))
  )
})

test_that(".warp_scale warps break/domain positions but keeps labels", {
  sc <- list(
    domain = c(1, 100),
    breaks = c(1, 10, 100),
    labels = c("1", "10", "100")
  )
  w <- .warp_scale(sc, log10)
  expect_equal(w$breaks, log10(c(1, 10, 100))) # positions warped
  expect_equal(w$domain, log10(c(1, 100)))
  expect_identical(w$labels, c("1", "10", "100")) # labels unchanged (still data values)
})

test_that("nonlinear axes densify straight polylines; linear axes do not", {
  nonlin <- list(
    trans = list(x_map = identity, y_map = log10, x_lin = TRUE, y_lin = FALSE)
  )
  d <- .trans_munch(nonlin, c(1, 100), c(1, 100))
  expect_gt(length(d$x), 2L) # a straight data segment becomes a curve
  lin <- list(
    trans = list(x_map = identity, y_map = identity, x_lin = TRUE, y_lin = TRUE)
  )
  d0 <- .trans_munch(lin, c(1, 100), c(1, 100))
  expect_equal(d0$x, c(1, 100)) # identity: no spurious densification
})

test_that("an identity coord_trans is byte-for-byte the plain plot (additive)", {
  plain <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
  ident <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    coord_trans(x = "identity", y = "identity")
  expect_identical(svg_of(ident), svg_of(plain))
})

test_that("coord_trans errors clearly on unsupported marks and out-of-domain data", {
  expect_error(
    svg_of(
      vplot(mtcars) |>
        mark_boxplot(x = factor(cyl), y = mpg) |>
        coord_trans(y = "log10")
    ),
    "does not yet support"
  )
  expect_error(
    svg_of(
      vplot(data.frame(x = 1:3, y = c(-1, 0, 1))) |>
        mark_point(x = x, y = y) |>
        coord_trans(y = "log10")
    ),
    "transform"
  )
  expect_error(
    vplot(mtcars) |> mark_point(x = wt, y = mpg) |> coord_trans(y = "nope"),
    "Unknown"
  )
})
