# Hexbin: stat hexbin + mark_hex (uses vellum's hexagon_grob).

resolve1 <- function(p) vellumplot:::.resolve_layers(p)[[1]]

set.seed(1)
d <- data.frame(x = rnorm(2000), y = rnorm(2000))

test_that("mark_hex sets mark/stat and a default count fill", {
  L <- (vplot(d) |> mark_hex(x = x, y = y))@layers[[1]]
  expect_identical(L@mark, "hex")
  expect_identical(L@stat, "hexbin")
  # default fill = after_stat(count)
  expect_true("fill" %in% names(L@encoding))
  expect_true(L@encoding[["fill"]]@after)
})

test_that("a British `colour =` suppresses the default count fill (H12)", {
  # A user-supplied colour (either spelling) must switch off the auto-injected
  # `fill = after_stat(count)` so the two do not conflict.
  dg <- data.frame(
    x = d$x,
    y = d$y,
    g = rep(letters[1:2], length.out = nrow(d))
  )
  L <- (vplot(dg) |> mark_hex(x = x, y = y, colour = g))@layers[[1]]
  expect_false("fill" %in% names(L@encoding))
  expect_true("color" %in% names(L@encoding)) # normalised from `colour`
})

test_that("stat hexbin bins to occupied hexes with count + radius", {
  r <- resolve1(vplot(d) |> mark_hex(x = x, y = y, bins = 25))
  expect_gt(r$n, 0)
  expect_lt(r$n, nrow(d)) # binning reduces the row count
  expect_true(is.numeric(r$values$fill)) # count
  expect_equal(sum(r$values$fill), nrow(d)) # every point counted once
  expect_true(is.numeric(r$values$width) && r$values$width[1] > 0) # hex radius
  # `height` (y-data extent) lets the emitter tile hexes at any panel aspect.
  expect_true(is.numeric(r$values$height) && r$values$height[1] > 0)
})

test_that(".hex_round preserves the axial constraint and is integer", {
  q <- c(0.2, 1.7, -0.6)
  r <- c(-0.1, 0.4, 2.2)
  out <- vellumplot:::.hex_round(q, r)
  expect_true(all(out$q == round(out$q)))
  expect_true(all(out$r == round(out$r)))
})

test_that("mark_hex renders (default and coord_fixed)", {
  f <- local_tempfile(fileext = ".png")
  render_plot(vplot(d) |> mark_hex(x = x, y = y, bins = 20), f)
  expect_gt(file.info(f)$size, 0)
  f2 <- local_tempfile(fileext = ".png")
  render_plot(
    vplot(d) |> mark_hex(x = x, y = y, bins = 20) |> coord_fixed(),
    f2
  )
  expect_gt(file.info(f2)$size, 0)
})

test_that("hexbin fills the central region with ink", {
  img <- render_px(vplot(d) |> mark_hex(x = x, y = y, bins = 20))
  expect_true(has_ink(
    img,
    rows = c(0.35, 0.65),
    cols = c(0.35, 0.65),
    thresh = 0.85
  ))
})
