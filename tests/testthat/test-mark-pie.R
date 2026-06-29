# mark_pie() / mark_donut(): stacked-bar + coord_polar(theta = "y") shortcuts,
# plus area/ribbon densification under polar.

df <- data.frame(part = c("a", "b", "c", "d"), n = c(3, 5, 2, 4))

test_that("mark_pie appends a stacked bar and sets a polar coord", {
  p <- vplot(df) |> mark_pie(value = n, fill = part)
  expect_length(p@layers, 1)
  L <- p@layers[[1]]
  expect_identical(L@mark, "bar")
  expect_identical(L@position, "stack")
  # value -> y, a constant single-band x, fill mapped
  expect_true("y" %in% names(L@encoding))
  expect_true("x" %in% names(L@encoding))
  expect_true("fill" %in% names(L@encoding))
  expect_identical(p@coord@kind, "polar")
  expect_identical(p@coord@theta, "y")
  expect_equal(p@coord@rmin, 0)
})

test_that("mark_donut sets the inner-hole radius", {
  p <- vplot(df) |> mark_donut(value = n, fill = part, hole = 0.6)
  expect_identical(p@coord@kind, "polar")
  expect_equal(p@coord@rmin, 0.6)
  expect_error(vplot(df) |> mark_donut(value = n, hole = 1.5))
})

test_that("mark_pie errors on a conflicting non-polar coord", {
  expect_error(
    vplot(df) |> coord_cartesian() |> mark_pie(value = n, fill = part),
    "polar"
  )
})

test_that("mark_pie keeps an existing polar coord (e.g. start/direction)", {
  p <- vplot(df) |>
    coord_polar(theta = "y", direction = -1) |>
    mark_pie(value = n, fill = part)
  expect_equal(p@coord@direction, -1)
})

test_that("mark_pie renders slices in the matching fills", {
  p <- vplot(df) |> mark_pie(value = n, fill = part)
  img <- render_px(p)
  pal <- vellumplot:::.qual_palette(4)
  hex2rgb <- function(h) grDevices::col2rgb(h)[, 1] / 255
  for (i in seq_len(4)) {
    expect_gt(count_near(img, hex2rgb(pal[i])), 300)
  }
})

test_that("mark_donut leaves an un-filled hole at the centre", {
  # the innermost radius is empty: far fewer slice-fill pixels in a central box
  # than the equivalent pie.
  pie <- vplot(df) |> mark_pie(value = n, fill = part)
  donut <- vplot(df) |> mark_donut(value = n, fill = part, hole = 0.6)
  box <- function(img) {
    H <- dim(img)[1]
    W <- dim(img)[2]
    img[
      seq(floor(0.45 * H), ceiling(0.55 * H)),
      seq(floor(0.3 * W), ceiling(0.5 * W)),
      1:3,
      drop = FALSE
    ]
  }
  pal <- vellumplot:::.qual_palette(4)
  hex2rgb <- function(h) grDevices::col2rgb(h)[, 1] / 255
  fillpx <- function(sub) {
    sum(vapply(
      seq_len(4),
      function(i) {
        rgb <- hex2rgb(pal[i])
        sum(
          abs(sub[,, 1] - rgb[1]) < 0.06 &
            abs(sub[,, 2] - rgb[2]) < 0.06 &
            abs(sub[,, 3] - rgb[3]) < 0.06
        )
      },
      numeric(1)
    ))
  }
  expect_lt(fillpx(box(render_px(donut))), fillpx(box(render_px(pie))))
})

test_that("polar area and ribbon render (densified bands)", {
  set.seed(1)
  d2 <- data.frame(a = 0:11, lo = runif(12, 1, 2))
  d2$hi <- d2$lo + runif(12, 2, 3)
  expect_no_error(render_px(
    vplot(d2) |>
      mark_ribbon(x = a, ymin = lo, ymax = hi) |>
      coord_polar(theta = "x")
  ))
  expect_no_error(render_px(
    vplot(d2) |> mark_area(x = a, y = hi) |> coord_polar(theta = "x")
  ))
})
