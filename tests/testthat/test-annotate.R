# annotate() + mark_segment.

base <- function() vplot(mtcars) |> mark_point(x = wt, y = mpg)
lastlayer <- function(p) p@layers[[length(p@layers)]]

test_that("each geom builds an own-data layer with the right mark", {
  p <- base() |> annotate("text", x = 4, y = 30, label = "a")
  expect_identical(lastlayer(p)@mark, "text")
  expect_false(is.null(lastlayer(p)@data))

  expect_identical(
    lastlayer(base() |> annotate("point", x = 1, y = 2))@mark,
    "point"
  )
  expect_identical(
    lastlayer(
      base() |> annotate("segment", x = 1, y = 1, xend = 2, yend = 2)
    )@mark,
    "segment"
  )
  expect_identical(
    lastlayer(
      base() |> annotate("rect", xmin = 1, xmax = 2, ymin = 1, ymax = 2)
    )@mark,
    "tile"
  )
})

test_that("annotate errors on unknown geom and missing aesthetics", {
  expect_error(base() |> annotate("wobble", x = 1, y = 1))
  expect_error(base() |> annotate("text", x = 1, y = 1), "label") # no label
  expect_error(base() |> annotate("segment", x = 1, y = 1), "xend") # no xend/yend
})

test_that("annotate geoms render (and flipped)", {
  builds <- list(
    function(p) annotate(p, "text", x = 4, y = 30, label = "n"),
    function(p) annotate(p, "point", x = 2, y = 15, color = "red"),
    function(p) annotate(p, "segment", x = 2, y = 30, xend = 5, yend = 12),
    function(p) {
      annotate(p, "rect", xmin = 3, xmax = 4, ymin = 15, ymax = 20, alpha = 0.3)
    }
  )
  for (b in builds) {
    f <- local_tempfile(fileext = ".png")
    render_plot(b(base()), f)
    expect_gt(file.info(f)$size, 0)
    f2 <- local_tempfile(fileext = ".png")
    render_plot(b(base()) |> coord_flip(), f2)
    expect_gt(file.info(f2)$size, 0)
  }
})

test_that("an annotation appears on every facet panel", {
  p <- base() |>
    facet_wrap(~cyl) |>
    annotate("text", x = 4, y = 30, label = "*")
  built <- vellumplot:::.build_panels(p)
  ns <- vapply(built$panels, function(pp) pp$resolved[[2]]$n, integer(1))
  expect_true(all(ns == 1L))
})

# mark_segment standalone

test_that("mark_segment draws x/y -> xend/yend", {
  d <- data.frame(x = 1:3, y = 1:3, xend = 4:6, yend = 3:1)
  expect_identical(
    (vplot(d) |> mark_segment(x = x, y = y, xend = xend, yend = yend))@layers[[
      1
    ]]@mark,
    "segment"
  )
  f <- local_tempfile(fileext = ".png")
  render_plot(
    vplot(d) |> mark_segment(x = x, y = y, xend = xend, yend = yend),
    f
  )
  expect_gt(file.info(f)$size, 0)
})
