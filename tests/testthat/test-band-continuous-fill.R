# A filled connected mark (area / ribbon) is one polygon per series, and a
# polygon takes one fill. A continuous fill varying along a series used to split
# the band into one sliver polygon per vertex; it now errors, as in ggplot2. A
# continuous value constant within each series is legitimate and still draws.

compile <- function(p) vellum::as_vellum_scene(p)
polygons <- function(p) {
  el <- vellum::scene_model(compile(p))$elements
  el[el$mark == "polygon", ]
}

d <- data.frame(x = 1:10, y = (1:10)^1.3, lo = 0, v = 1:10)
d2 <- data.frame(
  x = rep(1:5, 2),
  lo = 0,
  hi = c(1:5, 2:6),
  id = rep(c("a", "b"), each = 5),
  v = rep(c(1, 2), each = 5)
)

test_that("a continuous fill varying along an area or ribbon errors", {
  expect_snapshot(error = TRUE, {
    compile(vplot(d) |> mark_area(x = x, y = y, fill = v))
    compile(vplot(d) |> mark_ribbon(x = x, ymin = lo, ymax = y, fill = v))
  })
  # `color` paints an area's fill too, so it is refused the same way.
  expect_error(
    compile(vplot(d) |> mark_area(x = x, y = y, color = v)),
    "can't vary color continuously"
  )
  # A mapped continuous alpha is one opacity per polygon as well.
  expect_error(
    compile(vplot(d) |> mark_area(x = x, y = y, alpha = v)),
    "can't vary alpha continuously"
  )
  # Varying within one of several groups still errors.
  d3 <- transform(d2, v = c(1, 1, 1, 1, 1, 2:6))
  expect_error(
    compile(
      vplot(d3) |>
        mark_ribbon(x = x, ymin = lo, ymax = hi, fill = v, group = id)
    ),
    "mark_ribbon"
  )
})

test_that("a continuous fill constant within each group draws one band per group", {
  p <- vplot(d2) |>
    mark_ribbon(
      x = x,
      ymin = lo,
      ymax = hi,
      fill = v,
      group = id,
      data_id = id
    )
  expect_no_error(compile(p))
  expect_identical(polygons(p)$key, c("a", "b"))
  pa <- vplot(d2) |>
    mark_area(x = x, y = hi, fill = v, group = id, position = "identity")
  expect_no_error(compile(pa))
  # One continuous value over the whole layer is also fine.
  one <- vplot(transform(d, v = 5)) |>
    mark_area(x = x, y = y, fill = v, data_id = "all")
  expect_identical(nrow(polygons(one)), 1L)
})

test_that("discrete fills and constant fills are unaffected", {
  p <- vplot(d2) |>
    mark_ribbon(x = x, ymin = lo, ymax = hi, fill = id, data_id = id)
  expect_identical(polygons(p)$key, c("a", "b"))
  expect_no_error(compile(
    vplot(d) |> mark_area(x = x, y = y, fill = "steelblue")
  ))
  expect_no_error(compile(vplot(d) |> mark_area(x = x, y = y, alpha = 0.5)))
})
