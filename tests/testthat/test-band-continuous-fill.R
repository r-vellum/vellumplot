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

test_that("violins and ridgelines never split on a continuous fill (VP-34)", {
  # Unlike areas before #204, the density marks draw one polygon per category
  # cell (`.by_category()`), whatever the fill does within it -- so a fill that
  # varies along the cell cannot shatter the shape into slivers.
  set.seed(1)
  dd <- data.frame(
    g = rep(c("a", "b"), each = 30),
    y = stats::rnorm(60),
    z = stats::runif(60)
  )
  # Provenance records one entry per emitted grob.
  grobs <- function(p, mark) {
    pr <- attr(compile(p), "vellumplot_provenance")
    Filter(function(e) identical(e$mark, mark), pr)
  }
  # (the varying fill itself is dropped with a warning, VP-37)
  expect_length(
    suppressWarnings(
      grobs(vplot(dd) |> mark_violin(x = g, y = y, fill = z), "violin")
    ),
    2L
  )
  expect_length(
    suppressWarnings(
      grobs(vplot(dd) |> mark_ridgeline(x = y, y = g, fill = z), "ridgeline")
    ),
    2L
  )
})

test_that("a fill varying within a violin or ridge is dropped, as ggplot2 does (VP-37)", {
  set.seed(1)
  dd <- data.frame(
    g = rep(c("a", "b"), each = 30),
    y = stats::rnorm(60),
    z = stats::runif(60),
    m = rep(c(1, 5), each = 30)
  )
  expect_warning(
    r <- .resolve_layers(
      vplot(dd) |> mark_violin(x = g, y = y, fill = z)
    )[[1]],
    class = "vellumplot_dropped_aes"
  )
  expect_null(r$values$fill)
  # no colour scale (and so no gradient legend) is trained for it
  p <- vplot(dd) |> mark_ridgeline(x = y, y = g, fill = z)
  sc <- suppressWarnings(
    .train_scales(p, .resolve_layers(p))
  )
  expect_null(sc$color)
  # a horizontal violin checks its cells along y
  expect_warning(
    .resolve_layers(
      vplot(dd) |> mark_violin(x = y, y = g, alpha = z)
    ),
    "alpha"
  )
  # a continuous fill constant within each category is kept, with its legend
  expect_no_warning(
    .resolve_layers(vplot(dd) |> mark_violin(x = g, y = y, fill = m))
  )
  r2 <- .resolve_layers(vplot(dd) |> mark_violin(x = g, y = y, fill = m))[[1]]
  expect_equal(unique(r2$values$fill), c(1, 5))
})
