# trained_scale(): the typed constructor + validator every trainer builds its
# scale through (R/trained-scale.R).

axis_scale <- function(...) {
  args <- utils::modifyList(
    list(
      aesthetic = "x",
      kind = "continuous",
      map = identity,
      discrete = FALSE,
      data_range = c(0, 1),
      domain = c(0, 1),
      breaks = c(0, 0.5, 1),
      labels = c("0", "0.5", "1")
    ),
    list(...)
  )
  do.call(trained_scale, args)
}

test_that("every trained scale of a plot is a valid typed scale with a kind", {
  df <- data.frame(x = 1:3, y = c(2, 4, 8), g = c("a", "b", "c"), s = 1:3)
  p <- vplot(df) |> mark_point(x = x, y = y, color = g, size = s, shape = g)
  scales <- .build_panels(p)$scales
  for (k in c("x", "y", "color", "size", "shape")) {
    sc <- scales[[k]]
    expect_s3_class(sc, "vellum_trained_scale")
    expect_identical(sc$aesthetic, k)
    expect_length(.trained_scale_problems(sc), 0L)
  }
  # Position and legend scales name their kind the same way.
  expect_identical(scales$x$kind, "continuous")
  expect_identical(scales$color$kind, "discrete")
  expect_identical(scales$size$kind, "size")
})

test_that("the constructor accepts a well-formed axis and rejects bad kinds", {
  expect_s3_class(axis_scale(), "vellum_trained_scale")
  expect_error(axis_scale(kind = "size"), "kind")
  expect_error(
    trained_scale("nope", "continuous", map = identity),
    "registered scale key"
  )
  expect_error(
    trained_scale("alpha", "size", map = identity),
    "kind"
  )
})

test_that("axis invariants: discrete flag, band width, one label per break", {
  expect_error(axis_scale(discrete = TRUE), "discrete")
  expect_error(axis_scale(band_width = 1), "band_width")
  expect_error(
    axis_scale(kind = "binned", discrete = FALSE),
    "band_width"
  )
  expect_error(axis_scale(labels = c("0", "1")), "one label per break")
  expect_error(axis_scale(no_guide = TRUE), "guide flags")
})

test_that("legend invariants: paired key vectors and flags", {
  ok <- trained_scale(
    "shape",
    "shape",
    map = identity,
    levels = c("a", "b"),
    shapes = c("circle", "square")
  )
  expect_s3_class(ok, "vellum_trained_scale")
  expect_error(
    trained_scale(
      "shape",
      "shape",
      map = identity,
      levels = c("a", "b"),
      shapes = "circle"
    ),
    "equal lengths"
  )
  expect_error(
    trained_scale("color", "discrete", map = identity, levels = "a"),
    "needs"
  )
  expect_error(
    trained_scale("color", "identity", map = identity),
    "identity = TRUE"
  )
  expect_error(
    .scale_set(ok, nested = "yes"),
    "TRUE/FALSE"
  )
})

test_that(".scale_set() sets, drops and re-checks fields", {
  sc <- axis_scale()
  sc2 <- .scale_set(sc, domain = c(-1, 2))
  expect_identical(sc2$domain, c(-1, 2))
  expect_s3_class(sc2, "vellum_trained_scale")
  expect_null(.scale_set(sc, sec = NULL)$sec)
  expect_error(.scale_set(sc, breaks = 1), "one label per break")
  expect_error(.scale_set(sc, kind = "discrete"), "discrete")
})

test_that("identity scales draw no guide", {
  sc <- .identity_scale("size", "size", as.numeric, "s")
  expect_true(sc$identity)
  expect_true(sc$no_guide)
  expect_error(
    trained_scale("size", "size", map = identity, identity = TRUE),
    "no guide"
  )
})
