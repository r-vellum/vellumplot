# trained_scale(): the typed constructor + validator every trainer builds its
# scale through (R/trained-scale.R).

axis_scale <- function(...) {
  args <- utils::modifyList(
    list(
      aesthetic = "x",
      type = "continuous",
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

test_that("every trained scale of a plot is a valid scale with a type", {
  df <- data.frame(x = 1:3, y = c(2, 4, 8), g = c("a", "b", "c"), s = 1:3)
  p <- vplot(df) |> mark_point(x = x, y = y, color = g, size = s, shape = g)
  scales <- .build_panels(p)$scales
  for (k in c("x", "y", "color", "size", "shape")) {
    sc <- scales[[k]]
    expect_s3_class(sc, "vellum_trained_scale")
    expect_identical(sc$aesthetic, k)
    expect_length(.trained_scale_problems(sc), 0L)
  }
  # `type` is the scale type for every scale; the aesthetic is its own field.
  expect_identical(scales$x$type, "continuous")
  expect_identical(scales$color$type, "discrete")
  expect_identical(scales$size$type, "continuous")
  expect_identical(scales$shape$type, "discrete")
  expect_null(scales$size$kind)
})

test_that("every legend scale of a plot has a type, and identity scales too", {
  df <- data.frame(
    x = 1:3,
    y = 1:3,
    g = c("a", "b", "c"),
    s = 1:3,
    col = c("red", "blue", "green")
  )
  p <- vplot(df) |>
    mark_line(
      x = x,
      y = y,
      linetype = g,
      alpha = s,
      linewidth = s
    ) |>
    mark_point(x = x, y = y, size = s, color = col) |>
    scale_color_identity()
  sc <- .build_panels(p)$scales
  expect_identical(sc$linetype$type, "discrete")
  expect_identical(sc$alpha$type, "continuous")
  expect_identical(sc$linewidth$type, "continuous")
  expect_identical(sc$color$type, "identity")
  expect_true(sc$color$identity)
})

test_that("the constructor accepts a well-formed axis and rejects bad types", {
  expect_s3_class(axis_scale(), "vellum_trained_scale")
  expect_error(axis_scale(type = "size"), "type")
  expect_error(
    trained_scale("nope", "continuous", map = identity),
    "registered scale key"
  )
  expect_error(
    trained_scale("alpha", "size", map = identity),
    "type"
  )
  expect_error(
    trained_scale("shape", "continuous", map = identity),
    "type"
  )
})

test_that("axis invariants: discrete flag, band width, one label per break", {
  expect_error(axis_scale(discrete = TRUE), "discrete")
  expect_error(axis_scale(band_width = 1), "band_width")
  expect_error(
    axis_scale(type = "binned", discrete = FALSE),
    "band_width"
  )
  expect_error(axis_scale(labels = c("0", "1")), "one label per break")
  expect_error(axis_scale(no_guide = TRUE), "guide flags")
})

test_that("legend invariants: paired key vectors and flags", {
  ok <- trained_scale(
    "shape",
    "discrete",
    map = identity,
    levels = c("a", "b"),
    shapes = c("circle", "square")
  )
  expect_s3_class(ok, "vellum_trained_scale")
  expect_error(
    trained_scale(
      "shape",
      "discrete",
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

test_that("nested is for size scales and stepped for binned ones", {
  shp <- trained_scale(
    "shape",
    "discrete",
    map = identity,
    levels = "a",
    shapes = "circle"
  )
  expect_error(.scale_set(shp, nested = TRUE), "size")
  expect_error(.scale_set(shp, stepped = TRUE), "binned")
  sz <- trained_scale(
    "size",
    "continuous",
    map = identity,
    range = c(0, 1),
    legend_breaks = 0.5,
    legend_labels = "0.5"
  )
  expect_true(.scale_set(sz, nested = TRUE)$nested)
  # the guides only set the flags where they apply
  df <- data.frame(x = 1:3, y = 1:3, g = c("a", "b", "c"), v = 1:3)
  p <- vplot(df) |>
    mark_point(x = x, y = y, shape = g, color = v) |>
    guides(
      shape = guide_legend(nested = TRUE),
      color = guide_coloursteps()
    )
  sc <- .build_panels(p)$scales
  expect_null(sc$shape$nested)
  expect_null(sc$color$stepped)
})

test_that(".scale_set() sets, drops and re-checks fields", {
  sc <- axis_scale()
  sc2 <- .scale_set(sc, domain = c(-1, 2))
  expect_identical(sc2$domain, c(-1, 2))
  expect_s3_class(sc2, "vellum_trained_scale")
  expect_null(.scale_set(sc, sec = NULL)$sec)
  expect_error(.scale_set(sc, breaks = 1), "one label per break")
  expect_error(.scale_set(sc, type = "discrete"), "discrete")
})

test_that("identity scales draw no guide", {
  sc <- .identity_scale("size", as.numeric, "s")
  expect_identical(sc$type, "identity")
  expect_true(sc$identity)
  expect_true(sc$no_guide)
  expect_error(
    trained_scale("size", "identity", map = identity, identity = TRUE),
    "no guide"
  )
  expect_error(
    trained_scale("size", "identity", map = identity, no_guide = TRUE),
    "identity = TRUE"
  )
})

test_that("provenance keeps its public scale types", {
  df <- data.frame(x = 1:3, y = 1:3, g = c("a", "b", "c"), s = 1:3)
  p <- vplot(df) |> mark_point(x = x, y = y, color = g, size = s, shape = g)
  sc <- .build_panels(p)$scales
  expect_identical(.scale_public_type("x", sc$x), "continuous")
  expect_identical(.scale_public_type("color", sc$color), "discrete")
  expect_identical(.scale_public_type("size", sc$size), "size")
  expect_identical(.scale_public_type("shape", sc$shape), "shape")
  ea <- trained_scale(
    "edge_alpha",
    "continuous",
    map = identity,
    range = c(0, 1),
    legend_breaks = 0.5,
    legend_labels = "0.5"
  )
  expect_identical(.scale_public_type("edge_alpha", ea), "alpha")
  refs <- .layer_channels(.build_panels(p)$panels[[1]]$resolved[[1]], sc)
  expect_identical(refs$size$type, "size")
  expect_identical(refs$color$type, "discrete")
})
