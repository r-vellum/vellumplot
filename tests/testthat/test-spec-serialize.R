# Phase A: the spec serializer (as_spec / from_spec / JSON bridge).

# The encoding-level part of a spec (everything but the data payload, whose hash
# is sensitive to trivial type/rowname differences on reconstruction).
enc_part <- function(spec) spec[setdiff(names(spec), "data")]

test_that("a basic plot round-trips structurally (idempotent IR)", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, color = cyl, size = 3) |>
    labs(title = "T", x = "Weight")
  s1 <- as_spec(p)
  p2 <- from_spec(s1)
  s2 <- as_spec(p2)
  expect_identical(enc_part(s1), enc_part(s2))
})

test_that("reconstructed plot compiles to a scene", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    mark_smooth(x = wt, y = mpg)
  p2 <- from_spec(as_spec(p))
  expect_no_error(vellum::as_vellum_scene(p2))
})

test_that("layers preserve mark, stat, params, position, encoding", {
  p <- vplot(mtcars) |>
    mark_histogram(x = mpg, bins = 20L) |>
    mark_bar(x = gear, position = "dodge")
  s <- as_spec(p)
  expect_identical(s$layers[[1]]$mark, "bar")
  expect_identical(s$layers[[1]]$stat, "bin")
  expect_identical(s$layers[[1]]$stat_params$bins, 20L)
  expect_identical(s$layers[[2]]$position, "dodge")
  expect_identical(names(s$layers[[1]]$encoding), "x")
  expect_identical(s$layers[[1]]$encoding$x$field, "mpg")
})

test_that("expression channels and after_stat round-trip", {
  p <- vplot(mtcars) |> mark_point(x = log(wt), y = mpg, color = factor(cyl))
  s <- as_spec(p)
  expect_identical(s$layers[[1]]$encoding$x$expr, "log(wt)")
  expect_identical(s$layers[[1]]$encoding$color$expr, "factor(cyl)")
  expect_null(s$layers[[1]]$encoding$x$field)
  p2 <- from_spec(s)
  expect_identical(as_spec(p2)$layers[[1]]$encoding$color$expr, "factor(cyl)")
})

test_that("scales round-trip (continuous, manual, gradient2)", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, color = hp) |>
    scale_x_continuous(trans = "log10", limits = c(1, 6)) |>
    scale_color_gradient2(midpoint = 100)
  s1 <- as_spec(p)
  s2 <- as_spec(from_spec(s1))
  expect_identical(enc_part(s1)$scales, enc_part(s2)$scales)
  xsc <- Filter(function(sc) sc$aesthetic == "x", s1$scales)[[1]]
  expect_identical(xsc$trans, "log10")
})

test_that("coord and facet round-trip", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    coord_flip() |>
    facet_wrap(~cyl, ncol = 2L)
  s1 <- as_spec(p)
  s2 <- as_spec(from_spec(s1))
  expect_identical(s1$coord$kind, "flip")
  expect_identical(s1$facet$type, "wrap")
  expect_identical(unlist(s1$facet$cols), "cyl")
  expect_identical(s1$facet, s2$facet)
})

test_that("theme round-trips by preset name plus settings", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    theme_minimal() |>
    theme(legend.position = "bottom")
  s <- as_spec(p)
  expect_identical(s$theme$preset, "minimal")
  expect_identical(s$theme$settings$legend.position, "bottom")
  p2 <- from_spec(s)
  expect_identical(as_spec(p2)$theme$preset, "minimal")
})

test_that("JSON round-trip is idempotent", {
  skip_if_not_installed("jsonlite")
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, color = cyl) |>
    facet_wrap(~gear)
  json <- spec_to_json(p)
  expect_type(json, "character")
  p2 <- spec_from_json(json)
  expect_identical(enc_part(as_spec(p)), enc_part(as_spec(p2)))
})

test_that("small data inlines and rebuilds; typed columns survive", {
  df <- data.frame(
    d = as.Date("2020-01-01") + 0:4,
    g = factor(c("a", "b", "a", "b", "a"), levels = c("b", "a")),
    v = 1:5
  )
  p <- vplot(df) |> mark_point(x = d, y = v, color = g)
  s <- as_spec(p)
  expect_false(is.null(s$data$values))
  p2 <- from_spec(s)
  expect_s3_class(p2@data$d, "Date")
  expect_identical(levels(p2@data$g), c("b", "a"))
  expect_identical(p2@data$v, 1:5)
})

test_that("large data stores by reference and needs from_spec(data=)", {
  df <- data.frame(x = 1:3000, y = rnorm(3000))
  p <- vplot(df) |> mark_point(x = x, y = y)
  s <- as_spec(p)
  expect_null(s$data[["values"]])
  expect_true(nzchar(s$data$hash))
  expect_error(from_spec(s), class = "vellumplot_missing_data")
  p2 <- from_spec(s, data = df)
  expect_identical(nrow(p2@data), 3000L)
})

test_that("unserializable pieces are refused with a classed error, not dropped", {
  # a secondary axis carries a transform closure
  p_sec <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    scale_y_continuous(sec.axis = sec_axis(~ . * 2))
  expect_error(as_spec(p_sec), class = "vellumplot_unserializable")

  # per-layer data is not serializable
  p_dat <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, data = head(mtcars))
  expect_error(as_spec(p_dat), class = "vellumplot_unserializable")
})

test_that("effects round-trip", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, effects = list(glow(size = 4)))
  s <- as_spec(p)
  expect_identical(s$layers[[1]]$effects[[1]]$effect, "glow")
  expect_identical(s$layers[[1]]$effects[[1]]$size, 4)
  p2 <- from_spec(s)
  expect_s7_class(p2@layers[[1]]@effects[[1]], GlowSpec)
})

test_that("spec_schema is valid JSON and self-describes", {
  skip_if_not_installed("jsonlite")
  sch <- spec_schema(as = "list")
  expect_identical(sch$title, "vellumplot spec (v1)")
  expect_true("layers" %in% names(sch$properties))
})
