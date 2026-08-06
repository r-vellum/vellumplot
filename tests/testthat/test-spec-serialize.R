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
  # `bins` is tagged in the IR so its integer type survives JSON (see
  # test-review4-batch4.R); the guarantee is that the round-trip restores it.
  expect_identical(from_spec(s)@layers[[1]]@stat_params$bins, 20L)
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

# --- REVIEW3 D1: slot-list constants drive both round-trip directions --------

test_that("a polar coord round-trips every non-default slot", {
  p <- vplot(mtcars) |>
    mark_bar(x = factor(cyl)) |>
    coord_radial(
      theta = "y",
      start = 0.5,
      end = 3,
      direction = -1,
      inner_radius = 0.3
    )
  co <- from_spec(as_spec(p))@coord
  expect_identical(co@kind, "polar")
  expect_identical(co@theta, "y")
  expect_equal(co@start, 0.5)
  expect_equal(co@end, 3)
  expect_equal(co@direction, -1)
  expect_equal(co@rmin, 0.3)
})

test_that("a coord_trans round-trips its per-axis transforms", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    coord_trans(x = "log10", y = "sqrt")
  co <- from_spec(as_spec(p))@coord
  expect_identical(co@kind, "trans")
  expect_identical(co@xtrans, "log10")
  expect_identical(co@ytrans, "sqrt")
})

test_that("marginal distributions round-trip with their field types intact", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    add_marginal(
      type = "histogram",
      sides = "t",
      size = 0.2,
      bins = 12L,
      group = TRUE
    )
  m <- from_spec(as_spec(p))@marginal
  expect_identical(m@type, "histogram")
  expect_identical(m@sides, "t")
  expect_equal(m@size, 0.2)
  expect_type(m@bins, "integer")
  expect_identical(m@bins, 12L)
  expect_true(m@group)
  # an unspecified field falls back to the MarginalSpec default, not a NULL
  expect_equal(m@adjust, 1)
})

test_that("interactivity/animation are refused (not silently dropped) by as_spec()", {
  d <- data.frame(x = 1:6, y = 1:6, g = rep(c("a", "b"), 3))
  base <- vplot(d) |> mark_point(x = x, y = y)
  expect_error(
    as_spec(base |> select_point("s")),
    class = "vellumplot_unserializable"
  )
  expect_error(
    as_spec(base |> select_point("s") |> filter_by("s")),
    class = "vellumplot_unserializable"
  )
  expect_error(
    as_spec(base |> transition_states(g)),
    class = "vellumplot_unserializable"
  )
  expect_error(
    as_spec(base |> inspect_source()),
    class = "vellumplot_unserializable"
  )
  # a plain plot still round-trips
  expect_no_error(from_spec(as_spec(base)))
})

test_that("ordered factors and POSIXct time zones survive a round-trip", {
  d <- data.frame(
    v = ordered(c("lo", "hi", "lo"), levels = c("lo", "hi")),
    t = as.POSIXct("2020-06-01 12:00:00", tz = "America/New_York"),
    y = 1:3
  )
  q <- from_spec(as_spec(vplot(d) |> mark_point(x = v, y = y)))
  expect_true(is.ordered(q@data$v)) # ordering preserved (ordinal, not nominal)
  expect_identical(levels(q@data$v), c("lo", "hi"))
  expect_identical(attr(q@data$t, "tzone"), "America/New_York") # display zone kept
  expect_equal(as.numeric(q@data$t), as.numeric(d$t)) # same instant
})
