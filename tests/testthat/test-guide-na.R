# NA legend keys for mapped size / shape aesthetics. Also: NA in a shape
# aesthetic no longer errors (it maps to a neutral shape). Phase 4.

resolved_one <- function(aes, values) {
  list(list(mark = "point", values = stats::setNames(list(values), aes)))
}

test_that("NA in a shape aesthetic renders (no crash) and adds an NA key", {
  df <- data.frame(x = 1:6, y = 1:6, g = c("a", "b", "a", "b", NA, NA))
  p <- vplot(df) |> mark_point(x = x, y = y, shape = g)
  f <- local_tempfile(fileext = ".svg")
  expect_no_error(render_plot(p, f)) # previously errored: "Unknown point shape: NA"

  sc <- .train_shape(p, resolved_one("shape", df$g))
  expect_true(isTRUE(sc$na))
  expect_identical(
    .guide_labels(list(kind = "shape", sc = sc)),
    c("a", "b", "NA")
  )
})

test_that("NA in a size aesthetic adds an NA key", {
  df <- data.frame(x = 1:6, y = 1:6, s = c(1, 2, 3, 4, NA, NA))
  p <- vplot(df) |> mark_point(x = x, y = y, size = s)
  f <- local_tempfile(fileext = ".svg")
  # the two NA rows draw no point, with ggplot2's warning (VR-38)
  expect_warning(render_plot(p, f), class = "vellumplot_missing_rows")

  sc <- .train_size(p, resolved_one("size", df$s))
  expect_true(isTRUE(sc$na))
  expect_identical(tail(.guide_labels(list(kind = "size", sc = sc)), 1L), "NA")
})

test_that("a horizontal continuous colour legend draws the NA key (H30)", {
  df <- data.frame(x = 1:6, y = 1:6, z = c(1, 2, 3, 4, NA, NA))
  svg_of <- function(p) {
    f <- local_tempfile(fileext = ".svg")
    render_plot(p, f)
    paste(readLines(f), collapse = "\n")
  }
  # bottom (horizontal) legend previously dropped the NA key that the vertical
  # legend always drew.
  bottom <- vplot(df) |>
    mark_point(x = x, y = y, color = z) |>
    theme(legend.position = "bottom")
  expect_match(svg_of(bottom), ">NA<")
  # control: no missing data -> no NA key
  df2 <- data.frame(x = 1:4, y = 1:4, z = 1:4)
  clean <- vplot(df2) |>
    mark_point(x = x, y = y, color = z) |>
    theme(legend.position = "bottom")
  expect_no_match(svg_of(clean), ">NA<")
})

test_that("no NA key when the aesthetic has no missing values (additive)", {
  df <- data.frame(
    x = 1:4,
    y = 1:4,
    g = c("a", "b", "a", "b"),
    s = c(1, 2, 3, 4)
  )
  ps <- vplot(df) |> mark_point(x = x, y = y, shape = g)
  sc_sh <- .train_shape(ps, resolved_one("shape", df$g))
  expect_false(isTRUE(sc_sh$na))
  expect_false("NA" %in% .guide_labels(list(kind = "shape", sc = sc_sh)))

  sc_sz <- .train_size(ps, resolved_one("size", df$s))
  expect_false(isTRUE(sc_sz$na))
  expect_false("NA" %in% .guide_labels(list(kind = "size", sc = sc_sz)))
})

# VR-38: a missing mapped size drew a NaN-sized marker (a NaN path in SVG, a
# crash in PDF). It is now left out, with ggplot2's "Removed n rows" warning.
point_count <- function(p) {
  m <- vellum::scene_model(suppressWarnings(vellum::as_vellum_scene(p)))
  sum(m$elements$mark == "point" & grepl("^panel-", m$elements$panel))
}

test_that("rows with a missing size draw no point and warn (VR-38)", {
  df <- data.frame(x = 1:6, y = 1:6, s = c(1, 2, 3, 4, NA, NA))
  p <- vplot(df) |> mark_point(x = x, y = y, size = s)
  expect_warning(
    vellum::as_vellum_scene(p),
    "Removed 2 rows containing missing values (layer 1, `mark_point()`).",
    fixed = TRUE,
    class = "vellumplot_missing_rows"
  )
  expect_identical(point_count(p), 4L)
  f <- local_tempfile(fileext = ".svg")
  suppressWarnings(render_plot(p, f))
  expect_false(any(grepl("NaN", readLines(f, warn = FALSE), fixed = TRUE)))
  # the warning is summed over panels: one per layer
  pf <- p |> facet_wrap(~ x > 3)
  w <- character(0)
  withCallingHandlers(
    vellum::as_vellum_scene(pf),
    vellumplot_missing_rows = function(c) {
      w <<- c(w, conditionMessage(c))
      invokeRestart("muffleWarning")
    }
  )
  expect_length(w, 1L)
  expect_match(w, "Removed 2 rows", fixed = TRUE)
})

test_that("a size censored by the scale limits warns too (VR-38)", {
  df <- data.frame(x = 1:4, y = 1:4, s = c(1, 2, 3, 10))
  p <- vplot(df) |>
    mark_point(x = x, y = y, size = s) |>
    scale_size(limits = c(1, 5), oob = "censor")
  expect_warning(
    vellum::as_vellum_scene(p),
    "Removed 1 row containing",
    class = "vellumplot_missing_rows"
  )
})

test_that("a discrete size with na.value still draws its NA rows (VR-38)", {
  df <- data.frame(x = 1:4, y = 1:4, g = c("a", "b", NA, "a"))
  p <- vplot(df) |>
    mark_point(x = x, y = y, size = g) |>
    scale_size_manual(values = c(a = 2, b = 4), na.value = 1)
  expect_no_warning(vellum::as_vellum_scene(p), class = "vellumplot_missing_rows")
  expect_identical(point_count(p), 4L)
  # without one the NA row has no size, so it is left out
  q <- vplot(df) |>
    mark_point(x = x, y = y, size = g) |>
    scale_size_manual(values = c(a = 2, b = 4))
  expect_warning(
    vellum::as_vellum_scene(q),
    "Removed 1 row containing",
    class = "vellumplot_missing_rows"
  )
})

test_that("no warning without missing sizes (VR-38)", {
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg, size = hp)
  expect_no_warning(vellum::as_vellum_scene(p), class = "vellumplot_missing_rows")
})
