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

test_that("a continuous size shows no NA key; a discrete one with na.value does (VR-38)", {
  df <- data.frame(x = 1:6, y = 1:6, s = c(1, 2, 3, 4, NA, NA))
  p <- vplot(df) |> mark_point(x = x, y = y, size = s)
  f <- local_tempfile(fileext = ".svg")
  # the two NA rows draw no point, with ggplot2's warning
  expect_warning(render_plot(p, f), class = "vellumplot_missing_rows")
  expect_false(any(grepl(">NA<", readLines(f, warn = FALSE), fixed = TRUE)))
  # ... so, as in ggplot2, the legend has no NA key to describe them
  sc <- .train_size(p, resolved_one("size", df$s))
  expect_false(isTRUE(sc$na))
  expect_false("NA" %in% .guide_labels(list(kind = "size", sc = sc)))

  dg <- data.frame(x = 1:4, y = 1:4, g = c("a", "b", NA, "a"))
  pg <- vplot(dg) |>
    mark_point(x = x, y = y, size = g) |>
    scale_size_manual(values = c(a = 2, b = 4), na.value = 1)
  sg <- .train_size(pg, resolved_one("size", dg$g))
  expect_identical(tail(.guide_labels(list(kind = "size", sc = sg)), 1L), "NA")
  # without na.value the NA level draws nothing, and has no key
  pn <- vplot(dg) |>
    mark_point(x = x, y = y, size = g) |>
    scale_size_manual(values = c(a = 2, b = 4))
  sn <- .train_size(pn, resolved_one("size", dg$g))
  expect_false("NA" %in% .guide_labels(list(kind = "size", sc = sn)))
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
    "Removed 1 row containing missing values or values outside the scale range (layer 1, `mark_point()`).",
    fixed = TRUE,
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

# Every "Removed n rows" warning a compile raises, muffled.
missing_row_warnings <- function(expr) {
  w <- character(0)
  withCallingHandlers(
    expr,
    vellumplot_missing_rows = function(c) {
      w <<- c(w, conditionMessage(c))
      invokeRestart("muffleWarning")
    }
  )
  w
}

test_that("a missing position and a missing size give one warning (VR-38)", {
  d <- mtcars
  d$hp[c(3, 7)] <- NA
  d$wt[1] <- NA
  w <- missing_row_warnings(
    vellum::as_vellum_scene(vplot(d) |> mark_point(x = wt, y = mpg, size = hp))
  )
  expect_identical(
    w,
    "Removed 3 rows containing missing values (layer 1, `mark_point()`)."
  )
})

test_that("every mark that maps size leaves out a missing size (VR-38)", {
  df <- data.frame(
    x = 1:4,
    y = c(1, 3, 2, 4),
    s = c(1, NA, 3, 4),
    lab = c("a", "b", "c", "d")
  )
  base <- vplot(df)
  marks <- list(
    text = base |> mark_text(x = x, y = y, label = lab, size = s),
    label = base |> mark_label(x = x, y = y, label = lab, size = s),
    pointrange = base |>
      mark_pointrange(x = x, y = y, ymin = y - 1, ymax = y + 1, size = s)
  )
  for (nm in names(marks)) {
    w <- missing_row_warnings(sc <- vellum::as_vellum_scene(marks[[nm]]))
    expect_length(w, 1L)
    expect_match(w, "Removed 1 row containing missing values", fixed = TRUE)
    f <- local_tempfile(fileext = ".pdf")
    expect_no_error(vellum::render(sc, f))
  }
  # a point-range leaves out the whole row: range and point
  m <- vellum::scene_model(suppressWarnings(
    vellum::as_vellum_scene(marks$pointrange)
  ))
  el <- m$elements[grepl("^panel-", m$elements$panel), ]
  expect_identical(sum(el$mark == "point"), 3L)
})

test_that("a constant missing size warns rather than drawing nothing silently (VR-38)", {
  w <- missing_row_warnings(
    vellum::as_vellum_scene(vplot(head(mtcars)) |> mark_point(x = wt, y = mpg, size = NA))
  )
  expect_identical(
    w,
    "Removed 6 rows containing missing values (layer 1, `mark_point()`)."
  )
})

test_that("a node with a missing size keeps the edges drawn (VR-38)", {
  skip_if_not_installed("igraph")
  g <- igraph::make_ring(5)
  igraph::V(g)$w <- c(1, NA, 3, 4, 5)
  p <- vgraph(g, layout = "circle") |>
    mark_edges(arrow = TRUE) |>
    mark_nodes(size = w)
  w <- missing_row_warnings(sc <- vellum::as_vellum_scene(p))
  expect_match(w, "Removed 1 row containing missing values", fixed = TRUE)
  m <- vellum::scene_model(sc)
  el <- m$elements[grepl("^panel-", m$elements$panel), ]
  expect_identical(sum(el$mark == "point"), 4L)
  # every edge is still drawn, with finite geometry
  edges <- el[el$mark != "point", ]
  expect_gte(nrow(edges), 5L)
  expect_true(all(is.finite(c(edges$x0, edges$y0, edges$x1, edges$y1))))
})

test_that("sf point features and text paths leave out a missing size (VR-38)", {
  skip_if_not_installed("sf")
  pts <- sf::st_sf(
    s = c(1, NA, 3),
    geometry = sf::st_sfc(
      sf::st_point(c(0, 0)),
      sf::st_point(c(1, 1)),
      sf::st_point(c(2, 0))
    )
  )
  w <- missing_row_warnings(
    sc <- vellum::as_vellum_scene(vplot(pts) |> mark_sf(size = s))
  )
  expect_match(w, "Removed 1 row containing missing values", fixed = TRUE)
  m <- vellum::scene_model(sc)
  expect_identical(
    sum(m$elements$mark == "point" & grepl("^panel-", m$elements$panel)),
    2L
  )

  d <- data.frame(
    x = 1:10,
    y = sin(1:10),
    g = rep(c("a", "b"), each = 5),
    s = rep(c(NA, 3), each = 5)
  )
  p <- vplot(d) |> mark_text_path(x = x, y = y, group = g, label = g, size = s)
  w <- missing_row_warnings(sc <- vellum::as_vellum_scene(p))
  expect_match(w, "Removed 5 rows containing missing values", fixed = TRUE)
  expect_no_error(vellum::render(sc, local_tempfile(fileext = ".pdf")))
})
