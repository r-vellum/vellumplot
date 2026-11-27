# An area or ribbon splits into runs at a missing value and leaves a gap there,
# as a line does, rather than dipping to the baseline (stacked) or joining
# across it with a malformed polygon (identity, ribbon). Each series stays one
# element: a polygon, or a path with one sub-path per run. The missing rows
# count once in the "Removed n rows" warning (VP-162).

# The provenance rows of each `mark` element (one per series).
band_rows <- function(p, mark) {
  prov <- suppressWarnings(plot_provenance(p))
  recs <- Filter(function(e) e$mark == mark && e$kind == "mark", prov)
  lapply(recs, function(e) sort(e$rows))
}

# The fill elements of the scene: per polygon or path, its number of runs
# (sub-paths) and its vertex count.
band_shapes <- function(p) {
  s <- vellum::as_scene_spec(suppressWarnings(vellum::as_vellum_scene(p)))
  val <- function(x) if (is.list(x) && !is.null(x$v)) x$v else x
  out <- list()
  walk <- function(node) {
    if (identical(node$class, "grob_polygon")) {
      out[[length(out) + 1L]] <<- c(runs = 1, n = length(unlist(val(node$x$value))))
    } else if (identical(node$class, "grob_path")) {
      nper <- unlist(val(node$nper))
      out[[length(out) + 1L]] <<- c(runs = length(nper), n = sum(nper))
    }
    for (ch in node) {
      if (is.list(ch)) walk(ch)
    }
  }
  walk(s$root)
  do.call(rbind, out)
}

d1 <- data.frame(x = 1:7, y = c(1, 2, 3, NA, 3, 4, 2))

test_that("a stacked area leaves a gap at a missing height", {
  p <- vplot(d1) |> mark_area(x = x, y = y)
  # one element, two runs: 1:3 and 5:7 (each closed down to the baseline)
  expect_identical(band_rows(p, "area"), list(c(1:3, 5:7)))
  expect_equal(unname(band_shapes(p)[, "runs"]), 2)
  expect_warning(
    vellum::as_vellum_scene(p),
    "Removed 1 row containing missing values (layer 1, `mark_area()`)",
    fixed = TRUE
  )
})

test_that("an identity area and a ribbon leave a gap too", {
  p <- vplot(d1) |> mark_area(x = x, y = y, position = "identity")
  expect_identical(band_rows(p, "area"), list(c(1:3, 5:7)))
  expect_equal(unname(band_shapes(p)[, "runs"]), 2)
  r <- vplot(d1) |> mark_ribbon(x = x, ymin = 0, ymax = y)
  expect_identical(band_rows(r, "ribbon"), list(c(1:3, 5:7)))
  expect_warning(
    vellum::as_vellum_scene(r),
    "Removed 1 row containing missing values (layer 1, `mark_ribbon()`)",
    fixed = TRUE
  )
  # A missing lower bound breaks a ribbon the same way.
  d2 <- data.frame(x = 1:6, lo = c(0, 0, NA, 0, 0, 0), hi = 2)
  r2 <- vplot(d2) |> mark_ribbon(x = x, ymin = lo, ymax = hi)
  expect_identical(band_rows(r2, "ribbon"), list(c(1:2, 4:6)))
  expect_equal(unname(band_shapes(r2)[, "runs"]), 2)
})

test_that("an outlined area strokes one broken line per series", {
  p <- vplot(d1) |> mark_area(x = x, y = y, fill = "grey", color = "black")
  prov <- suppressWarnings(plot_provenance(p))
  expect_length(Filter(function(e) e$mark == "area", prov), 2L) # fill + line
})

stack_values <- function(p) {
  L <- suppressWarnings(vellumplot:::.resolve_layers(p))[[1]]
  data.frame(
    x = L$values$x,
    g = as.character(L$values$fill),
    ymin = L$values$ymin,
    ymax = L$values$ymax
  )
}

test_that("a missing height is interpolated as the baseline of the groups above", {
  # "a" is stacked on top of "b" (the last level is at the bottom).
  d <- data.frame(
    x = rep(1:5, 2),
    y = c(1, 2, NA, 2, 1, 1, 2, 3, 2, 1),
    g = rep(c("a", "b"), each = 5)
  )
  v <- stack_values(vplot(d) |> mark_area(x = x, y = y, fill = g))
  # The gappy top group is not drawn at x = 3; "b" below is untouched.
  expect_true(is.na(v$ymax[v$g == "a" & v$x == 3]))
  expect_equal(v$ymax[v$g == "b"], c(1, 2, 3, 2, 1))
  # Now the lower group has the gap: "a" keeps its place on b's height
  # interpolated between its neighbours (2 and 4) instead of dropping to the
  # axis.
  d$y <- c(1, 2, 3, 2, 1, 1, 2, NA, 4, 1)
  v <- stack_values(vplot(d) |> mark_area(x = x, y = y, fill = g))
  expect_true(is.na(v$ymax[v$g == "b" & v$x == 3]))
  expect_equal(v$ymin[v$g == "a" & v$x == 3], 3) # b: (2 + 4) / 2
  expect_equal(v$ymax[v$g == "a" & v$x == 3], 6)
  # Under fill the interpolated height is part of the total: no spike to 100%.
  v <- stack_values(
    vplot(d) |> mark_area(x = x, y = y, fill = g, position = "fill")
  )
  expect_equal(v$ymin[v$g == "a" & v$x == 3], 0.5)
  expect_equal(v$ymax[v$g == "a" & v$x == 3], 1)
  # Only the gappy group breaks.
  rows <- band_rows(vplot(d) |> mark_area(x = x, y = y, fill = g), "area")
  expect_length(rows, 2L)
})

test_that("a missing height at a series end breaks the groups above it", {
  # No neighbour to interpolate from, so the stack above is unknown there.
  d <- data.frame(
    x = rep(1:4, 2),
    y = c(1, 1, 1, 1, NA, 2, 2, 2),
    g = rep(c("a", "b"), each = 4)
  )
  v <- stack_values(vplot(d) |> mark_area(x = x, y = y, fill = g))
  expect_true(is.na(v$ymax[v$g == "b" & v$x == 1]))
  expect_true(is.na(v$ymax[v$g == "a" & v$x == 1]))
  expect_equal(v$ymin[v$g == "a" & v$x == 2], 2)
  # Only "b"'s own row is missing data; "a" breaks without a warning row.
  rep <- spec_diagnose(vplot(d) |> mark_area(x = x, y = y, fill = g))$report
  expect_identical(rep$missing, 1L)
})

test_that("interpolating groups onto a shared x grid never fills the gap", {
  d3 <- data.frame(
    x = c(1:5, 1.5, 2.5, 3.5, 4.5),
    y = c(1, 2, NA, 2, 1, 1, 1, 1, 1),
    g = rep(c("a", "b"), c(5, 4))
  )
  L3 <- suppressWarnings(vellumplot:::.resolve_layers(
    vplot(d3) |> mark_area(x = x, y = y, fill = g)
  ))[[1]]
  a3 <- as.character(L3$values$fill) == "a" & !is.na(L3$values$ymax)
  expect_false(any(L3$values$x[a3] > 2 & L3$values$x[a3] < 4))
})

test_that("an infinite height is not a gap and not a missing row", {
  d <- data.frame(x = 1:5, y = c(1, 2, Inf, 2, 1))
  p <- vplot(d) |> mark_area(x = x, y = y, position = "identity")
  expect_identical(spec_diagnose(p)$report$missing, 0L)
  expect_identical(band_rows(p, "area"), list(1:5))
})

test_that("the missing rows count once in spec_diagnose() and the warning", {
  p <- vplot(d1) |> mark_area(x = x, y = y)
  rep <- spec_diagnose(p)$report
  expect_identical(rep$missing[rep$layer == 1L], 1L)
  w <- character()
  withCallingHandlers(
    vellum::as_vellum_scene(p),
    warning = function(cnd) {
      w <<- c(w, conditionMessage(cnd))
      invokeRestart("muffleWarning")
    }
  )
  expect_length(grep("Removed", w), 1L)
})

test_that("an area without missing values is one polygon, as before", {
  p <- vplot(data.frame(x = 1:4, y = 1:4)) |> mark_area(x = x, y = y)
  expect_identical(band_rows(p, "area"), list(1:4))
  expect_equal(unname(band_shapes(p)[, "runs"]), 1)
  expect_no_warning(vellum::as_vellum_scene(p))
})

test_that("an animated area keeps one element when a state has a gap", {
  # Before, the gap split the series into two polygons: mid-transition the
  # whole area morphed into its first run (its corners lifting off the
  # baseline) and the second run faded in as a new element.
  d <- data.frame(
    x = rep(1:7, 2),
    y = c(1, 2, 3, 3, 3, 4, 2, 1, 2, 3, NA, 3, 4, 2),
    s = rep(c("s1", "s2"), each = 7)
  )
  p <- vplot(d, width = 3, height = 2, dpi = 50) |>
    mark_area(x = x, y = y) |>
    transition_states(s, wrap = FALSE)
  a <- suppressWarnings(animate(p, nframes = 6))
  pr <- vellum::vl_tween_pairing(a@scenes)
  expect_identical(pr$entered, 0L)
  expect_identical(pr$exited, 0L)
  # The area switches whole at the midpoint: a frame before it is the first
  # keyframe's picture, one after it the second's.
  px <- function(frac) {
    vellum:::.tween_frame(a@scenes[[1]], a@scenes[[2]], frac)$rgba()
  }
  expect_identical(px(0.4), px(0))
  expect_identical(px(0.6), px(1))
  expect_false(identical(px(0), px(1)))
})

# VP-169: a ribbon over `y` from `xmin` to `xmax` drew nothing; it is now a
# horizontal band, with the same gaps at missing values.
test_that("a ribbon with xmin/xmax and no ymin/ymax is horizontal (VP-169)", {
  dh <- data.frame(y = 1:7, lo = 0, hi = c(1, 2, 3, NA, 3, 4, 2))
  p <- vplot(dh) |> mark_ribbon(y = y, xmin = lo, xmax = hi)
  expect_warning(
    vellum::as_vellum_scene(p),
    "Removed 1 row containing missing values (layer 1, `mark_ribbon()`)",
    fixed = TRUE
  )
  expect_identical(band_rows(p, "ribbon"), list(c(1:3, 5:7)))
  expect_equal(unname(band_shapes(p)[, "runs"]), 2)
  # Without the gap, the band is the vertical ribbon of the swapped data
  # turned on its side: the same polygon with x and y exchanged.
  dv <- data.frame(y = 1:4, lo = c(0, 1, 0, 1), hi = c(2, 3, 3, 2))
  h <- vplot(dv) |> mark_ribbon(y = y, xmin = lo, xmax = hi)
  v <- vplot(dv) |> mark_ribbon(x = y, ymin = lo, ymax = hi)
  poly <- function(p) {
    s <- vellum::as_scene_spec(vellum::as_vellum_scene(p))
    val <- function(x) if (is.list(x) && !is.null(x$v)) x$v else x
    out <- NULL
    walk <- function(node) {
      if (identical(node$class, "grob_polygon")) {
        out <<- list(
          x = unlist(val(node$x$value)),
          y = unlist(val(node$y$value))
        )
      }
      for (ch in node) if (is.list(ch)) walk(ch)
    }
    walk(s$root)
    out
  }
  ph <- poly(h)
  pv <- poly(v)
  expect_length(ph$x, 8L)
  expect_equal(ph$x, pv$y)
  expect_equal(ph$y, pv$x)
  # Grouped horizontal ribbons draw one band per group.
  dg <- rbind(transform(dv, g = "a"), transform(dv, lo = lo + 4, hi = hi + 4, g = "b"))
  g <- vplot(dg) |> mark_ribbon(y = y, xmin = lo, xmax = hi, fill = g)
  expect_identical(band_rows(g, "ribbon"), list(1:4, 5:8))
})

# VP-170: a line, path or step with a missing value leaves a gap and now counts
# the row in the "Removed n rows" warning, as spec_diagnose() reports it.
test_that("lines, paths and steps count their gap rows in the warning (VP-170)", {
  for (mk in list(mark_line, mark_path, mark_step)) {
    p <- vplot(d1) |> mk(x = x, y = y)
    rep <- spec_diagnose(p)$report
    expect_identical(rep$missing[rep$layer == 1L], 1L)
    w <- character()
    withCallingHandlers(
      vellum::as_vellum_scene(p),
      warning = function(cnd) {
        w <<- c(w, conditionMessage(cnd))
        invokeRestart("muffleWarning")
      }
    )
    expect_length(grep("Removed 1 row containing missing values", w), 1L)
  }
  # A line with no missing value gives no warning.
  expect_no_warning(vellum::as_vellum_scene(
    vplot(data.frame(x = 1:3, y = 1:3)) |> mark_line(x = x, y = y)
  ))
})
