# 2-D density / contour: mark_contour() (lines) and mark_contour_filled() (bands),
# tracing a KDE field (MASS) or a supplied z surface with isoband. Phase 3.

skip_if_no_contour <- function() {
  skip_if_not_installed("isoband")
  skip_if_not_installed("MASS")
}

test_that("mark_contour / mark_contour_filled declare contour layers", {
  p <- vplot(faithful) |> mark_contour(x = eruptions, y = waiting)
  L <- p@layers[[length(p@layers)]]
  expect_identical(L@mark, "contour")
  expect_identical(L@stat, "density_2d")
  expect_true("color" %in% names(L@encoding)) # default color = after_stat(level)

  pf <- vplot(faithful) |> mark_contour_filled(x = eruptions, y = waiting)
  Lf <- pf@layers[[length(pf@layers)]]
  expect_identical(Lf@mark, "contour_filled")
  expect_true(isTRUE(Lf@stat_params$filled))
  expect_true("fill" %in% names(Lf@encoding))
})

test_that(".stat_density_2d yields grouped ordered vertices (lines and bands)", {
  skip_if_no_contour()
  L <- list(
    values = list(x = faithful$eruptions, y = faithful$waiting),
    stat_params = list(bins = 6, n = 50), after = list(), types = list()
  )
  sdf <- .stat_density_2d(L)
  expect_setequal(names(sdf), c("x", "y", "level", ".piece", ".ring"))
  expect_gt(nrow(sdf), 0L)
  expect_gt(length(unique(sdf$.piece)), 1L) # multiple traced pieces
  expect_true(all(is.na(sdf$.ring))) # lines carry no ring id

  Lf <- L
  Lf$stat_params$filled <- TRUE
  bf <- .stat_density_2d(Lf)
  expect_false(all(is.na(bf$.ring))) # bands carry ring ids (holes)
})

test_that("contour marks render and are coloured by level", {
  skip_if_no_contour()
  distinct_strokes <- function(p) {
    f <- local_tempfile(fileext = ".svg")
    render_plot(p, f)
    s <- paste(readLines(f), collapse = "")
    length(unique(unlist(regmatches(s, gregexpr('stroke="#[0-9a-fA-F]{6}"', s)))))
  }
  expect_gt(distinct_strokes(vplot(faithful) |> mark_contour(x = eruptions, y = waiting, bins = 8)), 1L)

  f <- local_tempfile(fileext = ".svg")
  expect_no_error(render_plot(vplot(faithful) |> mark_contour_filled(x = eruptions, y = waiting), f))
})

test_that("a z surface is contoured when x/y form a regular grid", {
  skip_if_no_contour()
  g <- expand.grid(x = 1:15, y = 1:15)
  g$z <- with(g, sin(x / 3) + cos(y / 3))
  f <- local_tempfile(fileext = ".svg")
  expect_no_error(render_plot(vplot(g) |> mark_contour(x = x, y = y, z = z), f))
  # a non-grid z is a clear error, not a silent misdraw
  expect_error(
    render_plot(
      vplot(data.frame(x = 1:5, y = 1:5, z = 1:5)) |> mark_contour(x = x, y = y, z = z),
      local_tempfile(fileext = ".svg")
    ),
    "regular grid"
  )
})
