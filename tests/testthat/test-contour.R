# 2-D density / contour: mark_contour() (lines) and mark_contour_filled() (bands),
# tracing a KDE field (MASS) or a supplied z surface with the engine's
# vl_contour(). A density field needs MASS; the tracing itself needs no isoband.

skip_if_no_contour <- function() {
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
    stat_params = list(bins = 6, n = 50),
    after = list(),
    types = list()
  )
  sdf <- .stat_density_2d(L)
  expect_setequal(names(sdf), c("x", "y", "level", ".piece", ".ring"))
  expect_gt(nrow(sdf), 0L)
  expect_gt(length(unique(sdf$.piece)), 1L) # multiple traced pieces
  expect_true(all(is.na(sdf$.ring))) # lines carry no ring id

  Lf <- L
  Lf$stat_params$filled <- TRUE
  bf <- .stat_density_2d(Lf)
  # filled rings carry a ring id (1); pieces are ordered by level ascending so
  # inner levels paint last (painter's order)
  expect_true(all(bf$.ring == 1L))
  piece_level <- tapply(bf$level, bf$.piece, `[`, 1L)
  expect_false(is.unsorted(piece_level))
})

test_that("a z surface is contoured in the right orientation (not transposed)", {
  # z increases with x only, so an iso-line at level L is the VERTICAL line
  # x == L (y free). A transposed field would trace a horizontal line instead --
  # the exact bug that reflected every contour across the diagonal.
  g <- expand.grid(x = 1:20, y = 1:20)
  g$z <- g$x # depends on x alone
  L <- list(
    values = list(x = g$x, y = g$y, z = g$z),
    stat_params = list(breaks = c(5, 10, 15)),
    after = list(),
    types = list()
  )
  sdf <- .stat_density_2d(L)
  # each traced piece hugs a single x (its level) and sweeps the full y range
  for (b in c(5, 10, 15)) {
    xs <- sdf$x[abs(sdf$level - b) < 1e-6]
    ys <- sdf$y[abs(sdf$level - b) < 1e-6]
    expect_equal(mean(xs), b, tolerance = 0.5)
    expect_gt(diff(range(ys)), 15) # spans y, not pinned to one row
  }
})

test_that("contour marks render and are coloured by level", {
  skip_if_no_contour()
  distinct_strokes <- function(p) {
    f <- local_tempfile(fileext = ".svg")
    render_plot(p, f)
    s <- paste(readLines(f), collapse = "")
    length(unique(unlist(regmatches(
      s,
      gregexpr('stroke="#[0-9a-fA-F]{6}"', s)
    ))))
  }
  expect_gt(
    distinct_strokes(
      vplot(faithful) |> mark_contour(x = eruptions, y = waiting, bins = 8)
    ),
    1L
  )

  f <- local_tempfile(fileext = ".svg")
  expect_no_error(render_plot(
    vplot(faithful) |> mark_contour_filled(x = eruptions, y = waiting),
    f
  ))
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
      vplot(data.frame(x = 1:5, y = 1:5, z = 1:5)) |>
        mark_contour(x = x, y = y, z = z),
      local_tempfile(fileext = ".svg")
    ),
    "regular grid"
  )
  # right row count but a duplicated (x,y) cell + a compensating gap (H27)
  skip_if_no_contour()
  g2 <- expand.grid(x = 1:5, y = 1:5)
  g2$z <- with(g2, x + y)
  g2$x[g2$x == 2 & g2$y == 1] <- 1 # (2,1) -> a second (1,1)
  expect_error(
    render_plot(
      vplot(g2) |> mark_contour(x = x, y = y, z = z),
      local_tempfile(fileext = ".svg")
    ),
    "regular grid"
  )
})

test_that("the after_stat default channel yields to an explicit mapping", {
  # contour_filled defaults fill = after_stat(level); an explicit fill wins.
  enc <- function(p) p@layers[[1]]@encoding
  auto <- vplot(faithful) |> mark_contour_filled(x = eruptions, y = waiting)
  expect_true("fill" %in% names(enc(auto)))
  set <- vplot(faithful) |>
    mark_contour_filled(x = eruptions, y = waiting, fill = "grey")
  # an explicit constant fill is a param, not an after_stat encoding
  expect_false("fill" %in% names(enc(set)))
  # contour defaults color = after_stat(level)
  cl <- vplot(faithful) |> mark_contour(x = eruptions, y = waiting)
  expect_true("color" %in% names(enc(cl)))
})
