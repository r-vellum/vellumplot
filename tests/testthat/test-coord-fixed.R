# coord_fixed()/coord_equal() + theme(aspect.ratio=): aspect lock via vellum's
# grid_layout(respect=).

lay_of <- function(p) {
  built <- quill:::.build_panels(p)
  rt <- quill:::.resolve_theme(quill:::.theme_of(p))
  quill:::.build_layout(
    built,
    list(),
    p@labels,
    rt,
    FALSE,
    quill:::.coord_of(p)
  )
}

# Numeric value of the i-th track in a vellum unit vector.
trackval <- function(u, i) vctrs::field(u, "value")[i]

test_that("coord_fixed sets respect and weights the panel by the data ranges", {
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg) |> coord_fixed()
  l <- lay_of(p)
  expect_true(l$respect)
  # default coord (no fixed) leaves respect off
  expect_false(lay_of(vplot(mtcars) |> mark_point(x = wt, y = mpg))$respect)
})

test_that("coord_equal is coord_fixed(ratio = 1)", {
  a <- vplot(mtcars) |> mark_point(x = wt, y = mpg) |> coord_fixed()
  b <- vplot(mtcars) |> mark_point(x = wt, y = mpg) |> coord_equal()
  expect_identical(a@coord@kind, "fixed")
  expect_identical(b@coord@kind, "fixed")
  expect_equal(b@coord@ratio, 1)
})

test_that("the panel null-weight ratio encodes ratio * yrange : xrange", {
  built <- quill:::.build_panels(
    vplot(mtcars) |> mark_point(x = wt, y = mpg) |> coord_fixed(ratio = 2)
  )
  sx <- built$scales$x$domain
  sy <- built$scales$y$domain
  rt <- quill:::.resolve_theme(quill:::.theme_default())
  l <- quill:::.build_layout(
    built,
    list(),
    list(),
    rt,
    FALSE,
    quill:::.coord_of(
      vplot(mtcars) |> mark_point(x = wt, y = mpg) |> coord_fixed(ratio = 2)
    )
  )
  # the single panel column/row carry weights xr and ratio*yr
  wcol <- trackval(l$widths, l$panel_col[1])
  hrow <- trackval(l$heights, l$panel_row[1])
  expect_equal(wcol, abs(diff(sx)), tolerance = 1e-6)
  expect_equal(hrow, 2 * abs(diff(sy)), tolerance = 1e-6)
})

test_that("theme(aspect.ratio=) locks aspect without coord_fixed", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    theme(aspect.ratio = 0.5)
  l <- lay_of(p)
  expect_true(l$respect)
  expect_equal(trackval(l$heights, l$panel_row[1]), 0.5, tolerance = 1e-6)
  expect_equal(trackval(l$widths, l$panel_col[1]), 1, tolerance = 1e-6)
})

test_that("coord_fixed renders (incl. a custom ratio and faceting)", {
  for (p in list(
    vplot(mtcars) |> mark_point(x = wt, y = mpg) |> coord_fixed(),
    vplot(mtcars) |> mark_point(x = wt, y = mpg) |> coord_fixed(ratio = 0.1),
    vplot(mtcars) |>
      mark_point(x = wt, y = mpg) |>
      facet_wrap(~cyl) |>
      coord_fixed()
  )) {
    f <- withr::local_tempfile(fileext = ".png")
    render_plot(p, f)
    expect_gt(file.info(f)$size, 0)
  }
})
