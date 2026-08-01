# Multi-page PDF output (pdf_pages) and parallel batch rendering (render_all).

p1 <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
p2 <- vplot(mtcars) |> mark_histogram(x = mpg, bins = 8)

# ---- pdf_pages -------------------------------------------------------------

test_that("pdf_pages() writes a multi-page PDF from a list of plots", {
  f <- withr::local_tempfile(fileext = ".pdf")
  expect_identical(pdf_pages(list(p1, p2), f), f)
  expect_gt(file.info(f)$size, 0)
  raw <- readBin(f, "raw", 5L)
  expect_identical(rawToChar(raw), "%PDF-") # a real PDF
  skip_if_not_installed("pdftools")
  expect_equal(pdftools::pdf_info(f)$pages, 2L)
})

test_that("pdf_pages() splits a faceted plot into one page per facet cell", {
  faceted <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    facet_wrap(~cyl)
  pages <- vellumplot:::.facet_split(faceted)
  expect_length(pages, 3L) # cyl 4/6/8
  expect_equal(vapply(pages, function(p) p@labels$title, ""), c("4", "6", "8"))
  # data is filtered per page, facet dropped
  expect_equal(vapply(pages, function(p) nrow(p@data), 0L), c(11L, 7L, 14L))
  expect_true(all(vapply(pages, function(p) is.null(p@facet), logical(1))))
  f <- withr::local_tempfile(fileext = ".pdf")
  expect_no_error(pdf_pages(faceted, f))
})

test_that("pdf_pages() rejects a single unfaceted plot and non-plot input", {
  expect_error(
    pdf_pages(p1, withr::local_tempfile(fileext = ".pdf")),
    "render_plot"
  )
  expect_error(pdf_pages(list(p1, 42), tempfile()), "must be a plot")
  expect_error(pdf_pages(list(), tempfile()), "empty")
})

# ---- render_all ------------------------------------------------------------

test_that("render_all() writes one file per plot", {
  d <- withr::local_tempdir()
  paths <- c(file.path(d, "a.png"), file.path(d, "b.png"))
  render_all(list(p1, p2), paths, workers = 1L)
  expect_true(all(file.exists(paths)))
  expect_true(all(file.info(paths)$size > 0))
})

test_that("render_all() accepts a named list + a directory shortcut", {
  d <- withr::local_tempdir()
  render_all(list(scatter = p1, hist = p2), d, workers = 1L)
  expect_true(file.exists(file.path(d, "scatter.png")))
  expect_true(file.exists(file.path(d, "hist.png")))
})

test_that("render_all() output is byte-identical to render_plot() per plot", {
  d <- withr::local_tempdir()
  batch <- file.path(d, "batch.png")
  one <- file.path(d, "one.png")
  render_all(list(p1), batch, workers = 1L)
  render_plot(p1, one)
  expect_identical(
    readBin(batch, "raw", file.info(batch)$size),
    readBin(one, "raw", file.info(one)$size)
  )
})

test_that("render_all() validates its input", {
  expect_error(render_all(list(1, 2), c("a", "b")), "must be a plot")
  expect_error(render_all(p1, "a.png"), "must be a list")
})
