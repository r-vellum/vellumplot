# Date/time position scales: Date/POSIXct axes work by class detection; the
# scale_*_date/datetime/time constructors add configurable breaks/labels.

test_that("a Date column gets a date axis with no scale declared", {
  df <- data.frame(
    day = as.Date("2020-01-01") + seq(0, 364, by = 7),
    y = seq_len(53)
  )
  b <- vellumplot:::.build_panels(vplot(df) |> mark_line(x = day, y = y))
  # labels are formatted dates, not raw numbers
  expect_true(any(grepl("2020", b$scales$x$labels)))
  expect_length(b$scales$x$breaks, length(b$scales$x$labels))
})

test_that("scale_x_date() honours date_breaks and date_labels", {
  df <- data.frame(
    day = as.Date("2020-01-01") + seq(0, 364, by = 7),
    y = seq_len(53)
  )
  p <- vplot(df) |>
    mark_line(x = day, y = y) |>
    scale_x_date(date_breaks = "3 months", date_labels = "%b %Y")
  b <- vellumplot:::.build_panels(p)
  expect_equal(
    b$scales$x$labels,
    c("Jan 2020", "Apr 2020", "Jul 2020", "Oct 2020")
  )
})

test_that("scale_x_datetime() formats POSIXct labels", {
  df <- data.frame(
    t = as.POSIXct("2020-01-01", tz = "UTC") +
      seq(0, 3600 * 24 * 10, by = 3600 * 24),
    y = seq_len(11)
  )
  p <- vplot(df) |>
    mark_line(x = t, y = y) |>
    scale_x_datetime(date_labels = "%d %b")
  b <- vellumplot:::.build_panels(p)
  expect_true(all(grepl("Jan", b$scales$x$labels)))
})

test_that("date scales render", {
  df <- data.frame(day = as.Date("2020-01-01") + 0:364, y = seq_len(365))
  f <- local_tempfile(fileext = ".png")
  expect_no_error(
    render_plot(
      vplot(df) |>
        mark_line(x = day, y = y) |>
        scale_x_date(date_breaks = "2 months"),
      f
    )
  )
})
