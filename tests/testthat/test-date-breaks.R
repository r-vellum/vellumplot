# Date axes choose their breaks by the available width, so the labels do not
# overlap (VP-92), and label them as pretty() / ggplot2 do.

.xlab <- function(p) vellumplot:::.build_panels(p)$scales$x$labels
.ylab <- function(p) vellumplot:::.build_panels(p)$scales$y$labels

ten_years <- data.frame(
  date = seq(as.Date("2000-01-01"), as.Date("2009-12-31"), length.out = 60),
  v = 1:60
)

test_that("a multi-year axis labels years, not full dates", {
  lab <- .xlab(vplot(ten_years) |> mark_line(x = date, y = v))
  expect_true(all(grepl("^[0-9]{4}$", lab)))
  expect_gte(length(lab), 3L)
})

test_that("fewer breaks on a narrower plot; sparser never denser", {
  # long labels force thinning; the estimate is generous on either platform
  p <- function(w) {
    vplot(ten_years, width = w) |>
      mark_line(x = date, y = v) |>
      scale_x_date(date_labels = "%d %B %Y")
  }
  wide <- .xlab(p(12))
  narrow <- .xlab(p(2.5))
  expect_lt(length(narrow), length(wide))
  expect_gte(length(narrow), 1L)
  # the plot's width is shared among facet columns
  d2 <- rbind(transform(ten_years, g = "a"), transform(ten_years, g = "b"))
  one <- .xlab(
    vplot(d2, width = 6) |>
      mark_line(x = date, y = v) |>
      scale_x_date(date_labels = "%d %B %Y")
  )
  two <- .xlab(
    vplot(d2, width = 6) |>
      mark_line(x = date, y = v) |>
      facet_wrap(~g) |>
      scale_x_date(date_labels = "%d %B %Y")
  )
  expect_lte(length(two), length(one))
})

test_that("the label fit compares tick spacing with measured label width", {
  spec <- vplot(ten_years, width = 4) |> mark_line(x = date, y = v)
  fit <- vellumplot:::.axis_label_fit(spec, "x")
  # two ticks a whole panel apart always fit; ten wide labels in a few mm not
  expect_true(fit(c(0, 10), c("a", "b"), c(0, 10)))
  expect_false(fit(0:9, rep("a very long label", 10), c(0, 9)))
  # polar coordinates are not thinned
  expect_null(vellumplot:::.axis_label_fit(coord_polar(spec), "x"))
})

test_that("explicit date_breaks and breaks are kept as given", {
  p <- vplot(ten_years, width = 2) |>
    mark_line(x = date, y = v) |>
    scale_x_date(date_breaks = "1 year", date_labels = "%Y")
  expect_length(.xlab(p), 10L)
})

test_that("under coord_flip() the date axis is fitted vertically", {
  m <- data.frame(
    date = seq(as.Date("2024-01-01"), as.Date("2024-06-30"), by = "day")
  )
  m$v <- seq_len(nrow(m))
  lab <- .xlab(vplot(m) |> mark_line(x = date, y = v) |> coord_flip())
  expect_equal(lab, c("Jan", "Feb", "Mar", "Apr", "May", "Jun"))
})
