# vtable(): a data frame laid out as text + sparkline columns.

mkdf <- function() {
  df <- data.frame(name = c("A", "B", "C"), mean = c(3.1, 5.4, 2.2))
  df$trend <- list(1:8, c(2, 1, 3, 5, 4), c(9, 7, 8, 6))
  df
}

test_that("vtable() builds a VTable with text + spark column descriptors", {
  vt <- vtable(mkdf(), spark = list(trend = "line"))
  expect_s3_class(vt, "vellumplot::VTable")
  expect_equal(vt@n, 3)
  kinds <- vapply(vt@descs, function(d) d$kind, character(1))
  expect_identical(unname(kinds), c("text", "text", "spark"))
  # spark cells are per-row sparkline PlotSpecs
  expect_length(vt@descs$trend$cells, 3L)
  expect_s3_class(vt@descs$trend$cells[[1]], "vellumplot::PlotSpec")
})

test_that("the spark spec accepts a type string or a builder function", {
  vt1 <- vtable(mkdf(), spark = list(trend = "bar"))
  expect_identical(
    vt1@descs$trend$cells[[1]]@layers[[1]]@stat_params$type,
    "bar"
  )
  vt2 <- vtable(
    mkdf(),
    spark = list(trend = function(v) vsparkline(v, type = "winloss"))
  )
  expect_identical(
    vt2@descs$trend$cells[[1]]@layers[[1]]@stat_params$type,
    "winloss"
  )
})

test_that("vtable() validates its inputs", {
  expect_error(vtable(1:3), "must be a data frame")
  expect_error(vtable(mkdf(), cols = c("name", "nope")), "Unknown column")
  # a spark spec on a non-list column is an error
  expect_error(
    vtable(mkdf(), spark = list(mean = "line")),
    "must be a list-column"
  )
})

test_that("text columns default to right-align for numeric, left otherwise", {
  vt <- vtable(mkdf())
  al <- vapply(vt@descs, function(d) d$align, character(1))
  expect_identical(al[["name"]], "left")
  expect_identical(al[["mean"]], "right")
})

test_that("a vtable renders (and render_plot accepts it)", {
  set.seed(1)
  df <- data.frame(metric = c("x", "y"), val = c(10, 20))
  df$trend <- list(cumsum(rnorm(15)), cumsum(rnorm(15)))
  df$vol <- list(rpois(12, 5), rpois(12, 5))
  vt <- vtable(df, spark = list(trend = "line", vol = "bar"))
  expect_no_error(vellum::as_vellum_scene(vt))
  f <- local_tempfile(fileext = ".png")
  expect_no_error(render_plot(vt, f))
  expect_gt(file.info(f)$size, 0)
})

test_that(".fmt_col formats numeric, date, and character", {
  expect_equal(vellumplot:::.fmt_col(c(1, 22.5)), c("1", "22.5"))
  expect_type(vellumplot:::.fmt_col(as.Date("2020-01-01")), "character")
  expect_identical(vellumplot:::.fmt_col(c("a", "b")), c("a", "b"))
})
