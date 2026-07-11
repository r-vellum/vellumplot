# NA legend keys for mapped size / shape aesthetics. Also: NA in a shape
# aesthetic no longer errors (it maps to a neutral shape). Phase 4.

resolved_one <- function(aes, values) list(list(mark = "point", values = stats::setNames(list(values), aes)))

test_that("NA in a shape aesthetic renders (no crash) and adds an NA key", {
  df <- data.frame(x = 1:6, y = 1:6, g = c("a", "b", "a", "b", NA, NA))
  p <- vplot(df) |> mark_point(x = x, y = y, shape = g)
  f <- local_tempfile(fileext = ".svg")
  expect_no_error(render_plot(p, f)) # previously errored: "Unknown point shape: NA"

  sc <- .train_shape(p, resolved_one("shape", df$g))
  expect_true(isTRUE(sc$na))
  expect_identical(.guide_labels(list(kind = "shape", sc = sc)), c("a", "b", "NA"))
})

test_that("NA in a size aesthetic adds an NA key", {
  df <- data.frame(x = 1:6, y = 1:6, s = c(1, 2, 3, 4, NA, NA))
  p <- vplot(df) |> mark_point(x = x, y = y, size = s)
  f <- local_tempfile(fileext = ".svg")
  expect_no_error(render_plot(p, f))

  sc <- .train_size(p, resolved_one("size", df$s))
  expect_true(isTRUE(sc$na))
  expect_identical(tail(.guide_labels(list(kind = "size", sc = sc)), 1L), "NA")
})

test_that("no NA key when the aesthetic has no missing values (additive)", {
  df <- data.frame(x = 1:4, y = 1:4, g = c("a", "b", "a", "b"), s = c(1, 2, 3, 4))
  ps <- vplot(df) |> mark_point(x = x, y = y, shape = g)
  sc_sh <- .train_shape(ps, resolved_one("shape", df$g))
  expect_false(isTRUE(sc_sh$na))
  expect_false("NA" %in% .guide_labels(list(kind = "shape", sc = sc_sh)))

  sc_sz <- .train_size(ps, resolved_one("size", df$s))
  expect_false(isTRUE(sc_sz$na))
  expect_false("NA" %in% .guide_labels(list(kind = "size", sc = sc_sz)))
})
