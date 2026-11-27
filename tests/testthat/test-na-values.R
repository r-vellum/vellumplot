# VP-6: missing values follow ggplot2 -- a missing category is a trailing "NA"
# level, a missing facet value a trailing "NA" panel, and a row missing a
# continuous position is removed with one warning per layer.

prov_rows <- function(p) {
  pr <- attr(vellum::as_vellum_scene(p), "vellumplot_provenance")
  sort(unique(unlist(lapply(pr, function(e) e$rows))))
}

test_that("a discrete position keeps NA as a trailing level", {
  df <- data.frame(g = c("b", NA, "a", "b"), y = 1:4)
  b <- .build_panels(vplot(df) |> mark_point(x = g, y = y))
  sc <- b$scales$x
  expect_identical(sc$labels, c("a", "b", "NA"))
  expect_identical(sc$map(c("a", NA, "b")), c(1L, 3L, 2L))
  # every row is drawn, the NA one included
  expect_identical(prov_rows(vplot(df) |> mark_point(x = g, y = y)), 1:4)
})

test_that("a factor position appends NA after its declared levels", {
  df <- data.frame(g = factor(c("lo", NA, "hi"), levels = c("lo", "hi")), y = 1:3)
  b <- .build_panels(vplot(df) |> mark_point(x = g, y = y))
  expect_identical(b$scales$x$labels, c("lo", "hi", "NA"))
})

test_that("explicit discrete limits decide what is shown, NA not added", {
  df <- data.frame(g = c("b", NA, "a"), y = 1:3)
  b <- .build_panels(
    vplot(df) |> mark_point(x = g, y = y) |> scale_x_discrete(limits = c("a", "b"))
  )
  expect_identical(b$scales$x$labels, c("a", "b"))
})

test_that("mark_bar counts missing categories as an NA bar", {
  df <- data.frame(g = c("a", NA, "b", NA, "a", "a"))
  b <- .build_panels(vplot(df) |> mark_bar(x = g))
  L <- b$panels[[1]]$resolved[[1]]
  expect_identical(b$scales$x$labels, c("a", "b", "NA"))
  expect_identical(as.numeric(L$values$y[is.na(L$values$x)]), 2)
})

test_that("a missing colour category gets the NA legend key in na_value", {
  df <- data.frame(x = 1:3, y = 1:3, g = c("a", NA, "b"))
  b <- .build_panels(
    vplot(df) |> mark_point(x = x, y = y, color = g)
  )
  sc <- b$scales$color
  expect_identical(sc$levels, c("a", "b"))
  expect_true(sc$na)
  expect_identical(sc$map(NA), sc$na_value)
})

test_that("a missing facet value gets its own trailing panel", {
  m <- mtcars
  m$cyl[c(1, 5, 9)] <- NA
  p <- vplot(m) |> mark_point(x = wt, y = mpg) |> facet_wrap(~cyl)
  b <- .build_panels(p)
  expect_identical(b$fa$wrap_labels, c("4", "6", "8", "NA"))
  n <- vapply(b$panels, function(p) p$resolved[[1]]$n, integer(1))
  expect_identical(sum(n), 32L)
  expect_identical(n[[4]], 3L)
  # a grid puts NA last on each dimension
  g <- .facet_assign(
    vplot(m) |> mark_point(x = wt, y = mpg) |> facet_grid(am ~ cyl)
  )
  expect_identical(g$col_labels, c("4", "6", "8", "NA"))
  expect_identical(g$row_labels, c("0", "1"))
})

test_that("a missing continuous position is removed with one warning per layer", {
  df <- data.frame(
    x = c(1, NA, 3, 4, NA, 6),
    y = c(1, 2, 3, NA, 5, 6),
    f = rep(c("a", "b"), 3)
  )
  p <- vplot(df) |> mark_point(x = x, y = y) |> facet_wrap(~f)
  w <- character(0)
  rows <- withCallingHandlers(
    prov_rows(p),
    warning = function(cnd) {
      w <<- c(w, conditionMessage(cnd))
      invokeRestart("muffleWarning")
    }
  )
  expect_length(w, 1L) # summed over both panels
  expect_match(w, "Removed 3 rows containing missing values", fixed = TRUE)
  # provenance does not count the removed rows as drawn
  expect_identical(rows, c(1L, 3L, 6L))
})

test_that("a line keeps its missing rows as a gap, counted in the warning", {
  # The row is not drawn, so it counts in "Removed n rows", as
  # spec_diagnose() counts it (VP-170).
  df <- data.frame(x = 1:5, y = c(1, 2, NA, 4, 5))
  expect_warning(
    vellum::as_vellum_scene(vplot(df) |> mark_line(x = x, y = y)),
    "Removed 1 row containing missing values"
  )
})

test_that("spec_diagnose counts the removed rows the compile removes", {
  df <- data.frame(x = c(1, NA, 3), y = 1:3, g = c("a", NA, "b"))
  # the report counts the row; the compile warning is not repeated
  p <- vplot(df) |> mark_point(x = x, y = y, color = g)
  expect_no_warning(spec_diagnose(p))
  expect_identical(spec_diagnose(p)$report$missing, 1L)
  expect_warning(
    vellum::as_vellum_scene(vplot(df) |> mark_point(x = x, y = y)),
    "Removed 1 row"
  )
})
