# An area or ribbon splits into runs at a missing value and leaves a gap there,
# as a line does, rather than dipping to the baseline (stacked) or joining
# across it with a malformed polygon (identity, ribbon). The missing rows count
# once in the "Removed n rows" warning (VP-162).

band_rows <- function(p, mark) {
  prov <- suppressWarnings(plot_provenance(p))
  recs <- Filter(function(e) e$mark == mark && e$kind == "mark", prov)
  lapply(recs, function(e) sort(e$rows))
}

d1 <- data.frame(x = 1:7, y = c(1, 2, 3, NA, 3, 4, 2))

test_that("a stacked area leaves a gap at a missing height", {
  p <- vplot(d1) |> mark_area(x = x, y = y)
  expect_identical(band_rows(p, "area"), list(1:3, 5:7))
  expect_warning(
    vellum::as_vellum_scene(p),
    "Removed 1 row containing missing values (layer 1, `mark_area()`)",
    fixed = TRUE
  )
})

test_that("an identity area and a ribbon leave a gap too", {
  p <- vplot(d1) |> mark_area(x = x, y = y, position = "identity")
  expect_identical(band_rows(p, "area"), list(1:3, 5:7))
  r <- vplot(d1) |> mark_ribbon(x = x, ymin = 0, ymax = y)
  expect_identical(band_rows(r, "ribbon"), list(1:3, 5:7))
  expect_warning(
    vellum::as_vellum_scene(r),
    "Removed 1 row containing missing values (layer 1, `mark_ribbon()`)",
    fixed = TRUE
  )
  # A missing lower bound breaks a ribbon the same way.
  d2 <- data.frame(x = 1:6, lo = c(0, 0, NA, 0, 0, 0), hi = 2)
  r2 <- vplot(d2) |> mark_ribbon(x = x, ymin = lo, ymax = hi)
  expect_identical(band_rows(r2, "ribbon"), list(1:2, 4:6))
})

test_that("a stacked area stacks each run; only the gappy group breaks", {
  d <- data.frame(
    x = rep(1:5, 2),
    y = c(1, 2, NA, 2, 1, 1, 1, 1, 1, 1),
    g = rep(c("a", "b"), each = 5)
  )
  p <- vplot(d) |> mark_area(x = x, y = y, fill = g)
  rows <- band_rows(p, "area")
  expect_length(rows, 3L)
  expect_true(list(1:2) %in% rows && list(4:5) %in% rows && list(6:10) %in% rows)
  # The missing row stacks as nothing: "a" sits on "b" either side of it.
  L <- suppressWarnings(vellumplot:::.resolve_layers(p))[[1]]
  a <- which(as.character(L$values$fill) == "a")
  expect_true(all(is.na(L$values$ymax[a][L$values$x[a] == 3])))
  expect_equal(L$values$ymin[a][L$values$x[a] %in% c(2, 4)], c(1, 1))
  # Interpolating groups onto a shared x grid never fills the gap.
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
  expect_no_warning(vellum::as_vellum_scene(p))
})
