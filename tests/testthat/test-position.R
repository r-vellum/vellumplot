# G6: position adjustments (stack / fill / dodge / jitter).

mt <- transform(mtcars, cyl = factor(cyl), am = factor(am))

test_that("bars stack by default", {
  expect_identical((vplot(mt) |> mark_bar(x = cyl, fill = am))@layers[[1]]@position, "stack")
})

test_that("stacking assigns non-overlapping [ymin, ymax] spans summing to the group total", {
  p <- vplot(mt) |> mark_bar(x = cyl, fill = am)
  r <- vellumplot:::.resolve_layers(p)[[1]]
  expect_false(is.null(r$values$ymin))
  # within each x the spans tile [0, total] with no gaps/overlap
  x <- as.character(r$values$x)
  for (xi in unique(x)) {
    rows <- which(x == xi)
    lo <- sort(r$values$ymin[rows])
    hi <- sort(r$values$ymax[rows])
    expect_equal(lo[1], 0)
    expect_equal(hi, c(lo[-1], max(hi))) # each ymax meets the next ymin
    expect_equal(max(hi), sum(table(mt$cyl, mt$am)[xi, ])) # = count in that x
  }
})

test_that("position = 'fill' normalises each x group to 1", {
  p <- vplot(mt) |> mark_bar(x = cyl, fill = am, position = "fill")
  r <- vellumplot:::.resolve_layers(p)[[1]]
  x <- as.character(r$values$x)
  tops <- vapply(unique(x), function(xi) max(r$values$ymax[x == xi]), numeric(1))
  expect_equal(unname(tops), rep(1, length(tops)))
})

test_that("an ungrouped bar gets a 0..y span", {
  p <- vplot(data.frame(g = factor(c("a", "a", "b")))) |> mark_bar(x = g)
  r <- vellumplot:::.resolve_layers(p)[[1]]
  expect_equal(r$values$ymin, c(0, 0))
  expect_equal(r$values$ymax, r$values$y)
})

test_that("stacked totals drive the y domain", {
  p <- vplot(mt) |> mark_bar(x = cyl, fill = am)
  sc <- vellumplot:::.build_panels(p)$scales
  expect_gte(sc$y$domain[2], max(table(mt$cyl)))
})

test_that("dodge and jitter are recorded and render", {
  expect_identical((vplot(mt) |> mark_bar(x = cyl, fill = am, position = "dodge"))@layers[[1]]@position, "dodge")
  for (pos in c("dodge", "fill", "stack")) {
    f <- withr::local_tempfile(fileext = ".png")
    render_plot(vplot(mt) |> mark_bar(x = cyl, fill = am, position = pos), f)
    expect_gt(file.info(f)$size, 0)
  }
  fj <- withr::local_tempfile(fileext = ".png")
  render_plot(vplot(mt) |> mark_point(x = cyl, y = mpg, position = "jitter"), fj)
  expect_gt(file.info(fj)$size, 0)
})
