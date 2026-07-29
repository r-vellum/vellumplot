# Feature 3: consuming provenance (provenance_join) + reproducibility manifest.

test_that("provenance_join ties elements to source rows and geometry", {
  df <- data.frame(wt = mtcars$wt, mpg = mtcars$mpg, cyl = factor(mtcars$cyl))
  p <- vplot(df) |> mark_point(x = wt, y = mpg, color = cyl)
  pj <- provenance_join(p)
  expect_s3_class(pj, "data.frame")
  expect_true(all(c("id", "mark", "rows", "x0", "y0", "n_rows") %in% names(pj)))
  expect_true(all(pj$mark == "point"))
  # colour splits into one grob per cyl level (3 groups), rows partitioned
  expect_gte(nrow(pj), 3)
  expect_type(pj$rows, "list")
  expect_identical(sum(pj$n_rows), nrow(df))
  # geometry is populated from scene_model
  expect_false(any(is.na(pj$x0)))
})

test_that("provenance_join is empty for a plot with no mark grobs", {
  p <- vplot(mtcars)
  expect_identical(nrow(provenance_join(p)), 0L)
})

test_that("plot_manifest fingerprints data and spec", {
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
  m <- plot_manifest(p)
  expect_identical(m$data$nrow, 32L)
  expect_true(nzchar(m$spec_hash))
  expect_gte(m$n_elements, 1)
  # a data change moves the data hash
  p2 <- vplot(mtcars[1:10, ]) |> mark_point(x = wt, y = mpg)
  expect_false(identical(plot_manifest(p2)$data$hash, m$data$hash))
})

test_that("plot_svg(manifest=TRUE) embeds a manifest that plot_verify reads", {
  skip_if_not_installed("jsonlite")
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
  svg <- plot_svg(p, manifest = TRUE)
  expect_match(svg, "vellumplot-manifest:")
  v <- plot_verify(svg, mtcars)
  expect_true(v$ok)
  # a different dataset fails verification
  expect_false(plot_verify(svg, mtcars[1:5, ])$ok)
})

test_that("plot_verify errors when no manifest is present", {
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
  svg <- plot_svg(p, manifest = FALSE)
  expect_error(plot_verify(svg, mtcars), "manifest")
})
