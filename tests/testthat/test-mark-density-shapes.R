# Density-shape marks: mark_violin, mark_ridgeline, mark_dotplot (new in 0.2.0).

set.seed(1)
df <- data.frame(g = rep(letters[1:3], each = 50), v = rnorm(150))

test_that("mark_violin() records its mark and renders (plain and filled)", {
  expect_identical((vplot(df) |> mark_violin(x = g, y = v))@layers[[1]]@mark, "violin")
  f <- withr::local_tempfile(fileext = ".png")
  expect_no_error(render_plot(vplot(df) |> mark_violin(x = g, y = v), f))
  expect_no_error(render_plot(vplot(df) |> mark_violin(x = g, y = v, fill = g), f))
})

test_that("mark_violin() provenance ties each violin to its category's rows", {
  prov <- plot_provenance(vplot(df) |> mark_violin(x = g, y = v))
  vio <- Filter(function(e) e$mark == "violin", prov)
  expect_length(vio, 3L)
  expect_true(all(vapply(vio, function(e) length(e$rows) == 50L, logical(1))))
})

test_that("mark_ridgeline() records its mark and renders", {
  expect_identical((vplot(df) |> mark_ridgeline(x = v, y = g))@layers[[1]]@mark, "ridgeline")
  f <- withr::local_tempfile(fileext = ".png")
  expect_no_error(render_plot(vplot(df) |> mark_ridgeline(x = v, y = g), f))
  expect_no_error(render_plot(vplot(df) |> mark_ridgeline(x = v, y = g, fill = g), f))
})

test_that("mark_dotplot() bins and stacks one dot per observation", {
  d <- data.frame(v = c(1, 1, 1, 2, 5))
  L <- vellumplot:::.resolve_layer((vplot(d) |> mark_dotplot(x = v, binwidth = 0.5))@layers[[1]], d)
  sdf <- vellumplot:::.apply_stat(L)
  # one row per observation, y = stack height within the bin
  expect_equal(nrow(sdf$values$x |> as.data.frame()), 5L)
  expect_equal(max(sdf$values$y), 2.5) # three 1's stacked -> heights 0.5,1.5,2.5
})

test_that("mark_dotplot() renders", {
  f <- withr::local_tempfile(fileext = ".png")
  expect_no_error(render_plot(vplot(df) |> mark_dotplot(x = v), f))
})
