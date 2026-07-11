# Distribution marks: mark_ecdf, mark_rug, mark_qq, mark_qq_line (new in 0.2.0).

test_that("mark_ecdf() computes a cumulative step", {
  df <- data.frame(v = c(3, 1, 2, 4))
  p <- vplot(df) |> mark_ecdf(x = v)
  L <- vellumplot:::.resolve_layer(p@layers[[1]], df)
  sdf <- vellumplot:::.apply_stat(L)
  # x is sorted; y is the cumulative proportion 1/n..1
  expect_equal(sdf$values$x, c(1, 2, 3, 4))
  expect_equal(sdf$values$y, c(0.25, 0.5, 0.75, 1))
})

test_that("mark_ecdf() records its mark/stat and renders (incl. grouped)", {
  df <- data.frame(v = rnorm(60), g = rep(c("a", "b"), 30))
  expect_identical(
    vplot(df) |> mark_ecdf(x = v) |> (\(p) p@layers[[1]]@stat)(),
    "ecdf"
  )
  f <- local_tempfile(fileext = ".png")
  expect_no_error(render_plot(vplot(df) |> mark_ecdf(x = v, color = g), f))
})

test_that("mark_rug() draws alongside another layer", {
  f <- local_tempfile(fileext = ".png")
  expect_no_error(
    render_plot(
      vplot(mtcars) |> mark_point(x = wt, y = mpg) |> mark_rug(x = wt, y = mpg),
      f
    )
  )
  expect_no_error(
    render_plot(
      vplot(mtcars) |>
        mark_point(x = wt, y = mpg) |>
        mark_rug(x = wt, sides = "b"),
      f
    )
  )
})

test_that("mark_qq() maps sample to theoretical vs observed quantiles", {
  df <- data.frame(s = qnorm(ppoints(50)) * 2 + 1) # linear in normal quantiles
  L <- vellumplot:::.resolve_layer(
    (vplot(df) |> mark_qq(sample = s))@layers[[1]],
    df
  )
  sdf <- vellumplot:::.apply_stat(L)
  # x = theoretical quantiles (roughly symmetric about 0), y = sorted sample
  expect_equal(mean(sdf$values$x), 0, tolerance = 0.01)
  expect_equal(sdf$values$y, sort(df$s))
})

test_that("mark_qq() + mark_qq_line() render", {
  f <- local_tempfile(fileext = ".png")
  expect_no_error(
    render_plot(
      vplot(mtcars) |> mark_qq(sample = mpg) |> mark_qq_line(sample = mpg),
      f
    )
  )
})
