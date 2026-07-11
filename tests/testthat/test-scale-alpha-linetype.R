# Mapped alpha and linetype aesthetics (new in 0.2.0).

df <- data.frame(
  x = rep(1:10, 2),
  y = rnorm(20),
  grp = rep(c("a", "b"), each = 10),
  w = runif(20)
)

test_that("a mapped alpha trains a continuous opacity scale with a legend", {
  p <- vplot(df) |> mark_point(x = x, y = y, alpha = w)
  b <- vellumplot:::.build_panels(p)
  expect_identical(b$scales$alpha$kind, "alpha")
  # data range maps into the [0.1, 1] default opacity range
  expect_equal(b$scales$alpha$map(range(df$w)), c(0.1, 1))
  guides <- vellumplot:::.legend_guides(b$scales)
  expect_true(any(vapply(guides, function(g) g$kind == "alpha", logical(1))))
})

test_that("scale_alpha(range=) sets the opacity output range", {
  p <- vplot(df) |>
    mark_point(x = x, y = y, alpha = w) |>
    scale_alpha(range = c(0.4, 0.8))
  b <- vellumplot:::.build_panels(p)
  expect_equal(b$scales$alpha$map(range(df$w)), c(0.4, 0.8))
})

test_that("a mapped linetype trains a discrete scale cycling line types", {
  p <- vplot(df) |> mark_line(x = x, y = y, linetype = grp)
  b <- vellumplot:::.build_panels(p)
  expect_identical(b$scales$linetype$map(c("a", "b")), c("solid", "dashed"))
  guides <- vellumplot:::.legend_guides(b$scales)
  expect_true(any(vapply(guides, function(g) g$kind == "linetype", logical(1))))
})

test_that("scale_linetype(values=) sets the line types and rejects unknown ones", {
  p <- vplot(df) |>
    mark_line(x = x, y = y, linetype = grp) |>
    scale_linetype(values = c("dotted", "dashed"))
  b <- vellumplot:::.build_panels(p)
  expect_identical(b$scales$linetype$map(c("a", "b")), c("dotted", "dashed"))
  expect_error(
    vplot(df) |>
      mark_line(x = x, y = y, linetype = grp) |>
      scale_linetype(values = "wiggly"),
    "Unknown line type"
  )
})

test_that("identity alpha/linetype use values verbatim and draw no legend", {
  d <- data.frame(
    x = 1:3,
    y = 1:3,
    a = c(0.2, 0.5, 0.9),
    lt = c("solid", "dashed", "dotted")
  )
  pa <- vplot(d) |>
    mark_point(x = x, y = y, alpha = a) |>
    scale_alpha_identity()
  ba <- vellumplot:::.build_panels(pa)
  expect_equal(ba$scales$alpha$map(c(0.2, 0.9)), c(0.2, 0.9))
  expect_length(vellumplot:::.legend_guides(ba$scales), 0L)
})

test_that("a constant alpha param still works (mapping is opt-in)", {
  f <- local_tempfile(fileext = ".png")
  expect_no_error(render_plot(
    vplot(df) |> mark_point(x = x, y = y, alpha = 0.3),
    f
  ))
})

test_that("alpha- and linetype-mapped plots render", {
  f <- local_tempfile(fileext = ".png")
  expect_no_error(render_plot(
    vplot(df) |> mark_point(x = x, y = y, alpha = w),
    f
  ))
  expect_no_error(render_plot(
    vplot(df) |> mark_line(x = x, y = y, linetype = grp),
    f
  ))
})
