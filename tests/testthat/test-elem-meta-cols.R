# VR-16: `.elem_meta()` builds records column-wise; it must give exactly the
# records of the per-row build.

test_that("column-wise element meta equals the per-row build", {
  rows <- c(3L, 1L, 2L, 5L)
  ctxs <- list(
    list(lg = list("color:a", "color:b", NULL, c("color:a", "shape:x"), "color:c")),
    list(
      tt = factor(c("p", "q", "r", "s", "t")),
      hg = 1:5,
      lg = list("color:a", NULL, "color:b", "color:b", NULL),
      fv = c(a = 0.1, b = 0.2, c = NA, d = 0.4, e = 0.5),
      cond_tags = c("s1:color", "s2:alpha"),
      filt_tags = "f",
      join = c("j1", "j2", "j3", "j4", "j5")
    ),
    list(
      tt = list("a", NULL, c("x", "y"), 4, "e"),
      hc = rep("red", 5),
      sc = rep("#00ff00", 5),
      esrc = letters[1:5],
      etgt = LETTERS[1:5],
      fv = list(1, NULL, 3, 4, NULL)
    )
  )
  for (ctx in ctxs) {
    expect_identical(
      vellumplot:::.elem_meta_cols(rows, ctx),
      vellumplot:::.elem_meta_rows(rows, ctx)
    )
  }
})

test_that("legend membership lists every series key per element", {
  sc <- list(type = "discrete")
  L <- list(n = 3L, values = list(color = c("a", "b", "a"), shape = c("x", "y", "z")))
  scales <- list(color = sc, shape = list(type = "discrete"))
  expect_identical(
    vellumplot:::.legend_membership(L, scales),
    list(c("color:a", "shape:x"), c("color:b", "shape:y"), c("color:a", "shape:z"))
  )
  L$values$shape <- NULL
  expect_identical(
    vellumplot:::.legend_membership(L, scales),
    list("color:a", "color:b", "color:a")
  )
})
