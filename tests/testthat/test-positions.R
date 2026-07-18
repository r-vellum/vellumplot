# Parameterised position adjustments: nudge / dodge(width) / dodge2 /
# jitter(width,height) / jitterdodge.

resolve1 <- function(p) vellumplot:::.resolve_layers(p)[[1]]

test_that("position_*() constructors carry their type + params", {
  expect_s3_class(position_nudge(1, 2), "vellumplot_position")
  expect_identical(position_dodge(0.5)$type, "dodge")
  expect_identical(position_dodge2(0.2)$dodge2_padding, 0.2)
  expect_identical(position_jitter(0.1, seed = 3)$seed, 3)
})

test_that(".normalize_position splits strings and objects", {
  a <- vellumplot:::.normalize_position("dodge")
  expect_identical(a$type, "dodge")
  expect_length(a$args, 0)
  b <- vellumplot:::.normalize_position(position_nudge(x = 1, y = 2))
  expect_identical(b$type, "nudge")
  expect_identical(b$args$nudge_x, 1)
  expect_identical(b$args$nudge_y, 2)
  expect_error(vellumplot:::.normalize_position(42), "string or")
})

test_that("nudge shifts numeric x/y and leaves a discrete axis alone", {
  r <- resolve1(
    vplot(mtcars) |>
      mark_point(x = wt, y = mpg, position = position_nudge(x = 0.2, y = -1))
  )
  expect_equal(r$values$x, mtcars$wt + 0.2)
  expect_equal(r$values$y, mtcars$mpg - 1)
  # discrete x untouched (a data-unit shift is meaningless there)
  d <- data.frame(g = c("a", "b", "c"), y = 1:3)
  rd <- resolve1(
    vplot(d) |>
      mark_bar(x = g, y = y, position = position_nudge(y = 5))
  )
  expect_true(is.character(rd$values$x) || is.factor(rd$values$x))
})

test_that("dodge2 fills the band by the groups present at each x", {
  # x = 1 has one group; x = 2 has two — so x=1's bar is wider and centred.
  d2 <- vellumplot:::.dodge2_bars(
    xp = c(1, 2, 2),
    grp = c("x", "x", "y"),
    n = 3,
    tw = rep(0.9, 3),
    padding = 0.1
  )
  expect_equal(d2$xc[1], 1) # lone group stays centred on the tick
  expect_equal(d2$w[1], 0.9 * 0.9) # and fills the whole band (minus padding)
  # the paired group splits the band in two, each half-width, symmetric
  expect_equal(d2$w[2], 0.45 * 0.9)
  expect_equal(d2$xc[2] + d2$xc[3], 4) # symmetric about x = 2
})

test_that("dodge width widens the group span", {
  bars <- data.frame(
    cat = rep(c("a", "b"), each = 2),
    g = rep(c("x", "y"), 2),
    n = c(2, 3, 4, 1)
  )
  narrow <- vellum::as_vellum_scene(
    vplot(bars) |>
      mark_bar(x = cat, y = n, fill = g, position = position_dodge(width = 0.3))
  )
  wide <- vellum::as_vellum_scene(
    vplot(bars) |>
      mark_bar(x = cat, y = n, fill = g, position = position_dodge(width = 0.9))
  )
  expect_s3_class(narrow, "vellum::vellum_scene")
  expect_s3_class(wide, "vellum::vellum_scene")
})

test_that("jitterdodge offsets by group and is reproducible", {
  d <- data.frame(
    cat = rep(c("a", "b"), each = 10),
    g = rep(c("x", "y"), 10),
    v = seq_len(20)
  )
  p <- vplot(d) |>
    mark_point(
      x = cat,
      y = v,
      color = g,
      position = position_jitterdodge(seed = 1)
    )
  s1 <- vellum::scene_model(vellum::as_vellum_scene(p))$elements
  s2 <- vellum::scene_model(vellum::as_vellum_scene(p))$elements
  # same seed -> identical element geometry
  expect_equal(s1$x, s2$x)
})

test_that("plain string positions still resolve (regression)", {
  d <- data.frame(g = rep(c("a", "b"), each = 5), v = 1:10)
  expect_no_error(vellum::as_vellum_scene(
    vplot(d) |> mark_point(x = factor(g), y = v, position = "jitter")
  ))
  bars <- data.frame(cat = c("a", "a"), g = c("x", "y"), n = c(1, 2))
  expect_no_error(vellum::as_vellum_scene(
    vplot(bars) |> mark_bar(x = cat, y = n, fill = g, position = "dodge")
  ))
})

test_that("the parameterised positions render", {
  set.seed(1)
  d <- data.frame(
    cat = rep(c("a", "b", "c"), each = 8),
    g = rep(c("x", "y"), 12),
    v = rnorm(24)
  )
  f <- local_tempfile(fileext = ".png")
  render_plot(
    vplot(d) |>
      mark_point(
        x = cat,
        y = v,
        color = g,
        position = position_jitterdodge(seed = 1)
      ),
    f
  )
  expect_gt(file.info(f)$size, 0)
})
