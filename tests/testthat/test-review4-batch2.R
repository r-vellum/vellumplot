# Regression tests for the REVIEW4 Batch 2 "aesthetic-drop" fixes: several mark
# emitters silently ignored alpha / linewidth / a mapped size. Each fix is proven
# behaviourally — setting the aesthetic must change the rendered SVG; if it were
# still dropped the two outputs would be byte-identical.

svg <- function(p) paste(plot_svg(p), collapse = "")
changes_output <- function(with, without) {
  expect_false(identical(svg(with), svg(without)))
}

d <- data.frame(
  g = rep(c("a", "b"), each = 6),
  x = c(1, 2, 3, 4, 5, 6, 1, 2, 3, 4, 5, 6),
  y = c(2, 4, 3, 6, 5, 7, 1, 3, 2, 5, 4, 6)
)

# CM4 -----------------------------------------------------------------------
test_that("mark_boxplot honours alpha", {
  changes_output(
    vplot(d) |> mark_boxplot(x = g, y = y, alpha = 0.3),
    vplot(d) |> mark_boxplot(x = g, y = y)
  )
})

test_that("mark_rule honours alpha", {
  base <- vplot(d) |> mark_point(x = x, y = y)
  changes_output(
    base |> mark_rule(yintercept = 3, alpha = 0.4),
    base |> mark_rule(yintercept = 3)
  )
})

# CM3 -----------------------------------------------------------------------
test_that("mark_smooth honours alpha (band) and linewidth (line)", {
  base <- vplot(d) |> mark_point(x = x, y = y)
  changes_output(
    base |> mark_smooth(x = x, y = y, alpha = 0.5),
    base |> mark_smooth(x = x, y = y)
  )
  changes_output(
    base |> mark_smooth(x = x, y = y, linewidth = 3),
    base |> mark_smooth(x = x, y = y)
  )
})

# CM2 -----------------------------------------------------------------------
test_that("mark_errorbar and mark_linerange honour alpha", {
  eb <- data.frame(x = c("a", "b"), y = c(2, 3), ymin = c(1, 2), ymax = c(3, 4))
  changes_output(
    vplot(eb) |>
      mark_errorbar(x = x, y = y, ymin = ymin, ymax = ymax, alpha = 0.6),
    vplot(eb) |> mark_errorbar(x = x, y = y, ymin = ymin, ymax = ymax)
  )
  changes_output(
    vplot(eb) |>
      mark_linerange(x = x, y = y, ymin = ymin, ymax = ymax, alpha = 0.6),
    vplot(eb) |> mark_linerange(x = x, y = y, ymin = ymin, ymax = ymax)
  )
})

# CM1 -----------------------------------------------------------------------
test_that("mark_halfeye honours alpha", {
  di <- data.frame(
    g = rep(c("a", "b"), each = 8),
    y = c(1, 2, 3, 4, 5, 6, 7, 8, 3, 4, 5, 6, 7, 8, 9, 10)
  )
  changes_output(
    vplot(di) |> mark_halfeye(x = g, y = y, alpha = 0.7),
    vplot(di) |> mark_halfeye(x = g, y = y)
  )
})

# CM5 -----------------------------------------------------------------------
test_that("text marks honour a mapped size", {
  tt <- data.frame(x = 1:3, y = 1:3, s = c(2, 6, 10), lab = c("a", "b", "c"))
  changes_output(
    vplot(tt) |> mark_text(x = x, y = y, label = lab, size = s),
    vplot(tt) |> mark_text(x = x, y = y, label = lab)
  )
  changes_output(
    vplot(tt) |> mark_label(x = x, y = y, label = lab, size = s),
    vplot(tt) |> mark_label(x = x, y = y, label = lab)
  )
})

test_that("a constant text size still renders", {
  tt <- data.frame(x = 1:3, y = 1:3, lab = c("a", "b", "c"))
  expect_no_error(svg(
    vplot(tt) |> mark_text(x = x, y = y, label = lab, size = 14)
  ))
})
