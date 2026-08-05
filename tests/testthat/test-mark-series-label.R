# Direct series labels (mark_series_label): one label per colour series at its
# line end, in place of a legend.

svg <- function(p) paste(plot_svg(p), collapse = "")
panels <- function(p) vellumplot:::.build_panels(p)

d <- data.frame(
  t = rep(1:10, 3),
  v = c(1:10, (1:10) * 1.5, 10:1),
  s = rep(c("alpha", "beta", "gamma"), each = 10)
)
base <- vplot(d) |> mark_line(x = t, y = v, color = s)

test_that("mark_series_label renders and labels each series at its line end", {
  expect_no_error(svg(base |> mark_series_label(x = t, y = v, color = s)))
  L <- panels(base |> mark_series_label(x = t, y = v, color = s))$panels[[
    1
  ]]$resolved[[2]]
  expect_equal(L$n, 3L) # one label per series
  expect_setequal(L$values$label, c("alpha", "beta", "gamma"))
  expect_true(all(L$values$x == 10)) # each at its largest x
  # v at t == 10 is 10 (alpha), 15 (beta), 1 (gamma)
  labs <- stats::setNames(L$values$y, L$values$label)
  expect_equal(unname(labs[c("alpha", "beta", "gamma")]), c(10, 15, 1))
})

test_that("the label inherits its series colour", {
  L <- panels(base |> mark_series_label(x = t, y = v, color = s))$panels[[
    1
  ]]$resolved[[2]]
  # the stat realigns `group` so the colour scale keeps mapping the series
  expect_setequal(as.character(L$values$color), c("alpha", "beta", "gamma"))
})

test_that("a single (ungrouped) series still labels its end", {
  one <- vplot(d[d$s == "alpha", ]) |> mark_line(x = t, y = v)
  expect_no_error(svg(one |> mark_series_label(x = t, y = v)))
})

test_that("mark_series_label round-trips through a spec", {
  expect_no_error(from_spec(as_spec(
    base |> mark_series_label(x = t, y = v, color = s)
  )))
})
