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

# Regression: a series label belongs AT its line end, so the repel pass must
# avoid only the other labels -- never the (diagonal-bbox) lines. Well-separated
# ends must therefore not be displaced at all.
repel_offsets <- function(p) {
  scene <- vellumplot:::.compile_plot(p)
  labs <- grep("^repel:", vellum::node_names(scene), value = TRUE)
  pr <- vellumplot:::.repel_scene_params(p)
  avoid <- if (vellumplot:::.repel_avoid_labels_only(p)) labs else NULL
  vellum::vl_place(
    scene,
    labels = labs,
    avoid = avoid,
    padding = pr$padding,
    max_shift = pr$max_shift
  )
}

test_that("series labels repel off each other only (not the lines)", {
  expect_true(vellumplot:::.repel_avoid_labels_only(
    base |> mark_series_label(x = t, y = v, color = s)
  ))
  # distinct line ends (v = 10, 15, 1) -> no spurious displacement off the lines
  sol <- repel_offsets(
    base |> mark_series_label(x = t, y = v, color = s) |> xlim(1, 12)
  )
  expect_true(all(sol$dx == 0 & sol$dy == 0))
})

test_that("crowded series ends are still separated", {
  d2 <- data.frame(
    t = rep(1:10, 3),
    v = c(1:10, (1:10) + 0.3, (1:10) - 0.3),
    s = rep(c("a", "b", "c"), each = 10)
  )
  sol <- repel_offsets(
    vplot(d2) |>
      mark_line(x = t, y = v, color = s) |>
      mark_series_label(x = t, y = v, color = s) |>
      xlim(1, 12)
  )
  expect_true(any(sol$dx != 0 | sol$dy != 0)) # near-coincident ends get spread
})

test_that("scatter-text repel is unchanged (still avoids marks)", {
  expect_false(vellumplot:::.repel_avoid_labels_only(
    vplot(mtcars) |>
      mark_point(x = wt, y = mpg) |>
      mark_text(x = wt, y = mpg, label = rownames(mtcars), repel = TRUE)
  ))
})

test_that("mark_series_label() errors clearly on a discrete x", {
  d <- data.frame(
    x = rep(c("a", "b", "c"), 3),
    y = 1:9,
    g = rep(c("s1", "s2", "s3"), each = 3)
  )
  expect_error(
    plot_svg(
      vplot(d) |>
        mark_line(x = x, y = y, color = g) |>
        mark_series_label(x = x, y = y, color = g)
    ),
    "continuous"
  )
})
