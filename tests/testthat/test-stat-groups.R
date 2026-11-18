# Stats that change the row count or order keep every carried aesthetic aligned
# with their output rows (never `rep_len`-recycled from the input order).

stat_values <- function(p) vellumplot:::.resolve_layers(p)[[1]]$values

test_that("a dot plot colours each dot by its own row", {
  d <- data.frame(x = c(5, 1, 3, 2, 4), g = c("a", "b", "a", "b", "a"))
  v <- stat_values(vplot(d) |> mark_dotplot(x = x, color = g, binwidth = 1))
  expect_equal(as.character(v$color[v$x == min(v$x)]), "b")
  # every dot carries its own observation's group, in the sorted order
  expect_equal(
    as.character(v$color[order(v$x)]),
    d$g[order(d$x)]
  )
})

test_that("hex and 2-D bins are counted per group, coloured by it", {
  set.seed(1)
  d <- data.frame(
    x = c(rnorm(50, 0), rnorm(50, 5)),
    y = c(rnorm(50, 0), rnorm(50, 5)),
    g = rep(c("lo", "hi"), each = 50)
  )
  for (mk in c(mark_hex, mark_bin2d)) {
    v <- stat_values(
      vplot(d) |>
        mk(x = x, y = y, color = g, alpha = after_stat(count), bins = 5)
    )
    expect_length(v$color, length(v$x))
    # the "lo" cloud sits near the origin, the "hi" one near (5, 5)
    expect_true(all(v$x[v$color == "lo"] < 3.5))
    expect_true(all(v$x[v$color == "hi"] > 1.5))
    # counts add up per group
    expect_equal(sum(v$alpha[v$color == "lo"]), 50)
  }
})

test_that("mark_contour(color =) estimates one density per group", {
  skip_if_not_installed("MASS")
  pooled <- stat_values(
    vplot(iris) |> mark_contour(x = Sepal.Length, y = Sepal.Width)
  )
  grouped <- stat_values(
    vplot(iris) |>
      mark_contour(x = Sepal.Length, y = Sepal.Width, color = Species)
  )
  expect_setequal(as.character(unique(grouped$color)), levels(iris$Species))
  expect_length(grouped$color, length(grouped$x))
  # each species' rings stay inside (roughly) that species' own range
  for (sp in levels(iris$Species)) {
    sub <- iris$Sepal.Length[iris$Species == sp]
    gx <- grouped$x[grouped$color == sp]
    expect_lt(min(gx), max(sub))
    expect_gt(max(gx), min(sub))
  }
  # no ring is shared between species
  piece_sp <- tapply(as.character(grouped$color), grouped$.piece, unique)
  expect_true(all(lengths(piece_sp) == 1L))
  expect_false(identical(pooled$x, grouped$x))
})

test_that("row-filtering stats carry aesthetics by row", {
  d <- data.frame(
    x = 1:12,
    y = c(rep(1, 11), 50),
    s = c(rep("small", 11), "big")
  )
  v <- stat_values(
    vplot(d) |> mark_outlier_label(x = x, y = y, size = 4, alpha = s)
  )
  expect_equal(v$alpha, "big")
})

test_that("an aesthetic varying within a group is dropped with a warning", {
  expect_warning(
    v <- stat_values(
      vplot(mtcars) |> mark_smooth(x = wt, y = mpg, color = hp, method = "lm")
    ),
    "Dropped.*color"
  )
  expect_null(v$color)
  # one constant within each group survives, aligned to the fitted rows
  d <- data.frame(
    x = rep(1:5, 2),
    y = c(1:5, 5:1),
    g = rep(c("a", "b"), each = 5),
    w = rep(c(0.3, 0.9), each = 5)
  )
  v <- stat_values(
    vplot(d) |> mark_smooth(x = x, y = y, color = g, alpha = w, method = "lm")
  )
  expect_equal(
    c(tapply(v$alpha, as.character(v$color), unique)),
    c(a = 0.3, b = 0.9)
  )
})

test_that(".merge_stat aborts on a carried aesthetic it cannot align", {
  L <- list(
    values = list(x = 1:4, color = c("a", "b", "c", "d")),
    types = list(),
    after = list(),
    params = list(),
    n = 4L
  )
  # a stat that returns fewer rows with neither `group` nor `.rows`, while the
  # colour is unrealignable (it is not of input length), trips the guard
  L$values$color <- c("a", "b", "c")
  expect_error(
    vellumplot:::.merge_stat(L, data.frame(x = 1:2, y = 1:2), "fake"),
    class = "rlang_error",
    regexp = "fake.*color"
  )
})
