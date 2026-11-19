# Histogram binning with ggplot2's semantics, mark_freqpoly(), the summary
# stat's fun.data, and horizontal box plots / violins (F-10).

bin_of <- function(...) {
  .resolve_layers(vplot(mtcars) |> mark_histogram(x = mpg, ...))[[
    1
  ]]
}

test_that("bins follow ggplot2: the outer bins are centred on the data ends", {
  r <- bin_of(bins = 10)
  # ggplot2::layer_data(ggplot(mtcars, aes(mpg)) + geom_histogram(bins = 10))
  expect_equal(r$values$y, c(2, 2, 7, 6, 6, 3, 2, 0, 3, 1))
  expect_equal(range(r$values$x), range(mtcars$mpg))
})

test_that("binwidth and boundary align the bins as in ggplot2", {
  r <- bin_of(binwidth = 2, boundary = 10)
  expect_equal(r$values$x, seq(11, 33, by = 2))
  expect_equal(r$values$y, c(2, 1, 7, 3, 5, 5, 2, 2, 1, 0, 2, 2))
  expect_equal(unique(r$values$width), 2)
  # center = 11 gives the same bins
  expect_equal(bin_of(binwidth = 2, center = 11)$values$y, r$values$y)
  # bins + boundary: ggplot2 counts 2 6 5 5 7 1 2 2 1 1 with an edge at 10
  b <- bin_of(bins = 10, boundary = 10)
  expect_equal(b$values$y, c(2, 6, 5, 5, 7, 1, 2, 2, 1, 1))
  expect_equal(min(b$values$xmin), 10)
})

test_that("explicit breaks give bins of their own widths", {
  r <- bin_of(breaks = c(10, 15, 20, 25, 35))
  expect_equal(r$values$x, c(12.5, 17.5, 22.5, 30))
  expect_equal(r$values$width, c(5, 5, 5, 10))
  expect_equal(r$values$y, c(6, 12, 8, 6))
  # the x axis covers the outer bar edges
  p <- vplot(mtcars) |> mark_histogram(x = mpg, breaks = c(10, 15, 20, 25, 35))
  sc <- .train_scales(p, .resolve_layers(p))
  expect_lte(sc$x$domain[1], 10)
  expect_gte(sc$x$domain[2], 35)
})

test_that("closed = 'left' puts edge values in the bin above", {
  d <- data.frame(v = c(1, 2, 2, 3))
  right <- .resolve_layers(
    vplot(d) |> mark_histogram(x = v, breaks = c(1, 2, 3))
  )[[1]]
  left <- .resolve_layers(
    vplot(d) |> mark_histogram(x = v, breaks = c(1, 2, 3), closed = "left")
  )[[1]]
  expect_equal(right$values$y, c(3, 1))
  expect_equal(left$values$y, c(1, 3))
})

test_that("bad binning arguments fail in the constructor", {
  p <- vplot(mtcars)
  expect_error(mark_histogram(p, x = mpg, binwidth = -1), "`binwidth`")
  expect_error(mark_histogram(p, x = mpg, breaks = 3), "`breaks`")
  expect_error(mark_histogram(p, x = mpg, boundary = "a"), "`boundary`")
  expect_error(
    mark_histogram(p, x = mpg, binwidth = 1, boundary = 0, center = 0.5),
    "only one"
  )
  expect_error(mark_histogram(p, x = mpg, closed = "top"), "closed")
})

test_that("mark_freqpoly() draws padded bin counts as one line per group", {
  p <- vplot(mtcars) |>
    mark_freqpoly(x = mpg, color = factor(cyl), binwidth = 2, boundary = 10)
  expect_identical(p@layers[[1]]@mark, "line")
  r <- .resolve_layers(p)[[1]]
  for (lev in c("4", "6", "8")) {
    sel <- as.character(r$values$color) == lev
    y <- r$values$y[sel]
    # an empty bin at each end
    expect_equal(y[c(1, length(y))], c(0, 0))
    expect_equal(sum(y), sum(mtcars$cyl == as.numeric(lev)))
    expect_equal(range(r$values$x[sel]), c(9, 35))
  }
  # the y axis reads "count", as for a histogram
  ti <- .plot_titles(p)
  expect_identical(ti$title[["y"]], "count")
  expect_no_error(vellum::as_vellum_scene(p))
})

test_that("mark_summary(fun.data = ) draws a point range per category", {
  p <- vplot(mtcars) |>
    mark_summary(x = factor(cyl), y = mpg, fun.data = "mean_se")
  expect_identical(p@layers[[1]]@mark, "pointrange")
  r <- .resolve_layers(p)[[1]]
  v <- mtcars$mpg[mtcars$cyl == 4]
  se <- sd(v) / sqrt(length(v))
  expect_equal(r$values$y[1], mean(v))
  expect_equal(r$values$ymin[1], mean(v) - se)
  expect_equal(r$values$ymax[1], mean(v) + se)
  # fun.args reaches the summary
  r2 <- .resolve_layers(
    vplot(mtcars) |>
      mark_summary(
        x = factor(cyl),
        y = mpg,
        fun.data = "mean_se",
        fun.args = list(mult = 2)
      )
  )[[1]]
  expect_equal(r2$values$ymax[1], mean(v) + 2 * se)
  # fun.min / fun.max
  r3 <- .resolve_layers(
    vplot(mtcars) |>
      mark_summary(x = factor(cyl), y = mpg, fun.min = min, fun.max = max)
  )[[1]]
  expect_equal(r3$values$ymin[1], min(v))
  expect_equal(r3$values$ymax[1], max(v))
  # a user function returning a data frame (like ggplot2::mean_se())
  f <- function(x) data.frame(y = median(x), ymin = min(x), ymax = max(x))
  r4 <- .resolve_layers(
    vplot(mtcars) |> mark_summary(x = factor(cyl), y = mpg, fun.data = f)
  )[[1]]
  expect_equal(r4$values$y[1], median(v))
  # grouped and dodged, it renders
  expect_no_error(vellum::as_vellum_scene(
    vplot(mtcars) |>
      mark_summary(
        x = factor(cyl),
        y = mpg,
        color = factor(am),
        fun.data = "mean_cl_normal",
        position = "dodge"
      )
  ))
  expect_error(
    mark_summary(vplot(mtcars), x = cyl, y = mpg, fun.data = "mean_nope"),
    "fun.data"
  )
  # without a range, mark_summary() still draws points
  expect_identical(
    (vplot(mtcars) |> mark_summary(x = cyl, y = mpg))@layers[[1]]@mark,
    "point"
  )
})

test_that("box plots and violins turn horizontal for a discrete y", {
  drawn <- function(p) {
    m <- vellum::scene_model(vellum::as_vellum_scene(p))
    m$elements
  }
  expect_identical(
    .dist_orient(
      .resolve_layers(
        vplot(mtcars) |> mark_boxplot(x = mpg, y = factor(cyl))
      )[[1]]
    ),
    "y"
  )
  expect_identical(
    .dist_orient(
      .resolve_layers(
        vplot(mtcars) |> mark_boxplot(x = factor(cyl), y = mpg)
      )[[1]]
    ),
    "x"
  )
  # a horizontal box is wider than tall; its median segment is vertical
  e <- drawn(vplot(mtcars) |> mark_boxplot(x = mpg, y = factor(cyl)))
  box <- e[e$mark == "rect", ][-1, ][1, ]
  expect_gt(box$x - box$x0, box$y - box$y0)
  # the horizontal violin trains x over the density and y over the bands
  p <- vplot(iris) |> mark_violin(x = Sepal.Length, y = Species, trim = FALSE)
  sc <- .train_scales(p, .resolve_layers(p))
  supp <- range(stats::density(iris$Sepal.Length[iris$Species == "setosa"])$x)
  expect_lte(sc$x$domain[1], supp[1])
  expect_true(isTRUE(sc$y$discrete))
  # orientation can be forced
  q <- vplot(mtcars) |>
    mark_violin(x = factor(cyl), y = mpg, orientation = "x")
  expect_identical(
    .dist_orient(.resolve_layers(q)[[1]]),
    "x"
  )
  expect_error(mark_boxplot(vplot(mtcars), orientation = "z"), "orientation")
  expect_no_error(drawn(
    vplot(mtcars) |> mark_boxplot(x = mpg, y = factor(cyl), fill = factor(am))
  ))
})
