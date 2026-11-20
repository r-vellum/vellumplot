# A layer groups by the interaction of all its discrete grouping aesthetics,
# as ggplot2's add_group() does (VP-52).

cars <- function() {
  d <- mtcars
  d$cyl <- factor(d$cyl)
  d$gear <- factor(d$gear)
  d$am <- factor(d$am)
  d$vs <- factor(d$vs)
  d
}

quiet_layer <- function(p) {
  expect_no_warning(L <- .resolve_layers(p)[[1]])
  L
}

test_that("a single discrete aesthetic is the key unchanged", {
  d <- cars()
  L <- .resolve_layer(
    (vplot(d) |> mark_point(x = wt, y = mpg, color = gear))@layers[[1]],
    d
  )
  expect_identical(.group_key(L), d$gear)
  # the same variable on two aesthetics splits nothing further
  L <- .resolve_layer(
    (vplot(d) |> mark_point(x = wt, y = mpg, color = gear, fill = gear))@layers[[
      1
    ]],
    d
  )
  expect_identical(.group_key(L), d$gear)
  # a continuous colour never groups
  L <- .resolve_layer(
    (vplot(d) |> mark_point(x = wt, y = mpg, color = hp, shape = am))@layers[[
      1
    ]],
    d
  )
  expect_identical(.group_key(L), d$am)
})

test_that("the interaction runs in mapping order, first slowest", {
  k <- .interaction_key(list(
    fill = factor(c("b", "a", "b", "a"), levels = c("b", "a")),
    color = c("y", "x", "x", "y")
  ))
  expect_identical(levels(k), c("b:x", "b:y", "a:x", "a:y"))
  expect_identical(as.character(k), c("b:y", "a:x", "b:x", "a:y"))
  # a leading constant gives way; a missing category is a group of its own
  expect_identical(.interaction_key(list(rep("k", 3), c("a", "b", "a"))), c(
    "a",
    "b",
    "a"
  ))
  k <- .interaction_key(list(c("a", "a", "b"), c("x", NA, "x")))
  expect_identical(levels(k), c("a:x", "a:NA", "b:x"))
})

test_that("bars count, stack and keep both aesthetics per (fill, color) pair", {
  d <- cars()
  L <- quiet_layer(vplot(d) |> mark_bar(x = cyl, fill = gear, color = am))
  expect_equal(L$n, 10L)
  expect_false(is.null(L$values$fill))
  expect_false(is.null(L$values$color))
  tab <- as.data.frame(table(d$cyl, d$gear, d$am))
  tab <- tab[tab$Freq > 0, ]
  h <- L$values$ymax - L$values$ymin
  got <- paste(L$values$x, L$values$fill, L$values$color, h)
  expect_setequal(got, paste(tab$Var1, tab$Var2, tab$Var3, tab$Freq))
  # stacked: each x's spans tile [0, total] without overlap
  for (x in levels(d$cyl)) {
    i <- which(L$values$x == x)
    o <- i[order(L$values$ymin[i])]
    expect_equal(L$values$ymin[o][-1], L$values$ymax[o][-length(o)])
    expect_equal(max(L$values$ymax[i]), sum(d$cyl == x))
  }
})

test_that("stacks and dodges match ggplot2's layer_data()", {
  skip_if_not_installed("ggplot2")
  d <- cars()
  L <- quiet_layer(vplot(d) |> mark_bar(x = cyl, fill = gear, color = am))
  g <- ggplot2::layer_data(
    ggplot2::ggplot(d, ggplot2::aes(cyl, fill = gear, colour = am)) +
      ggplot2::geom_bar()
  )
  v <- data.frame(x = as.integer(L$values$x), ymin = L$values$ymin)
  v <- v[order(v$x, v$ymin), ]
  g <- g[order(g$x, g$ymin), ]
  expect_equal(v$ymin, g$ymin)
  expect_equal(length(unique(.group_key(L))), 4L)

  # dodge: one slot per pair present at an x
  L <- quiet_layer(
    vplot(d) |> mark_bar(x = cyl, fill = gear, color = am, position = "dodge")
  )
  dd <- .dodge_offsets(as.numeric(L$values$x), .group_key(L), 0.9)
  at4 <- as.integer(L$values$x) == 1L
  expect_equal(sort(dd$offset[at4]), c(-0.3375, -0.1125, 0.1125, 0.3375))
})

test_that("boxplots dodge one box per (fill, color) pair, as ggplot2", {
  d <- cars()
  p <- vplot(d) |> mark_boxplot(x = cyl, y = mpg, fill = am, color = vs)
  L <- quiet_layer(p)
  sc <- list(x = list(map = function(v) match(as.character(v), levels(d$cyl))))
  cells <- .by_category(L, sc, "x", span = 0.75)
  centres <- vapply(cells, `[[`, numeric(1), "centre")
  # ggplot2::layer_data(ggplot(d, aes(cyl, mpg, fill = am, colour = vs)) +
  #   geom_boxplot())$x
  expect_equal(centres, c(0.75, 1, 1.25, 1.8125, 2.1875, 2.8125, 3.1875))
})

test_that("densities and fits run per pair, and draw one series each", {
  d <- cars()
  L <- quiet_layer(vplot(d) |> mark_density(x = mpg, color = am, linetype = vs))
  expect_equal(length(unique(.group_key(L))), 4L)
  expect_equal(L$n, 4L * 512L)

  L <- quiet_layer(
    vplot(d) |>
      mark_smooth(x = wt, y = mpg, color = am, linetype = vs, method = "lm")
  )
  expect_false(is.null(L$values$linetype))
  # a smooth styles its ribbon by colour only; the key still keeps the four
  # fits apart
  col <- as.character(L$values$color)
  groups <- .series_groups(L, L$n, list(col = col))
  expect_length(groups, 4L)
  expect_true(all(vapply(
    groups,
    function(i) length(unique(as.character(.group_key(L))[i])) == 1L,
    logical(1)
  )))
})
