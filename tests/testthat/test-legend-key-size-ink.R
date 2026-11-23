# VP-113: point legend keys take the layer's constant point size (with its
# shape's size factor), capped at half the key side; a size legend keeps its
# mapped sizes.
# VP-118: a point key is no smaller than the theme's `legend.key.point.min`
# (default 1 mm, an unsized point's radius), so tiny points keep readable keys.
# VP-114: line, line-width, line-only merged and alpha keys draw in their
# layer's ink -- its constant colour, else its default (black, the theme's
# ink) -- not grey35 / grey20.

# The legend's point and segment grobs: class, shape, fill, outline, size (mm).
legend_keys <- function(p) {
  s <- vellum::as_scene_spec(vellum::as_vellum_scene(p))
  val <- function(x) if (is.list(x)) x$v else x
  out <- list()
  walk <- function(node, inside) {
    nm <- val(node$name)
    if (is.character(nm) && length(nm) == 1L && grepl("legend", nm)) {
      inside <- TRUE
    }
    if (inside && isTRUE(node$class %in% c("grob_points", "grob_segments"))) {
      out[[length(out) + 1L]] <<- data.frame(
        class = node$class,
        shape = as.character(val(node$shape) %||% NA)[1],
        fill = format(val(node$gp$fill) %||% NA)[1],
        col = format(val(node$gp$col) %||% NA)[1],
        size = val(node$size$value)[1] %||% NA_real_
      )
    }
    for (ch in node) {
      if (is.list(ch)) walk(ch, inside)
    }
  }
  walk(s$root, FALSE)
  do.call(rbind, out)
}

mt <- transform(mtcars, fcyl = factor(cyl))
ec <- data.frame(
  x = rep(1:10, 2),
  y = c(1:10, 10:1),
  g = rep(c("a", "b"), each = 10)
)

test_that("shape and colour keys take the layer's constant point size", {
  k <- legend_keys(
    vplot(mt) |>
      mark_point(x = wt, y = mpg, shape = fcyl, size = 1) |>
      scale_shape_manual(values = c(16, 17, 15))
  )
  # pch 16, 17 (at R's size for it, VP-111), 15
  expect_equal(
    k$size,
    c(1, sqrt(4 * pi / (3 * sqrt(3))), 1)
  )
  k <- legend_keys(
    vplot(mt) |> mark_point(x = wt, y = mpg, color = fcyl, size = 1.2)
  )
  expect_equal(unique(k$size), 1.2)
})

test_that("a large constant point size is capped at half the key side", {
  k <- legend_keys(
    vplot(mt) |> mark_point(x = wt, y = mpg, color = fcyl, size = 6)
  )
  expect_equal(unique(k$size), 2)
  # no constant size: half the key side, as before
  k <- legend_keys(vplot(mt) |> mark_point(x = wt, y = mpg, color = fcyl))
  expect_equal(unique(k$size), 2)
})

test_that("a merged colour + shape legend takes the constant point size", {
  k <- legend_keys(
    vplot(mt) |>
      mark_point(x = wt, y = mpg, color = fcyl, shape = fcyl, size = 1)
  )
  expect_equal(k$size[k$shape == "circle"], 1)
})

test_that("a size legend keeps its mapped sizes", {
  base <- legend_keys(vplot(mt) |> mark_point(x = wt, y = mpg, size = hp))
  k <- legend_keys(
    vplot(mt) |> mark_point(x = wt, y = mpg, size = hp, shape = 17)
  )
  expect_equal(k$size, base$size * sqrt(4 * pi / (3 * sqrt(3))))
  expect_gt(length(unique(k$size)), 1)
})

test_that("line keys draw in the layer's ink, not grey35", {
  lt <- legend_keys(vplot(ec) |> mark_line(x = x, y = y, linetype = g))
  expect_identical(unique(lt$col), "black")
  lw <- legend_keys(vplot(ec) |> mark_line(x = x, y = y, group = g, linewidth = y))
  expect_identical(unique(lw$col), "black")
  # the layer's constant colour
  red <- legend_keys(
    vplot(ec) |> mark_line(x = x, y = y, linetype = g, color = "red")
  )
  expect_identical(unique(red$col), "red")
  # a line-only merged legend
  mg <- suppressWarnings(legend_keys(
    vplot(ec) |> mark_line(x = x, y = y, linetype = g, alpha = g)
  ))
  expect_identical(unique(mg$col), "black")
})

test_that("line keys take the theme's ink", {
  lt <- legend_keys(
    vplot(ec) |> mark_line(x = x, y = y, linetype = g) |> theme_cyberpunk()
  )
  # theme_cyberpunk()'s ink, as its lines are drawn
  expect_identical(unique(tolower(lt$col)), "#c7d0f0")
})

test_that("alpha keys of a point layer take its colour and size", {
  k <- legend_keys(vplot(mt) |> mark_point(x = wt, y = mpg, alpha = hp))
  expect_identical(unique(k$fill), "black")
  k <- legend_keys(
    vplot(mt) |> mark_point(x = wt, y = mpg, alpha = hp, color = "navy", size = 1.5)
  )
  expect_identical(unique(k$fill), "navy")
  expect_equal(unique(k$size), 1.5)
  # a bar layer keeps the neutral grey
  k <- legend_keys(
    vplot(mt) |> mark_bar(x = fcyl, y = mpg, alpha = hp, stat = "identity")
  )
  expect_identical(unique(k$fill), "grey20")
})

test_that("SVG icon shape keys keep the key size, whatever the layer size", {
  icons <- c(
    a = "M0 0 L10 0 L10 10 Z",
    b = "M0 0 L10 10 L0 10 Z",
    c = "M5 0 L10 10 L0 10 Z"
  )
  p <- suppressWarnings(
    vplot(mt) |>
      mark_point(x = wt, y = mpg, shape = fcyl, size = 1.5) |>
      scale_shape_manual(values = unname(icons))
  )
  expect_no_error(vellum::as_vellum_scene(p))
  half <- vellumplot:::.key_point_size(list(size = 1.5), list(key = 4), icons[[1]])
  expect_equal(half, 2)
  expect_equal(
    vellumplot:::.key_point_size(list(size = 1.5), list(key = 4), "circle"),
    1.5
  )
})

test_that("a tiny constant point size keys at the minimum key size (VP-118)", {
  # colour keys
  k <- legend_keys(
    vplot(mt) |> mark_point(x = wt, y = mpg, color = fcyl, size = 0.4)
  )
  expect_equal(unique(k$size), 1)
  # shape keys keep their shape's size factor on top of the minimum
  k <- legend_keys(
    vplot(mt) |>
      mark_point(x = wt, y = mpg, shape = fcyl, size = 0.3) |>
      scale_shape_manual(values = c(16, 17, 15))
  )
  expect_equal(k$size, c(1, sqrt(4 * pi / (3 * sqrt(3))), 1))
  # merged colour + shape keys
  k <- legend_keys(
    vplot(mt) |>
      mark_point(x = wt, y = mpg, color = fcyl, shape = fcyl, size = 0.5)
  )
  expect_equal(k$size[k$shape == "circle"], 1)
  # alpha keys
  k <- legend_keys(
    vplot(mt) |> mark_point(x = wt, y = mpg, alpha = hp, size = 0.2)
  )
  expect_equal(unique(k$size), 1)
})

test_that("sizes above the minimum key unchanged (VP-118)", {
  k <- legend_keys(
    vplot(mt) |> mark_point(x = wt, y = mpg, color = fcyl, size = 1)
  )
  expect_equal(unique(k$size), 1)
  k <- legend_keys(
    vplot(mt) |> mark_point(x = wt, y = mpg, color = fcyl, size = 1.8)
  )
  expect_equal(unique(k$size), 1.8)
  # the cap at half the key side still holds
  k <- legend_keys(
    vplot(mt) |> mark_point(x = wt, y = mpg, color = fcyl, size = 5)
  )
  expect_equal(unique(k$size), 2)
})

test_that("a size legend keeps tiny mapped sizes below the minimum (VP-118)", {
  p <- vplot(mt) |>
    mark_point(x = wt, y = mpg, size = hp) |>
    scale_size(range = c(0.1, 0.6))
  k <- legend_keys(p)
  expect_gt(length(unique(k$size)), 1)
  expect_true(all(k$size < 1))
  # a raised minimum leaves them alone too
  k2 <- legend_keys(p |> theme(legend.key.point.min = 1.5))
  expect_equal(k2$size, k$size)
  # as does an override.aes size on a colour key
  k <- legend_keys(
    vplot(mt) |>
      mark_point(x = wt, y = mpg, color = fcyl, size = 0.3) |>
      guides(color = guide_legend(override.aes = list(size = 0.5)))
  )
  expect_equal(unique(k$size), 0.5)
})

test_that("legend.key.point.min is themeable and validated (VP-118)", {
  p <- vplot(mt) |> mark_point(x = wt, y = mpg, color = fcyl, size = 0.4)
  k <- legend_keys(p |> theme(legend.key.point.min = 1.6))
  expect_equal(unique(k$size), 1.6)
  # 0 restores the layer's own size
  k <- legend_keys(p |> theme(legend.key.point.min = 0))
  expect_equal(unique(k$size), 0.4)
  # a minimum past the key keeps to half the key side
  k <- legend_keys(p |> theme(legend.key.point.min = 5))
  expect_equal(unique(k$size), 2)
  # no constant size: half the key side, whatever the minimum
  k <- legend_keys(
    vplot(mt) |>
      mark_point(x = wt, y = mpg, color = fcyl) |>
      theme(legend.key.point.min = 0.5)
  )
  expect_equal(unique(k$size), 2)
  expect_error(theme(p, legend.key.point.min = -1), "legend.key.point.min")
  expect_error(theme(p, legend.key.point.min = "big"), "legend.key.point.min")
})
