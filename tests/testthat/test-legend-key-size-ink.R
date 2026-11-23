# VP-113: point legend keys take the layer's constant point size (with its
# shape's size factor), capped at half the key side; a size legend keeps its
# mapped sizes.
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
