# VP-67: the legend keys of filled marks take the layer alpha, as ggplot2's
# draw_key_rect() / draw_key_polygon() do. A density's default fill alpha
# (VP-66) reaches the key's fill and leaves its outline opaque; a set alpha
# reaches fill and outline alike, as the mark draws them.

# The legend's key rects (fill, col, alpha) in drawing order: every grob_rect
# inside the legend viewport.
ka_rects <- function(p) {
  s <- vellum::as_scene_spec(vellum::as_vellum_scene(p))
  val <- function(x) if (is.list(x)) x$v else x
  out <- list()
  walk <- function(node, inside) {
    nm <- val(node$name)
    if (is.character(nm) && length(nm) == 1L && grepl("legend", nm)) {
      inside <- TRUE
    }
    if (inside && identical(node$class, "grob_rect") && is.list(node$gp)) {
      g <- node$gp
      a <- val(g$alpha)
      out[[length(out) + 1L]] <<- data.frame(
        fill = format(val(g$fill) %||% NA),
        col = format(val(g$col) %||% NA),
        alpha = if (is.null(a)) NA_real_ else as.numeric(a)
      )
    }
    for (ch in node) {
      if (is.list(ch)) walk(ch, inside)
    }
  }
  walk(s$root, FALSE)
  do.call(rbind, out)
}

ka_strip <- data.frame(x = 1:10, y = 1:10, g = rep(c("a", "b"), 5))

test_that("density keys take the default fill alpha", {
  for (th in list(identity, theme_dark)) {
    k <- ka_rects(th(
      vplot(mtcars) |> mark_density(x = mpg, fill = factor(cyl))
    ))
    expect_identical(k$alpha, rep(0.8, 3))
  }
})

test_that("density keys take a set alpha", {
  k <- ka_rects(
    vplot(mtcars) |> mark_density(x = mpg, fill = factor(cyl), alpha = 0.3)
  )
  expect_identical(k$alpha, rep(0.3, 3))
})

test_that("a density key's outline stays opaque under the default alpha", {
  k <- ka_rects(
    vplot(mtcars) |> mark_density(x = mpg, fill = factor(cyl), color = "black")
  )
  # each key is a see-through fill under an opaque outline
  expect_identical(nrow(k), 6L)
  expect_identical(k$alpha[c(1, 3, 5)], rep(0.8, 3))
  expect_true(all(k$col[c(1, 3, 5)] == "NA"))
  expect_true(all(is.na(k$alpha[c(2, 4, 6)])))
  expect_true(all(k$fill[c(2, 4, 6)] == "NA"))
  expect_true(all(k$col[c(2, 4, 6)] == "black"))
  # a merged fill + colour legend does the same
  m <- ka_rects(
    vplot(mtcars) |>
      mark_density(x = mpg, fill = factor(cyl), color = factor(cyl))
  )
  expect_identical(m$alpha, rep(c(0.8, NA), 3))
})

test_that("a set alpha reaches the key's outline, as it does the mark's", {
  k <- ka_rects(
    vplot(mtcars) |>
      mark_density(x = mpg, fill = factor(cyl), color = "black", alpha = 0.5)
  )
  expect_identical(nrow(k), 3L)
  expect_identical(k$alpha, rep(0.5, 3))
  expect_true(all(k$col == "black"))
})

test_that("area-like marks pass a set alpha to their keys", {
  cases <- list(
    area = vplot(ka_strip) |> mark_area(x = x, y = y, fill = g, alpha = 0.4),
    ribbon = vplot(ka_strip) |>
      mark_ribbon(x = x, ymin = y - 1, ymax = y + 1, fill = g, alpha = 0.4),
    bar = vplot(transform(mtcars, g = factor(cyl))) |>
      mark_bar(x = factor(gear), fill = g, alpha = 0.4),
    tile = vplot(expand.grid(x = 1:2, y = 1:2)) |>
      mark_tile(x = x, y = y, fill = factor(x), alpha = 0.4),
    boxplot = vplot(ka_strip) |>
      mark_boxplot(x = g, y = y, fill = g, alpha = 0.4)
  )
  for (nm in names(cases)) {
    k <- ka_rects(cases[[nm]])
    expect_true(nrow(k) >= 2L, label = nm)
    expect_true(all(k$alpha == 0.4), label = nm)
  }
  # a colour-outlined violin key (an outlined box) too
  v <- ka_rects(
    vplot(ka_strip) |> mark_violin(x = g, y = y, color = g, alpha = 0.4)
  )
  expect_true(all(v$alpha == 0.4))
})

test_that("keys stay opaque without an alpha", {
  k <- ka_rects(vplot(ka_strip) |> mark_area(x = x, y = y, fill = g))
  expect_true(all(is.na(k$alpha)))
  k <- ka_rects(
    vplot(ka_strip) |> mark_area(x = x, y = y, fill = g, color = "black")
  )
  expect_identical(nrow(k), 2L)
  expect_true(all(is.na(k$alpha)))
})

test_that("a mapped alpha keeps its own legend and the fill keys opaque", {
  # alpha mapped to another variable: its legend draws the alphas, the fill
  # keys stay opaque (ggplot2 does the same)
  p <- suppressWarnings(
    vplot(mtcars) |>
      mark_density(x = mpg, fill = factor(cyl), alpha = factor(am))
  )
  k <- suppressWarnings(ka_rects(p))
  expect_identical(nrow(k), 3L)
  expect_true(all(is.na(k$alpha)))
  sc <- suppressWarnings(
    vellumplot:::.train_scales(p, vellumplot:::.resolve_layers(p))
  )
  expect_false(is.null(sc$alpha))
  # alpha mapped to the fill variable merges into one legend of alphas
  m <- suppressWarnings(ka_rects(
    vplot(mtcars) |>
      mark_density(x = mpg, fill = factor(cyl), alpha = factor(cyl))
  ))
  expect_length(unique(m$alpha), 3L)
  expect_false(any(m$alpha == 0.8))
})

test_that("override.aes alpha still wins", {
  k <- ka_rects(
    vplot(mtcars) |>
      mark_density(x = mpg, fill = factor(cyl)) |>
      guides(fill = guide_legend(override.aes = list(alpha = 1)))
  )
  expect_identical(k$alpha, rep(1, 3))
})

test_that("point keys are unchanged by a layer alpha", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, color = factor(cyl), alpha = 0.4)
  expect_null(
    vellumplot:::.key_alpha_for_aes(vellumplot:::.resolve_layers(p), "color")
  )
})
