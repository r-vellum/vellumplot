# VP-126: a bare symbol in a mark argument that is not a column of the layer's
# data and holds a single value is a setting (like the literal it holds), not a
# mapping: no scale, no legend. A non-column symbol holding a longer vector
# still maps; `I(x)` forces a setting, `.env$x` forces a mapping.

has_legend <- function(p) "legend" %in% vellum::node_names(vellum::as_vellum_scene(p))
color_scale <- function(p) vellumplot:::.build_panels(p)$scales$color

test_that("a single-value R variable is a setting, not a mapping", {
  ink <- "#aa3311"
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg, color = ink)
  expect_null(color_scale(p))
  expect_false(has_legend(p))
  # drawn in that colour, as the literal would be
  lit <- vplot(mtcars) |> mark_point(x = wt, y = mpg, color = "#aa3311")
  expect_identical(
    vellum::scene_raster(vellum::as_vellum_scene(p)),
    vellum::scene_raster(vellum::as_vellum_scene(lit))
  )
  # numbers too: size / alpha / linewidth
  big <- 3
  half <- 0.5
  q <- vplot(mtcars) |> mark_point(x = wt, y = mpg, size = big, alpha = half)
  b <- vellumplot:::.build_panels(q)
  expect_null(b$scales$size)
  expect_null(b$scales$alpha)
})

test_that("a column wins over an R variable of the same name", {
  cyl <- "red"
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg, color = cyl)
  expect_false(is.null(color_scale(p)))
  expect_true(has_legend(p))
})

test_that("a longer R vector still maps", {
  grp <- rep(c("a", "b"), 16)
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg, color = grp)
  expect_identical(color_scale(p)$levels, c("a", "b"))
})

test_that("the column check uses the layer's own data", {
  ink <- "#112233"
  extra <- data.frame(u = 1:3, v = 3:1)
  # `ink` is a column of the plot data but not of the layer's own data
  d <- transform(mtcars, ink = factor(cyl))
  p <- vplot(d) |>
    mark_point(x = wt, y = mpg, color = ink) |>
    mark_line(x = u, y = v, color = ink, data = extra)
  b <- vellumplot:::.build_panels(p)
  # the point layer maps the column; the line layer takes the constant
  expect_identical(b$scales$color$levels, c("4", "6", "8"))
  L2 <- b$panels[[1]]$resolved[[2]]
  expect_identical(L2$params$color, "#112233")
})

test_that("I() forces a setting and .env$ forces a mapping", {
  ink <- "Observed"
  set <- vplot(mtcars) |> mark_point(x = wt, y = mpg, color = I("#aa3311"))
  expect_null(color_scale(set))
  mapped <- vplot(mtcars) |> mark_point(x = wt, y = mpg, color = .env$ink)
  sc <- color_scale(mapped)
  expect_identical(sc$levels, "Observed")
  expect_identical(sc$name, "ink") # the pronoun is left out of the title
  expect_true(has_legend(mapped))
  # `!!` injects the value: a setting, as before
  col <- "#aa3311"
  expect_null(color_scale(vplot(mtcars) |> mark_point(x = wt, y = mpg, color = !!col)))
})

test_that("a condition() whose if_true is a single-value variable is a setting", {
  ink <- "#aa3311"
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, color = condition("pick", ink, "grey80")) |>
    add_selection(select_point("pick"))
  expect_null(color_scale(p))
  expect_false(has_legend(p))
})

test_that("faceted and graph layers settle per layer", {
  ink <- "#aa3311"
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, color = ink) |>
    facet_wrap(~am)
  expect_false(has_legend(p))
  skip_if_not_installed("igraph")
  g <- igraph::make_ring(5)
  ec <- "#336699"
  gp <- vgraph(g) |> mark_edges(edge_color = ec) |> mark_nodes()
  expect_null(vellumplot:::.build_panels(gp)$scales$edge_color)
})

test_that("an animation keeps the setting in every frame", {
  ink <- "#aa3311"
  d <- data.frame(s = rep(c("A", "B"), each = 3), x = c(1, 2, 3, 3, 2, 1), y = 1:3)
  a <- suppressWarnings(animate(
    vplot(d, width = 3, height = 2, dpi = 50) |>
      mark_point(x = x, y = y, color = ink, size = 3) |>
      transition_states(s, wrap = FALSE, state_length = 0),
    nframes = 5
  ))
  expect_false("legend" %in% vellum::node_names(a@scenes[[1]]))
  # a mid-transition frame draws its points in the constant colour
  skip_if_not_installed("png")
  dir <- local_tempfile()
  dir.create(dir)
  vellumplot:::.anim_render(a, dir, "frames")
  fr <- png::readPNG(sort(list.files(dir, full.names = TRUE))[[3]])
  expect_gt(count_near(fr, c(0xaa, 0x33, 0x11) / 255, tol = 0.05), 10)
})

test_that("a spec round trip keeps the constant's value", {
  p <- local({
    ink <- "#aa3311"
    vplot(mtcars) |> mark_point(x = wt, y = mpg, color = ink)
  })
  ir <- as_spec(p)
  expect_identical(ir$layers[[1]]$params$color, "#aa3311")
  expect_null(ir$layers[[1]]$encoding$color)
  back <- from_spec(ir, data = mtcars)
  expect_null(color_scale(back))
  vl <- spec_to_vegalite(p)
  expect_identical(vl$encoding$color$value, "#aa3311")
})
