# The ggplot2-equivalent geometry marks of F-16: mark_rect(), mark_polygon(),
# mark_path(), mark_curve() and mark_spoke().

prov_of <- function(p, mark) {
  pr <- attr(as_vellum_scene(p), "vellumplot_provenance")
  Filter(function(e) identical(e$mark, mark), pr)
}
layer_values <- function(p) vellumplot:::.resolve_layers(p)[[1]]$values
data_keys <- function(p) {
  el <- vellum::scene_model(vellum::as_vellum_scene(p))$elements
  k <- el$key[!is.na(el$key)]
  k[!startsWith(k, "legend:")]
}

recs <- data.frame(
  start = c(1, 4, 7),
  end = c(3, 5, 10),
  party = c("A", "B", "A"),
  id = c("r1", "r2", "r3")
)

test_that("mark_rect() is a layer mark with corner bounds", {
  p <- vplot(recs) |>
    mark_rect(xmin = start, xmax = end, ymin = 0, ymax = 1, fill = party)
  expect_identical(p@layers[[1]]@mark, "rect")
  # a literal bound is a coordinate (a value, not a style param) and trains y
  v <- layer_values(p)
  expect_equal(v$ymin, 0)
  sc <- vellumplot:::.build_panels(p)$scales
  expect_true(sc$y$domain[1] <= 0 && sc$y$domain[2] >= 1)
  expect_true(sc$x$domain[1] <= 1 && sc$x$domain[2] >= 10)
  expect_length(prov_of(p, "rect"), 2L) # one rect grob per fill group
  # interactive keys, one per rectangle
  k <- vplot(recs) |>
    mark_rect(xmin = start, xmax = end, ymin = 0, ymax = 1, data_id = id)
  expect_setequal(data_keys(k), recs$id)
})

test_that("mark_rect() outlines with color when a fill is given", {
  fo <- function(...) {
    p <- vplot(recs) |>
      mark_rect(xmin = start, xmax = end, ymin = 0, ymax = 1, ...)
    L <- vellumplot:::.resolve_layers(p)[[1]]
    L$n <- 3L
    vellumplot:::.fill_and_outline(L, list(), "grey35")
  }
  expect_identical(fo()$fill, rep("grey35", 3))
  expect_true(all(is.na(fo()$col)))
  both <- fo(fill = "white", color = "red")
  expect_identical(both$fill, rep("white", 3))
  expect_identical(both$col, rep("red", 3))
  # a colour alone fills, as on the other filled marks
  only <- fo(color = "red")
  expect_identical(only$fill, rep("red", 3))
  expect_true(all(is.na(only$col)))
  expect_no_error(vellum::as_vellum_scene(
    vplot(recs) |>
      mark_rect(
        xmin = start,
        xmax = end,
        ymin = 0,
        ymax = 1,
        fill = party,
        color = "black"
      ) |>
      facet_wrap(~party)
  ))
})

test_that("a literal xend / ymin trains its axis (not a style param)", {
  p <- vplot(data.frame(x = 1, y = 1)) |>
    mark_segment(x = x, y = y, xend = 5, yend = 3)
  expect_equal(layer_values(p)$xend, 5)
  sc <- vellumplot:::.build_panels(p)$scales
  expect_gte(sc$x$domain[2], 5)
})

tri <- data.frame(
  id = rep(c("a", "b"), each = 3),
  x = c(0, 1, 0.5, 2, 3, 2.5),
  y = c(0, 0, 1, 0, 0, 1)
)

test_that("mark_polygon() draws one shape per group", {
  p <- vplot(tri) |> mark_polygon(x = x, y = y, group = id)
  pr <- prov_of(p, "polygon")
  expect_length(pr, 2L)
  expect_identical(lapply(pr, `[[`, "rows"), list(1:3, 4:6))
  # a mapped fill groups too, and keys each polygon by its rows
  q <- vplot(tri) |> mark_polygon(x = x, y = y, fill = id, data_id = id)
  expect_length(prov_of(q, "polygon"), 2L)
  expect_setequal(unique(data_keys(q)), c("a", "b"))
  expect_no_error(vellum::as_vellum_scene(
    vplot(tri) |> mark_polygon(x = x, y = y, fill = id) |> facet_wrap(~id)
  ))
  expect_no_error(vellum::as_vellum_scene(
    vplot(tri) |> mark_polygon(x = x, y = y, group = id) |> coord_polar()
  ))
})

test_that("mark_polygon() cuts holes with subgroup", {
  sq <- data.frame(
    x = c(0, 4, 4, 0, 1, 3, 3, 1),
    y = c(0, 0, 4, 4, 1, 1, 3, 3),
    ring = rep(1:2, each = 4)
  )
  p <- vplot(sq) |> mark_polygon(x = x, y = y, subgroup = ring)
  expect_length(prov_of(p, "polygon"), 1L) # one path, two rings
  img <- render_px(p)
  # the centre of the hole shows the panel, the frame is the dark fill
  H <- dim(img)[1]
  W <- dim(img)[2]
  mid <- img[round(H * 0.45), round(W * 0.55), 1:3]
  expect_gt(mean(mid), 0.5)
})

test_that("mark_path() keeps the data order; mark_line() sorts by x", {
  d <- data.frame(x = c(3, 1, 2), y = c(1, 2, 3))
  path <- prov_of(vplot(d) |> mark_path(x = x, y = y), "line")
  line <- prov_of(vplot(d) |> mark_line(x = x, y = y), "line")
  expect_identical(path[[1]]$rows, 1:3)
  expect_identical(line[[1]]$rows, c(2L, 3L, 1L))
  p <- vplot(rbind(d, d)) |>
    mark_path(x = x, y = y, group = rep(1:2, each = 3))
  expect_length(prov_of(p, "line"), 2L)
})

test_that(".curve_points() bends like grid's curveGrob()", {
  cp <- vellumplot:::.curve_points
  # curvature 1 is a half circle on the chord
  s <- cp(0, 0, 2, 0, 1)
  expect_equal(range(s$x), c(0, 2))
  expect_equal(max(abs(s$y)), 1, tolerance = 1e-3)
  # positive curvature bends to the right of the direction of travel
  expect_lt(min(cp(0, 0, 2, 0, 0.5)$y), 0)
  expect_gt(max(cp(0, 0, 2, 0, -0.5)$y), 0)
  # the sagitta is curvature x half-chord
  expect_equal(max(abs(cp(0, 0, 2, 0, 0.5)$y)), 0.5, tolerance = 1e-3)
  # angle < 90 moves the apex towards the start
  a <- cp(0, 0, 2, 0, 0.5, angle = 45)
  expect_lt(a$x[which.min(a$y)], 1)
  # zero curvature is the straight segment
  expect_identical(cp(0, 0, 2, 1, 0), list(x = c(0, 2), y = c(0, 1)))
})

test_that("mark_curve() draws one curve per row and validates its arguments", {
  d <- data.frame(x = c(1, 1), y = c(1, 3), xend = c(5, 5), yend = c(4, 1))
  p <- vplot(d) |> mark_curve(x = x, y = y, xend = xend, yend = yend)
  expect_identical(p@layers[[1]]@mark, "segment")
  expect_identical(p@layers[[1]]@stat_params$curvature, 0.5)
  pr <- prov_of(p, "segment")
  expect_length(pr, 2L)
  expect_identical(lapply(pr, `[[`, "rows"), list(1L, 2L))
  expect_error(mark_curve(vplot(d), curvature = NA), "curvature")
  expect_error(mark_curve(vplot(d), angle = 180), "angle")
  expect_no_error(vellum::as_vellum_scene(
    vplot(d) |>
      mark_curve(x = x, y = y, xend = xend, yend = yend, color = factor(x)) |>
      coord_polar()
  ))
})

test_that("mark_spoke() matches ggplot2's geom_spoke() end points", {
  d <- data.frame(
    x = c(1, 2, 3),
    y = c(1, 2, 1),
    a = c(0, pi / 2, pi / 4),
    r = c(1, 0.5, 2)
  )
  v <- layer_values(vplot(d) |> mark_spoke(x = x, y = y, angle = a, radius = r))
  # ggplot2::layer_data(ggplot(d, aes(x, y, angle = a, radius = r)) + geom_spoke())
  expect_equal(v$xend, c(2, 2, 4.414214), tolerance = 1e-6)
  expect_equal(v$yend, c(1, 2.5, 2.414214), tolerance = 1e-6)
  expect_null(v$angle)
  # a constant radius works too
  w <- layer_values(vplot(d) |> mark_spoke(x = x, y = y, angle = a, radius = 1))
  expect_equal(w$xend, d$x + cos(d$a))
  expect_error(mark_spoke(vplot(d), x = x, y = y), "angle")
})

test_that("the new marks round-trip through the spec serializer", {
  d <- data.frame(x = c(1, 1), y = c(1, 3), xend = c(5, 5), yend = c(4, 1))
  for (p in list(
    vplot(tri) |> mark_polygon(x = x, y = y, group = id, fill = id),
    vplot(tri) |> mark_path(x = x, y = y),
    vplot(d) |>
      mark_curve(x = x, y = y, xend = xend, yend = yend, curvature = -0.3),
    vplot(d) |> mark_spoke(x = x, y = y, angle = 1, radius = 0.5),
    vplot(recs) |> mark_rect(xmin = start, xmax = end, ymin = 0, ymax = 1)
  )) {
    s <- as_spec(p)
    p2 <- from_spec(s)
    expect_identical(as_spec(p2), s)
    expect_identical(p2@layers[[1]]@stat_params, p@layers[[1]]@stat_params)
    expect_no_error(vellum::as_vellum_scene(p2))
  }
})
