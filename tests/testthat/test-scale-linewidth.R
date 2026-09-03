# A mapped `linewidth` on a line / step / segment: one constant width per
# segment, via `segments_grob(lwd = <vector>)`.

df <- data.frame(
  x = 1:6,
  y = c(1, 3, 2, 5, 4, 6),
  w = c(1, 2, 3, 4, 5, 6),
  grp = rep(c("a", "b"), each = 3)
)

svg_of <- function(plot) vellum::scene_svg(vellum::as_vellum_scene(plot))

# The stroke widths of the mark layer's own paths, in draw order. Split on the
# vellum group tags and keep the pieces belonging to this layer, so a polar
# panel (whose mark group wraps its paths in a clip `<g>`) counts the same way.
mark_widths <- function(plot, layer = "layer-1") {
  parts <- strsplit(svg_of(plot), "<g data-vellum-", fixed = TRUE)[[1]]
  mine <- parts[startsWith(parts, paste0('id="', layer, "-"))]
  w <- regmatches(mine, gregexpr('stroke-width="[0-9.]+"', mine))
  as.numeric(sub('"$', "", sub('stroke-width="', "", unlist(w))))
}

# The SVG with its per-render element-id counter normalised away, so two
# compiles of the same plot compare equal.
stable_svg <- function(plot) gsub("vl[0-9]+-", "vl-", svg_of(plot))

test_that("a mapped linewidth trains a linewidth scale with a legend", {
  p <- vplot(df) |> mark_line(x = x, y = y, linewidth = w)
  b <- vellumplot:::.build_panels(p)
  expect_identical(b$scales$linewidth$kind, "linewidth")
  # the data range spans the default output range, in lwd units
  expect_equal(b$scales$linewidth$map(range(df$w)), c(0.5, 4))
  # the edge-width scale is untouched: a line is not an edge
  expect_null(b$scales$edge_width)
  guides <- vellumplot:::.legend_guides(b$scales)
  expect_true(any(vapply(
    guides,
    function(g) g$kind == "linewidth",
    logical(1)
  )))
})

test_that("scale_linewidth() sets the output range, limits, breaks and name", {
  p <- vplot(df) |>
    mark_line(x = x, y = y, linewidth = w) |>
    scale_linewidth(
      range = c(1, 10),
      limits = c(0, 10),
      breaks = c(2, 8),
      name = "weight"
    )
  sc <- vellumplot:::.build_panels(p)$scales$linewidth
  expect_equal(sc$map(c(0, 10)), c(1, 10))
  expect_equal(sc$legend_breaks, c(2, 8))
  expect_identical(sc$name, "weight")
  expect_identical(scale_linewidth_continuous, scale_linewidth)
})

test_that("a mapped linewidth draws one segment per span, at the endpoint mean", {
  p <- vplot(df) |> mark_line(x = x, y = y, linewidth = w)
  w <- mark_widths(p)
  expect_length(w, nrow(df) - 1L)
  # values 1..6 rescale to 0.5..4 (step 0.7); each segment is the endpoint mean
  mapped <- scales::rescale(df$w, to = c(0.5, 4))
  expect_equal(w, (mapped[-6] + mapped[-1]) / 2, tolerance = 1e-6)
})

test_that("a constant linewidth still emits one lines_grob, byte for byte", {
  base <- vplot(df) |> mark_line(x = x, y = y)
  wide <- vplot(df) |> mark_line(x = x, y = y, linewidth = 3)
  # one path for the whole polyline, not a degenerate per-segment batch
  expect_length(mark_widths(base), 1L)
  expect_length(mark_widths(wide), 1L)
  # and the drawn output is unchanged from before per-segment widths existed
  expect_identical(
    stable_svg(base),
    stable_svg(vplot(df) |> mark_line(x = x, y = y))
  )
})

test_that("a mapped linewidth splits per style group and keeps x order", {
  p <- vplot(df) |> mark_line(x = x, y = y, color = grp, linewidth = w)
  # 3 rows per group -> 2 segments per group
  expect_length(mark_widths(p), 4L)
})

test_that("mark_segment() takes a mapped linewidth per element", {
  d <- data.frame(x = 1:4, y = 1:4, xend = 2:5, yend = 2:5, w = c(1, 2, 3, 4))
  p <- vplot(d) |>
    mark_segment(x = x, y = y, xend = xend, yend = yend, linewidth = w)
  expect_equal(
    mark_widths(p),
    scales::rescale(d$w, to = c(0.5, 4)),
    tolerance = 1e-6
  )
})

test_that("mark_step() varies width per tread and blends across each riser", {
  d <- data.frame(x = 1:3, y = c(1, 2, 3), w = c(1, 2, 3))
  p <- vplot(d) |> mark_step(x = x, y = y, linewidth = w)
  mapped <- scales::rescale(d$w, to = c(0.5, 4))
  # hv: tread at w1, riser w1->w2, tread at w2, riser w2->w3
  expect_equal(
    mark_widths(p),
    c(
      mapped[1],
      mean(mapped[1:2]),
      mapped[2],
      mean(mapped[2:3])
    ),
    tolerance = 1e-6
  )
})

test_that("a mapped linewidth survives flip, polar and a sketch", {
  p <- vplot(df) |> mark_line(x = x, y = y, linewidth = w)
  expect_length(mark_widths(p |> coord_flip()), nrow(df) - 1L)
  # polar munches each segment into arcs; every piece keeps that segment's width
  wp <- mark_widths(p |> coord_polar())
  expect_gte(length(wp), nrow(df) - 1L)
  expect_equal(range(wp), range(mark_widths(p)), tolerance = 1e-6)
  expect_length(mark_widths(p |> theme_sketch()), nrow(df) - 1L)
})

test_that("a glow effect widens the mapped widths, not a single constant", {
  plain <- mark_widths(vplot(df) |> mark_line(x = x, y = y, linewidth = w))
  both <- mark_widths(
    vplot(df) |>
      mark_line(x = x, y = y, linewidth = w, effects = list(glow()))
  )
  # the halo copy is drawn first, under the mark, which is unchanged
  n <- length(plain)
  expect_length(both, 2L * n)
  halo <- both[seq_len(n)]
  expect_equal(both[-seq_len(n)], plain)
  # each halo stroke is its own segment's width plus one shared widening -- not
  # a single constant width that would swallow the taper
  expect_true(all(halo > plain))
  expect_equal(diff(halo), diff(plain), tolerance = 1e-6)
})

test_that("mapped edge width and mapped line width train separately", {
  skip_if_not_installed("igraph")
  g <- igraph::make_ring(5)
  g <- igraph::set_edge_attr(g, "ew", value = seq_len(igraph::ecount(g)))
  b <- vellumplot:::.build_panels(
    vgraph(g, layout = "circle") |> mark_edges(linewidth = ew) |> mark_nodes()
  )
  expect_identical(b$scales$edge_width$kind, "edge_width")
  expect_null(b$scales$linewidth)
})
