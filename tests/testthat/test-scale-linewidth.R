# A mapped `linewidth` on a line / step: one tapered stroke per series, via
# `lines_grob(lwd_profile = <one width per vertex>)` (#165), falling back to one
# constant width per segment (`segments_grob(lwd = <vector>)`) where a profile
# can't express the series. On a segment: one width per element.

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

# The grobs a mark layer hands to `.draw()`, with the layer rows each records,
# captured while the plot compiles. Lets a test read a ribbon's `lwd_profile`
# (a filled outline in SVG, so it has no `stroke-width` to parse).
drawn_grobs <- function(plot) {
  got <- list()
  draw <- .draw
  testthat::local_mocked_bindings(
    .draw = function(scene, grob, rows = NULL) {
      got[[length(got) + 1L]] <<- list(grob = grob, rows = rows)
      draw(scene, grob, rows)
    },
    .package = "vellumplot"
  )
  vellum::as_vellum_scene(plot)
  got
}

# The `lwd_profile` ribbons among them.
profiles <- function(plot) {
  g <- drawn_grobs(plot)
  Filter(
    function(e) {
      S7::prop_exists(e$grob, "lwd_profile") && !is.null(e$grob@lwd_profile)
    },
    g
  )
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

test_that("a mapped linewidth draws one tapered stroke, one width per vertex", {
  p <- vplot(df) |> mark_line(x = x, y = y, linewidth = w)
  pr <- profiles(p)
  expect_length(pr, 1L)
  g <- pr[[1]]$grob
  # the profile IS the width: lwd 1, one multiplier per vertex (values 1..6
  # rescale to 0.5..4), and no per-segment batch alongside it
  expect_identical(g@gp@lwd, 1)
  expect_equal(
    g@lwd_profile,
    scales::rescale(df$w, to = c(0.5, 4)),
    tolerance = 1e-6
  )
  expect_identical(pr[[1]]$rows, seq_len(nrow(df)))
  # in SVG it is one filled outline, not a per-segment batch of strokes
  parts <- strsplit(svg_of(p), "<g data-vellum-", fixed = TRUE)[[1]]
  mine <- parts[startsWith(parts, 'id="layer-1-')]
  expect_identical(sum(lengths(regmatches(mine, gregexpr("<path", mine)))), 1L)
  expect_length(mark_widths(p), 0L)
})

test_that("a tapered stroke is one element per series in scene_model()", {
  d <- data.frame(
    x = rep(1:5, 2),
    y = c(1:5, 5:1),
    w = c(1:5, 5:1),
    g = rep(c("a", "b"), each = 5)
  )
  p <- vplot(d) |>
    mark_line(x = x, y = y, linewidth = w, color = g, data_id = g)
  sc <- vellum::as_vellum_scene(p)
  el <- vellum::scene_model(sc)$elements
  mine <- el[!is.na(el$key) & el$key %in% c("a", "b"), ]
  expect_identical(sort(mine$key), c("a", "b"))
  expect_true(all(mine$mark == "line"))
  geo <- vellum::element_geometry(sc)
  geo <- geo[!is.na(geo$key) & geo$key %in% c("a", "b"), ]
  # each series is one polyline through its own five vertices
  expect_identical(as.vector(table(geo$key)), c(5L, 5L))
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
  d <- df[c(3, 1, 2, 6, 4, 5), ]
  p <- vplot(d) |> mark_line(x = x, y = y, color = grp, linewidth = w)
  pr <- profiles(p)
  # one ribbon per group, its vertices (and their widths) in x order
  expect_length(pr, 2L)
  mapped <- scales::rescale(df$w, to = c(0.5, 4))
  expect_equal(pr[[1]]$grob@lwd_profile, mapped[1:3], tolerance = 1e-6)
  expect_equal(pr[[2]]$grob@lwd_profile, mapped[4:6], tolerance = 1e-6)
  expect_identical(d$x[pr[[1]]$rows], 1:3)
})

test_that("the segment fallback keeps the endpoint-mean width per span", {
  # A dashed line can't be a filled ribbon, so it keeps one segment per span,
  # each at the mean of its two endpoint widths.
  p <- vplot(df) |>
    mark_line(x = x, y = y, linewidth = w, linetype = "dashed")
  expect_length(profiles(p), 0L)
  w <- mark_widths(p)
  expect_length(w, nrow(df) - 1L)
  mapped <- scales::rescale(df$w, to = c(0.5, 4))
  expect_equal(w, (mapped[-6] + mapped[-1]) / 2, tolerance = 1e-6)
})

test_that("a continuous colour keeps per-segment colour and width", {
  p <- vplot(df) |> mark_line(x = x, y = y, linewidth = w, color = y)
  expect_length(profiles(p), 0L)
  expect_length(mark_widths(p), nrow(df) - 1L)
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

test_that("mark_step() holds width along each tread and blends across each riser", {
  d <- data.frame(x = 1:3, y = c(1, 2, 3), w = c(1, 2, 3))
  p <- vplot(d) |> mark_step(x = x, y = y, linewidth = w)
  mapped <- scales::rescale(d$w, to = c(0.5, 4))
  pr <- profiles(p)
  expect_length(pr, 1L)
  # hv vertices: (1,1) (2,1) (2,2) (3,2) (3,3) -- each tread runs at its own
  # datum's width, each riser tapers from one to the next
  expect_equal(
    pr[[1]]$grob@lwd_profile,
    mapped[c(1, 1, 2, 2, 3)],
    tolerance = 1e-6
  )
})

test_that("a mapped linewidth survives flip, polar and a sketch", {
  p <- vplot(df) |> mark_line(x = x, y = y, linewidth = w)
  mapped <- scales::rescale(df$w, to = c(0.5, 4))
  pf <- profiles(p |> coord_flip())
  expect_length(pf, 1L)
  expect_equal(pf[[1]]$grob@lwd_profile, mapped, tolerance = 1e-6)
  # polar munches the series into arcs; the profile is resampled onto the extra
  # vertices, so it stays one ribbon spanning the same width range
  pp <- profiles(p |> coord_polar())
  expect_length(pp, 1L)
  wp <- pp[[1]]$grob@lwd_profile
  expect_gt(length(wp), nrow(df))
  expect_equal(range(wp), range(mapped), tolerance = 1e-6)
  # vellum refuses sketch + profile, so a sketched line keeps the segments
  ps <- p |> theme_sketch()
  expect_length(profiles(ps), 0L)
  expect_length(mark_widths(ps), nrow(df) - 1L)
})

test_that("a glow effect widens the mapped widths, not a single constant", {
  plain <- profiles(vplot(df) |> mark_line(x = x, y = y, linewidth = w))
  both <- profiles(
    vplot(df) |>
      mark_line(x = x, y = y, linewidth = w, effects = list(glow()))
  )
  # the halo copy is drawn first, under the mark, which is unchanged
  expect_length(plain, 1L)
  expect_length(both, 2L)
  pw <- plain[[1]]$grob@lwd_profile
  halo <- both[[1]]$grob@lwd_profile
  expect_equal(both[[2]]$grob@lwd_profile, pw)
  # each halo vertex is its own width plus one shared widening -- not a single
  # constant width that would swallow the taper
  expect_true(all(halo > pw))
  expect_equal(diff(halo), diff(pw), tolerance = 1e-6)
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

test_that("a missing width thins its segments instead of erasing them", {
  # `w[2]` is missing: the two segments meeting at vertex 2 must still draw,
  # each taking the width of the endpoint that IS known -- a non-finite lwd
  # would otherwise reach the backend as width 0 and vanish silently.
  d <- data.frame(x = 1:4, y = c(1, 2, 1, 2), w = c(0, NA, 10, 0))
  p <- vplot(d) |>
    mark_line(x = x, y = y, linewidth = w) |>
    scale_linewidth(range = c(1, 11))
  # a missing width can't ride a profile: the series keeps the segments
  expect_length(profiles(p), 0L)
  w <- mark_widths(p)
  expect_length(w, 3L) # no segment dropped
  # vertices map to 1, NA, 11, 1 -> seg1 from v1 alone, seg2 from v3 alone,
  # seg3 the ordinary mean of 11 and 1
  expect_equal(w, c(1, 11, 6), tolerance = 1e-6)
})

test_that("a missing width costs no ink", {
  d <- data.frame(
    x = 1:5,
    y = rep(c(1, 2), length.out = 5),
    w = c(5, NA, 5, 5, 5)
  )
  # every span still draws: the missing width costs only its own contribution
  gap <- vplot(d) |> mark_line(x = x, y = y, linewidth = w)
  expect_length(mark_widths(gap), nrow(d) - 1L)
})

test_that("a segment missing a width at both ends is dropped, keeping provenance", {
  d <- data.frame(
    x = 1:5,
    y = rep(c(1, 2), length.out = 5),
    w = c(5, NA, NA, 5, 5)
  )
  p <- vplot(d) |> mark_line(x = x, y = y, linewidth = w)
  # 4 spans, but the middle one has no known width at either end
  expect_length(mark_widths(p), 3L)
  b <- vellumplot:::.build_panels(p)
  sc <- b$scales
  r <- vellumplot:::.resolve_layers(p)
  wv <- vellumplot:::.mapped_linewidth(r[[1]], sc, 5)
  s <- vellumplot:::.seg_widths_path(sc, d$x, d$y, wv)
  expect_identical(s$row, c(1L, 3L, 4L)) # the dropped span leaves no stale row
})

test_that("an all-missing width draws nothing rather than an invisible batch", {
  d <- data.frame(x = 1:4, y = c(1, 2, 1, 2), w = rep(NA_real_, 4))
  p <- vplot(d) |> mark_line(x = x, y = y, linewidth = w)
  expect_length(mark_widths(p), 0L)
})

test_that("guides()/lims() on `linewidth` address the line-width scale", {
  skip_if_not_installed("igraph")
  # The split means `linewidth` no longer canonicalises to `edge_width`: a
  # graph's edges are addressed as `edge_width`. Locked so the redirection (and
  # the documented migration) cannot regress unnoticed.
  expect_identical(vellumplot:::.canonical_lim_aes("linewidth"), "linewidth")
  g <- igraph::make_ring(5)
  g <- igraph::set_edge_attr(g, "ew", value = seq_len(igraph::ecount(g)))
  base <- function() {
    vgraph(g, layout = "circle") |> mark_edges(linewidth = ew) |> mark_nodes()
  }
  # `edge_width` reaches the edge scale's domain; `linewidth` does not touch it
  b_edge <- vellumplot:::.build_panels(base() |> lims(edge_width = c(0, 40)))
  expect_equal(b_edge$scales$edge_width$range, c(0, 40))
  b_line <- vellumplot:::.build_panels(base() |> lims(linewidth = c(0, 40)))
  expect_false(isTRUE(all.equal(b_line$scales$edge_width$range, c(0, 40))))
})
