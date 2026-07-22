# N10: dendrogram / unrooted layouts, hclust coercion, bracket routing,
# vdendrogram() preset, and the mark_node_text() label-override fix.

skip_if_not_installed("igraph")

hc <- hclust(dist(USArrests[1:8, ]))

# ---- hclust -> igraph coercion ---------------------------------------------

test_that(".hclust_to_igraph builds a directed tree with heights + labels", {
  g <- vellumplot:::.hclust_to_igraph(hc)
  expect_true(igraph::is_directed(g))
  L <- nrow(hc$merge) + 1L
  expect_equal(igraph::vcount(g), L + nrow(hc$merge)) # leaves + merges
  expect_equal(igraph::ecount(g), 2L * nrow(hc$merge)) # 2 children per merge
  # leaves carry their labels + height 0; merges are blank-labelled at height > 0
  expect_identical(igraph::V(g)$label[seq_len(L)], rownames(USArrests)[1:8])
  expect_true(all(igraph::V(g)$height[seq_len(L)] == 0))
  expect_true(all(igraph::V(g)$leaf[seq_len(L)]))
  expect_true(all(!igraph::V(g)$leaf[(L + 1L):igraph::vcount(g)]))
  expect_true(all(nzchar(igraph::V(g)$name))) # names unique + non-empty
  expect_false(anyDuplicated(igraph::V(g)$name) > 0)
})

test_that("vgraph() accepts hclust / dendrogram directly", {
  expect_s3_class(vgraph(hc, layout = "dendrogram"), "vellumplot::PlotSpec")
  expect_s3_class(
    vgraph(as.dendrogram(hc), layout = "dendrogram"),
    "vellumplot::PlotSpec"
  )
})

# ---- dendrogram layout ------------------------------------------------------

test_that("dendrogram layout puts leaves at height 0 and the root highest", {
  g <- vellumplot:::.hclust_to_igraph(hc)
  xy <- vellumplot:::.layout_dendrogram(g, direction = "down")
  leaf <- igraph::V(g)$leaf
  # down: y is height. leaves at 0, root at max height.
  expect_true(all(abs(xy[leaf, 2]) < 1e-9))
  root <- which.max(igraph::V(g)$height)
  expect_equal(xy[root, 2], max(igraph::V(g)$height))
  # "right" swaps the axes: height on x
  xr <- vellumplot:::.layout_dendrogram(g, direction = "right")
  expect_true(all(abs(xr[leaf, 1]) < 1e-9))
})

test_that("a dendrogram plot is not aspect-locked (unlike a network)", {
  expect_identical(vgraph(hc, layout = "dendrogram")@coord@kind, "cartesian")
  expect_identical(
    vgraph(igraph::make_ring(4), layout = "stress")@coord@kind,
    "fixed"
  )
})

# ---- bracket elbow ----------------------------------------------------------

test_that(".elbow_points: `at`/`axis` control the corner; defaults unchanged", {
  # default = midpoint S-bend (historical behaviour)
  d <- vellumplot:::.elbow_points(0, 0, 2, 10)
  expect_equal(d$y, c(0, 5, 5, 10)) # corner at mid-height
  # bracket: forced vertical axis, corner at the source level
  b <- vellumplot:::.elbow_points(0, 10, 2, 0, at = "start", axis = "v")
  expect_equal(b$x, c(0, 0, 2, 2))
  expect_equal(b$y, c(10, 10, 10, 0)) # horizontal bar at the parent's height
  # forced horizontal axis: vertical bar at the source level, then across
  h <- vellumplot:::.elbow_points(10, 0, 0, 2, at = "start", axis = "h")
  expect_equal(h$x, c(10, 10, 10, 0))
  expect_equal(h$y, c(0, 0, 2, 2))
})

# ---- unrooted layout --------------------------------------------------------

test_that("unrooted layout is available via graphlayouts", {
  skip_if_not_installed("graphlayouts", minimum_version = "1.2.5")
  g <- igraph::make_tree(15, 3, "undirected")
  p <- vgraph(g, layout = "unrooted", mode = "equalangle")
  expect_s3_class(p, "vellumplot::PlotSpec")
  expect_equal(nrow(p@data), 15L)
})

# ---- mark_node_text label override (regression) ----------------------------

test_that("mark_node_text(label=) overrides the vertex-name default", {
  node_labels <- function(p) {
    root <- vellum:::.materialize(vellum::as_vellum_scene(p))
    acc <- list()
    walk <- function(n) {
      if (S7::S7_inherits(n, vellum:::gtree)) {
        for (c in n@children) {
          walk(c)
        }
      } else if (S7::S7_inherits(n, vellum:::grob_text)) {
        acc[[length(acc) + 1L]] <<- n@label
      }
    }
    walk(root)
    unlist(acc)
  }
  g <- igraph::make_ring(3)
  igraph::V(g)$name <- c("a", "b", "c")
  igraph::V(g)$lab2 <- c("X", "Y", "Z")
  # explicit mapping wins
  expect_setequal(
    node_labels(vgraph(g, layout = "circle") |> mark_node_text(label = lab2)),
    c("X", "Y", "Z")
  )
  # default is still the vertex name
  expect_setequal(
    node_labels(vgraph(g, layout = "circle") |> mark_node_text()),
    c("a", "b", "c")
  )
})

# ---- vdendrogram() preset ---------------------------------------------------

test_that("vdendrogram() builds a ready dendrogram spec", {
  p <- vdendrogram(hc)
  expect_s3_class(p, "vellumplot::PlotSpec")
  marks <- vapply(p@layers, function(l) l@mark, character(1))
  expect_true("edges" %in% marks && "text" %in% marks) # bracket edges + labels
  expect_false("nodes" %in% marks) # no node markers by default
  # the edge layer routes as a bracket
  edge_L <- Find(function(l) l@mark == "edges", p@layers)
  expect_identical(edge_L@stat_params$routing, "elbow")
  expect_identical(edge_L@stat_params$elbow_at, "start")
  expect_identical(edge_L@stat_params$elbow_axis, "v") # down -> vertical
})

test_that("vdendrogram() labels only leaves, vertical for a top/bottom tree", {
  text_L <- function(p) Find(function(l) l@mark == "text", p@layers)
  # merge nodes are dropped from the text layer -- only the leaves are labelled
  expect_equal(nrow(text_L(vdendrogram(hc))@data), nrow(hc$merge) + 1L)
  # down/up rotate the labels vertical; left/right leave them horizontal
  rotated <- function(p) {
    grepl(
      "rotate(-90",
      vellum::scene_svg(vellum::as_vellum_scene(p)),
      fixed = TRUE
    )
  }
  expect_true(rotated(vdendrogram(hc, direction = "down")))
  expect_true(rotated(vdendrogram(hc, direction = "up")))
  expect_false(rotated(vdendrogram(hc, direction = "right")))
  expect_false(rotated(vdendrogram(hc, direction = "left")))
})

test_that("leaf labels justify away from the tree (not centred on the leaf)", {
  # regression: labels must clear the edges. A centred anchor would run the text
  # back over the edge; they should be end-/start-anchored so they sit outside.
  anchor <- function(p) {
    svg <- vellum::scene_svg(vellum::as_vellum_scene(p))
    t <- regmatches(svg, gregexpr("<text[^>]*>[^<]+</text>", svg))[[1]][1]
    sub('.*text-anchor="([a-z]+)".*', "\\1", t)
  }
  expect_identical(anchor(vdendrogram(hc, direction = "right")), "end")
  expect_identical(anchor(vdendrogram(hc, direction = "left")), "start")
  expect_identical(anchor(vdendrogram(hc, direction = "down")), "end")
})

test_that("vdendrogram(k=) cuts the tree and carries clusters", {
  g <- vellumplot:::.hclust_to_igraph(hc, k = 3)
  expect_setequal(
    stats::na.omit(unique(igraph::V(g)$cluster)),
    c("1", "2", "3")
  )
  # every edge's cluster is its child's (NA above the cut)
  ends <- igraph::ends(g, igraph::E(g), names = FALSE)
  expect_identical(igraph::E(g)$cluster, igraph::V(g)$cluster[ends[, 2]])
  # rendering with a cut is a drawable
  f <- local_tempfile(fileext = ".png")
  render_plot(vdendrogram(hc, k = 3), f)
  expect_gt(file.info(f)$size, 0)
})

test_that("vdendrogram() rejects non-clustering input", {
  expect_error(vdendrogram(mtcars), "hclust")
})
