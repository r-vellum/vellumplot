# VP-71: an interactive mark_flow_map() keys its branches, so flows get hover,
# tooltips and selection. A branch reaching a vertex the root has an edge to
# keys by that edge ("<edge key>\001flow", apart from the edge's own
# mark_edges() key), carries the edge's endpoints (node keys) and the edge key
# as `join`. A branch merging several flows keys as "flow\001<root>\001b<k>"
# with no endpoints or join.

vp71_graph <- function() {
  g <- igraph::make_star(5, mode = "undirected")
  igraph::V(g)$name <- paste0("v", 1:5)
  igraph::V(g)$grp <- c("a", "a", "a", "b", "b")
  igraph::E(g)$w <- c(10, 20, 30, 40)
  g
}

vp71_meta <- function(el, field) {
  vapply(el$meta, function(m) m[[field]] %||% NA_character_, character(1))
}

vp71_elements <- function(p) {
  el <- vellum::scene_model(as_vellum_scene(p))$elements
  el[!is.na(el$key), , drop = FALSE]
}

vp71_plot <- function(...) {
  vgraph(vp71_graph(), layout = "stress") |>
    mark_edges() |>
    mark_flow_map(root = "v1", weight = w, ...) |>
    mark_nodes(size = 3) |>
    select_neighbours()
}

test_that("flow branches key by the edge they carry", {
  skip_if_not_installed("igraph")
  skip_if_not_installed("edgebundle")
  el <- vp71_elements(vp71_plot())
  expect_false(anyDuplicated(el$key) > 0)
  seg <- el[el$mark == "segment", ]
  pts <- el[el$mark == "point", ]
  fl <- el[el$mark == "line", ]
  by_edge <- fl[endsWith(fl$key, "\001flow"), ]
  # one branch per destination, keyed apart from its edge's mark_edges() key
  expect_identical(nrow(by_edge), 4L)
  expect_setequal(by_edge$key, paste0(seg$key, "\001flow"))
  expect_setequal(vp71_meta(by_edge, "join"), seg$key)
  src <- vp71_meta(by_edge, "source")
  tgt <- vp71_meta(by_edge, "target")
  expect_true(all(src == "v1"))
  expect_setequal(tgt, paste0("v", 2:5))
  expect_true(all(c(src, tgt) %in% pts$key))
  # the default tooltip names the flow
  tt <- vp71_meta(by_edge, "tooltip")
  expect_identical(
    sort(tt),
    paste0("v1 \u2192 v", 2:5, ": ", c(10, 20, 30, 40))
  )
})

test_that("a branch merging flows keys apart, with no endpoints or join", {
  skip_if_not_installed("igraph")
  skip_if_not_installed("edgebundle")
  el <- vp71_elements(vp71_plot())
  fl <- el[el$mark == "line", ]
  merged <- fl[!endsWith(fl$key, "\001flow"), ]
  expect_gt(nrow(merged), 0L)
  expect_true(all(grepl("^flow\001v1\001b[0-9]+$", merged$key)))
  expect_true(all(is.na(vp71_meta(merged, "source"))))
  expect_true(all(is.na(vp71_meta(merged, "target"))))
  expect_true(all(is.na(vp71_meta(merged, "join"))))
  expect_true(all(startsWith(vp71_meta(merged, "tooltip"), "v1 \u2192 ")))
})

test_that("a steiner flow map keys each destination path by its edge", {
  skip_if_not_installed("igraph")
  skip_if_not_installed("edgebundle")
  skip_if_not_installed("interp")
  el <- suppressWarnings(vp71_elements(vp71_plot(type = "steiner")))
  expect_false(anyDuplicated(el$key) > 0)
  fl <- el[el$mark == "line", ]
  seg <- el[el$mark == "segment", ]
  expect_setequal(fl$key, paste0(seg$key, "\001flow"))
})

test_that("a declared tooltip reaches the branches that carry an edge", {
  skip_if_not_installed("igraph")
  skip_if_not_installed("edgebundle")
  el <- vp71_elements(vp71_plot(tooltip = paste("w =", w)))
  fl <- el[el$mark == "line" & endsWith(el$key, "\001flow"), ]
  expect_setequal(vp71_meta(fl, "tooltip"), paste("w =", c(10, 20, 30, 40)))
})

test_that("faceted flow branches key per panel and link to their edge", {
  skip_if_not_installed("igraph")
  skip_if_not_installed("edgebundle")
  el <- vp71_elements(vp71_plot() |> facet_wrap(~grp))
  expect_false(anyDuplicated(el$key) > 0)
  fl <- el[el$mark == "line", ]
  pts <- el[el$mark == "point", ]
  # only the root's panel draws a flow: to v2 and v3
  expect_true(all(startsWith(fl$key, "p1-1:")))
  by_edge <- fl[endsWith(fl$key, "\001flow"), ]
  expect_identical(nrow(by_edge), 2L)
  src <- vp71_meta(by_edge, "source")
  tgt <- vp71_meta(by_edge, "target")
  expect_true(all(c(src, tgt) %in% pts$key))
  expect_true(all(startsWith(c(src, tgt), "p1-1:")))
  # the join is the bare edge key, as the faceted edge's own join
  seg <- el[el$mark == "segment", ]
  expect_setequal(vp71_meta(by_edge, "join"), vp71_meta(seg, "join"))
})

test_that("a composition prefixes every flow key", {
  skip_if_not_installed("igraph")
  skip_if_not_installed("edgebundle")
  p <- vp71_plot()
  el <- vp71_elements(hconcat(p, p))
  expect_false(anyDuplicated(el$key) > 0)
  fl <- el[el$mark == "line", ]
  pts <- el[el$mark == "point", ]
  expect_true(all(startsWith(fl$key, "subplot-")))
  ends <- c(vp71_meta(fl, "source"), vp71_meta(fl, "target"))
  ends <- ends[!is.na(ends)]
  expect_gt(length(ends), 0L)
  expect_true(all(ends %in% pts$key))
})

test_that("a static flow map stays unkeyed", {
  skip_if_not_installed("igraph")
  skip_if_not_installed("edgebundle")
  p <- vgraph(vp71_graph(), layout = "stress") |>
    mark_flow_map(root = "v1", weight = w)
  el <- vellum::scene_model(as_vellum_scene(p))$elements
  expect_true(all(is.na(el$key)))
})
