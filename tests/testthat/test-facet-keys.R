# VW-11: default element keys (an interactive plot with no data_id) must be
# unique across facet panels. They used to be row positions within each panel's
# layer, so they repeated: hovering or brushing in one panel lit up the marks
# sharing those positions in every other panel.
#
# VP-55: every default key names its panel, layer and row --
# "p<r>-<c>:l<layer>:<row>" -- in single-panel plots too, so default keys never
# collide across panels or layers; composition cells prefix it with their id.

.fk_keyed <- function(p) {
  el <- vellum::scene_model(vellum::as_vellum_scene(p))$elements
  el[!is.na(el$key), , drop = FALSE]
}

.fk_join <- function(el) {
  vapply(el$meta, function(m) m$join %||% NA_character_, character(1))
}

test_that("default keys are unique across facet_wrap panels", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    facet_wrap(~cyl) |>
    select_point("s")
  el <- .fk_keyed(p)
  expect_equal(nrow(el), nrow(mtcars))
  expect_false(anyDuplicated(el$key) > 0L)
  # "p<r>-<c>:l<layer>:<source row>", and the source row rides along as `join`
  expect_true(all(grepl("^p\\d+-\\d+:l1:\\d+$", el$key)))
  expect_setequal(.fk_join(el), as.character(seq_len(nrow(mtcars))))
  expect_identical(sub("^.*:", "", el$key), .fk_join(el))
})

test_that("default keys are unique across facet_grid panels", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, color = condition("s", "red", "grey")) |>
    facet_grid(am ~ gear) |>
    select_point("s")
  el <- .fk_keyed(p)
  expect_gte(nrow(el), nrow(mtcars))
  expect_false(anyDuplicated(el$key) > 0L)
})

test_that("a layer repeated in every panel still gets unique keys", {
  # the second layer's data has no facet column, so every panel draws all of it
  ref <- data.frame(wt = c(2, 4), mpg = c(15, 30))
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    mark_point(data = ref, x = wt, y = mpg, color = "red") |>
    facet_wrap(~cyl) |>
    select_point("s")
  el <- .fk_keyed(p)
  ref_el <- el[el$id != el$id[[1L]] & grepl("layer-2", el$id), , drop = FALSE]
  expect_equal(nrow(ref_el), 2L * 3L)
  expect_false(anyDuplicated(ref_el$key) > 0L)
  # ... while the same reference row keeps one join id in every panel
  expect_setequal(.fk_join(ref_el), c("1", "2"))
})

test_that("a single-panel plot keys by panel, layer and row, with no join", {
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg) |> select_point("s")
  el <- .fk_keyed(p)
  expect_identical(el$key, paste0("p1-1:l1:", seq_len(nrow(mtcars))))
  expect_true(all(is.na(.fk_join(el))))
})

test_that("a declared data_id is used as given in a faceted plot", {
  df <- data.frame(wt = mtcars$wt, mpg = mtcars$mpg, cyl = mtcars$cyl)
  df$model <- rownames(mtcars)
  p <- vplot(df) |>
    mark_point(x = wt, y = mpg, data_id = model) |>
    facet_wrap(~cyl)
  el <- .fk_keyed(p)
  expect_setequal(el$key, df$model)
  expect_true(all(is.na(.fk_join(el))))
})

test_that("an aggregated layer in facets gets unique keys without a join", {
  p <- vplot(mtcars) |>
    mark_bar(x = factor(gear), stat = "count") |>
    facet_wrap(~am) |>
    select_point("s")
  el <- .fk_keyed(p)
  skip_if(nrow(el) == 0L, "bars not keyed")
  expect_false(anyDuplicated(el$key) > 0L)
  expect_true(all(is.na(.fk_join(el))))
})

test_that("faceted composition cells prefix their keys with the cell id", {
  # VP-51: each faceted cell names its panels `panel-r-c`, so without the cell
  # id two cells shared "panel-1-1:3" and a hover in one lit up the other.
  fp <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    facet_wrap(~am) |>
    select_point("s")
  el <- .fk_keyed(hconcat(fp, fp))
  expect_equal(nrow(el), 2L * nrow(mtcars))
  expect_false(anyDuplicated(el$key) > 0L)
  expect_true(all(grepl("^cell-[12]:p1-[12]:l1:\\d+$", el$key)))
  # the source row is still the join, shared by both cells
  expect_identical(sub("^.*:", "", el$key), .fk_join(el))
  expect_setequal(
    .fk_join(el)[startsWith(el$key, "cell-2:")],
    as.character(seq_len(nrow(mtcars)))
  )
})

test_that("an unaligned single-panel cell next to a faceted one is prefixed too", {
  sp <- vplot(mtcars) |> mark_point(x = wt, y = mpg, tooltip = cyl)
  fp <- sp |> facet_wrap(~am)
  el <- .fk_keyed(hconcat(sp, fp))
  el <- el[!grepl("^legend:", el$key), , drop = FALSE]
  one <- el[startsWith(el$key, "cell-1:"), , drop = FALSE]
  expect_equal(nrow(one), nrow(mtcars))
  expect_identical(one$key, paste0("cell-1:p1-1:l1:", .fk_join(one)))
  expect_true(all(startsWith(el$key, "cell-")))
})

test_that("a lone faceted plot's keys are unchanged by the cell prefix", {
  fp <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    facet_wrap(~am) |>
    select_point("s")
  el <- .fk_keyed(fp)
  expect_true(all(grepl("^p1-[12]:l1:\\d+$", el$key)))
})

test_that("default keys never equal another layer's user data_id", {
  # VP-54: the line's default key was its first row, "1" -- the key of the
  # point whose data_id is 1 -- so a host selected both together. A default key
  # now always carries panel and layer, so it cannot look like a plain user id.
  ts <- data.frame(t = 1:5, v = c(2, 4, 3, 5, 1))
  for (p in list(
    vplot(ts) |>
      mark_line(x = t, y = v) |>
      mark_point(x = t, y = v, data_id = t) |>
      select_point("s"),
    vplot(ts) |>
      mark_line(x = t, y = v, tooltip = "line") |>
      mark_point(x = t, y = v, data_id = t)
  )) {
    el <- .fk_keyed(p)
    line <- el$key[el$mark == "line"]
    pts <- el$key[el$mark == "point"]
    expect_identical(pts, as.character(ts$t)) # user ids as given
    expect_identical(line, "p1-1:l1:1")
    expect_length(intersect(line, pts), 0L)
  }
})

test_that("two default-keyed layers never share a key (VP-55)", {
  # a line series' key is its first row, so "1"/"4" used to equal point keys and
  # selecting a point selected a line too
  df <- data.frame(
    t = rep(1:3, 2),
    v = c(1, 3, 2, 2, 1, 3),
    g = rep(c("a", "b"), each = 3)
  )
  el <- .fk_keyed(
    vplot(df) |>
      mark_line(x = t, y = v, color = g) |>
      mark_point(x = t, y = v) |>
      select_point("s")
  )
  el <- el[!grepl("^legend:", el$key), , drop = FALSE]
  line <- el$key[el$mark == "line"]
  pts <- el$key[el$mark == "point"]
  expect_identical(line, c("p1-1:l1:1", "p1-1:l1:4"))
  expect_identical(pts, paste0("p1-1:l2:", 1:6))
  expect_length(intersect(line, pts), 0L)
  # a tooltip-only second layer too
  el <- .fk_keyed(
    vplot(df) |>
      mark_line(x = t, y = v) |>
      mark_point(x = t, y = v, tooltip = v)
  )
  expect_identical(el$key[el$mark == "point"], paste0("p1-1:l2:", 1:6))
})

test_that("an explicit data_id shared by two layers still links them", {
  df <- data.frame(
    t = rep(1:3, 2),
    v = c(1, 3, 2, 2, 1, 3),
    g = rep(c("a", "b"), each = 3)
  )
  el <- .fk_keyed(
    vplot(df) |>
      mark_line(x = t, y = v, color = g, data_id = g) |>
      mark_point(x = t, y = v, data_id = g) |>
      select_point("s")
  )
  el <- el[!grepl("^legend:", el$key), , drop = FALSE]
  expect_identical(el$key[el$mark == "line"], c("a", "b"))
  expect_identical(el$key[el$mark == "point"], df$g)
})

test_that("tooltip-only default keys are unique across facet panels (VP-55)", {
  # `meta$default_id` keys used to be row positions per panel, repeating
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, tooltip = cyl) |>
    facet_wrap(~am)
  el <- .fk_keyed(p)
  el <- el[!grepl("^legend:", el$key), , drop = FALSE]
  expect_equal(nrow(el), nrow(mtcars))
  expect_false(anyDuplicated(el$key) > 0L)
  expect_true(all(grepl("^p1-[12]:l1:\\d+$", el$key)))
  expect_setequal(.fk_join(el), as.character(seq_len(nrow(mtcars))))
  # also after a summarising stat
  p <- vplot(mtcars) |>
    mark_bar(x = factor(gear), stat = "count", tooltip = "n") |>
    facet_wrap(~am)
  el <- .fk_keyed(p)
  expect_gt(nrow(el), 0L)
  expect_false(anyDuplicated(el$key) > 0L)
  expect_true(all(grepl("^p1-[12]:l1:\\d+$", el$key)))
})

test_that("aligned composition cells prefix the default key, row as join", {
  sp <- vplot(mtcars) |> mark_point(x = wt, y = mpg) |> select_point("s")
  el <- .fk_keyed(hconcat(sp, sp))
  expect_equal(nrow(el), 2L * nrow(mtcars))
  expect_false(anyDuplicated(el$key) > 0L)
  expect_identical(
    el$key,
    paste0("subplot-", rep(1:2, each = 32), ":p1-1:l1:", 1:32)
  )
  expect_identical(.fk_join(el), as.character(rep(1:32, 2)))
})

test_that("annotate() layers stay unkeyed under a plot-level selection", {
  el <- .fk_keyed(
    vplot(mtcars) |>
      mark_point(x = wt, y = mpg) |>
      annotate("text", x = 3, y = 30, label = "note") |>
      select_point("s")
  )
  expect_identical(unique(el$mark), "point")
  expect_identical(el$key, paste0("p1-1:l1:", seq_len(nrow(mtcars))))
})

# VP-63: a faceted graph keys each panel's copy of a node / edge uniquely, its
# edge endpoints with the same prefix, and keeps the vertex name as `join`.
test_that("faceted graph keys are unique per panel and still link", {
  skip_if_not_installed("igraph")
  g <- igraph::make_ring(6)
  igraph::V(g)$name <- paste0("v", 1:6)
  igraph::V(g)$grp <- rep(c("a", "b"), each = 3)
  p <- vgraph(g) |>
    mark_edges() |>
    mark_nodes(size = 3) |>
    select_neighbours() |>
    facet_wrap(~grp)
  el <- vellum::scene_model(as_vellum_scene(p))$elements
  keyed <- el[!is.na(el$key), ]
  expect_false(anyDuplicated(keyed$key) > 0)
  pts <- keyed[keyed$mark == "point", ]
  expect_true(all(grepl("^p1-[12]:v[1-6]$", pts$key)))
  seg <- keyed[keyed$mark == "segment", ]
  src <- vapply(seg$meta, function(m) m$source %||% NA_character_, "")
  # each edge's endpoints carry its own panel's prefix
  expect_identical(sub(":.*", "", src), sub(":.*", "", seg$key))
  joins <- vapply(pts$meta, function(m) m$join %||% NA_character_, "")
  expect_identical(joins, sub("^p1-[12]:", "", pts$key))
  # an unfaceted graph keeps the bare vertex names
  q <- vgraph(g) |> mark_edges() |> mark_nodes() |> select_neighbours()
  el <- vellum::scene_model(as_vellum_scene(q))$elements
  expect_setequal(el$key[!is.na(el$key) & el$mark == "point"], paste0("v", 1:6))
})

test_that("a composition prefixes graph edge endpoints like its node keys", {
  skip_if_not_installed("igraph")
  g <- igraph::make_ring(4)
  igraph::V(g)$name <- paste0("v", 1:4)
  p <- vgraph(g) |> mark_edges() |> mark_nodes() |> select_neighbours()
  el <- vellum::scene_model(as_vellum_scene(hconcat(p, p)))$elements
  pts <- el[!is.na(el$key) & el$mark == "point", ]
  seg <- el[!is.na(el$key) & el$mark == "segment", ]
  src <- vapply(seg$meta, function(m) m$source %||% NA_character_, "")
  tgt <- vapply(seg$meta, function(m) m$target %||% NA_character_, "")
  expect_true(all(c(src, tgt) %in% pts$key))
  expect_true(all(startsWith(src, "subplot-")))
})

# VP-64: a faceted graph draws, in each panel, only the edges whose two
# endpoints are both among that panel's nodes, as ggraph's facet_nodes() does.
test_that("a vertex facet keeps only the panel's own edges", {
  skip_if_not_installed("igraph")
  g <- igraph::make_ring(6)
  igraph::V(g)$name <- paste0("v", 1:6)
  igraph::V(g)$grp <- rep(c("a", "b"), each = 3)
  p <- vgraph(g) |>
    mark_edges() |>
    mark_nodes(size = 3) |>
    select_neighbours() |>
    facet_wrap(~grp)
  el <- vellum::scene_model(as_vellum_scene(p))$elements
  seg <- el[!is.na(el$key) & el$mark == "segment", ]
  pts <- el[!is.na(el$key) & el$mark == "point", ]
  # v3-v4 and v6-v1 cross the two groups, so neither panel draws them
  expect_setequal(
    seg$key,
    c(
      "p1-1:v1\001v2\0011",
      "p1-1:v2\001v3\0012",
      "p1-2:v4\001v5\0014",
      "p1-2:v5\001v6\0015"
    )
  )
  src <- vapply(seg$meta, function(m) m$source %||% NA_character_, "")
  tgt <- vapply(seg$meta, function(m) m$target %||% NA_character_, "")
  expect_true(all(c(src, tgt) %in% pts$key))
  expect_false(anyDuplicated(c(seg$key, pts$key)) > 0)
  # the static render draws the same edges: 2 per panel
  st <- vellum::scene_model(as_vellum_scene(
    vgraph(g) |> mark_edges() |> mark_nodes() |> facet_wrap(~grp)
  ))$elements
  expect_identical(sum(st$mark == "segment"), 4L)
})

test_that("an unfaceted graph still draws every edge", {
  skip_if_not_installed("igraph")
  g <- igraph::make_ring(6)
  igraph::V(g)$grp <- rep(c("a", "b"), 3)
  el <- vellum::scene_model(as_vellum_scene(
    vgraph(g) |> mark_edges() |> mark_nodes()
  ))$elements
  expect_identical(sum(el$mark == "segment"), 6L)
})

test_that("a faceted graph caps each kept edge at its own endpoints", {
  skip_if_not_installed("igraph")
  caps <- function(p) {
    out <- list()
    walk <- function(x) {
      if (is.list(x)) {
        if (!is.null(x$start_cap)) {
          out[[length(out) + 1L]] <<- rbind(
            x$start_cap$value$v,
            x$end_cap$value$v
          )
        }
        lapply(x, walk)
      }
      invisible()
    }
    walk(vellum::as_scene_spec(as_vellum_scene(p)))
    do.call(cbind, out)
  }
  g <- igraph::make_ring(6)
  igraph::V(g)$name <- paste0("v", 1:6)
  igraph::V(g)$grp <- rep(c("a", "b"), each = 3)
  igraph::V(g)$deg <- c(1, 4, 2, 3, 5, 2)
  p <- vgraph(g) |> mark_edges() |> mark_nodes(size = deg)
  whole <- caps(p)
  # the panels keep ring edges 1, 2 (group a) and 4, 5 (group b); the shared
  # size scale gives each the caps it has in the unfaceted plot
  expect_identical(caps(p |> facet_wrap(~grp)), whole[, c(1, 2, 4, 5)])
})
