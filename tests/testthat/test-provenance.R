# Emitted-scene provenance: the row-key / scale-ref metadata schema (DESIGN §4).
# `plot_provenance()` is the public accessor; these tests keep the schema
# populated and correct, and pin the row-key refinement of the emitters.

test_that("plot_provenance() is exported and returns the record schema", {
  expect_true("plot_provenance" %in% getNamespaceExports("vellumplot"))
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
  prov <- plot_provenance(p)
  expect_type(prov, "list")
  expect_setequal(
    names(prov[[1]]),
    c("id", "layer", "mark", "kind", "panel", "channels", "rows")
  )
})

test_that("refined emitters carry per-row keys, not the whole layer", {
  df <- data.frame(
    x = c(1, 2, 3, 1, 2, 3),
    y = c(1, 2, 3, 2, 3, 4),
    g = rep(c("a", "b"), each = 3)
  )
  # two lines (one per colour group): each record's rows are its group's rows,
  # covering the layer exactly once between them (not the whole layer each).
  prov <- plot_provenance(vplot(df) |> mark_line(x = x, y = y, color = g))
  lines <- Filter(function(e) e$mark == "line" && e$kind == "mark", prov)
  expect_length(lines, 2L)
  expect_false(any(vapply(lines, function(e) length(e$rows) == nrow(df), logical(1))))
  expect_setequal(sort(unlist(lapply(lines, `[[`, "rows"))), seq_len(nrow(df)))

  # an area mark refines the same way
  aprov <- plot_provenance(vplot(df) |> mark_area(x = x, y = y, fill = g))
  areas <- Filter(function(e) e$mark == "area" && e$kind == "mark", aprov)
  expect_setequal(sort(unlist(lapply(areas, `[[`, "rows"))), seq_len(nrow(df)))
})

test_that("every emitted mark grob gets a provenance record with a stable id", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, color = factor(cyl)) |>
    mark_smooth(x = wt, y = mpg)
  prov <- plot_provenance(p)
  expect_gt(length(prov), 0)
  ids <- vapply(prov, `[[`, character(1), "id")
  # ids are unique (the join key) and stable (prefixed by layer-<i>-<mark>).
  expect_equal(length(unique(ids)), length(ids))
  expect_true(all(grepl("^layer-\\d+-\\w+-g\\d+$", ids)))
  expect_true(any(grepl("^layer-1-point", ids)))
  expect_true(any(grepl("^layer-2-smooth", ids)))
})

test_that("the grob id matches the SVG data-vellum-id (the join key holds)", {
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
  prov <- plot_provenance(p)
  id <- prov[[1]]$id
  f <- withr::local_tempfile(fileext = ".svg")
  render_plot(p, f)
  svg <- paste(readLines(f), collapse = "")
  expect_match(svg, id, fixed = TRUE)
})

test_that("channels carry the scale-ref for each mapped aesthetic", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, color = factor(cyl), size = hp)
  e <- plot_provenance(p)[[1]]
  expect_setequal(names(e$channels), c("x", "y", "color", "size"))
  expect_equal(e$channels$x$scale, "x")
  expect_equal(e$channels$x$type, "continuous")
  expect_equal(e$channels$color$scale, "color")
  expect_equal(e$channels$color$type, "discrete")
})

test_that("row-keys partition the layer across style groups", {
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg, color = factor(cyl))
  prov <- plot_provenance(p)
  pts <- Filter(function(e) e$mark == "point" && e$kind == "mark", prov)
  rows <- sort(unlist(lapply(pts, `[[`, "rows")))
  # each input row appears in exactly one style group
  expect_equal(rows, seq_len(nrow(mtcars)))
})

test_that("faceting yields unique ids and per-panel keys", {
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg) |> facet_wrap(~cyl)
  prov <- plot_provenance(p)
  ids <- vapply(prov, `[[`, character(1), "id")
  expect_equal(length(unique(ids)), length(ids))
  panels <- unique(vapply(prov, `[[`, character(1), "panel"))
  expect_true(all(grepl("^panel-\\d+-\\d+$", panels)))
  expect_gt(length(panels), 1)
})

test_that("composition accumulates provenance across sub-plots", {
  p <- (vplot(mtcars) |> mark_point(x = wt, y = mpg)) |>
    concat(vplot(mtcars) |> mark_point(x = hp, y = mpg))
  prov <- plot_provenance(p)
  ids <- vapply(prov, `[[`, character(1), "id")
  expect_equal(length(unique(ids)), length(ids))
  expect_setequal(
    unique(vapply(prov, `[[`, character(1), "panel")),
    c("subplot-1", "subplot-2")
  )
})

test_that("effect underlay copies are tagged kind = 'effect'", {
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg, effects = list(glow()))
  prov <- plot_provenance(p)
  kinds <- vapply(prov, `[[`, character(1), "kind")
  expect_true(any(kinds == "effect")) # the glow copies
  expect_equal(sum(kinds == "mark"), 1L) # the single crisp core
})

test_that("provenance records are serializable (plain data, no closures)", {
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg, color = factor(cyl))
  prov <- plot_provenance(p)
  # round-trips through serialize(): fails if any leaf is an environment/closure
  expect_identical(unserialize(serialize(prov, NULL)), prov)
})
