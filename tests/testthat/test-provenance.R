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
    c("id", "layer", "mark", "kind", "panel", "channels", "rows", "source")
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
  expect_false(any(vapply(
    lines,
    function(e) length(e$rows) == nrow(df),
    logical(1)
  )))
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
  f <- local_tempfile(fileext = ".svg")
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

test_that("channels carry alpha, linetype, linewidth and pattern scale-refs", {
  df <- data.frame(
    x = rep(1:4, 2),
    y = c(1:4, 4:1),
    g = rep(c("a", "b"), each = 4),
    w = rep(c(1, 3), each = 4)
  )
  e <- plot_provenance(
    vplot(df) |> mark_line(x = x, y = y, linetype = g, linewidth = w)
  )[[1]]
  expect_equal(e$channels$linetype$scale, "linetype")
  expect_equal(e$channels$linetype$type, "linetype")
  # a line reads the line-width scale, not the edge-width one
  expect_equal(e$channels$linewidth$scale, "linewidth")

  e <- plot_provenance(
    vplot(mtcars) |> mark_point(x = wt, y = mpg, alpha = hp)
  )[[1]]
  expect_equal(e$channels$alpha$scale, "alpha")

  e <- plot_provenance(
    vplot(mtcars) |>
      mark_tile(x = factor(cyl), y = factor(gear), pattern = factor(am))
  )[[1]]
  expect_equal(e$channels$pattern$scale, "pattern")
})

test_that("every consumer reads the one aesthetic registry", {
  reg <- vellumplot:::.AES_REGISTRY
  # the trainer trains exactly the registered scales, in registry order
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
  sc <- vellumplot:::.train_scales(p, vellumplot:::.resolve_layers(p))
  expect_identical(names(sc), names(reg))
  # every legend row names a trainer, and every trainer exists
  for (e in reg) {
    if (!is.null(e$guide)) {
      expect_false(is.null(e$trainer))
    }
    if (!is.null(e$trainer)) {
      expect_true(is.function(vellumplot:::.aes_trainer(e)))
    }
  }
  # channel routing: fill shares colour; linewidth splits by mark
  expect_identical(
    vellumplot:::.aes_scale_for_channel("fill", "point"),
    "color"
  )
  expect_identical(
    vellumplot:::.aes_scale_for_channel("linewidth", "edges"),
    "edge_width"
  )
  expect_identical(
    vellumplot:::.aes_scale_for_channel("linewidth", "line"),
    "linewidth"
  )
})

test_that("legend merge rules come from the registry", {
  # the keyed legends join ggplot2's title-and-keys merge (VP-7)
  merging <- names(Filter(
    function(e) isTRUE(e$legend_merge),
    .AES_REGISTRY
  ))
  expect_setequal(
    merging,
    c("color", "size", "shape", "linewidth", "alpha", "linetype")
  )
  # the edge colour guide folds into the node one on the registry's fields
  col <- list(kind = "discrete", name = "g", levels = c("a", "b"), colors = 1:2)
  edge <- col
  expect_true(.aes_can_merge(edge, col, "edge_color", "color"))
  edge$levels <- c("a", "c")
  expect_false(.aes_can_merge(edge, col, "edge_color", "color"))
  # no registered partner, no fold
  expect_false(.aes_can_merge(col, edge, "color", "edge_color"))
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

# --- row keys refer to the source table, not the panel subset ----------------

# "<panel>|<rows>" for every mark record, in emission order.
prov_rows <- function(p) {
  vapply(
    Filter(function(e) e$kind == "mark", plot_provenance(p)),
    function(e) paste0(e$panel, "|", paste(e$rows, collapse = ",")),
    character(1)
  )
}

test_that("facet_wrap row keys are rows of the plot data, per panel", {
  d <- data.frame(x = 1:6, y = 1:6, f = rep(c("a", "b"), each = 3))
  p <- vplot(d) |> mark_point(x = x, y = y) |> facet_wrap(~f)
  expect_identical(prov_rows(p), c("panel-1-1|1,2,3", "panel-1-2|4,5,6"))
  expect_true(all(vapply(plot_provenance(p), `[[`, "", "source") == "plot"))
})

test_that("facet_grid row keys are rows of the plot data, per panel", {
  d <- data.frame(
    x = 1:8,
    y = 1:8,
    r = rep(c("p", "q"), each = 4),
    c = rep(c("u", "v"), times = 4)
  )
  p <- vplot(d) |> mark_point(x = x, y = y) |> facet_grid(r ~ c)
  expect_identical(
    prov_rows(p),
    c("panel-1-1|1,3", "panel-1-2|2,4", "panel-2-1|5,7", "panel-2-2|6,8")
  )
})

test_that("grouped emitters under facets key each group's original rows", {
  d <- data.frame(
    x = 1:8,
    y = c(1, 3, 2, 4, 1, 3, 2, 4),
    g = rep(c("a", "b"), times = 4),
    f = rep(c("p", "q"), each = 4)
  )
  p <- vplot(d) |> mark_line(x = x, y = y, color = g) |> facet_wrap(~f)
  prov <- Filter(function(e) e$kind == "mark", plot_provenance(p))
  for (e in prov) {
    # every row in a line belongs to one colour group and to this panel's facet
    expect_length(unique(d$g[e$rows]), 1L)
    expect_length(unique(d$f[e$rows]), 1L)
  }
  expect_setequal(unlist(lapply(prov, `[[`, "rows")), seq_len(nrow(d)))
})

test_that("an aggregating stat keys the whole panel subset under facets", {
  p <- vplot(mtcars) |> mark_histogram(x = mpg, bins = 5) |> facet_wrap(~am)
  prov <- Filter(function(e) e$kind == "mark", plot_provenance(p))
  by_panel <- split(prov, vapply(prov, `[[`, "", "panel"))
  expect_setequal(by_panel[["panel-1-1"]][[1]]$rows, which(mtcars$am == 0))
  expect_setequal(by_panel[["panel-1-2"]][[1]]$rows, which(mtcars$am == 1))
  for (e in prov) {
    expect_identical(e$source, "plot")
  }
})

test_that("a layer's own data keys rows of that data and says so", {
  d <- data.frame(x = 1:6, y = 1:6)
  ld <- data.frame(x = c(1.5, 2.5, 3.5), y = c(5, 4, 3), g = c("a", "b", "a"))
  p <- vplot(d) |>
    mark_point(x = x, y = y) |>
    mark_point(data = ld, x = x, y = y, color = g)
  prov <- plot_provenance(p)
  l2 <- Filter(function(e) e$layer == 2L, prov)
  expect_true(all(vapply(l2, `[[`, "", "source") == "layer:2"))
  expect_setequal(unlist(lapply(l2, `[[`, "rows")), 1:3)
  for (e in l2) {
    expect_length(unique(ld$g[e$rows]), 1L)
  }
  l1 <- Filter(function(e) e$layer == 1L, prov)
  expect_identical(l1[[1]]$source, "plot")
})

test_that("faceted layer data keys rows of that data, per panel", {
  d <- data.frame(x = 1:4, y = 1:4, f = rep(c("a", "b"), each = 2))
  ld <- data.frame(x = c(2, 3, 2.5), y = c(2, 3, 1), f = c("b", "a", "b"))
  p <- vplot(d) |>
    mark_point(x = x, y = y) |>
    mark_point(data = ld, x = x, y = y) |>
    facet_wrap(~f)
  l2 <- Filter(function(e) e$layer == 2L, plot_provenance(p))
  rows <- stats::setNames(
    lapply(l2, `[[`, "rows"),
    vapply(l2, `[[`, "", "panel")
  )
  expect_identical(rows[["panel-1-1"]], 2L)
  expect_identical(rows[["panel-1-2"]], c(1L, 3L))
})

test_that("NA rows do not shift the keys of the rows after them", {
  d <- data.frame(
    x = c(1, NA, 3, 4, 5, 6),
    y = c(1, 2, 3, NA, 5, 6),
    g = c("a", NA, "b", "a", NA, "b"),
    f = rep(c("p", "q"), each = 3)
  )
  p <- vplot(d) |> mark_point(x = x, y = y, color = g) |> facet_wrap(~f)
  # rows 2 and 4 miss a position: removed, so no element keys them (VP-6)
  expect_warning(
    prov <- Filter(function(e) e$kind == "mark", plot_provenance(p)),
    "Removed 2 rows"
  )
  for (e in prov) {
    # each colour group's rows share one colour (NA included) and one panel
    expect_length(unique(d$g[e$rows]), 1L)
    expect_length(unique(d$f[e$rows]), 1L)
  }
  q <- Filter(function(e) e$panel == "panel-1-2", prov)
  expect_setequal(unlist(lapply(q, `[[`, "rows")), 5:6)
  expect_false(any(c(2L, 4L) %in% unlist(lapply(prov, `[[`, "rows"))))
})

test_that("z-reordered layers still name their own data by spec position", {
  d <- data.frame(x = 1:3, y = 1:3)
  ld <- data.frame(x = c(1.5, 2.5), y = c(2, 1))
  p <- vplot(d) |>
    mark_point(x = x, y = y) |>
    mark_point(data = ld, x = x, y = y)
  p@layers[[1]]@z <- 1L # draw the plot-data layer last
  prov <- plot_provenance(p)
  srcs <- unique(vapply(prov, `[[`, "", "source"))
  expect_setequal(srcs, c("plot", "layer:2"))
  ld_rows <- unlist(lapply(
    Filter(function(e) e$source == "layer:2", prov),
    `[[`,
    "rows"
  ))
  expect_setequal(ld_rows, 1:2)
})

test_that("provenance_payload() ships the table each element's rows refer to", {
  d <- data.frame(x = 1:4, y = 1:4, f = rep(c("a", "b"), each = 2))
  ld <- data.frame(x = c(2.5, 3.5), y = c(1, 2), lab = c("p", "q"))
  p <- vplot(d) |>
    mark_point(x = x, y = y) |>
    mark_point(data = ld, x = x, y = y)
  pl <- provenance_payload(p, values = TRUE)
  srcs <- vapply(pl$elements, `[[`, "", "source")
  expect_setequal(srcs, c("plot", "layer:2"))
  expect_setequal(names(pl$sources), c("plot", "layer:2"))
  expect_identical(unlist(pl$sources[["layer:2"]]$fields), names(ld))
  expect_length(pl$sources[["layer:2"]]$data, nrow(ld))
  expect_identical(pl$sources[["layer:2"]]$data[[2]]$lab, "q")
  expect_length(pl$sources$plot$data, nrow(d))
  # the top-level `fields` / `data` stay the plot data for existing hosts
  expect_identical(unlist(pl$fields), names(d))
  expect_length(pl$data, nrow(d))
  # every element's rows index its own source table
  for (e in pl$elements) {
    n <- length(pl$sources[[e$source]]$data)
    expect_true(all(e$rows >= 1L & e$rows <= n))
  }
  # without values, sources carry fields only
  pl0 <- provenance_payload(p)
  expect_null(pl0$sources[["layer:2"]]$data)
  expect_null(pl0$data)
})
