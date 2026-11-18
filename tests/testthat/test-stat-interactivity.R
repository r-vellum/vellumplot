# Per-row interactivity (tooltip / data_id / hover_group) and provenance after a
# stat that changes the rows. A row-preserving stat (dot plot, outlier, window,
# count) reorders or filters rows and records `.rows`; a summarising stat (bin,
# hex, density) has no single input row per output row.

keyed_elements <- function(p) {
  el <- vellum::scene_model(as_vellum_scene(p))$elements
  el[!is.na(el$key) & !grepl("^legend:", el$key), , drop = FALSE]
}
tooltips <- function(el) {
  vapply(el$meta, function(m) m$tooltip %||% NA_character_, character(1))
}

# VP-31 ---------------------------------------------------------------------

test_that("a dot plot's keys and tooltips follow each dot's own row (VP-31)", {
  # descending input: the stat sorts the dots ascending, reversing the rows
  d <- data.frame(v = c(5, 4, 3, 2, 1), id = c("a", "b", "c", "d", "e"))
  el <- keyed_elements(
    vplot(d) |>
      mark_dotplot(
        x = v,
        data_id = id,
        tooltip = paste("row", id),
        binwidth = 1
      )
  )
  el <- el[order(el$x), , drop = FALSE]
  # left to right the dots are v = 1..5, i.e. rows e, d, c, b, a
  expect_identical(el$key, c("e", "d", "c", "b", "a"))
  expect_identical(tooltips(el), paste("row", el$key))
})

test_that("an outlier callout keys only its surviving rows (VP-31)", {
  d <- data.frame(x = 1:12, y = c(1:10, 100, -100), id = letters[1:12])
  L <- vellumplot:::.apply_stat(vellumplot:::.resolve_layer(
    (vplot(d) |>
      mark_outlier_label(x = x, y = y, data_id = id, tooltip = id))@layers[[1]],
    d
  ))
  expect_identical(L$meta$data_id, c("k", "l"))
  expect_identical(L$meta$tooltip, c("k", "l"))
})

test_that("bin2d/hex cells never carry per-row keys (VP-31)", {
  # one point per bin: as many cells as rows, which used to slip past the length
  # guard and key each cell with an unrelated row
  d <- data.frame(
    x = c(4, 1, 3, 2),
    y = c(4, 1, 3, 2),
    id = c("d", "a", "c", "b")
  )
  expect_equal(
    nrow(keyed_elements(
      vplot(d) |> mark_bin2d(x = x, y = y, data_id = id, tooltip = id, bins = 4)
    )),
    0L
  )
  expect_equal(
    nrow(keyed_elements(
      vplot(d) |> mark_hex(x = x, y = y, data_id = id, bins = 4)
    )),
    0L
  )
  # a constant tooltip still reaches every cell, keyed by cell index
  el <- keyed_elements(
    vplot(d) |> mark_bin2d(x = x, y = y, tooltip = "a bin", bins = 4)
  )
  expect_equal(nrow(el), 4L)
  expect_identical(sort(el$key), as.character(1:4))
  expect_true(all(tooltips(el) == "a bin"))
})

test_that("a summarising stat keeps values constant within each group (VP-31)", {
  set.seed(31)
  d <- data.frame(
    v = c(stats::rnorm(30), stats::rnorm(30, 3)),
    g = rep(c("p", "q"), each = 30)
  )
  L <- vellumplot:::.apply_stat(vellumplot:::.resolve_layer(
    (vplot(d) |>
      mark_density(x = v, fill = g, data_id = g, tooltip = g))@layers[[1]],
    d
  ))
  expect_length(L$meta$data_id, L$n)
  expect_identical(L$meta$data_id, as.character(L$values$fill))
  expect_identical(as.character(L$meta$tooltip), as.character(L$values$fill))
  # a per-row value varies within a group: no single value, so it is dropped
  L2 <- vellumplot:::.apply_stat(vellumplot:::.resolve_layer(
    (vplot(d) |>
      mark_density(x = v, fill = g, data_id = g, tooltip = v))@layers[[1]],
    d
  ))
  expect_null(L2$meta$tooltip)
  expect_identical(L2$meta$data_id, as.character(L2$values$fill))
})

# VP-32 ---------------------------------------------------------------------

set.seed(32)
dots <- data.frame(
  v = round(stats::rnorm(40), 1),
  g = rep(c("p", "q"), 20),
  f = rep(c("A", "B"), each = 20)
)

test_that("dot-plot provenance names each element's own source rows (VP-32)", {
  prov <- plot_provenance(vplot(dots) |> mark_dotplot(x = v, fill = g))
  expect_length(prov, 2L) # one point grob per fill level
  for (e in prov) {
    expect_length(unique(dots$g[e$rows]), 1L)
    expect_length(e$rows, 20L)
  }
  expect_setequal(unlist(lapply(prov, `[[`, "rows")), seq_len(nrow(dots)))
})

test_that("faceted dot-plot provenance maps to the plot data's rows (VP-32)", {
  prov <- plot_provenance(
    vplot(dots) |> mark_dotplot(x = v, fill = g) |> facet_wrap(~f)
  )
  expect_length(prov, 4L)
  for (e in prov) {
    expect_length(unique(dots$g[e$rows]), 1L)
    expect_length(unique(dots$f[e$rows]), 1L)
    expect_length(e$rows, 10L)
  }
  expect_setequal(unlist(lapply(prov, `[[`, "rows")), seq_len(nrow(dots)))
})

test_that("layer-data dot-plot provenance keys the layer's own rows (VP-32)", {
  prov <- plot_provenance(
    vplot(data.frame(a = 1, b = 1)) |>
      mark_point(x = a, y = b) |>
      mark_dotplot(data = dots, x = v, fill = g)
  )
  dp <- Filter(function(e) identical(e$source, "layer:2"), prov)
  expect_length(dp, 2L)
  for (e in dp) {
    expect_length(unique(dots$g[e$rows]), 1L)
  }
})

test_that("a count bubble names every coincident row it merges (VP-32)", {
  d <- data.frame(
    x = c(1, 1, 2, 1),
    y = c(1, 1, 2, 1),
    g = c("a", "a", "b", "a")
  )
  prov <- plot_provenance(vplot(d) |> mark_count(x = x, y = y, color = g))
  rows <- lapply(prov, `[[`, "rows")
  expect_true(list(c(1L, 2L, 4L)) %in% rows)
  expect_true(list(3L) %in% rows)
})
