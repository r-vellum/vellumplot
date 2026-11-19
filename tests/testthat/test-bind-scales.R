# bind_scale() / select_interval(bind = "scales") and the per-cell panel
# descriptors a host needs to zoom the right cell of a composition (F-15).

ts_df <- data.frame(t = 1:40, v = sin(1:40 / 5))

panel_rows <- function(x) {
  m <- vellum::scene_model(vellum::as_vellum_scene(x))
  p <- m$panels
  p[grepl("panel-", p$name), , drop = FALSE]
}

test_that("select_interval(bind = 'scales') is recorded only when set", {
  s <- select_interval("z", encodings = "x", bind = "scales")
  expect_identical(s@bind, "scales")
  p <- vplot(ts_df) |> mark_line(x = t, y = v) |> add_selection(s)
  rec <- interaction_model(p)$selections[[1]]
  expect_identical(rec$bind, "scales")

  plain <- vplot(ts_df) |>
    mark_line(x = t, y = v) |>
    select_interval("b", encodings = "x")
  expect_null(interaction_model(plain)$selections[[1]]$bind)
})

test_that("select_interval() validates bind", {
  expect_error(select_interval("z", bind = "zoom"), "bind")
  expect_error(select_interval("z", bind = TRUE), "bind")
  expect_error(
    select_interval("z", region = "lasso", bind = "scales"),
    "region"
  )
})

test_that("bind_scale() must follow an interval selection on a brushed axis", {
  pt <- vplot(ts_df) |>
    mark_point(x = t, y = v) |>
    select_point("pick") |>
    bind_scale("pick")
  expect_error(interaction_model(pt), "interval selection")

  ax <- vplot(ts_df) |>
    mark_point(x = t, y = v) |>
    select_interval("b", encodings = "x") |>
    bind_scale("b", aes = "y")
  expect_error(interaction_model(ax), "does not constrain")
})

test_that("a lone plot's binds carry no cell", {
  p <- vplot(ts_df) |>
    mark_point(x = t, y = v) |>
    select_interval("b", encodings = "x") |>
    bind_scale("b")
  im <- interaction_model(p)
  expect_null(im$binds[[1]]$cell)
  expect_null(im$selections[[1]]$cells)
  expect_identical(panel_rows(p)$name, "panel-1-1")
})

test_that("a composition names each cell's panels and tags binds by cell", {
  brush <- select_interval("brush", encodings = "x")
  cmp <- vconcat(
    vplot(ts_df) |> mark_line(x = t, y = v) |> add_selection(brush),
    vplot(ts_df) |> mark_line(x = t, y = v) |> bind_scale(brush)
  )
  im <- interaction_model(cmp)
  expect_identical(im$binds[[1]]$cell, "cell-2")
  expect_identical(unlist(im$selections[[1]]$cells), "cell-1")

  pr <- panel_rows(cmp)
  expect_identical(pr$name, c("cell-1-panel-1-1", "cell-2-panel-1-1"))
  cells <- vapply(pr$meta, function(m) m$cell, character(1))
  expect_identical(cells, c("cell-1", "cell-2"))
  # aligned cells now carry the scale descriptors and a pan group, like a plot
  expect_true(all(vapply(pr$meta, function(m) isTRUE(m$scales$cartesian), logical(1))))
  svg <- vellum::scene_svg(vellum::as_vellum_scene(cmp))
  expect_match(svg, 'data-vellum-pan="cell-1-panel-1-1"', fixed = TRUE)
  expect_match(svg, 'data-vellum-pan="cell-2-panel-1-1"', fixed = TRUE)
})

test_that("faceted and nested cells get unique panel names in leaf order", {
  brush <- select_interval("brush", encodings = "x")
  faceted <- vplot(ts_df) |>
    mark_point(x = t, y = v) |>
    facet_wrap(~ t > 20) |>
    bind_scale(brush)
  cmp <- vconcat(
    vplot(ts_df) |> mark_line(x = t, y = v) |> add_selection(brush),
    hconcat(
      vplot(ts_df) |> mark_point(x = t, y = v),
      faceted
    )
  )
  nm <- panel_rows(cmp)$name
  expect_false(anyDuplicated(nm) > 0)
  expect_setequal(
    nm,
    c(
      "cell-1-panel-1-1",
      "cell-2-panel-1-1",
      "cell-3-panel-1-1",
      "cell-3-panel-1-2"
    )
  )
  im <- interaction_model(cmp)
  expect_identical(im$binds[[1]]$cell, "cell-3")
})

test_that("a selection registered on several cells lists them all", {
  z <- select_interval("z", encodings = "x", bind = "scales")
  cmp <- hconcat(
    vplot(ts_df) |> mark_point(x = t, y = v) |> add_selection(z),
    vplot(ts_df) |> mark_point(x = t, y = v) |> add_selection(z)
  )
  im <- interaction_model(cmp)
  expect_length(im$selections, 1L)
  expect_identical(unlist(im$selections[[1]]$cells), c("cell-1", "cell-2"))
})
