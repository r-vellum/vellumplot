# Legends beside each facet panel (VP-53): `position = "panel"`, the default
# of a non-position scale resolved "independent".

pl_df <- data.frame(
  x = 1:12,
  y = c(3, 1, 2, 5, 4, 6, 9, 7, 8, 2, 4, 6),
  g = rep(c("a", "b", "c", "d"), each = 3),
  h = rep(c("top", "bottom"), each = 6),
  k = c("p", "q", "r", "q", "s", "t", "u", "u", "v", "p", "p", "w"),
  v = c(1, 2, 3, 10, 20, 30, 100, 200, 300, 4, 5, 6)
)
pl_wrap <- vplot(pl_df) |>
  mark_point(x = x, y = y, color = k) |>
  facet_wrap(~g, ncol = 2) |>
  resolve_scale(color = "independent")

pl_layout <- function(p) {
  built <- vellumplot:::.build_panels(p)
  vellumplot:::.build_layout(
    built,
    vellumplot:::.facet_legend_guides(built),
    p@labels,
    vellumplot:::.resolve_theme(p@theme)
  )
}
pl_panels <- function(p) {
  vellum::scene_model(vellum::as_vellum_scene(p))$panels
}

test_that("guide constructors accept position = 'panel'", {
  expect_identical(guide_legend(position = "panel")$position, "panel")
  expect_identical(guide_colourbar(position = "panel")$position, "panel")
  expect_identical(guide_bins(position = "panel")$position, "panel")
})

test_that("an independent scale's legends sit right of each panel column", {
  lay <- pl_layout(pl_wrap)
  pl <- lay$panel_legends
  expect_identical(pl$side, "right")
  expect_length(pl$guides, 4L)
  expect_true(all(lengths(pl$guides) == 1L))
  # one track per panel column, right after that column's panel
  expect_identical(pl$cols, lay$panel_col + 1L)
  expect_true(all(is.na(pl$rows)))
  # nothing goes to the shared legend area
  expect_true(is.na(lay$legend_col))
  nms <- pl_panels(pl_wrap)$name
  expect_true(all(
    c("legend-panel-1-1", "legend-panel-1-2", "legend-panel-2-1", "legend-panel-2-2") %in%
      nms
  ))
  expect_false("legend" %in% nms)
})

test_that("a column track is as wide as its widest panel legend", {
  lay <- pl_layout(pl_wrap)
  built <- vellumplot:::.build_panels(pl_wrap)
  rt <- vellumplot:::.resolve_theme(pl_wrap@theme)
  w <- function(i) {
    vellum::vl_convert(
      vellumplot:::.legend_width(lay$panel_legends$guides[[i]], rt),
      "mm"
    )
  }
  # panels 1, 3 are column 1; 2, 4 column 2
  col1 <- vellum::vl_convert(lay$widths[lay$panel_legends$cols[1]], "mm")
  expect_equal(col1, max(w(1), w(3)))
})

test_that("the theme's legend.position picks the side of panel legends", {
  side_of <- function(pos) {
    pl_layout(pl_wrap |> theme(legend.position = pos))$panel_legends
  }
  left <- pl_layout(pl_wrap |> theme(legend.position = "left"))
  expect_identical(left$panel_legends$side, "left")
  expect_lt(left$panel_legends$cols[1], left$ytitle_col)
  expect_lt(left$panel_legends$cols[2], left$panel_col[2])
  top <- pl_layout(pl_wrap |> theme(legend.position = "top"))
  expect_true(all(top$panel_legends$rows < top$panel_row))
  expect_true(all(is.na(top$panel_legends$cols)))
  bottom <- pl_layout(pl_wrap |> theme(legend.position = "bottom"))
  expect_gt(bottom$panel_legends$rows[1], bottom$panel_row[1])
  expect_lt(bottom$panel_legends$rows[1], bottom$panel_row[2])
  # the last row's legends sit below the x-axis title
  expect_gt(bottom$panel_legends$rows[2], bottom$xtitle_row)
  inside <- side_of("inside")
  expect_identical(inside$side, "inside")
  expect_false(is.null(inside$inside))
  expect_true(all(is.na(inside$cols)) && all(is.na(inside$rows)))
  # (tall enough for legend rows above / below both panel rows, VP-62)
  tall <- pl_wrap
  tall@height <- 7
  for (pos in c("left", "top", "bottom", "inside")) {
    expect_no_message(expect_no_error(vellum::as_vellum_scene(
      tall |> theme(legend.position = pos)
    )))
  }
  # "none" hides them too
  expect_null(pl_layout(pl_wrap |> theme(legend.position = "none"))$panel_legends)
})

test_that("facet_grid gets per-column and per-row legend tracks", {
  p <- vplot(pl_df) |>
    mark_point(x = x, y = y, color = k) |>
    facet_grid(h ~ g) |>
    resolve_scale(color = "independent")
  lay <- pl_layout(p)
  cols <- lay$panel_legends$cols
  expect_false(anyNA(cols))
  # the last column's legends sit outside the row strips
  expect_gt(cols[length(cols)], lay$rowstrip_col)
  lay_b <- pl_layout(p |> theme(legend.position = "bottom"))
  expect_false(anyNA(lay_b$panel_legends$rows))
  # (wide enough for four panels beside their legends, VP-59, and tall enough
  # for a legend row above each panel row, VP-62)
  p@width <- 10
  p@height <- 7
  expect_no_message(vellum::as_vellum_scene(p))
  expect_no_message(vellum::as_vellum_scene(
    p |> theme(legend.position = "top")
  ))
})

test_that("shared and per-panel legends mix", {
  p <- vplot(pl_df) |>
    mark_point(x = x, y = y, color = v, size = v) |>
    facet_wrap(~g) |>
    resolve_scale(color = "independent")
  lay <- pl_layout(p)
  expect_length(lay$legend_groups$right, 1L) # the shared size legend
  expect_identical(lay$legend_groups$right[[1]]$kind, "size")
  expect_true(all(lengths(lay$panel_legends$guides) == 1L))
  nms <- pl_panels(p)$name
  expect_true("legend" %in% nms)
  expect_true("legend-panel-1-1" %in% nms)
})

test_that("a shared scale placed 'panel' repeats its legend beside every panel", {
  p <- vplot(pl_df) |>
    mark_point(x = x, y = y, color = k, size = v) |>
    facet_wrap(~g) |>
    guides(size = guide_legend(position = "panel"))
  lay <- pl_layout(p)
  expect_true(all(lengths(lay$panel_legends$guides) == 1L))
  expect_identical(lay$panel_legends$guides[[2]][[1]]$kind, "size")
  expect_length(lay$legend_groups$right, 1L) # the colour legend
  # an unfaceted plot: beside its single panel
  q <- vplot(pl_df) |>
    mark_point(x = x, y = y, color = k) |>
    guides(color = guide_legend(position = "panel"))
  lq <- pl_layout(q)
  expect_identical(lq$panel_legends$cols, lq$panel_col + 1L)
  expect_no_error(vellum::as_vellum_scene(q))
})

test_that("a panel with no values gets no legend; a column with none no track", {
  d <- data.frame(
    x = 1:6,
    y = 1:6,
    g = rep(c("a", "b"), each = 3),
    k = c("p", "q", "r", NA, NA, NA)
  )
  p <- vplot(d) |>
    mark_point(x = x, y = y) |>
    mark_point(data = d[d$g == "a", ], x = x, y = y, color = k) |>
    facet_wrap(~g) |>
    resolve_scale(color = "independent")
  lay <- pl_layout(p)
  expect_identical(lengths(lay$panel_legends$guides), c(1L, 0L))
  expect_false(is.na(lay$panel_legends$cols[1]))
  expect_true(is.na(lay$panel_legends$cols[2]))
  expect_no_error(vellum::as_vellum_scene(p))
})

test_that("legend keys repeat per panel as before", {
  keys <- function(p) {
    el <- vellum::scene_model(vellum::as_vellum_scene(p))$elements
    k <- el$key[!is.na(el$key)]
    sort(k[grepl("^legend:", k)])
  }
  keyed <- vplot(pl_df) |>
    mark_point(x = x, y = y, color = k, data_id = paste0("r", x)) |>
    facet_wrap(~g) |>
    resolve_scale(color = "independent")
  collected <- keyed |> guides(color = guide_legend(position = "right"))
  expect_gt(length(keys(keyed)), 0L)
  expect_identical(keys(keyed), keys(collected))
})

test_that("legend.justification aligns per-panel legends along the panel", {
  p <- pl_wrap |> theme(legend.justification = "top")
  pan <- pl_panels(p)
  pan_c <- pl_panels(pl_wrap)
  # the legend cell is the same; only its content moves
  a <- pan[pan$name == "legend-panel-1-1", ]
  b <- pan_c[pan_c$name == "legend-panel-1-1", ]
  expect_equal(a$py0, b$py0)
  expect_identical(
    vellumplot:::.build_layout(
      vellumplot:::.build_panels(p),
      vellumplot:::.facet_legend_guides(vellumplot:::.build_panels(p)),
      p@labels,
      vellumplot:::.resolve_theme(p@theme)
    )$panel_legends$just,
    c(0.5, 1)
  )
  expect_false(identical(
    vellum::scene_svg(vellum::as_vellum_scene(p)),
    vellum::scene_svg(vellum::as_vellum_scene(pl_wrap))
  ))
})

test_that("position = 'panel' round-trips through the spec", {
  skip_if_not_installed("jsonlite")
  p <- pl_wrap |>
    mark_point(x = x, y = y, size = v) |>
    guides(size = guide_legend(position = "panel"))
  p2 <- spec_from_json(spec_to_json(p))
  expect_identical(vellumplot:::.scale_for(p2, "size")@guide$position, "panel")
  norm <- function(s) gsub("vl[0-9]+", "vl", s)
  svg <- function(p) vellum::scene_svg(vellum::as_vellum_scene(p))
  expect_identical(norm(svg(p2)), norm(svg(p)))
})

test_that("a faceted plot with panel legends still composes", {
  a <- pl_wrap
  b <- vplot(pl_df) |>
    mark_point(x = x, y = y, color = k) |>
    guides(color = guide_legend(position = "panel"))
  expect_no_error(vellum::as_vellum_scene(hconcat(a, b)))
  expect_no_error(vellum::as_vellum_scene(hconcat(b, b, guides = "keep")))
})

test_that("a layout built outside a render measures legends at the theme size", {
  # an unthemed plot's NULL @theme resolves the default theme, so legend text
  # has a size and a finite, positive width (VR-15: it was measured at no size)
  rt <- vellumplot:::.resolve_theme(NULL)
  expect_identical(rt, vellumplot:::.resolve_theme(vellumplot:::.theme_default()))
  m <- vellumplot:::.legend_metrics(rt)
  expect_true(is.numeric(m$text_fs) && m$text_fs > 0)
  w <- vellum::vl_convert(
    vellumplot:::.legend_width(pl_layout(pl_wrap)$panel_legends$guides[[1]], rt),
    "mm"
  )
  expect_true(is.finite(w) && w > 0 && w < 100)
})

test_that("per-panel legends that would squeeze the panels are collected (VP-59)", {
  narrow <- pl_wrap
  narrow@width <- 2.5
  expect_message(
    nms <- pl_panels(narrow)$name,
    "Collecting the per-panel legends"
  )
  # one plot legend, titled by panel, instead of a legend beside each panel
  expect_true("legend" %in% nms)
  expect_false(any(startsWith(nms, "legend-panel-")))
  built <- vellumplot:::.build_panels(narrow)
  guides <- vellumplot:::.facet_legend_guides(built, collect = TRUE)
  titles <- vapply(guides, function(g) g$sc$name, "")
  expect_setequal(titles, paste0("k (", c("a", "b", "c", "d"), ")"))
  expect_true(all(vapply(guides, function(g) is.null(g$panel), NA)))
  # a page wide enough keeps them beside the panels, without a message
  wide <- pl_wrap
  wide@width <- 9
  expect_no_message(nms <- pl_panels(wide)$name)
  expect_true("legend-panel-1-1" %in% nms)
  # a shared scale's legend repeated beside every panel collapses to one
  shared <- vplot(pl_df) |>
    mark_point(x = x, y = y, color = k) |>
    facet_wrap(~g, ncol = 4) |>
    guides(color = guide_legend(position = "panel"))
  shared@width <- 3
  expect_message(nms <- pl_panels(shared)$name, "per-panel legends")
  expect_identical(sum(startsWith(nms, "legend")), 1L)
})

test_that("per-panel legends above / below that would squeeze panel heights are collected (VP-62)", {
  low <- pl_wrap |> theme(legend.position = "bottom")
  low@height <- 2.5
  expect_message(
    nms <- pl_panels(low)$name,
    "shorter than"
  )
  expect_true("legend" %in% nms)
  expect_false(any(startsWith(nms, "legend-panel-")))
  # the collected legend sits below the panels, as the theme asks
  pan <- suppressMessages(pl_panels(low))
  leg <- pan[pan$name == "legend", ]
  panels <- pan[startsWith(pan$name, "panel-"), ]
  expect_gt(leg$y0, max(panels$y1))
  # a tall enough page keeps them under each panel row, without a message
  tall <- low
  tall@height <- 6
  expect_no_message(nms <- pl_panels(tall)$name)
  expect_true("legend-panel-1-1" %in% nms)
  # the width check is unchanged: a narrow page with bottom legends is not
  # squeezed sideways
  narrow <- low
  narrow@height <- 6
  narrow@width <- 3
  expect_no_message(pl_panels(narrow))
})
