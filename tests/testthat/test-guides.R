# Generalized guide list: colour, size, shape.

train <- function(p) {
  vellumplot:::.train_scales(p, vellumplot:::.resolve_layers(p))
}
kinds <- function(p) {
  vapply(
    vellumplot:::.legend_guides(train(p)),
    function(g) g$kind,
    character(1)
  )
}

test_that("minor breaks extrapolate the local gap at each end (H33)", {
  mb <- vellumplot:::.minor_breaks(c(0, 1, 10))
  # low end uses the first gap (1), high end uses the last gap (9)
  expect_equal(mb[1], 0 - 1 / 2)
  expect_equal(mb[length(mb)], 10 + 9 / 2) # not 10 + 1/2
  expect_equal(mb[2:3], c(0.5, 5.5)) # interior midpoints unchanged
  # evenly spaced breaks are unaffected (below, midpoint, above)
  expect_equal(vellumplot:::.minor_breaks(c(2, 4)), c(1, 3, 5))
})

test_that("guides are built per mapped channel in colour/size/shape order", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, color = hp, size = disp, shape = factor(cyl))
  expect_identical(kinds(p), c("color_continuous", "size", "shape"))
})

test_that("only mapped channels produce guides", {
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg, shape = factor(cyl))
  expect_identical(kinds(p), "shape")
  p0 <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
  expect_length(kinds(p0), 0)
})

test_that("a plot with three guides renders", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, color = hp, size = disp, shape = factor(cyl))
  f <- local_tempfile(fileext = ".png")
  render_plot(p, f)
  expect_gt(file.info(f)$size, 0)
})

test_that(".legend_width handles a shape guide without error", {
  built <- vellumplot:::.build_panels(
    vplot(mtcars) |>
      mark_point(x = wt, y = mpg, color = hp, shape = factor(carb))
  )
  rt <- vellumplot:::.resolve_theme(vellumplot:::.theme_default())
  guides <- vellumplot:::.legend_guides(built$scales)
  expect_length(guides, 2)
  expect_silent(vellumplot:::.legend_width(guides, rt))
})

test_that("legend.position = none drops all guides", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, color = hp, shape = factor(cyl)) |>
    theme(legend.position = "none")
  built <- vellumplot:::.build_panels(p)
  lay <- vellumplot:::.build_layout(
    built,
    vellumplot:::.legend_guides(built$scales),
    p@labels,
    vellumplot:::.resolve_theme(p@theme)
  )
  expect_true(is.na(lay$legend_col))
  expect_true(is.na(lay$legend_row))
})

# Layout placement of the legend for each side.
layout_for <- function(pos) {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, color = factor(cyl), size = disp) |>
    theme(legend.position = pos)
  built <- vellumplot:::.build_panels(p)
  vellumplot:::.build_layout(
    built,
    vellumplot:::.legend_guides(built$scales),
    p@labels,
    vellumplot:::.resolve_theme(p@theme)
  )
}

test_that("right/left take a legend column, not a row", {
  for (pos in c("right", "left")) {
    lay <- layout_for(pos)
    expect_false(is.na(lay$legend_col))
    expect_true(is.na(lay$legend_row))
  }
  # left places the legend before the y-title column; right after the panels.
  expect_lt(layout_for("left")$legend_col, layout_for("left")$ytitle_col)
  expect_gt(layout_for("right")$legend_col, layout_for("right")$panel_col[1])
})

test_that("top/bottom take a legend row, not a column", {
  for (pos in c("top", "bottom")) {
    lay <- layout_for(pos)
    expect_true(is.na(lay$legend_col))
    expect_false(is.na(lay$legend_row))
  }
  expect_lt(layout_for("top")$legend_row, layout_for("top")$panel_row[1])
  expect_gt(layout_for("bottom")$legend_row, layout_for("bottom")$panel_row[1])
})

test_that("each legend position renders", {
  for (pos in c("right", "left", "top", "bottom")) {
    p <- vplot(mtcars) |>
      mark_point(x = wt, y = mpg, color = hp, size = disp) |>
      theme(legend.position = pos)
    f <- local_tempfile(fileext = ".png")
    render_plot(p, f)
    expect_gt(file.info(f)$size, 0)
  }
})

test_that("legend.position rejects unknown values", {
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
  expect_error(theme(p, legend.position = "middle"), "legend.position")
})

test_that("the colour key glyph matches the mark that maps colour", {
  pt <- vplot(mtcars) |> mark_point(x = wt, y = mpg, color = factor(cyl))
  expect_identical(train(pt)$color$key_glyph, "point")

  ln <- vplot(mtcars) |> mark_line(x = wt, y = mpg, color = factor(cyl))
  expect_identical(train(ln)$color$key_glyph, "line")

  df <- data.frame(g = c("a", "b"), h = c(1, 2), k = c("x", "y"))
  br <- vplot(df) |> mark_bar(x = g, y = h, fill = k)
  expect_identical(train(br)$color$key_glyph, "square")
})

test_that("colour + shape on one variable merge into a single guide", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, color = factor(cyl), shape = factor(cyl))
  expect_identical(kinds(p), "merged")
  # the merged pseudo-scale carries aligned per-key style vectors
  g <- vellumplot:::.legend_guides(train(p))[[1]]$sc
  expect_length(g$fills, length(g$labels))
  expect_length(g$shapes, length(g$labels))
  expect_null(g$sizes_mm)
})

test_that("continuous colour + size on one variable merge into a single guide", {
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg, color = hp, size = hp)
  expect_identical(kinds(p), "merged")
  g <- vellumplot:::.legend_guides(train(p))[[1]]$sc
  expect_length(g$sizes_mm, length(g$labels))
  expect_length(g$fills, length(g$labels))
})

test_that("distinct scale names keep colour and its partner separate", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, color = factor(cyl), shape = factor(cyl)) |>
    scale_shape(name = "Cylinders")
  expect_identical(kinds(p), c("color_discrete", "shape"))
})

test_that("different variables do not merge", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, color = factor(cyl), shape = factor(gear))
  expect_identical(kinds(p), c("color_discrete", "shape"))
})

test_that("a merged legend still renders in every position", {
  for (pos in c("right", "left", "top", "bottom")) {
    p <- vplot(mtcars) |>
      mark_point(x = wt, y = mpg, color = factor(cyl), shape = factor(cyl)) |>
      theme(legend.position = pos)
    f <- local_tempfile(fileext = ".png")
    render_plot(p, f)
    expect_gt(file.info(f)$size, 0)
  }
})

# --- multi-column legend wrapping (#80) --------------------------------------

test_that(".legend_columns wraps guides to fit the available height", {
  m <- .legend_metrics(.resolve_theme(.theme_default()))
  mkg <- function(k) {
    list(
      kind = "size",
      sc = list(
        legend_labels = as.character(seq_len(k)),
        legend_sizes = seq_len(k)
      )
    )
  }
  guides <- replicate(4, mkg(4), simplify = FALSE)
  gh <- .guide_height_mm(guides[[1]], m)
  # Inf height (the default / composition / horizontal paths) keeps one column.
  expect_length(.legend_columns(guides, m, Inf), 1L)
  # A height fitting ~2 guides wraps four guides into two columns.
  cols <- .legend_columns(guides, m, avail_h = gh * 2 + m$spacing + 0.5)
  expect_length(cols, 2L)
  expect_identical(sort(unlist(cols)), 1:4)
})

test_that("many legends stay on-canvas by wrapping into columns (#80)", {
  skip_if_not_installed("igraph")
  skip_if_not_installed("graphlayouts")
  g <- igraph::make_graph("Zachary")
  g <- igraph::set_vertex_attr(g, "grp", value = factor(igraph::degree(g) %% 4))
  g <- igraph::set_vertex_attr(g, "deg", value = igraph::degree(g))
  g <- igraph::set_edge_attr(g, "w", value = seq_len(igraph::ecount(g)))
  # four legends (fill, size, edge colour, edge width) on a short figure
  p <- vgraph(g, width = 6, height = 4) |>
    mark_edges(color = w, linewidth = w) |>
    mark_nodes(fill = grp, size = deg) |>
    scale_edge_color(palette = "Grays") |>
    scale_edge_width(range = c(0.3, 2.5)) |>
    scale_size(range = c(2, 9))
  f <- local_tempfile(fileext = ".svg")
  render_plot(p, f)
  svg <- paste(readLines(f), collapse = "\n")
  height_px <- as.numeric(sub(
    '.*<svg[^>]* height="([0-9.]+)".*',
    "\\1",
    regmatches(svg, regexpr("<svg[^>]*>", svg))
  ))
  # every legend-title y (x/y within its translate) must sit inside the canvas
  titles <- regmatches(
    svg,
    gregexpr(
      '<text x="[0-9.]+" y="[0-9.]+"[^>]*transform="matrix\\([^)]*\\)"[^>]*>(grp|deg|w)</text>',
      svg
    )
  )[[1]]
  expect_gte(length(titles), 4L) # all four legends are emitted
  ys <- vapply(
    titles,
    function(t) {
      y <- as.numeric(sub('.* y="([0-9.]+)".*', "\\1", t))
      ty <- as.numeric(sub(
        ".*matrix\\(1 0 0 1 [0-9.-]+ ([0-9.-]+)\\).*",
        "\\1",
        t
      ))
      y + ty
    },
    numeric(1)
  )
  expect_true(all(ys >= 0 & ys <= height_px)) # none clipped off top/bottom
})
