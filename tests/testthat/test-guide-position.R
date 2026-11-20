# A guide's own `position =` (VP-53): overrides theme(legend.position) for
# that legend only, as ggplot2 3.5's `guide_legend(position =)` does.

pos_plot <- vplot(mtcars) |>
  mark_point(x = wt, y = mpg, color = factor(cyl), size = hp)

pos_layout <- function(p) {
  built <- vellumplot:::.build_panels(p)
  vellumplot:::.build_layout(
    built,
    vellumplot:::.facet_legend_guides(built),
    p@labels,
    vellumplot:::.resolve_theme(p@theme)
  )
}
panel_names <- function(p) {
  vellum::scene_model(vellum::as_vellum_scene(p))$panels$name
}

test_that("every legend guide constructor takes and checks position", {
  expect_null(guide_legend()$position)
  expect_identical(guide_legend(position = "bottom")$position, "bottom")
  expect_identical(guide_colourbar(position = "top")$position, "top")
  expect_identical(guide_colorbar(position = "left")$position, "left")
  expect_identical(guide_coloursteps(position = "inside")$position, "inside")
  expect_identical(guide_bins(position = "bottom")$position, "bottom")
  expect_identical(guide_bivariate(position = "left")$position, "left")
  expect_error(guide_legend(position = "middle"), "position")
  expect_error(guide_colourbar(position = c("top", "left")), "position")
  expect_error(guide_bins(position = 1), "position")
})

test_that("the ggplot2 guard stays first: ggplot2 still gets its own guides", {
  skip_if_not_installed("ggplot2")
  from_ggplot2 <- new.env(parent = asNamespace("ggplot2"))
  for (nm in c("guide_legend", "guide_colourbar", "guide_coloursteps", "guide_bins")) {
    f <- getExportedValue("vellumplot", nm)
    res <- eval(quote(f()), list(f = f), from_ggplot2)
    expect_true(inherits(res, "ggproto"), info = nm)
  }
})

test_that("a guide's position overrides the theme for that legend only", {
  lay <- pos_layout(pos_plot |> guides(color = guide_legend(position = "bottom")))
  g <- lay$legend_groups
  expect_length(g$right, 1L)
  expect_length(g$bottom, 1L)
  expect_identical(g$bottom[[1]]$sc$name, "factor(cyl)")
  expect_false(is.na(lay$legend_cols[["right"]]))
  expect_false(is.na(lay$legend_rows[["bottom"]]))
  expect_gt(lay$legend_rows[["bottom"]], lay$panel_row[1])
  # the right legend stays the theme's "legend"; the other is named by side
  nms <- panel_names(pos_plot |> guides(color = guide_legend(position = "bottom")))
  expect_true(all(c("legend", "legend-bottom") %in% nms))
})

test_that("legends can use all four sides and the inside at once", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, color = factor(cyl), size = hp, shape = factor(am)) |>
    mark_line(x = wt, y = mpg, linetype = factor(gear), alpha = qsec) |>
    guides(
      color = guide_legend(position = "left"),
      size = guide_legend(position = "top"),
      shape = guide_legend(position = "bottom"),
      alpha = guide_legend(position = "inside")
    )
  lay <- pos_layout(p)
  expect_lt(lay$legend_cols[["left"]], lay$ytitle_col)
  expect_gt(lay$legend_cols[["right"]], lay$panel_col[1])
  expect_lt(lay$legend_rows[["top"]], lay$panel_row[1])
  expect_gt(lay$legend_rows[["bottom"]], lay$panel_row[1])
  expect_false(is.null(lay$legend_inside))
  nms <- panel_names(p)
  expect_true(all(
    c("legend", "legend-left", "legend-top", "legend-bottom", "legend-inside") %in%
      nms
  ))
})

test_that("an inside guide uses legend.position.inside whatever the theme", {
  p <- pos_plot |>
    theme(legend.position = "bottom", legend.position.inside = c(0.9, 0.9)) |>
    guides(size = guide_legend(position = "inside"))
  lay <- pos_layout(p)
  expect_identical(lay$legend_inside$pos, c(0.9, 0.9))
  expect_length(lay$legend_groups$inside, 1L)
  expect_length(lay$legend_groups$bottom, 1L)
  expect_true(is.na(lay$legend_col))
})

test_that("legend.position = 'none' hides even a positioned guide", {
  p <- pos_plot |>
    theme(legend.position = "none") |>
    guides(color = guide_legend(position = "bottom"))
  lay <- pos_layout(p)
  expect_true(is.na(lay$legend_col))
  expect_true(is.na(lay$legend_row))
  expect_null(lay$legend_inside)
})

test_that("a guide at the theme's own position changes nothing", {
  svg <- function(p) vellum::scene_svg(vellum::as_vellum_scene(p))
  expect_identical(
    svg(pos_plot |> guides(color = guide_legend(position = "right"))),
    svg(pos_plot)
  )
})

test_that("legends of one variable only merge at the same position", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, color = factor(cyl), shape = factor(cyl))
  n_guides <- function(p) {
    length(vellumplot:::.facet_legend_guides(vellumplot:::.build_panels(p)))
  }
  expect_identical(n_guides(p), 1L)
  expect_identical(
    n_guides(p |> guides(color = guide_legend(position = "bottom"))),
    2L
  )
  merged <- p |>
    guides(
      color = guide_legend(position = "bottom"),
      shape = guide_legend(position = "bottom")
    )
  g <- vellumplot:::.facet_legend_guides(vellumplot:::.build_panels(merged))
  expect_length(g, 1L)
  expect_identical(vellumplot:::.guide_position(g[[1]]), "bottom")
})

test_that("colour bars, bins and steps honour position", {
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg, color = hp)
  lay <- pos_layout(p |> guides(color = guide_colourbar(position = "top")))
  expect_length(lay$legend_groups$top, 1L)
  lay <- pos_layout(
    p |> scale_color_binned() |> guides(color = guide_coloursteps(position = "left"))
  )
  expect_length(lay$legend_groups$left, 1L)
  lay <- pos_layout(
    vplot(mtcars) |>
      mark_point(x = wt, y = mpg, size = hp) |>
      scale_size_binned() |>
      guides(size = guide_bins(position = "bottom"))
  )
  expect_length(lay$legend_groups$bottom, 1L)
  expect_no_error(vellum::as_vellum_scene(
    p |> guides(color = guide_colourbar(position = "bottom"))
  ))
})

test_that("a guide position round-trips through the spec", {
  skip_if_not_installed("jsonlite")
  p <- pos_plot |>
    guides(
      color = guide_legend(position = "bottom"),
      size = guide_legend(position = "inside")
    )
  p2 <- spec_from_json(spec_to_json(p))
  expect_identical(
    vellumplot:::.scale_for(p2, "color")@guide$position,
    "bottom"
  )
  norm <- function(s) gsub("vl[0-9]+", "vl", s)
  svg <- function(p) vellum::scene_svg(vellum::as_vellum_scene(p))
  expect_identical(norm(svg(p2)), norm(svg(p)))
})
