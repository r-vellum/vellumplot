# Single-channel plots (only `color`, or only `fill`) must render exactly as
# they did when colour and fill shared one scale (VP-48). The signature keeps
# every paint attribute, text node and element tag of the SVG, in order, but no
# coordinates. The snapshot was recorded on macOS before VP-48; the SVG still
# differs in detail between platforms (and vellum builds), so it is checked
# locally only.
svg_signature <- function(plot) {
  svg <- plot_svg(plot)
  # page-unique ids differ per compile; geometry differs per platform
  svg <- gsub('\\s(d|x|y|x1|x2|y1|y2|cx|cy|r|rx|ry|width|height|points|transform|viewBox|font-size|textLength|dx|dy|id|clip-path|aria-labelledby|href|xlink:href|stroke-width)="[^"]*"', "", svg)
  svg <- gsub("url\\(#[^)]*\\)", "url()", svg)
  rlang::hash(svg)
}

test_that("single-channel colour / fill plots are unchanged by VP-48", {
  skip_on_ci()
  skip_on_os(c("windows", "linux", "solaris"))
  df <- data.frame(
    x = rep(c("a", "b", "c"), each = 4),
    y = c(3, 5, 2, 6, 4, 7, 5, 3, 8, 6, 4, 5),
    g = rep(c("p", "q"), 6),
    z = seq(0.5, 6, by = 0.5)
  )
  tiles <- expand.grid(a = 1:6, b = 1:5)
  tiles$v <- tiles$a * tiles$b
  plots <- list(
    bar_fill = vplot(mtcars) |>
      mark_bar(x = factor(cyl), fill = factor(gear)),
    bar_fill_outline = vplot(mtcars) |>
      mark_bar(x = factor(cyl), fill = factor(gear), color = "black"),
    point_colour_shape = vplot(mtcars) |>
      mark_point(x = wt, y = mpg, color = factor(cyl), shape = factor(cyl)),
    point_colour_cont = vplot(mtcars) |>
      mark_point(x = wt, y = mpg, color = hp),
    tile_fill_cont = vplot(tiles) |> mark_tile(x = a, y = b, fill = v),
    tile_viridis = vplot(tiles) |>
      mark_tile(x = a, y = b, fill = v) |>
      scale_fill_viridis_c(),
    tile_binned = vplot(tiles) |>
      mark_tile(x = a, y = b, fill = v) |>
      scale_fill_binned(style = "quantile", n = 5),
    box_fill = vplot(df) |> mark_boxplot(x = x, y = y, fill = x),
    box_colour = vplot(df) |> mark_boxplot(x = x, y = y, color = x),
    bar_brewer = vplot(df) |>
      mark_bar(x = x, y = y, fill = g, stat = "identity") |>
      scale_fill_brewer(palette = "Set2"),
    density_fill = vplot(df) |>
      mark_density(x = y, fill = g, alpha = 0.5),
    labs_fill = vplot(df) |>
      mark_bar(x = x, y = y, fill = g, stat = "identity") |>
      labs(fill = "Group"),
    labs_colour = vplot(df) |>
      mark_line(x = z, y = y, color = g) |>
      labs(color = "Group"),
    facet_fill = vplot(mtcars) |>
      mark_bar(x = factor(gear), fill = factor(am)) |>
      facet_wrap(~cyl),
    marginal_group = vplot(mtcars) |>
      mark_point(x = wt, y = mpg, color = factor(cyl)) |>
      add_marginal(group = TRUE),
    interactive_fill = vplot(df) |>
      mark_bar(x = x, y = y, fill = g, stat = "identity", data_id = g),
    interactive_cont_fill = vplot(tiles) |>
      mark_tile(x = a, y = b, fill = v, data_id = v)
  )
  sigs <- vapply(plots, svg_signature, character(1))
  expect_snapshot(sigs)
})
