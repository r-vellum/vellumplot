# Positions on every mark: the shared dodge math, the generic draw-time step,
# the unsupported-position error, and grouped distribution marks.

# Element centres (device x) of one layer's grobs, from the scene model.
layer_x <- function(p, prefix, mark) {
  el <- vellum::scene_model(as_vellum_scene(p))$elements
  el <- el[!is.na(el$id) & startsWith(el$id, prefix) & el$mark == mark, ]
  round(el$x, 3)
}

prov_mark <- function(p, mark) {
  pr <- attr(as_vellum_scene(p), "vellumplot_provenance")
  Filter(function(e) identical(e$mark, mark), pr)
}

summ <- data.frame(
  cat = rep(c("a", "b"), each = 2),
  g = rep(c("x", "y"), 2),
  m = c(3, 5, 4, 2),
  se = c(0.5, 0.4, 0.6, 0.3)
)

test_that("dodged error bars sit over dodged bars", {
  p <- vplot(summ) |>
    mark_bar(x = cat, y = m, fill = g, position = "dodge") |>
    mark_errorbar(
      x = cat,
      ymin = m - se,
      ymax = m + se,
      group = g,
      position = "dodge",
      width = 0.3
    )
  bars <- layer_x(p, "layer-1-bar", "rect")
  bars_x <- sort(unique(bars))
  err <- sort(unique(layer_x(p, "layer-2-errorbar", "segment")))
  expect_length(bars_x, 4L)
  expect_equal(err, bars_x)
  # the same for the other range marks
  for (mk in c(mark_linerange, mark_pointrange)) {
    q <- vplot(summ) |>
      mark_bar(x = cat, y = m, fill = g, position = "dodge") |>
      mk(
        x = cat,
        y = m,
        ymin = m - se,
        ymax = m + se,
        color = g,
        position = "dodge"
      )
    el <- vellum::scene_model(as_vellum_scene(q))$elements
    el <- el[!is.na(el$id) & startsWith(el$id, "layer-2-"), ]
    expect_true(all(round(el$x, 3) %in% bars_x))
  }
})

test_that("text stacks onto stacked bars, at position_stack(vjust =)", {
  base <- vplot(summ) |> mark_bar(x = cat, y = m, fill = g)
  top <- vellumplot:::.resolve_layers(
    base |> mark_text(x = cat, y = m, label = m, group = g, position = "stack")
  )[[2]]
  # ggplot2 order: the last group (y) at the bottom, the first (x) on top
  expect_equal(top$values$y, c(8, 5, 6, 2))
  mid <- vellumplot:::.resolve_layers(
    base |>
      mark_text(
        x = cat,
        y = m,
        label = m,
        group = g,
        position = position_stack(vjust = 0.5)
      )
  )[[2]]
  expect_equal(mid$values$y, c(6.5, 2.5, 4, 1))
  # fill normalises the stack to 1, like the bars
  fl <- vellumplot:::.resolve_layers(
    vplot(summ) |>
      mark_text(x = cat, y = m, label = m, fill = g, position = "fill")
  )[[1]]
  expect_equal(fl$values$y, c(1, 5 / 8, 1, 2 / 6))
})

test_that("text and labels dodge with the bars they annotate", {
  for (mk in c(mark_text, mark_label)) {
    p <- vplot(summ) |>
      mark_bar(x = cat, y = m, fill = g, position = "dodge") |>
      mk(x = cat, y = m, label = m, group = g, position = "dodge")
    r <- vellumplot:::.resolve_layers(p)
    sc <- vellumplot:::.train_scales(p, r)
    pn <- vellumplot:::.position_native(r[[2]], sc)
    band <- sc$x$band_width
    # the generic step moves x onto the bar slots: +/- a quarter band for 2 groups
    expect_equal(
      pn$L$values$x,
      sc$x$map(summ$cat) + c(-1, 1, -1, 1) * band / 4
    )
    # and hands the emitter an identity x map over the shifted positions
    expect_equal(pn$scales$x$map(pn$L$values$x), pn$L$values$x)
  }
})

test_that("every mark constructor takes `position` as a formal argument", {
  ns <- asNamespace("vellumplot")
  marks <- grep("^mark_", getNamespaceExports("vellumplot"), value = TRUE)
  # decorations use `position` for a panel corner; composites forward `...`
  marks <- setdiff(
    marks,
    c(
      "mark_scalebar",
      "mark_compass",
      "mark_raincloud",
      "mark_pie",
      "mark_donut",
      "mark_chord",
      "mark_hierarchy",
      "mark_sankey"
    )
  )
  has <- vapply(
    marks,
    function(f) "position" %in% names(formals(get(f, ns))),
    logical(1)
  )
  expect_true(all(has), info = paste(marks[!has], collapse = ", "))
})

test_that("a position a mark does not support is an error naming the mark", {
  expect_error(
    vplot(summ) |> mark_line(x = cat, y = m, position = "dodge"),
    class = "rlang_error",
    regexp = "mark_line.*does not support.*dodge"
  )
  expect_snapshot(
    error = TRUE,
    vplot(summ) |>
      mark_errorbar(x = cat, ymin = m, ymax = m, position = "stack")
  )
  expect_snapshot(
    error = TRUE,
    vplot(summ) |> mark_text(x = cat, y = m, label = m, position = "dodgy")
  )
  # identity and nudge are accepted everywhere
  expect_no_error(vplot(summ) |> mark_rule(y = m, position = "identity"))
  expect_no_error(
    vplot(summ) |>
      mark_segment(x = 1, y = m, xend = 2, yend = m, position = "nudge")
  )
})

test_that("grouped box plots draw one dodged box per sub-group", {
  p <- vplot(mtcars) |>
    mark_boxplot(x = factor(cyl), y = mpg, fill = factor(am))
  boxes <- layer_x(p, "layer-1-boxplot", "rect")
  expect_length(boxes, 6L)
  # each box keys exactly its (cyl, am) cell's rows
  pr <- prov_mark(p, "boxplot")
  cells <- split(
    seq_len(nrow(mtcars)),
    list(mtcars$am, mtcars$cyl),
    drop = TRUE
  )
  keyed <- unique(lapply(pr, function(e) sort(e$rows)))
  expect_true(all(vapply(
    cells,
    function(r) any(vapply(keyed, identical, logical(1), sort(r))),
    logical(1)
  )))
  # violins and slab-intervals split the same way
  pv <- vplot(mtcars) |>
    mark_violin(x = factor(cyl), y = mpg, fill = factor(am))
  expect_length(prov_mark(pv, "violin"), 6L)
  # a ridgeline overlays its sub-groups on the shared baseline
  pr2 <- vplot(mtcars) |>
    mark_ridgeline(x = mpg, y = factor(cyl), fill = factor(am))
  expect_length(prov_mark(pr2, "ridgeline"), 6L)
})

test_that("box plots use ggplot2 colour semantics: color strokes, fill fills", {
  paint <- function(p) {
    r <- vellumplot:::.resolve_layers(p)
    sc <- vellumplot:::.train_scales(p, r)
    vellumplot:::.dist_paint(r[[1]], sc, "white")
  }
  col_only <- paint(
    vplot(mtcars) |> mark_boxplot(x = factor(cyl), y = mpg, color = factor(am))
  )
  expect_true(all(col_only$fill == "white"))
  expect_length(unique(col_only$col), 2L)
  fill_only <- paint(
    vplot(mtcars) |> mark_boxplot(x = factor(cyl), y = mpg, fill = factor(am))
  )
  expect_length(unique(fill_only$fill), 2L)
  expect_true(all(fill_only$col == "grey20"))
  # a colour-only box plot's legend key is an outlined box
  r <- vellumplot:::.resolve_layers(
    vplot(mtcars) |> mark_boxplot(x = factor(cyl), y = mpg, color = factor(am))
  )
  expect_identical(vellumplot:::.key_glyph_for_aes(r, "color"), "outline")
})
