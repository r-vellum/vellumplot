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

test_that("horizontal range marks dodge along y, their category axis", {
  # native positions after the draw-time step: each category's two groups sit
  # a quarter of the bar width either side of it; the value axis is untouched
  native <- function(p) {
    L <- vellumplot:::.resolve_layers(p)[[1]]
    sc <- vellumplot:::.build_panels(p)$scales
    vellumplot:::.position_native(L, sc)
  }
  for (mk in c(mark_errorbar, mark_linerange, mark_pointrange)) {
    p <- vplot(summ) |>
      mk(
        y = cat,
        x = m,
        xmin = m - se,
        xmax = m + se,
        color = g,
        orientation = "y",
        position = "dodge"
      )
    out <- native(p)
    expect_equal(out$L$values$y, c(0.775, 1.225, 1.775, 2.225))
    expect_equal(out$L$values$xmin, summ$m - summ$se)
    expect_equal(out$L$values$xmax, summ$m + summ$se)
    expect_identical(
      out$scales$x$map,
      vellumplot:::.build_panels(p)$scales$x$map
    )
  }
  # the drawn bars: four distinct y positions, and a cap narrowed to its slot
  p <- vplot(summ) |>
    mark_errorbar(
      y = cat,
      xmin = m - se,
      xmax = m + se,
      group = g,
      orientation = "y",
      position = "dodge",
      width = 0.3
    )
  el <- vellum::scene_model(as_vellum_scene(p))$elements
  el <- el[!is.na(el$id) & startsWith(el$id, "layer-1-errorbar"), ]
  bar <- el[el$h == 0, ]
  cap <- el[el$w == 0, ]
  expect_length(unique(round(bar$y, 3)), 4L)
  # with the box plot's dodge width, the same centres as a horizontal dodged
  # box plot
  raw <- data.frame(
    cat = rep(summ$cat, each = 5),
    g = rep(summ$g, each = 5),
    v = rep(summ$m, each = 5) + rep(-2:2, 4) * 0.1
  )
  bx <- vplot(raw) |>
    mark_boxplot(y = cat, x = v, fill = g, position = "dodge")
  q <- vplot(raw) |>
    mark_errorbar(
      y = cat,
      xmin = v - 1,
      xmax = v + 1,
      group = g,
      orientation = "y",
      position = position_dodge(width = 0.75)
    )
  dy <- function(p) {
    L <- vellumplot:::.resolve_layers(p)[[1]]
    sc <- vellumplot:::.build_panels(p)$scales
    list(L = L, sc = sc)
  }
  b <- dy(bx)
  cells <- vellumplot:::.by_category(b$L, b$sc, "y", span = 0.75)
  centres <- sort(unique(vapply(cells, function(cl) cl$centre, 1)))
  expect_equal(sort(unique(native(q)$L$values$y)), centres)
  # each cap fits inside its slot
  expect_true(all(cap$h < min(diff(sort(unique(bar$y))))))
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
    # the generic step moves x onto the bar slots: +/- a quarter of the bar
    # width (90% of the band) for 2 groups
    expect_equal(
      pn$L$values$x,
      sc$x$map(summ$cat) + c(-1, 1, -1, 1) * 0.9 * band / 4
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

# --- dodge by the groups present at each x (VP-56) ---------------------------

# Data-space centres and widths of layer 1's `mark` elements: a reference point
# layer at the three cyl categories turns device x into category units.
dodge_geom <- function(p, mark) {
  p <- p |>
    mark_point(x = cyl, y = 0, data = data.frame(cyl = factor(c(4, 6, 8))))
  el <- vellum::scene_model(as_vellum_scene(p))$elements
  el <- el[!is.na(el$id), ]
  ref <- sort(unique(el$x[startsWith(el$id, "layer-2")]))
  s <- ref[2] - ref[1]
  g <- el[startsWith(el$id, "layer-1") & el$mark == mark, ]
  out <- unique(round(data.frame(x = 1 + (g$x - ref[1]) / s, w = g$w / s), 4))
  out[order(out$x), ]
}

dodge_cars <- function() {
  d <- mtcars
  d$cyl <- factor(d$cyl)
  d$gear <- factor(d$gear)
  d
}

test_that("dodged bars split the width between the groups present at an x", {
  d <- dodge_cars()
  # cyl = 8 has no gear-4 cars: two wider bars, centred (ggplot2's layer_data)
  g <- dodge_geom(
    vplot(d) |> mark_bar(x = cyl, fill = gear, position = "dodge"),
    "rect"
  )
  expect_equal(g$x, c(0.7, 1, 1.3, 1.7, 2, 2.3, 2.775, 3.225))
  expect_equal(g$w, c(rep(0.3, 6), 0.45, 0.45))
  # preserve = "single": every bar one group wide, packed from the left
  g <- dodge_geom(
    vplot(d) |>
      mark_bar(
        x = cyl,
        fill = gear,
        position = position_dodge(preserve = "single")
      ),
    "rect"
  )
  expect_equal(g$x, c(0.7, 1, 1.3, 1.7, 2, 2.3, 2.7, 3))
  expect_equal(g$w, rep(0.3, 8))
})

test_that("dodged boxes and points follow the groups present, as ggplot2", {
  d <- dodge_cars()
  g <- dodge_geom(
    vplot(d) |> mark_boxplot(x = cyl, y = mpg, fill = gear, position = "dodge"),
    "rect"
  )
  expect_equal(g$x, c(0.75, 1, 1.25, 1.75, 2, 2.25, 2.8125, 3.1875))
  expect_equal(g$w, c(rep(0.25, 6), 0.375, 0.375))
  g <- dodge_geom(
    vplot(d) |>
      mark_point(
        x = cyl,
        y = mpg,
        color = gear,
        position = position_dodge(width = 0.75)
      ),
    "point"
  )
  expect_equal(
    sort(unique(g$x)),
    c(0.75, 1, 1.25, 1.75, 2, 2.25, 2.8125, 3.1875)
  )
  g <- dodge_geom(
    vplot(d) |>
      mark_point(
        x = cyl,
        y = mpg,
        color = gear,
        position = position_jitterdodge(jitter.width = 0)
      ),
    "point"
  )
  expect_equal(
    sort(unique(g$x)),
    c(0.75, 1, 1.25, 1.75, 2, 2.25, 2.8125, 3.1875)
  )
})

test_that("dodge geometry matches ggplot2's layer_data()", {
  skip_if_not_installed("ggplot2")
  d <- dodge_cars()
  gg <- function(p) {
    ld <- ggplot2::layer_data(p)
    out <- unique(round(data.frame(x = ld$x, w = ld$xmax - ld$xmin), 4))
    out[order(out$x), ]
  }
  aes <- ggplot2::aes
  for (pres in c("total", "single")) {
    v <- dodge_geom(
      vplot(d) |>
        mark_bar(x = cyl, fill = gear, position = position_dodge(preserve = pres)),
      "rect"
    )
    g <- gg(
      ggplot2::ggplot(d, aes(cyl, fill = gear)) +
        ggplot2::geom_bar(position = ggplot2::position_dodge(preserve = pres))
    )
    expect_equal(v, g, ignore_attr = TRUE, info = pres)
    v <- dodge_geom(
      vplot(d) |>
        mark_boxplot(
          x = cyl,
          y = mpg,
          fill = gear,
          position = position_dodge(preserve = pres)
        ),
      "rect"
    )
    g <- gg(
      ggplot2::ggplot(d, aes(cyl, mpg, fill = gear)) +
        ggplot2::geom_boxplot(
          position = ggplot2::position_dodge(preserve = pres)
        )
    )
    expect_equal(v, g, ignore_attr = TRUE, info = pres)
  }
  v <- dodge_geom(
    vplot(d) |> mark_bar(x = cyl, fill = gear, position = "dodge2"),
    "rect"
  )
  g <- gg(
    ggplot2::ggplot(d, aes(cyl, fill = gear)) +
      ggplot2::geom_bar(position = "dodge2")
  )
  expect_equal(v, g, ignore_attr = TRUE)
})

test_that("position_dodge() checks `preserve`", {
  expect_identical(position_dodge()$dodge_preserve, "total")
  expect_error(position_dodge(preserve = "all"), "preserve")
})

# VP-63: ggplot2 counts preserve = "single" groups over the whole layer.
# Native (data-unit) xmin / xmax of a faceted plot's rects, per panel.
facet_rects <- function(p) {
  m <- vellum::scene_model(as_vellum_scene(p))
  e <- m$elements
  e <- e[!is.na(e$id) & startsWith(e$id, "layer-1") & e$mark == "rect", ]
  do.call(rbind, lapply(split(e, e$panel), function(r) {
    pn <- m$panels[m$panels$name == r$panel[1], ]
    f <- function(px) {
      pn$xscale_lo + (px - pn$x0) / (pn$x1 - pn$x0) * (pn$xscale_hi - pn$xscale_lo)
    }
    data.frame(panel = r$panel, xmin = round(f(r$x0), 4), xmax = round(f(r$x1), 4))
  }))
}

test_that("preserve = 'single' sizes slots from the fullest x of every panel", {
  d <- dodge_cars()
  d$vs <- factor(d$vs)
  p <- vplot(d) |>
    mark_bar(x = cyl, fill = gear, position = position_dodge(preserve = "single")) |>
    facet_wrap(~vs)
  r <- facet_rects(p)
  # vs = 1 has three gears at cyl 4, so every bar in both panels is 0.9 / 3
  expect_equal(unique(round(r$xmax - r$xmin, 4)), 0.3)
  # panel 1 (vs = 0): cyl 6 holds gears 4 and 5, packed from the left
  p1 <- r[r$panel == "panel-1-1", ]
  expect_equal(sort(p1$xmin), c(0.55, 1.55, 1.85, 2.55, 2.85))
  skip_if_not_installed("ggplot2")
  ld <- ggplot2::layer_data(
    ggplot2::ggplot(d, ggplot2::aes(cyl, fill = gear)) +
      ggplot2::geom_bar(position = ggplot2::position_dodge(preserve = "single")) +
      ggplot2::facet_wrap(~vs)
  )
  g <- ld[order(ld$PANEL, ld$xmin), c("xmin", "xmax")]
  v <- r[order(r$panel, r$xmin), c("xmin", "xmax")]
  expect_equal(v, g, ignore_attr = TRUE)
})

test_that("jitterdodge jitters 0.4 * resolution / (groups + 2), not vertically", {
  set.seed(3)
  n <- 600
  d <- data.frame(
    x = factor(sample(c("a", "b", "c"), n, TRUE)),
    g = factor(sample(c("p", "q", "r"), n, TRUE)),
    y = round(runif(n) * 10)
  )
  p <- vplot(d) |> mark_point(x = x, y = y, color = g, position = "jitterdodge")
  m <- vellum::scene_model(as_vellum_scene(p))
  e <- m$elements
  e <- e[!is.na(e$id) & startsWith(e$id, "layer-1") & e$mark == "point", ]
  pn <- m$panels[m$panels$name == "panel-1-1", ]
  fx <- function(px) {
    pn$xscale_lo + (px - pn$x0) / (pn$x1 - pn$x0) * (pn$xscale_hi - pn$xscale_lo)
  }
  # distance from the nearest dodged slot centre (3 slots of 0.25 per x)
  x <- fx(e$x)
  base <- round(x)
  centre <- base + (floor((x - base + 0.375) / 0.25) + 0.5) * 0.25 - 0.375
  amp <- max(abs(x - centre))
  expect_lte(amp, 0.4 / 5 + 1e-6)
  expect_gt(amp, 0.4 / 5 * 0.9)
  # no vertical jitter: every point keeps its data row's height
  expect_identical(length(unique(round(e$y, 3))), length(unique(d$y)))
})
