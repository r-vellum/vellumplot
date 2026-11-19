# VP-46: on the filled marks a `color` given alongside a `fill` outlines the
# shapes (ggplot2's semantics), while a lone `color` still fills them.

fo_svg <- function(p) paste(plot_svg(p), collapse = "")

# The fill / stroke hex colours used inside the first layer's mark group(s).
fo_layer_paint <- function(p) {
  s <- fo_svg(p)
  # the layer groups (`data-vellum-id="layer-1-..."`) up to the panel close
  parts <- strsplit(s, 'data-vellum-id="layer-', fixed = TRUE)[[1]][-1]
  body <- paste(sub("</g></g>.*", "", parts), collapse = " ")
  grab <- function(attr) {
    m <- regmatches(
      body,
      gregexpr(paste0(attr, '="#[0-9A-Fa-f]{6}"'), body)
    )[[1]]
    unique(tolower(gsub(paste0(attr, '="|"'), "", m)))
  }
  list(fill = grab("fill"), stroke = grab("stroke"))
}

set.seed(46)
fo_d <- data.frame(
  x = rnorm(120),
  y = rnorm(120),
  g = rep(c("a", "b"), 60)
)
fo_cnt <- data.frame(k = c("a", "b", "c"), n = c(3, 5, 2))
fo_ts <- data.frame(x = 1:8, y = c(3, 4, 2, 5, 6, 4, 5, 7))
fo_ts$lo <- fo_ts$y - 1
fo_ts$hi <- fo_ts$y + 1
fo_tl <- expand.grid(x = 1:3, y = 1:3)
fo_cb <- data.frame(x = c("a", "b"), y = c(2, 3), lo = c(1, 2), hi = c(3, 4))

fo_cases <- list(
  bar = function(...) vplot(fo_cnt) |> mark_bar(x = k, y = n, ...),
  histogram = function(...) vplot(fo_d) |> mark_histogram(x = x, ...),
  area = function(...) vplot(fo_ts) |> mark_area(x = x, y = y, ...),
  ribbon = function(...) {
    vplot(fo_ts) |> mark_ribbon(x = x, ymin = lo, ymax = hi, ...)
  },
  density = function(...) vplot(fo_d) |> mark_density(x = x, ...),
  tile = function(...) vplot(fo_tl) |> mark_tile(x = x, y = y, ...),
  hex = function(...) vplot(fo_d) |> mark_hex(x = x, y = y, bins = 5, ...),
  bin2d = function(...) vplot(fo_d) |> mark_bin2d(x = x, y = y, bins = 5, ...),
  crossbar = function(...) {
    vplot(fo_cb) |> mark_crossbar(x = x, y = y, ymin = lo, ymax = hi, ...)
  },
  dotplot = function(...) vplot(fo_d[1:30, ]) |> mark_dotplot(x = x, ...),
  pie = function(...) vplot(fo_cnt) |> mark_pie(value = n, ...),
  donut = function(...) vplot(fo_cnt) |> mark_donut(value = n, ...),
  boxplot = function(...) vplot(fo_d) |> mark_boxplot(x = g, y = y, ...),
  violin = function(...) vplot(fo_d) |> mark_violin(x = g, y = y, ...),
  ridgeline = function(...) vplot(fo_d) |> mark_ridgeline(x = x, y = g, ...),
  polygon = function(...) {
    vplot(data.frame(x = c(0, 1, 1), y = c(0, 0, 1))) |>
      mark_polygon(x = x, y = y, ...)
  },
  rect = function(...) {
    vplot(fo_cb) |> mark_rect(xmin = lo, xmax = hi, ymin = 0, ymax = y, ...)
  }
)

test_that("a color given with a fill outlines every filled mark (VP-46)", {
  for (nm in names(fo_cases)) {
    pt <- fo_layer_paint(fo_cases[[nm]](fill = "#00ff00", color = "#ff0000"))
    expect_true("#00ff00" %in% pt$fill, info = paste(nm, "fills with fill"))
    expect_true("#ff0000" %in% pt$stroke, info = paste(nm, "strokes color"))
    expect_false("#00ff00" %in% pt$stroke, info = paste(nm, "fill not stroke"))
  }
})

test_that("the histogram of the bug report draws filled, white-edged bars", {
  pt <- fo_layer_paint(
    vplot(fo_d) |> mark_histogram(x = x, fill = "#3182bd", color = "white")
  )
  expect_identical(pt$fill, "#3182bd")
  expect_identical(pt$stroke, "#ffffff")
})

test_that("a lone color still fills the filled marks (back-compat)", {
  # the marks where colour has always filled (not the outline-only box plot,
  # violin and ridgeline, nor hex / bin2d whose fill defaults to the count)
  keep <- c(
    "bar",
    "histogram",
    "area",
    "ribbon",
    "density",
    "tile",
    "dotplot",
    "pie",
    "donut",
    "polygon",
    "rect"
  )
  for (nm in keep) {
    pt <- fo_layer_paint(fo_cases[[nm]](color = "#ff0000"))
    expect_true("#ff0000" %in% pt$fill, info = paste(nm, "fills with color"))
  }
  # no outline is added to a bar given only a colour
  pt <- fo_layer_paint(fo_cases$bar(color = "#ff0000"))
  expect_length(pt$stroke, 0L)
})

test_that("a lone fill draws no outline on bars, areas and tiles", {
  for (nm in c("bar", "area", "tile", "pie")) {
    pt <- fo_layer_paint(fo_cases[[nm]](fill = "#00ff00"))
    expect_identical(pt$fill, "#00ff00", info = nm)
    expect_length(pt$stroke, 0L)
  }
})

test_that("an area outlines its top only, a ribbon both edges", {
  count_lines <- function(p) {
    s <- fo_svg(p)
    lengths(regmatches(s, gregexpr('fill="none" stroke="#ff0000"', s)))
  }
  expect_identical(
    count_lines(fo_cases$area(fill = "#00ff00", color = "#ff0000")),
    1L
  )
  expect_identical(
    count_lines(fo_cases$ribbon(fill = "#00ff00", color = "#ff0000")),
    2L
  )
})

test_that("a mapped fill with a constant color outlines each bar", {
  pt <- fo_layer_paint(
    vplot(fo_cnt) |> mark_bar(x = k, y = n, fill = k, color = "#ff0000")
  )
  expect_length(setdiff(pt$fill, "#ff0000"), 3L)
  expect_identical(pt$stroke, "#ff0000")
})

test_that("a mapped color with a constant fill outlines in the mapped colour", {
  pt <- fo_layer_paint(
    vplot(fo_cnt) |> mark_bar(x = k, y = n, color = k, fill = "#00ff00")
  )
  expect_identical(pt$fill, "#00ff00")
  expect_length(pt$stroke, 3L)
})

test_that("a mapped fill and a mapped color stroke and fill separately", {
  d <- data.frame(k = c("a", "b"), n = c(1, 2), h = c("x", "x"))
  p <- vplot(d) |> mark_bar(x = k, y = n, fill = k, color = h)
  pt <- fo_layer_paint(p)
  # the bars are filled by k's two colours, stroked by h's one colour -- from
  # separate scales, so h's single level takes the palette's first colour
  expect_length(pt$stroke, 1L)
  expect_length(pt$fill, 2L)
  expect_identical(pt$stroke, pt$fill[[1]])
})

test_that("legend keys show the constant outline / fill of a filled mark", {
  legend_paths <- function(p) {
    s <- fo_svg(p)
    leg <- sub('.*data-vellum-panel="legend', "", s)
    regmatches(leg, gregexpr("<path[^>]*/>", leg))[[1]]
  }
  # mapped fill + constant colour: filled swatches with a white edge
  k1 <- legend_paths(
    vplot(fo_cnt) |> mark_bar(x = k, y = n, fill = k, color = "white")
  )
  expect_identical(sum(grepl('stroke="#ffffff"', k1)), 3L)
  # mapped colour + constant fill: white boxes outlined in the mapped colour
  k2 <- legend_paths(
    vplot(fo_cnt) |> mark_bar(x = k, y = n, color = k, fill = "white")
  )
  expect_identical(sum(grepl('^<path[^>]*fill="#ffffff"', k2)), 3L)
  expect_identical(sum(grepl('stroke="#', k2)), 3L)
  # a lone mapped colour still draws plain filled squares
  k3 <- legend_paths(vplot(fo_cnt) |> mark_bar(x = k, y = n, color = k))
  expect_false(any(grepl("stroke=", k3)))
})

test_that("key style resolves from the first filled layer", {
  rl <- function(p) vellumplot:::.resolve_layers(p)
  ks <- vellumplot:::.key_style_for_aes
  expect_identical(
    ks(rl(vplot(fo_cnt) |> mark_bar(x = k, y = n, fill = k, color = "red")),
      "fill"),
    list(col = "red")
  )
  expect_identical(
    ks(rl(vplot(fo_cnt) |> mark_bar(x = k, y = n, color = k, fill = "red")),
      "color"),
    list(fill = "red")
  )
  expect_null(ks(rl(vplot(fo_cnt) |> mark_bar(x = k, y = n, fill = k)), "fill"))
  expect_null(
    ks(rl(vplot(fo_cnt) |> mark_point(x = k, y = n, color = k, fill = "red")),
      "color")
  )
})
