# Pattern (hatch) fills: pattern_*() builders + the constant paint-fill path.

test_that("pattern builders return a vellum_pattern", {
  expect_s3_class(pattern_stripe(), "vellum_pattern")
  expect_s3_class(pattern_crosshatch(), "vellum_pattern")
  expect_s3_class(pattern_grid(), "vellum_pattern")
  expect_s3_class(pattern_dot(), "vellum_pattern")
  expect_s3_class(pattern_checker(), "vellum_pattern")
})

test_that("stripe angle is restricted to the seamless orientations", {
  for (a in c(0, 45, 90, 135, 225)) {
    # 225 %% 180 == 45, so it is accepted (normalised)
    expect_s3_class(pattern_stripe(angle = a), "vellum_pattern")
  }
  expect_error(pattern_stripe(angle = 30), "must be one of")
  expect_error(pattern_crosshatch(angle = 10), "must be one of")
})

test_that("a pattern fill is captured as a constant paint value, not a channel", {
  p <- vplot(data.frame(g = c("a", "b"), n = c(3, 5))) |>
    mark_bar(x = g, y = n, fill = pattern_stripe())
  L <- p@layers[[1]]
  expect_s3_class(L@params$fill, "vellum_pattern") # a param, not a mapped channel
  expect_false("fill" %in% names(L@encoding))
  expect_true(vellumplot:::.is_paint(L@params$fill))
})

test_that("pattern fills render across the filled marks", {
  set.seed(1)
  df <- data.frame(
    g = rep(c("a", "b", "c"), each = 30),
    y = rnorm(90),
    x = runif(90)
  )
  bars <- data.frame(g = c("a", "b", "c"), n = c(5, 3, 7))
  plots <- list(
    vplot(bars) |> mark_bar(x = g, y = n, fill = pattern_stripe()),
    vplot(expand.grid(a = 1:3, b = 1:3)) |>
      mark_tile(x = a, y = b, fill = pattern_dot()),
    vplot(df) |> mark_boxplot(x = g, y = y, fill = pattern_crosshatch()),
    vplot(df) |> mark_violin(x = g, y = y, fill = pattern_stripe(angle = 90)),
    vplot(df) |>
      mark_point(x = x, y = y) |>
      mark_hull(x = x, y = y, fill = pattern_checker())
  )
  for (p in plots) {
    f <- local_tempfile(fileext = ".png")
    expect_no_error(render_plot(p, f))
    expect_gt(file.info(f)$size, 0)
  }
})

test_that("a paint fill on an unsupported mark is a clear error", {
  p <- vplot(data.frame(x = 1:3, y = 1:3)) |>
    mark_point(x = x, y = y, fill = pattern_stripe())
  expect_error(
    vellum::as_vellum_scene(p),
    "not supported for the .*point"
  )
})

test_that("a pattern fill embeds a real tiling pattern in PDF", {
  p <- vplot(data.frame(g = c("a", "b"), n = c(3, 5))) |>
    mark_bar(x = g, y = n, fill = pattern_stripe())
  f <- local_tempfile(fileext = ".pdf")
  render_plot(p, f)
  raw <- readBin(f, "raw", file.info(f)$size)
  # a real tiling pattern object in the PDF, not a flat-colour degrade
  expect_gt(length(grepRaw("/Pattern", raw, fixed = TRUE)), 0L)
})

test_that("gradient fills still work (no paint-path regression)", {
  p <- vplot(data.frame(x = 1:10, y = cumsum(runif(10)))) |>
    mark_area(
      x = x,
      y = y,
      fill = linear_gradient(
        c("#00e5ff", "#00e5ff00"),
        x1 = 0,
        y1 = 1,
        x2 = 0,
        y2 = 0
      )
    )
  f <- local_tempfile(fileext = ".png")
  expect_no_error(render_plot(p, f))
})

# --- mapped pattern aesthetic + scale_pattern() -----------------------------

test_that("mapping the pattern aesthetic trains a discrete pattern scale", {
  bars <- data.frame(g = c("a", "b", "c"), n = c(3, 5, 2))
  sc <- vellumplot:::.train_pattern(
    vplot(bars) |> mark_bar(x = g, y = n, pattern = g),
    list(list(mark = "bar", values = list(pattern = bars$g)))
  )
  expect_identical(sc$kind, "pattern")
  expect_setequal(sc$levels, c("a", "b", "c"))
  expect_length(sc$patterns, 3L)
  expect_s3_class(sc$patterns[[1]], "vellum_pattern")
})

test_that("scale_pattern(values=) accepts builder names and pattern objects", {
  expect_length(vellumplot:::.resolve_pattern_values(c("stripe", "dot")), 2L)
  objs <- vellumplot:::.resolve_pattern_values(list(
    pattern_stripe(),
    pattern_dot()
  ))
  expect_s3_class(objs[[2]], "vellum_pattern")
  expect_error(
    vellumplot:::.resolve_pattern_values("nope"),
    "Unknown pattern"
  )
})

test_that("too many levels for the palette is a clear error", {
  df <- data.frame(g = letters[1:9], n = 1:9)
  expect_error(
    render_plot(
      vplot(df) |>
        mark_bar(x = g, y = n, pattern = g) |>
        scale_pattern(values = c("stripe", "dot")),
      local_tempfile(fileext = ".png")
    ),
    "Not enough patterns"
  )
})

test_that("a mapped pattern renders (bar + boxplot) with a legend", {
  bars <- data.frame(g = c("a", "b", "c", "d"), n = c(4, 7, 5, 6))
  p1 <- vplot(bars) |> mark_bar(x = g, y = n, pattern = g)
  f1 <- local_tempfile(fileext = ".svg")
  render_plot(p1, f1)
  svg <- paste(readLines(f1, warn = FALSE), collapse = "\n")
  # one <pattern> per bar plus the legend swatches -> several tiling patterns
  expect_gt(lengths(regmatches(svg, gregexpr("<pattern", svg))), 1L)

  set.seed(1)
  df <- data.frame(grp = rep(c("a", "b", "c"), each = 30), y = rnorm(90))
  p2 <- vplot(df) |>
    mark_boxplot(x = grp, y = y, pattern = grp) |>
    scale_pattern(values = c("stripe", "dot", "crosshatch"), name = "grp")
  expect_no_error(render_plot(p2, local_tempfile(fileext = ".png")))
})
