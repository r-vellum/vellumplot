# Hand-drawn ("sketch") rendering: the sketch() constructor, per-mark / element /
# plot-wide (theme_sketch) attachment, resolution order, and rendering.

# --- constructor + validation -----------------------------------------------

test_that("sketch() re-exports a vellum_sketch with tunable knobs", {
  s <- sketch(roughness = 1.4, fill_style = "crosshatch", seed = 7)
  expect_s3_class(s, "vellum_sketch")
  expect_identical(s$fill_style, "crosshatch")
  expect_equal(s$roughness, 1.4)
  expect_equal(s$seed, 7)
})

test_that(".check_sketch normalises NULL / NA / FALSE / object", {
  expect_null(vellumplot:::.check_sketch(NULL))
  expect_true(is.na(vellumplot:::.check_sketch(NA)))
  expect_true(is.na(vellumplot:::.check_sketch(FALSE)))
  s <- sketch()
  expect_identical(vellumplot:::.check_sketch(s), s)
  expect_error(vellumplot:::.check_sketch("nope"), "sketch")
  expect_error(vellumplot:::.check_sketch(1), "sketch")
})

# --- per-mark attachment -----------------------------------------------------

test_that("sketch = rides a geometry layer", {
  s <- sketch(roughness = 2)
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg, sketch = s)
  expect_identical(p@layers[[1]]@sketch, s)

  # NA / FALSE -> forced-crisp marker stored on the layer
  p2 <- vplot(mtcars) |> mark_line(x = wt, y = mpg, sketch = NA)
  expect_true(is.na(p2@layers[[1]]@sketch))

  # default: NULL (inherit)
  p3 <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
  expect_null(p3@layers[[1]]@sketch)
})

test_that("sketch on an unsupported mark (via ...) errors", {
  expect_error(
    vplot(mtcars) |> mark_text(x = wt, y = mpg, label = "a", sketch = sketch()),
    "not an aesthetic"
  )
})

# --- element slot ------------------------------------------------------------

test_that("element_line / element_rect carry a validated sketch slot", {
  el <- element_line(sketch = sketch(roughness = 0.5))
  expect_s3_class(el@sketch, "vellum_sketch")
  er <- element_rect(sketch = NA)
  expect_true(is.na(er@sketch))
  expect_null(element_line()@sketch)
  expect_error(element_rect(sketch = "x"), "sketch")
})

test_that(".el_sketch resolves an element to a sketch-or-NULL", {
  expect_null(vellumplot:::.el_sketch(element_line()))
  expect_null(vellumplot:::.el_sketch(element_line(sketch = NA)))
  expect_null(vellumplot:::.el_sketch(element_blank()))
  s <- vellumplot:::.el_sketch(element_line(sketch = sketch(seed = 3)), offset = 5L)
  expect_s3_class(s, "vellum_sketch")
  expect_equal(s$seed, 8) # 3 + offset 5
})

# --- theme_sketch() ----------------------------------------------------------

test_that("theme_sketch() sets a plot-wide default + hand-drawn elements", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    theme_sketch(roughness = 1.2, seed = 4)
  expect_s3_class(vellumplot:::.theme_sketch_default(p), "vellum_sketch")
  expect_equal(vellumplot:::.theme_sketch_default(p)$roughness, 1.2)
  # gridlines / axis lines carry the sketch element slot
  expect_s3_class(p@theme[["panel.grid.major"]]@sketch, "vellum_sketch")
  expect_s3_class(p@theme[["axis.line"]]@sketch, "vellum_sketch")
})

test_that("a plot without theme_sketch has no plot-wide sketch", {
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
  expect_null(vellumplot:::.theme_sketch_default(p))
})

# --- rendering ---------------------------------------------------------------

test_that("a sketched plot renders and differs from the crisp one", {
  base <- vplot(mtcars) |> mark_point(x = wt, y = mpg, size = 5)
  crisp <- render_px(base)
  drawn <- render_px(
    vplot(mtcars) |> mark_point(x = wt, y = mpg, size = 5, sketch = sketch(roughness = 2))
  )
  expect_identical(dim(crisp), dim(drawn))
  # the wobble redistributes ink, so the frames are not identical
  expect_true(mean(abs(crisp - drawn)) > 1e-4)
})

test_that("sketch = NA renders identically to no sketch (forced crisp)", {
  crisp <- render_px(vplot(mtcars) |> mark_point(x = wt, y = mpg, size = 5))
  forced <- render_px(
    vplot(mtcars) |> mark_point(x = wt, y = mpg, size = 5, sketch = NA)
  )
  expect_equal(crisp, forced)
})

test_that("sketch output is deterministic given a seed", {
  p <- function() {
    vplot(mtcars) |> mark_point(x = wt, y = mpg, size = 4, sketch = sketch(seed = 11))
  }
  expect_equal(render_px(p()), render_px(p()))
})

test_that("theme_sketch renders across PNG / SVG / PDF without error", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, color = factor(cyl)) |>
    theme_sketch()
  for (ext in c(".png", ".svg", ".pdf")) {
    f <- withr::local_tempfile(fileext = ext)
    expect_no_error(render_plot(p, f))
    expect_true(file.exists(f))
  }
})

test_that("filled marks accept every fill style", {
  df <- data.frame(g = c("a", "b", "c"), n = c(3, 5, 2))
  for (fs in c("solid", "hachure", "crosshatch", "zigzag", "dots")) {
    p <- vplot(df) |> mark_bar(x = g, y = n, sketch = sketch(fill_style = fs))
    expect_no_error(render_px(p))
  }
})
