# The accessibility arc: CVD-simulated render, plot_lint(), pattern_hatch(),
# tagged-PDF furniture roles, and font pinning in the reproducibility manifest.

svg_of <- function(p) vellum::scene_svg(vellum::as_vellum_scene(p))

# ---- render_plot(cvd =) -----------------------------------------------------

test_that("render_plot(cvd =) forwards CVD simulation and changes the pixels", {
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg, color = factor(cyl))
  f0 <- withr::local_tempfile(fileext = ".png")
  f1 <- withr::local_tempfile(fileext = ".png")
  render_plot(p, f0)
  render_plot(p, f1, cvd = "deuteranopia")
  expect_gt(file.info(f1)$size, 0)
  # the simulated render is not byte-identical to the normal one (colours shift)
  expect_false(
    identical(
      readBin(f0, "raw", file.info(f0)$size),
      readBin(f1, "raw", file.info(f1)$size)
    )
  )
})

# ---- plot_lint() ------------------------------------------------------------

test_that("plot_lint() returns the vellum_lint shape and is empty for a clean plot", {
  lt <- plot_lint(vplot(mtcars) |> mark_point(x = wt, y = mpg))
  expect_s3_class(lt, "vellum_lint")
  # Findings carry the box they refer to, which is what lets
  # `vellum::vl_lint_overlay()` draw them back onto the plot.
  expect_named(
    lt,
    c("rule", "severity", "node", "message", "x0", "y0", "x1", "y1")
  )
  expect_equal(nrow(lt), 0L)
})

test_that("plot_lint() passes the engine's arguments through", {
  p <- vplot(transform(mtcars, grp = "one group")) |>
    mark_point(x = wt, y = mpg, color = grp) |>
    theme(axis.text = element_text(size = 2))
  expect_true(all(c("tiny_text", "single_level_scale") %in% plot_lint(p)$rule))
  # `rules =` selects, grammar rules included.
  expect_equal(
    unique(plot_lint(p, rules = "single_level_scale")$rule),
    "single_level_scale"
  )
  # `exclude =` takes a grammar finding's node without complaining that it
  # matched nothing -- that node is not in the engine's node table.
  expect_silent(plot_lint(p, exclude = "scale:color"))
  expect_false(
    "single_level_scale" %in% plot_lint(p, exclude = "scale:color")$rule
  )
  # A threshold that stops the geometric rule firing.
  expect_false(
    "tiny_text" %in% plot_lint(p, min_text_px = 1, min_text_pt = 1)$rule
  )
})

test_that("a plot's findings can be drawn back onto it", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    theme(axis.text = element_text(size = 2))
  scene <- vellum::as_vellum_scene(p)
  ov <- vellum::vl_lint_overlay(scene, plot_lint(p))
  expect_true(any(grepl("^lint_box_", vellum::node_names(ov))))
  expect_gt(length(vellum::scene_png(ov)), 0L)
})

test_that("the grammar rules live in the engine's registry", {
  reg <- vellum::vl_lint_rules()
  expect_true(all(c("single_level_scale", "legend_overflow") %in% reg$rule))
  expect_equal(reg$tags[reg$rule == "single_level_scale"], "grammar")
  # So a plain engine lint of a compiled plot reports the encoding too, without
  # going through plot_lint() at all.
  p <- vplot(transform(mtcars, grp = "one group")) |>
    mark_point(x = wt, y = mpg, color = grp)
  expect_true(
    "single_level_scale" %in% vellum::vl_lint(vellum::as_vellum_scene(p))$rule
  )
  # And they stay quiet on a scene that did not come from vellumplot.
  plain <- vellum::draw(
    vellum::vl_scene(2, 2, dpi = 96),
    vellum::rect_grob(width = 0.2, height = 0.2)
  )
  expect_equal(nrow(vellum::vl_lint(plain, rules = "single_level_scale")), 0L)
})

test_that("the scales summary on a compiled scene is inert", {
  # The grammar rules reach the trained scales through an attribute left on the
  # scene by `.draw_plot()`. It must not change what the scene *is*: not its
  # hash, not its serialised form, and not a pixel of its output.
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg, color = factor(cyl))
  scene <- vellum::as_vellum_scene(p)
  expect_false(is.null(attr(scene, "vellumplot_lint_scales", exact = TRUE)))
  bare <- scene
  attr(bare, "vellumplot_lint_scales") <- NULL
  expect_equal(vellum::scene_hash(scene), vellum::scene_hash(bare))
  expect_identical(vellum::scene_png(scene), vellum::scene_png(bare))
  f1 <- withr::local_tempfile(fileext = ".rds")
  f2 <- withr::local_tempfile(fileext = ".rds")
  vellum::scene_write(scene, f1)
  vellum::scene_write(bare, f2)
  expect_equal(file.info(f1)$size, file.info(f2)$size)
})

test_that("the scales summary carries levels, not the plot's data", {
  # The point of summarising rather than keeping a back-reference to the spec: a
  # scene must not pin the data frame alive for as long as someone holds it.
  d <- data.frame(
    x = runif(20000),
    y = runif(20000),
    g = factor(sample(letters[1:4], 20000, replace = TRUE))
  )
  p <- vplot(d) |> mark_point(x = x, y = y, color = g)
  summ <- attr(
    vellum::as_vellum_scene(p),
    "vellumplot_lint_scales",
    exact = TRUE
  )
  expect_named(summ, "color")
  expect_named(summ$color, c("kind", "levels"))
  expect_equal(summ$color$levels, letters[1:4])
  # Kilobytes, not the megabyte-plus the spec for this data would cost.
  expect_lt(as.numeric(object.size(summ)), 20000)
})

test_that("a composition reports no grammar findings rather than its last cell", {
  # Cells are drawn through `.draw_plot()`, which leaves its summary behind, so
  # without clearing it a composition would report on whichever cell was drawn
  # last as though it were the whole figure.
  one <- vplot(transform(mtcars, grp = "one group")) |>
    mark_point(x = wt, y = mpg, color = grp)
  scene <- vellum::as_vellum_scene(hconcat(one, one))
  expect_null(attr(scene, "vellumplot_lint_scales", exact = TRUE))
  found <- vellum::vl_lint(scene)
  expect_false(any(
    c("single_level_scale", "legend_overflow") %in% found$rule
  ))
})

test_that("plot_lint() flags tiny text (geometric rule from the engine)", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    theme(axis.text = element_text(size = 2))
  lt <- plot_lint(p)
  expect_true("tiny_text" %in% lt$rule)
})

test_that("plot_lint() flags a single-level discrete scale (grammar rule)", {
  d <- data.frame(
    wt = mtcars$wt,
    mpg = mtcars$mpg,
    grp = factor(rep("only", nrow(mtcars)))
  )
  lt <- plot_lint(vplot(d) |> mark_point(x = wt, y = mpg, color = grp))
  hit <- lt[lt$rule == "single_level_scale", , drop = FALSE]
  expect_equal(nrow(hit), 1L)
  expect_equal(hit$node, "scale:color")
})

test_that("plot_lint() flags an over-long discrete legend (grammar rule)", {
  d <- data.frame(
    x = 1:30,
    y = 1:30,
    g = factor(sprintf("lvl%02d", 1:30))
  )
  lt <- plot_lint(vplot(d) |> mark_point(x = x, y = y, color = g))
  hit <- lt[lt$rule == "legend_overflow", , drop = FALSE]
  expect_equal(nrow(hit), 1L)
  expect_equal(hit$severity, "warning")
})

# ---- pattern_hatch() --------------------------------------------------------

test_that("pattern_hatch() is a vellum_hatch usable as a fill", {
  h <- pattern_hatch(angle = 30, spacing = 4)
  expect_s3_class(h, "vellum_hatch")
  d <- data.frame(g = c("a", "b", "c"), n = c(3, 5, 2))
  f <- withr::local_tempfile(fileext = ".png")
  expect_no_error(render_plot(vplot(d) |> mark_bar(x = g, y = n, fill = h), f))
  expect_gt(file.info(f)$size, 0)
})

test_that("pattern_hatch() works as a scale_pattern value (redundant encoding)", {
  d <- data.frame(g = c("a", "b"), n = c(3, 5))
  p <- vplot(d) |>
    mark_bar(x = g, y = n, pattern = g) |>
    scale_pattern(
      values = list(pattern_hatch(angle = 0), pattern_hatch(angle = 90))
    )
  f <- withr::local_tempfile(fileext = ".png")
  expect_no_error(render_plot(p, f))
  expect_gt(file.info(f)$size, 0)
})

# ---- tagged-PDF furniture roles ---------------------------------------------

test_that("furniture carries decorative roles (PDF artifacts, not read aloud)", {
  svg <- svg_of(vplot(mtcars) |> mark_point(x = wt, y = mpg))
  # ticks + panel/plot backgrounds are role="presentation"; gridlines role="grid"
  # (both are treated as decorative by the PDF backend)
  expect_match(svg, 'role="presentation"')
  expect_match(svg, 'role="grid"')
})

# ---- font pinning in the manifest -------------------------------------------

test_that("plot_manifest() records the resolved fonts", {
  m <- plot_manifest(vplot(mtcars) |> mark_point(x = wt, y = mpg))
  expect_true(!is.null(m$fonts) && nrow(m$fonts) > 0)
  expect_true(all(c("path", "index", "glyphs") %in% names(m$fonts)))
})

test_that("plot_verify() reports a font mismatch distinctly from a data mismatch", {
  skip_if_not_installed("jsonlite")
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
  svg <- plot_svg(p, manifest = TRUE)
  # same machine, same data: everything matches
  v <- plot_verify(svg, mtcars)
  expect_true(v$ok)
  expect_true(v$data_ok)
  expect_true(v$fonts_ok)
  # wrong data: data_ok fails, fonts_ok stays TRUE -> a distinct cause
  v2 <- plot_verify(svg, mtcars[1:5, ])
  expect_false(v2$ok)
  expect_false(v2$data_ok)
  expect_true(v2$fonts_ok)
})
