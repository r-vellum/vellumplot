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
  expect_named(lt, c("rule", "severity", "node", "message"))
  expect_equal(nrow(lt), 0L)
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
