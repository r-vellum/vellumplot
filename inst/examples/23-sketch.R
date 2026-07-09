# Hand-drawn ("sketch") rendering: wobbly outlines and hachure fills, generated
# natively in the vellum engine (so they are exact, cross-backend, and work in
# PDF). Sketch is a *geometry* property, not a layer effect: it perturbs the
# mark itself. Three levels of control, most-specific-wins:
#   per-mark  sketch =           (one layer)
#   element   element_*(sketch=) (one theme element)
#   plot-wide theme_sketch()     (the headline one-liner)
# `sketch = NA` forces an element crisp; text is never sketched.

library(vellumplot)
outdir <- "figures"
dir.create(outdir, showWarnings = FALSE)

set.seed(1)

# --- 1. per-mark sketch on a scatter ----------------------------------------
vplot(mtcars) |>
  mark_point(
    x = wt,
    y = mpg,
    color = factor(cyl),
    size = 4,
    sketch = sketch(roughness = 1.4, seed = 3)
  ) |>
  labs(title = "Sketched scatter", color = "cyl") |>
  render_plot(file.path(outdir, "23-scatter.png"))

# --- 2. fill styles on bars -------------------------------------------------
# hachure (default), solid, crosshatch, zigzag, dots — one per panel-less demo.
bars <- data.frame(g = c("a", "b", "c", "d"), n = c(6, 9, 4, 7))
vplot(bars) |>
  mark_bar(
    x = g,
    y = n,
    fill = g,
    sketch = sketch(fill_style = "crosshatch", roughness = 1.2)
  ) |>
  labs(title = "Crosshatch bars") |>
  render_plot(file.path(outdir, "23-bars-crosshatch.png"))

# --- 3. the whole plot, one line: theme_sketch() ----------------------------
# A plot-wide default that gridlines, axis lines, ticks, marks and legend keys
# all inherit, on a paper background. Pair with a handwriting font if installed.
vplot(datasets::iris) |>
  mark_point(x = Sepal.Length, y = Petal.Length, color = Species, size = 3) |>
  labs(title = "Hand-drawn iris") |>
  theme_sketch(roughness = 1.1, seed = 2) |>
  render_plot(file.path(outdir, "23-theme-sketch.png"))

# --- 4. mixing crisp and sketched layers ------------------------------------
# theme_sketch() sets the default; a single crisp reference line opts out with
# sketch = NA, and a smooth keeps a gentle wobble of its own.
vplot(mtcars) |>
  mark_point(x = wt, y = mpg) |>
  mark_smooth(x = wt, y = mpg, sketch = sketch(roughness = 0.8)) |>
  mark_rule(yintercept = 20, sketch = NA) |>
  labs(title = "Crisp rule over a sketched plot") |>
  theme_sketch() |>
  render_plot(file.path(outdir, "23-mixed.png"))

# --- 5. per-element control via theme() -------------------------------------
# Only the gridlines go hand-drawn; the panel and marks stay crisp.
vplot(mtcars) |>
  mark_point(x = wt, y = mpg, color = factor(gear)) |>
  theme(
    panel.grid = element_line(sketch = sketch(roughness = 0.6)),
    axis.line = element_line(colour = "grey30", sketch = sketch())
  ) |>
  labs(title = "Sketched grid, crisp marks") |>
  render_plot(file.path(outdir, "23-sketch-grid.png"))

# --- 6. a hand-drawn bar chart across backends ------------------------------
# Sketch geometry is generated once in the engine, so PNG / SVG / PDF agree.
p <- vplot(bars) |>
  mark_bar(x = g, y = n, fill = g) |>
  labs(title = "Same wobble, every backend") |>
  theme_sketch(fill_style = "hachure", seed = 5)
render_plot(p, file.path(outdir, "23-backend.svg"))
render_plot(p, file.path(outdir, "23-backend.pdf"))
