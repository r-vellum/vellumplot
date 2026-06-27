# vellumplot 0.0.0.9000

First version: a single-panel grammar of graphics that compiles a declarative
spec into a `vellum` scene.

## Features

* `vplot()` starts an inspectable, serializable `PlotSpec`.
* Marks: `mark_point()`, `mark_line()`, `mark_rule()` (reference lines via
  `xintercept` / `yintercept`).
* Encodings captured with tidy evaluation: `x`, `y`, `color`/`fill`, `size`,
  `shape`, `alpha`. Scalar values become constant aesthetics.
* Scales: `scale_x_continuous()` / `scale_y_continuous()` (linear and `log10`)
  with auto-trained domains and ggplot-style expansion; `scale_color_continuous()`
  (perceptual ramp) and `scale_color_discrete()` (qualitative palette).
* Guides: trained x/y axes (breaks, labels, titles), a grey panel with white
  gridlines, and a colour legend (continuous gradient bar or discrete swatches).
* Layering: multiple marks on one panel, with scales trained across all layers.
* Output: `render_plot(plot, path)`; `vellum::render(plot, path)` and
  `print(plot)` also work. The compiler is registered on vellum's
  `as_vellum_scene()` seam.

## Not yet implemented (planned for v2)

Faceting / `concat` / `repeat`, reactivity, statistical transforms (binning,
aggregation, smoothers), scene-space initializers (dodge / jitter / hexbin),
`datashade` auto-marks, additional themes, non-cartesian coordinates, the
algebraic `*` / `+` layer combinators, and rich/plotmath axis labels.
`mark_bar()` is not yet available.
