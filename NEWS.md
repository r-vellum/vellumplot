# vellumplot 0.0.0.9000

A single-panel grammar of graphics that compiles a declarative spec into a
`vellum` scene.

## Features

* `vplot()` starts an inspectable, serializable `PlotSpec`.
* Marks: `mark_point()`, `mark_line()`, `mark_rule()` (reference lines via
  `xintercept` / `yintercept`), and `mark_bar()` (uses explicit `y` heights, or
  counts rows per category when `y` is omitted).
* Encodings captured with tidy evaluation: `x`, `y`, `color`/`fill`, `size`,
  `shape`, `alpha`. Scalar values become constant aesthetics.
* Scales: `scale_x_continuous()` / `scale_y_continuous()` (linear and `log10`)
  with auto-trained domains and ggplot-style expansion; **discrete (band)
  position scales** are trained automatically for categorical `x`/`y` (bars);
  `scale_color_continuous()` (perceptual ramp), `scale_color_discrete()`
  (qualitative palette), and a trained **size scale** for a mapped `size`.
* Guides: trained x/y axes (breaks, labels, titles), a grey panel with white
  gridlines, and a legend area that **stacks multiple guides** — a colour legend
  (continuous gradient bar or discrete swatches) and a size legend.
* Layering: multiple marks on one panel, with scales trained across all layers.
* **Faceting**: `facet_wrap(~var)` and `facet_grid(rows ~ cols)` split the data
  into a panel grid with facet strips and aligned, shared axes.
* **Scale resolution** (`resolve_scale()` / `facet_*(scales=)`): position scales
  are shared across panels by default; opt into `"free_x"` / `"free_y"` /
  `"free"` (independent per panel) for per-panel ranges and axes.
* **Statistical transforms**: `mark_histogram()` (bin a continuous variable into
  count bars) and `mark_smooth()` (an `"lm"` fit drawn as a line with an optional
  confidence ribbon). `mark_bar()` with no `y` uses the count stat. Map computed
  variables with `after_stat()`, e.g. `y = after_stat(density)`.
* **Position adjustments** (`position =`): grouped bars **stack** by default;
  `"dodge"` places them side by side and `"fill"` normalises each group to 1.
  `mark_point(position = "jitter")` spreads overlapping points.
* **Datashading**: `mark_datashade()` aggregates a large point cloud into a
  density raster (via `vellum::datashade()`) that fills the panel — cost
  independent of point count. `mark_point(auto = TRUE)` switches to this
  automatically above ~50k rows.
* **Themes**: `theme_gray()` (default), `theme_minimal()`, `theme_bw()`, and
  `set_theme()` for ad-hoc overrides (panel background, gridlines, text, strip
  background).
* **Composition**: `hconcat()`, `vconcat()`, and `concat()` arrange several
  independent plots (each with its own scales, axes, and legend) on a grid.
* Output: `render_plot(plot, path)`; `vellum::render(plot, path)` and
  `print(plot)` also work. The compiler is registered on vellum's
  `as_vellum_scene()` seam.

## Not yet implemented (planned)

`repeat` composition, reactivity, further statistical transforms (aggregation,
density, 2-D contours), hexagonal binning (`hexbin` — deferred until
display-aspect locking lands, see `_docs/DESIGN.md` §3.2), non-cartesian
coordinates, the algebraic `*` / `+` layer combinators, independent
*non-position* scales across facets, and rich/plotmath axis labels.
Independent *non-position* (colour/size) scales across facets are not yet
supported (those legends stay shared). Spatial (`sf`) and network (`igraph`)
layers are on the roadmap (see `_docs/DESIGN.md`).
