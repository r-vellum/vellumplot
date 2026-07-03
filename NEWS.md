# quill 0.0.0.9000

A declarative grammar of graphics that compiles an inspectable spec into a
`vellum` scene, with faceting, coordinate systems, and multi-plot composition.

## Features

* **Auto-display**: printing a plot (or composition) draws it into the active
  graphics device — the RStudio / Positron Plots pane, or a knitr/Quarto chunk —
  like ggplot2 (via `vellum::display()`). `summary()` shows the inspectable spec
  tree instead; `render_plot()` still writes a file.
* `vplot()` starts an inspectable, serializable `PlotSpec`.
* **Output resolution**: `vplot(dpi =)` sets the authored resolution and
  `render_plot(dpi =)` overrides it per render, so an exported PNG's pixel
  dimensions are `width * dpi` by `height * dpi`. Compositions inherit the first
  sub-plot's `dpi` (or take an explicit `concat(dpi =)`).
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
* **Legend layout** is measured in millimetres: each guide is sized to its
  content (a title line above one row per key), the row pitch is driven by the
  key's drawn size so large bubble keys never overlap, and the guide block is
  centred in the legend track. Titles sit directly above their keys, horizontal
  (top/bottom) legends pack keys tightly instead of spreading them across the
  full width, and a vertical legend uses the full figure height.
* **Legend keys match the mark**: a colour legend draws the glyph the plot
  actually uses — a filled circle for point layers, a short line for line-like
  layers (line/step/smooth/segment/…), and a filled square for bar/area/tile/
  polygon layers — instead of always a square swatch.
* **Legend geometry is themeable**: `theme(legend.key.size=, legend.spacing=,
  legend.margin=)` set the key/swatch side, the gap between stacked guides, and
  the inset around the legend block (all in millimetres).
* Continuous colour bars now carry **white break ticks** aligned to their
  labels, on both vertical and horizontal (top/bottom) legends.
* **Merged legends**: mapping one variable to two aesthetics draws a single
  legend whose keys carry both encodings — discrete `colour` + `shape` become
  coloured shape keys; continuous `colour` + `size` become colour-graded,
  size-graduated points. Merging follows ggplot2's rule (same title and
  breaks/levels); give one scale a different `name=` to keep them separate.
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
* **More marks**: areas/ribbons (`mark_area()`, `mark_ribbon()`), steps
  (`mark_step()`), intervals (`mark_errorbar()`, `mark_linerange()`,
  `mark_segment()`), `mark_boxplot()`, tiles/heatmaps (`mark_tile()`,
  `mark_raster()`), 2-D binning (`mark_bin2d()`, `mark_hex()`), `mark_text()` /
  `mark_label()`, and pie/donut shortcuts (`mark_pie()`, `mark_donut()`).
* **More statistical transforms**: `mark_density()` (kernel density),
  `mark_summary()` (aggregate `y` per category), in addition to the histogram /
  binning / smooth stats. Map computed variables with `after_stat()`.
* **Coordinate systems**: `coord_cartesian()` (view-window zoom),
  `coord_flip()`, `coord_fixed()` / `coord_equal()` (aspect lock), and
  `coord_polar()` (pie / coxcomb / radar).
* **Themes**: `theme_gray()` (default), `theme_minimal()`, `theme_bw()`,
  `theme_classic()`, `theme_void()`, and `theme()` for ad-hoc overrides (panel
  background, gridlines, text, strip background, legend position, margins).
* **Effects & paints**: stroked and point marks take a layer
  `effects = list(...)` argument — `glow()` (neon halo), `outline()` (contrasting
  halo for legibility), and `shadow()` (drop / ambient). `linear_gradient()` /
  `radial_gradient()` can be used as a `fill` value (area / ribbon / bar), and
  `theme_cyberpunk()` ties glow + gradients + a neon palette into the
  [mplcyberpunk](https://github.com/dhaitz/mplcyberpunk) look.
* **Composition**: `hconcat()`, `vconcat()`, `concat()`, and `wrap_plots()`
  arrange several plots on a grid; the aligned path lines up panel edges and can
  **collect guides** across sub-plots. `inset()` overlays a plot, and
  `compose_annotation()` adds figure-level titles and auto-tags (`A`, `B`, ...).
* **Repeat**: `repeat_()` replicates a plot across a set of fields, zipping one
  or more encodings to produce a composition.
* **Blend modes**: marks take a `blend =` argument (the CSS `mix-blend-mode` set
  — `"multiply"`, `"screen"`, `"darken"`, ...). The layer composites as one
  isolated group against the panel and earlier layers, e.g. two overlapping
  translucent layers under `"multiply"`.
* **Spatial**: `mark_sf()` draws the geometry column of an `sf` object as a map
  layer (polygons / lines / points), with `coord_sf()` to reproject to a target
  CRS and lock the map aspect ratio; `scale_fill_binned()` / `scale_color_binned()`
  bin a continuous fill/colour into discrete classes for choropleths.
* **Network**: `vgraph()` starts a node-link diagram from an `igraph` graph —
  it runs a layout (stress majorization by default, via `graphlayouts`;
  `"sparse_stress"`, `"backbone"`, `"fr"`, `"circle"`, ... or a matrix/function),
  builds a node and an edge table, and locks the aspect with a void theme.
  `mark_edges()` draws edges (straight, batched; reciprocal/parallel edges offset
  off the centre line, self-loops nested, optional `arrow`), `mark_nodes()` the
  vertices, and `mark_node_text()` the labels — with a fixed edges-under-nodes
  draw order. Edges are capped exactly at each endpoint's node boundary (per
  vertex, at any resolution), so arrowheads land on the node edge. `scale_edge_width()`
  maps a weight to edge width with its own legend.
  `igraph` / `graphlayouts` are optional (`Suggests`). See `_docs/DESIGN-igraph.md`.
* Output: `render_plot(plot, path)`; `vellum::render(plot, path)` and
  `print(plot)` also work. The compiler is registered on vellum's
  `as_vellum_scene()` seam.

## Under the hood

* **Compiled-scene provenance** (foundation for interactivity / accessibility /
  linked views): every emitted mark grob now carries a globally-unique, stable
  `id` (surfacing as `data-vellum-id` in SVG), and the compiler builds a
  serializable row-key / scale-ref table — one record per grob tying it back to
  the data rows and trained scales that produced it — carried on the compiled
  scene as `attr(scene, "quill_provenance")`. Populated on every compile;
  additive metadata only (raster/PDF output is byte-for-byte unchanged). See
  `_docs/DESIGN.md` §4.
* **Continuous integration**: GitHub Actions now run `R CMD check` (R
  release/devel/oldrel on Linux + macOS, with the Rust toolchain the `vellum`
  backend needs) and report test coverage, including a nightly run against
  `vellum`'s `main` to catch cross-layer breaks early.

## Not yet implemented (planned)

Reactivity, 2-D contour stats, and the algebraic `*` / `+` layer combinators.
Independent *non-position* (colour/size) scales across facets are not yet
supported (those legends stay shared). On the network side, community hulls and
alternative idioms (arc/matrix/hive) are deferred (see `_docs/DESIGN-igraph.md`).
