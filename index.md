# vellumplot

vellumplot is a declarative, pipe-first grammar of graphics built on the
[vellum](https://github.com/r-vellum/vellum) graphics backend. You
describe a plot as an inspectable, serializable *spec*; nothing is drawn
until the spec is compiled into a vellum scene and rendered.

The compile is a real pipeline (spec → resolve encodings → train scales
→ measure layout → compile guides → compile marks → vellum scene) and it
runs without a graphics device, because vellum measures text itself. If
you already know ggplot2 the grammar will feel familiar; the reason to
reach for this stack is what the retained vector scene underneath makes
possible.

### What is a little different here

Most of these exist somewhere in R; having them in one grammar, from one
spec, is what is unusual. None of it is a reason to switch on its own —
but together they cover a few gaps.

- **One spec, one solved layout, several destinations — including a
  *tagged* PDF.**
  [`render_plot()`](https://r-vellum.github.io/vellumplot/reference/render_plot.md)
  writes PNG, SVG, or PDF from the *same* compiled scene rather than
  re-solving the layout per device, and the PDF carries a real structure
  tree and alt text, so a screen reader can navigate it. Accessible
  (tagged) PDF output is uncommon for R graphics.
- **Accessibility is checkable, not just aspirational.** Render through
  a colour-vision-deficiency simulation (`render(cvd = "deutan")`) to
  see the figure as a colour-blind reader would, and run
  [`plot_lint()`](https://r-vellum.github.io/vellumplot/reference/plot_lint.md)
  to be told about tiny text, low contrast, or a single-level legend
  *before* you publish it.
- **The interactive widget uses the marks you declared.** A compiled
  plot *is* a vellum scene, so each element keeps its data key and its
  resolved device-pixel box
  ([`vellum::scene_model()`](https://r-vellum.github.io/vellum/reference/scene_model.html)).
  [vellumwidget](https://github.com/r-vellum/vellumwidget)’s
  `as_widget()` reads those for tooltips, brushing, and linked selection
  — no `*_interactive()` twins, and no second engine (Vega, plotly)
  re-drawing the plot in the browser, so the static and interactive
  figures cannot drift.
- **Effects and regions stay vector where they can.**
  [`glow()`](https://r-vellum.github.io/vellumplot/reference/glow.md) /
  [`shadow()`](https://r-vellum.github.io/vellumplot/reference/shadow.md)
  are real Gaussian blur (and work on text); Venn/Euler diagrams and
  merged choropleth regions are computed as boolean *geometry* rather
  than alpha-composited overlaps, so they stay crisp in a PDF instead of
  being flattened to pixels.
- **Texture, not only hue.** `pattern_*()` hatch fills stay legible in
  greyscale print and under colour-vision deficiency, and render on
  every backend including PDF.
- **Animation from the same grammar.**
  [`transition_states()`](https://r-vellum.github.io/vellumplot/reference/transition_states.md) +
  [`animate()`](https://r-vellum.github.io/vellumplot/reference/animate.md)
  compile one keyframe per state (scales frozen, so the animation is
  non-reactive) and
  [`anim_save()`](https://r-vellum.github.io/vellumplot/reference/anim_save.md)
  encodes a GIF, an APNG, or a resolution-independent **animated SVG**
  that honours `prefers-reduced-motion`.
- **A hand-drawn mode that is exact.**
  [`theme_sketch()`](https://r-vellum.github.io/vellumplot/reference/theme_sketch.md)
  (or a `sketch =` argument) gives a plot a wobbly, hand-drawn look
  generated *natively* in the engine, so it is identical across PNG,
  SVG, and PDF rather than a post-hoc filter.
- **The spec is plain data.**
  [`summary()`](https://rdrr.io/r/base/summary.html) shows a plot’s
  structure without drawing it, and the spec round-trips, so a plot is
  something you can inspect, store, and program against.

## Installation

``` r

# install.packages("pak")
pak::pak("r-vellum/vellumplot")
```

vellumplot needs the [vellum](https://github.com/r-vellum/vellum)
backend, which compiles a Rust crate, so you also need a Rust toolchain
(`cargo`/`rustc`); pak pulls vellum in automatically.

## Usage

Building a plot returns a spec; printing it draws into the Plots pane
(and embeds in a knitr/Quarto chunk), like ggplot2. Use
[`render_plot()`](https://r-vellum.github.io/vellumplot/reference/render_plot.md)
to write a file.

``` r

library(vellumplot)

# a scatter with a continuous colour legend
vplot(mtcars) |>
  mark_point(x = wt, y = mpg, color = hp) |>
  scale_color_continuous()
```

![plot of chunk example](reference/figures/README-example-1.png)

plot of chunk example

Layer marks on a single panel; scales train across every layer:

``` r

vplot(mtcars) |>
  mark_point(x = wt, y = mpg) |>
  mark_smooth(x = wt, y = mpg)
```

![plot of chunk layers](reference/figures/README-layers-1.png)

plot of chunk layers

Facet into a grid of panels
([`facet_wrap()`](https://r-vellum.github.io/vellumplot/reference/facet_wrap.md)
/
[`facet_grid()`](https://r-vellum.github.io/vellumplot/reference/facet_wrap.md)),
with shared or free scales:

``` r

vplot(mtcars) |>
  mark_point(x = wt, y = mpg) |>
  facet_wrap(~cyl)
```

![plot of chunk facet](reference/figures/README-facet-1.png)

plot of chunk facet

Draw spatial data:
[`mark_sf()`](https://r-vellum.github.io/vellumplot/reference/mark_sf.md)
renders an `sf` geometry column and
[`coord_sf()`](https://r-vellum.github.io/vellumplot/reference/coord_sf.md)
reprojects and locks the map aspect ratio:

``` r

nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
vplot(nc) |>
  mark_sf(fill = BIR74) |>
  coord_sf()
```

![plot of chunk sf](reference/figures/README-sf-1.png)

plot of chunk sf

Draw a network:
[`vgraph()`](https://r-vellum.github.io/vellumplot/reference/vgraph.md)
lays out an `igraph` graph (stress majorization by default), then
[`mark_edges()`](https://r-vellum.github.io/vellumplot/reference/mark_graph.md)
/
[`mark_nodes()`](https://r-vellum.github.io/vellumplot/reference/mark_graph.md)
draw it — aspect-locked, no axes, edges under nodes:

``` r

g <- igraph::make_graph("Zachary")
g <- igraph::set_vertex_attr(
  g,
  "grp",
  value = as.factor(igraph::cluster_louvain(g)$membership)
)
g <- igraph::set_vertex_attr(g, "deg", value = igraph::degree(g))
vgraph(g, layout = "stress") |>
  mark_edges(alpha = 0.4) |>
  mark_nodes(size = deg, fill = grp) |>
  scale_size(range = c(2, 8))
```

![plot of chunk network](reference/figures/README-network-1.png)

plot of chunk network

The spec is just data —
[`summary()`](https://rdrr.io/r/base/summary.html) shows its structure
without drawing:

``` r

summary(vplot(mtcars) |> mark_point(x = wt, y = mpg, color = hp))
#> <PlotSpec> 32x11 (11 columns), page 6x4 in
#> 
#> ── layers
#> • mark_point(x = wt, y = mpg, color = hp)
```

Write to a file with
[`render_plot()`](https://r-vellum.github.io/vellumplot/reference/render_plot.md)
(the format follows the extension):

``` r

p <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
render_plot(p, "cars.png")
```

## What’s included

- Marks:
  [`mark_point()`](https://r-vellum.github.io/vellumplot/reference/mark_point.md),
  [`mark_line()`](https://r-vellum.github.io/vellumplot/reference/mark_point.md),
  [`mark_step()`](https://r-vellum.github.io/vellumplot/reference/mark_area.md),
  [`mark_rule()`](https://r-vellum.github.io/vellumplot/reference/mark_point.md),
  [`mark_bar()`](https://r-vellum.github.io/vellumplot/reference/mark_point.md)
  (explicit heights, or row counts per category),
  [`mark_area()`](https://r-vellum.github.io/vellumplot/reference/mark_area.md)
  /
  [`mark_ribbon()`](https://r-vellum.github.io/vellumplot/reference/mark_area.md),
  intervals
  ([`mark_errorbar()`](https://r-vellum.github.io/vellumplot/reference/mark_boxplot.md),
  [`mark_linerange()`](https://r-vellum.github.io/vellumplot/reference/mark_boxplot.md),
  [`mark_segment()`](https://r-vellum.github.io/vellumplot/reference/mark_segment.md)),
  [`mark_boxplot()`](https://r-vellum.github.io/vellumplot/reference/mark_boxplot.md),
  tiles/heatmaps
  ([`mark_tile()`](https://r-vellum.github.io/vellumplot/reference/mark_tile.md),
  [`mark_raster()`](https://r-vellum.github.io/vellumplot/reference/mark_tile.md)),
  2-D binning
  ([`mark_bin2d()`](https://r-vellum.github.io/vellumplot/reference/mark_tile.md),
  [`mark_hex()`](https://r-vellum.github.io/vellumplot/reference/mark_tile.md)),
  2-D density contours
  ([`mark_contour()`](https://r-vellum.github.io/vellumplot/reference/mark_contour.md),
  [`mark_contour_filled()`](https://r-vellum.github.io/vellumplot/reference/mark_contour.md)),
  text
  ([`mark_text()`](https://r-vellum.github.io/vellumplot/reference/mark_text.md),
  [`mark_label()`](https://r-vellum.github.io/vellumplot/reference/mark_text.md)),
  and pie/donut shortcuts
  ([`mark_pie()`](https://r-vellum.github.io/vellumplot/reference/mark_pie.md),
  [`mark_donut()`](https://r-vellum.github.io/vellumplot/reference/mark_pie.md)).
- Spatial:
  [`mark_sf()`](https://r-vellum.github.io/vellumplot/reference/mark_sf.md)
  draws an `sf` geometry column as a map layer, with
  [`coord_sf()`](https://r-vellum.github.io/vellumplot/reference/coord_sf.md)
  to reproject and lock the aspect ratio.
- Network:
  [`vgraph()`](https://r-vellum.github.io/vellumplot/reference/vgraph.md)
  starts a node-link diagram from an `igraph` graph (stress layout by
  default, via `graphlayouts`), with
  [`mark_edges()`](https://r-vellum.github.io/vellumplot/reference/mark_graph.md),
  [`mark_nodes()`](https://r-vellum.github.io/vellumplot/reference/mark_graph.md),
  [`mark_node_text()`](https://r-vellum.github.io/vellumplot/reference/mark_graph.md),
  and
  [`scale_edge_width()`](https://r-vellum.github.io/vellumplot/reference/scale_edge_width.md).
- Encodings (tidy-eval): `x`, `y`, `color`/`fill`, `size`, `shape`,
  `alpha`.
- Position scales
  ([`scale_x_continuous()`](https://r-vellum.github.io/vellumplot/reference/scale_x_continuous.md),
  [`scale_y_continuous()`](https://r-vellum.github.io/vellumplot/reference/scale_x_continuous.md);
  linear and `log10`) with auto-trained, expanded domains; discrete band
  scales
  ([`scale_x_discrete()`](https://r-vellum.github.io/vellumplot/reference/scale_x_continuous.md),
  [`scale_y_discrete()`](https://r-vellum.github.io/vellumplot/reference/scale_x_continuous.md))
  for categorical axes.
- Colour scales
  ([`scale_color_continuous()`](https://r-vellum.github.io/vellumplot/reference/scale_color_continuous.md)
  /
  [`scale_fill_continuous()`](https://r-vellum.github.io/vellumplot/reference/scale_color_continuous.md),
  [`scale_color_discrete()`](https://r-vellum.github.io/vellumplot/reference/scale_color_continuous.md)
  /
  [`scale_fill_discrete()`](https://r-vellum.github.io/vellumplot/reference/scale_color_continuous.md),
  `_gradient()`, `_binned()`, `_manual()`),
  [`scale_shape()`](https://r-vellum.github.io/vellumplot/reference/scale_shape.md),
  and a trained
  [`scale_size()`](https://r-vellum.github.io/vellumplot/reference/scale_size.md),
  with stacked legends.
- Trained axes, a panel with gridlines, and layering on one panel.
- Faceting
  ([`facet_wrap()`](https://r-vellum.github.io/vellumplot/reference/facet_wrap.md),
  [`facet_grid()`](https://r-vellum.github.io/vellumplot/reference/facet_wrap.md))
  with shared or free scales, via the
  [`resolve_scale()`](https://r-vellum.github.io/vellumplot/reference/resolve_scale.md)
  lattice.
- Statistical marks:
  [`mark_histogram()`](https://r-vellum.github.io/vellumplot/reference/mark_histogram.md),
  [`mark_density()`](https://r-vellum.github.io/vellumplot/reference/mark_tile.md),
  [`mark_summary()`](https://r-vellum.github.io/vellumplot/reference/mark_boxplot.md),
  [`mark_smooth()`](https://r-vellum.github.io/vellumplot/reference/mark_histogram.md)
  (with
  [`after_stat()`](https://r-vellum.github.io/vellumplot/reference/after_stat.md)).
- Coordinate systems:
  [`coord_cartesian()`](https://r-vellum.github.io/vellumplot/reference/coord_cartesian.md),
  [`coord_flip()`](https://r-vellum.github.io/vellumplot/reference/coord_cartesian.md),
  [`coord_fixed()`](https://r-vellum.github.io/vellumplot/reference/coord_cartesian.md)
  /
  [`coord_equal()`](https://r-vellum.github.io/vellumplot/reference/coord_cartesian.md),
  [`coord_trans()`](https://r-vellum.github.io/vellumplot/reference/coord_trans.md)
  (nonlinear display remap),
  [`coord_polar()`](https://r-vellum.github.io/vellumplot/reference/coord_polar.md)
  (pie / coxcomb / radar), and
  [`coord_sf()`](https://r-vellum.github.io/vellumplot/reference/coord_sf.md).
- Position adjustments: stack / dodge / fill bars, jittered points.
- Per-mark `blend =` modes (CSS `mix-blend-mode`: `"multiply"`,
  `"screen"`, …).
- [`mark_datashade()`](https://r-vellum.github.io/vellumplot/reference/mark_datashade.md)
  for million-point density rasters.
- Annotations:
  [`annotate()`](https://r-vellum.github.io/vellumplot/reference/annotate.md),
  [`labs()`](https://r-vellum.github.io/vellumplot/reference/labs.md),
  and [`md()`](https://r-vellum.github.io/vellumplot/reference/md.md)
  markdown titles.
- Themes
  ([`theme_gray()`](https://r-vellum.github.io/vellumplot/reference/theme_gray.md)
  default,
  [`theme_minimal()`](https://r-vellum.github.io/vellumplot/reference/theme_gray.md),
  [`theme_bw()`](https://r-vellum.github.io/vellumplot/reference/theme_gray.md),
  [`theme_classic()`](https://r-vellum.github.io/vellumplot/reference/theme_gray.md),
  [`theme_void()`](https://r-vellum.github.io/vellumplot/reference/theme_gray.md),
  [`theme_cyberpunk()`](https://r-vellum.github.io/vellumplot/reference/theme_gray.md),
  [`theme()`](https://r-vellum.github.io/vellumplot/reference/theme_gray.md)
  /
  [`set_theme()`](https://r-vellum.github.io/vellumplot/reference/theme_gray.md))
  and multi-plot composition
  ([`hconcat()`](https://r-vellum.github.io/vellumplot/reference/concat.md),
  [`vconcat()`](https://r-vellum.github.io/vellumplot/reference/concat.md),
  [`concat()`](https://r-vellum.github.io/vellumplot/reference/concat.md),
  [`wrap_plots()`](https://r-vellum.github.io/vellumplot/reference/concat.md),
  [`inset()`](https://r-vellum.github.io/vellumplot/reference/inset.md),
  [`repeat_()`](https://r-vellum.github.io/vellumplot/reference/repeat_.md)).
- Layer effects
  ([`glow()`](https://r-vellum.github.io/vellumplot/reference/glow.md),
  [`outline()`](https://r-vellum.github.io/vellumplot/reference/outline.md),
  [`shadow()`](https://r-vellum.github.io/vellumplot/reference/shadow.md),
  [`motion()`](https://r-vellum.github.io/vellumplot/reference/motion.md),
  [`echo()`](https://r-vellum.github.io/vellumplot/reference/motion.md))
  and gradient fills
  ([`linear_gradient()`](https://r-vellum.github.io/vellumplot/reference/gradients.md),
  [`radial_gradient()`](https://r-vellum.github.io/vellumplot/reference/gradients.md)).
- Pattern (hatch) fills:
  [`pattern_stripe()`](https://r-vellum.github.io/vellumplot/reference/patterns.md),
  [`pattern_crosshatch()`](https://r-vellum.github.io/vellumplot/reference/patterns.md),
  [`pattern_grid()`](https://r-vellum.github.io/vellumplot/reference/patterns.md),
  [`pattern_dot()`](https://r-vellum.github.io/vellumplot/reference/patterns.md),
  [`pattern_checker()`](https://r-vellum.github.io/vellumplot/reference/patterns.md),
  mapped via
  [`scale_pattern()`](https://r-vellum.github.io/vellumplot/reference/scale_pattern.md)
  — greyscale- and colour-vision-safe, on every backend.
- Boolean marks:
  [`vvenn()`](https://r-vellum.github.io/vellumplot/reference/vvenn.md)
  draws 2-/3-set Venn/Euler diagrams as solid geometry, and
  `mark_sf(merge = TRUE)` dissolves adjacent same-value regions into
  one.
- SVG icon markers (`shape =` a `d` path string or a `.svg` file) and
  repelled labels (`mark_text_repel()`, over the engine’s overlap
  solver).
- Accessibility: tagged PDF output,
  [`plot_lint()`](https://r-vellum.github.io/vellumplot/reference/plot_lint.md)
  for legibility/contrast problems, colour-vision-deficiency simulation
  at render time, and font pinning for reproducible text.
- Animation:
  [`transition_states()`](https://r-vellum.github.io/vellumplot/reference/transition_states.md)
  /
  [`transition_time()`](https://r-vellum.github.io/vellumplot/reference/transition_time.md)
  /
  [`transition_reveal()`](https://r-vellum.github.io/vellumplot/reference/transition_reveal.md),
  [`ease_aes()`](https://r-vellum.github.io/vellumplot/reference/ease_aes.md),
  [`animate()`](https://r-vellum.github.io/vellumplot/reference/animate.md),
  and
  [`anim_save()`](https://r-vellum.github.io/vellumplot/reference/anim_save.md)
  to GIF / APNG / animated SVG.
- Output:
  [`render_plot()`](https://r-vellum.github.io/vellumplot/reference/render_plot.md)
  (PNG/SVG/PDF),
  [`pdf_pages()`](https://r-vellum.github.io/vellumplot/reference/pdf_pages.md)
  (multi-page reports or one page per facet),
  [`render_all()`](https://r-vellum.github.io/vellumplot/reference/render_all.md)
  (parallel batch), and
  [`plot_svg()`](https://r-vellum.github.io/vellumplot/reference/plot_svg.md)
  for an inline SVG string (e.g. a sparkline inside a `gt` table).
- Hand-drawn rendering:
  [`sketch()`](https://r-vellum.github.io/vellumplot/reference/sketch.md)
  gives any geometry mark a wobbly, hachure- filled
  [Rough.js](https://roughjs.com) look (a `sketch =` argument on marks,
  an
  [`element_line()`](https://r-vellum.github.io/vellumplot/reference/element.md)
  /
  [`element_rect()`](https://r-vellum.github.io/vellumplot/reference/element.md)
  `sketch =` slot, or the plot-wide
  [`theme_sketch()`](https://r-vellum.github.io/vellumplot/reference/theme_sketch.md)
  one-liner). Generated natively in the engine, so it is exact and works
  across PNG / SVG / PDF.

## The vellum ecosystem

vellumplot is the grammar layer of a small ecosystem of packages that
share the vellum scene model:

- **[vellum](https://github.com/r-vellum/vellum)** — the parchment: the
  low-level graphics backend (Rust scene graph, PNG/SVG/PDF renderer).
- **[vellumplot](https://github.com/r-vellum/vellumplot)** — the pen:
  this package.
- **[vellumwidget](https://github.com/r-vellum/vellumwidget)** — the
  annotation: turns a vellumplot plot (or a raw vellum scene) into a
  client-side interactive HTML widget via `as_widget()`.
- **[vellumverse](https://github.com/r-vellum/vellumverse)** — installs
  and loads the whole ecosystem in one step.
