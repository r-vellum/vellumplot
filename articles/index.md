# Articles

### Get started

- [Get
  started](https://r-vellum.github.io/vellumplot/articles/vellumplot.md):

  From a data frame to a rendered plot: build a spec, layer marks, train
  scales, and render it into a vellum scene.

### Building plots

- [Marks, layer by
  layer](https://r-vellum.github.io/vellumplot/articles/marks.md):

  A tour of the mark families: points, lines and bars, areas and
  intervals, tiles and bins, text, images, and pie. How encodings map,
  and how scales train across every layer on the panel.

- [Scales and
  guides](https://r-vellum.github.io/vellumplot/articles/scales-and-guides.md):

  How data values become colour, size, shape, opacity, line type, and
  axis positions. Continuous, discrete, binned, manual, gradient,
  date/time, and identity scales, limit shortcuts, trained domains, and
  controlling the legends they produce.

- [Facets and
  composition](https://r-vellum.github.io/vellumplot/articles/facets-and-composition.md):

  Two ways to make many panels: facet one plot across a variable with
  shared or free scales, and combine separate plots into one figure with
  concat, insets, and repeats.

### Beyond scatter plots

- [Spatial and
  networks](https://r-vellum.github.io/vellumplot/articles/spatial-and-networks.md):

  Two data shapes that are not tidy rows of x and y: sf geometries drawn
  as maps with coord_sf, and igraph graphs laid out and drawn as
  node-link diagrams with vgraph.

- [Flows and
  hierarchies](https://r-vellum.github.io/vellumplot/articles/flows-and-hierarchies.md):

  Plot types whose positions come from a layout, not from x/y columns:
  sankey flow diagrams with vsankey (and, later, hierarchies).
  Axis-free, computed R-side, drawn through vellum primitives.

- [Statistical
  marks](https://r-vellum.github.io/vellumplot/articles/statistical-marks.md):

  Marks that transform data before drawing: histograms, densities,
  per-group summaries, linear smooths, ECDFs, Q-Q plots, rugs, violins,
  ridgelines, and dot plots, plus after_stat for reaching the computed
  variables and mark_datashade for millions of points.

- [Datashading a third of a billion
  points](https://r-vellum.github.io/vellumplot/articles/datashading.md):

  When there are too many points to draw one marker each, datashading
  bins them into a raster and colours by density. A tour of
  mark_datashade, ending with the full US Census (about 306 million
  points) shaded two ways.

- [Effects and
  themes](https://r-vellum.github.io/vellumplot/articles/effects-and-themes.md):

  The non-data look of a plot: built-in themes, custom theme elements,
  layer effects like glow and shadow, gradient fills, and the one-line
  hand-drawn sketch mode.

### Background

- [The compiler: spec to
  scene](https://r-vellum.github.io/vellumplot/articles/the-compiler.md):

  vellumplot is not a wrapper around drawing calls. A plot is an
  inspectable, serializable spec, and a real compiler turns it into a
  vellum scene. This is what that means and why it is useful.

- [Accessibility](https://r-vellum.github.io/vellumplot/articles/accessibility.md):

  Every plot the vellum ecosystem produces is accessible by default: a
  labelled, described SVG, a tagged PDF, an auto-generated text
  alternative, and a keyboard- and screen-reader-navigable interactive
  widget. This is how it works and how to author it well.
