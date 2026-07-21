# Sunburst (radial hierarchy) diagram

`vsunburst()` draws a tree as concentric rings of sectors: depth maps to
a ring and each node's angular span is its share of its parent's. Input
is a *parent list* — `id`, `parent` (`NA` at the root), and `value`
(leaf values; an internal node's value is the sum of its children). Like
[`vgraph()`](https://r-vellum.github.io/vellumplot/reference/vgraph.md)
/
[`vsankey()`](https://r-vellum.github.io/vellumplot/reference/vsankey.md)
it returns a ready, axis-free, aspect-locked
[PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md);
`mark_sunburst()` is the layer it adds.

## Usage

``` r
vsunburst(
  data,
  id,
  parent,
  value,
  inner_radius = 0,
  label = TRUE,
  show_values = FALSE,
  orientation = c("auto", "radial", "tangential", "horizontal"),
  root_label = FALSE,
  width = 6,
  height = 6,
  dpi = 96
)

mark_sunburst(
  plot,
  id,
  parent,
  value,
  inner_radius = 0,
  label = TRUE,
  show_values = FALSE,
  orientation = c("auto", "radial", "tangential", "horizontal"),
  root_label = FALSE
)
```

## Arguments

- data:

  A data frame describing a hierarchy (a parent list).

- id, parent, value:

  Columns (tidy-eval): the node id, its parent id (`NA`/`""` for the
  root), and its value (used for leaves).

- inner_radius:

  Central hole radius, a fraction in `[0, 1)`; `0` (default) fills to
  the centre.

- label:

  Label each segment with its `id`? Default `TRUE`. Segments too small
  for their label (in every allowed orientation) are left unlabelled.

- show_values:

  Append each node's value to its label, e.g. `"A1 (3)"` (and, with
  `root_label`, the root's total). Default `FALSE`.

- orientation:

  How segment labels are angled: `"auto"` (default) picks tangential /
  radial / horizontal per segment to best fit the wedge, or force one of
  `"radial"`, `"tangential"`, `"horizontal"`. Labels are always kept
  upright (never upside-down).

- root_label:

  Write the root's name in the centre? Default `FALSE`.

- width, height, dpi:

  Page size (inches) and resolution.

- plot:

  A
  [PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

## Value

A
[PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md)
(`vsunburst()`) or the modified plot (`mark_sunburst()`).

## Details

The root is the centre (not drawn as a wedge); `inner_radius` opens a
hole. Nodes are coloured by depth. The input must be a single-rooted
tree.

By default each segment is labelled with its `id`, oriented to fit its
wedge (see `orientation`); a label that fits in no orientation is
dropped, so a dense sunburst stays legible. `show_values` appends the
node's value, and `root_label` writes the root's name (and, with
`show_values`, its total) in the centre.

## Examples

``` r
h <- data.frame(
  id = c("root", "A", "B", "A1", "A2", "B1"),
  parent = c(NA, "root", "root", "A", "A", "B"),
  value = c(NA, NA, NA, 3, 2, 4)
)
vsunburst(h, id, parent, value, show_values = TRUE)
```
