# Hierarchy diagrams: sunburst, icicle, treemap, circle-pack

`vhierarchy()` draws a tree as a space-filling diagram, choosing the
geometry with `type`: `"sunburst"` (concentric ring sectors), `"icicle"`
(rectangular partition), `"treemap"` (squarified nested rectangles), or
`"circlepack"` (circles within circles). All four take the same *parent
list* — `id`, `parent` (`NA`/`""` at the root), and `value` (leaf
values; an internal node's value is the sum of its children) — so
switching `type` re-encodes the same data. Like
[`vgraph()`](https://r-vellum.github.io/vellumplot/reference/vgraph.md)
/
[`vsankey()`](https://r-vellum.github.io/vellumplot/reference/vsankey.md)
it returns a ready, axis-free, aspect-locked
[PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md);
`mark_hierarchy()` is the layer it adds.

## Usage

``` r
vhierarchy(
  data,
  id,
  parent,
  value,
  type = c("sunburst", "icicle", "treemap", "circlepack"),
  inner_radius = 0,
  flow = c("down", "up", "right", "left"),
  label = TRUE,
  show_values = FALSE,
  orientation = c("auto", "radial", "tangential", "horizontal"),
  root_label = FALSE,
  width = 6,
  height = 6,
  dpi = 96
)

mark_hierarchy(
  plot,
  id,
  parent,
  value,
  type = c("sunburst", "icicle", "treemap", "circlepack"),
  inner_radius = 0,
  flow = c("down", "up", "right", "left"),
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

- type:

  Diagram geometry: `"sunburst"` (default), `"icicle"`, `"treemap"`, or
  `"circlepack"`.

- inner_radius:

  Sunburst only: central hole radius, a fraction in `[0, 1)`; `0`
  (default) fills to the centre.

- flow:

  Icicle only: the direction of increasing depth — `"down"` (default),
  `"up"`, `"right"`, or `"left"`.

- label:

  Label each node with its `id`? Default `TRUE`. Nodes too small for
  their label are left unlabelled.

- show_values:

  Append each node's value to its label, e.g. `"A1 (3)"`. Default
  `FALSE`.

- orientation:

  Sunburst labels only: `"auto"` (default) angles each label
  tangentially / radially to fit its wedge, or force `"radial"`,
  `"tangential"`, or `"horizontal"`. Labels are kept upright.

- root_label:

  Write the root's name (and, with `show_values`, its total) in the
  centre? Default `FALSE`. Most useful for `"sunburst"`.

- width, height, dpi:

  Page size (inches) and resolution.

- plot:

  A
  [PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

## Value

A
[PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md)
(`vhierarchy()`) or the modified plot (`mark_hierarchy()`).

## Details

The root is structural and never drawn (in a sunburst it is the centre);
nodes are coloured by their depth-1 branch, lightened with depth. Each
node is labelled with its `id` where the label fits, and `show_values`
appends the value; small nodes are left unlabelled so a dense diagram
stays legible.

## Examples

``` r
h <- data.frame(
  id = c("root", "A", "B", "A1", "A2", "B1"),
  parent = c(NA, "root", "root", "A", "A", "B"),
  value = c(NA, NA, NA, 3, 2, 4)
)
vhierarchy(h, id, parent, value, show_values = TRUE)

vhierarchy(h, id, parent, value, type = "treemap")
```
