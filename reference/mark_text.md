# Text marks

`mark_text()` draws the `label` aesthetic as text at each `(x, y)`;
`mark_label()` adds a filled rounded background behind each label.
`size` is the font size in points; `angle` (degrees) may be mapped or
constant.

## Usage

``` r
mark_text(
  plot,
  ...,
  size = NULL,
  family = NULL,
  fontface = NULL,
  hjust = "centre",
  vjust = "centre",
  angle = NULL,
  nudge_x = 0,
  nudge_y = 0,
  repel = FALSE,
  box_padding = 1,
  point_padding = 1,
  min_segment_length = 2,
  seed = NULL,
  effects = list(),
  blend = NULL,
  data = NULL
)

mark_label(
  plot,
  ...,
  size = NULL,
  family = NULL,
  fontface = NULL,
  hjust = "centre",
  vjust = "centre",
  angle = NULL,
  nudge_x = 0,
  nudge_y = 0,
  fill = "white",
  repel = FALSE,
  box_padding = 1,
  point_padding = 1,
  min_segment_length = 2,
  seed = NULL,
  effects = list(),
  blend = NULL,
  sketch = NULL,
  data = NULL
)
```

## Arguments

- plot:

  A
  [PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md)
  (from
  [`vplot()`](https://r-vellum.github.io/vellumplot/reference/vplot.md)).

- ...:

  Encodings (tidy-eval): `x`, `y`, `label` (+ `color`, `angle`).

- size:

  Font size in points.

- family, fontface:

  Font family / face (`"plain"`, `"bold"`, `"italic"`, `"bold.italic"`).

- hjust, vjust:

  Horizontal / vertical justification (constant; `"left"`, `"centre"`,
  `"right"`, `"bottom"`, `"top"`, or numeric in `[0, 1]`).

- angle:

  Text rotation in degrees.

- nudge_x, nudge_y:

  Shift each label by an exact absolute distance in millimetres (`+x`
  right, `+y` up), device-resolved so the nudge is constant regardless
  of scale or panel aspect. Default `0`.

- repel:

  Move overlapping labels apart (force-directed, ggrepel-style), with
  leader lines to the points? Single cartesian panel only.

- box_padding:

  Extra space (mm) kept around each label box during repulsion.

- point_padding:

  Retained for back-compatibility and currently ignored: the repulsion
  solver is deterministic and pads uniformly.

- min_segment_length:

  Shortest leader line (mm) worth drawing; a label that barely moved
  gets none.

- seed:

  Retained for back-compatibility and currently a no-op: the repel
  layout is deterministic, so there is nothing for a seed to vary.

- effects:

  A list of layer render effects applied to the mark at draw time —
  [`glow()`](https://r-vellum.github.io/vellumplot/reference/glow.md),
  [`outline()`](https://r-vellum.github.io/vellumplot/reference/outline.md),
  and
  [`shadow()`](https://r-vellum.github.io/vellumplot/reference/shadow.md).
  Available on stroked and point marks.

- blend:

  Optional blend mode for compositing this layer against what is already
  drawn beneath it (the panel and earlier layers), one of the CSS
  `mix-blend-mode` names, e.g. `"multiply"`, `"screen"`, `"darken"`. The
  whole layer composites as one isolated group (not per element).

- data:

  Optional layer data frame; overrides the plot data for this layer.

- fill:

  For `mark_label()`, the label background: a constant colour, or a
  mapped encoding (e.g. `fill = group`) coloured through the fill/colour
  scale.

- sketch:

  A
  [`sketch()`](https://r-vellum.github.io/vellumplot/reference/sketch.md)
  spec giving this layer a hand-drawn look (wobbly outlines, hachure
  fills), `NA`/`FALSE` to force it crisp (overriding a plot-wide
  [`theme_sketch()`](https://r-vellum.github.io/vellumplot/reference/theme_sketch.md)),
  or `NULL` (default) to inherit. Geometry marks accept it; text,
  raster, hex and datashade marks do not.

## Value

The modified
[PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

## Details

The `label` for `mark_text()` may be plain text (embedded newlines `\n`
wrap onto stacked lines) or rich
[`vellum::md()`](https://r-vellum.github.io/vellum/reference/md.html)
labels — map `label = md(<expr>)` for a per-datum styled label
(bold/italic/super-/subscript/colour). (Rich labels are not yet
supported by `mark_label()`'s background box.)

Set `repel = TRUE` to move overlapping labels apart with a
force-directed layout (like ggrepel), drawing a thin leader line back to
each label's point. Repulsion is resolved exactly against the true
rendered panel size, so it does not depend on the data scale. It is
currently limited to a single cartesian panel (no facets / composition /
polar).

## Examples

``` r
vplot(mtcars) |> mark_text(x = wt, y = mpg, label = rownames(mtcars))

vplot(mtcars) |> mark_point(x = wt, y = mpg) |>
  mark_text(x = wt, y = mpg, label = rownames(mtcars), nudge_y = 2)
```
