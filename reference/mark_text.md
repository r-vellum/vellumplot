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

- blend:

  Optional blend mode for compositing this layer against what is already
  drawn beneath it (the panel and earlier layers), one of the CSS
  `mix-blend-mode` names, e.g. `"multiply"`, `"screen"`, `"darken"`. The
  whole layer composites as one isolated group (not per element).

- data:

  Optional layer data frame; overrides the plot data for this layer.

- fill:

  For `mark_label()`, the background fill colour.

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
[`vellum::md()`](https://rdrr.io/pkg/vellum/man/md.html) labels — map
`label = md(<expr>)` for a per-datum styled label
(bold/italic/super-/subscript/colour). (Rich labels are not yet
supported by `mark_label()`'s background box.)

## Examples

``` r
vplot(mtcars) |> mark_text(x = wt, y = mpg, label = rownames(mtcars))

vplot(mtcars) |> mark_point(x = wt, y = mpg) |>
  mark_text(x = wt, y = mpg, label = rownames(mtcars), nudge_y = 2)
```
