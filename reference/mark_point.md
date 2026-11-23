# Add marks to a plot

Each `mark_*()` appends a drawing layer to a
[PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).
Encodings are bare column names (or expressions) captured with tidy
evaluation, e.g. `x = wt, y = mpg, color = hp`. Scalar values (e.g.
`size = 3`, `color = "red"`) are treated as constant aesthetics rather
than data mappings.

## Usage

``` r
mark_point(
  plot,
  ...,
  size = NULL,
  shape = NULL,
  position = "identity",
  auto = FALSE,
  seed = NULL,
  blend = NULL,
  effects = list(),
  sketch = NULL,
  data = NULL
)

mark_line(
  plot,
  ...,
  position = "identity",
  window = NULL,
  auto = FALSE,
  blend = NULL,
  effects = list(),
  sketch = NULL,
  data = NULL
)

mark_rule(
  plot,
  ...,
  position = "identity",
  blend = NULL,
  effects = list(),
  sketch = NULL,
  data = NULL
)

mark_bar(
  plot,
  ...,
  position = "stack",
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

  Encodings: named channel expressions such as `x`, `y`, `color`,
  `fill`, `size`, `shape`, `alpha`. Stroked marks (line, step, segment,
  rule, linerange) also take `linewidth` and `linetype`. On
  `mark_line()`,
  [`mark_step()`](https://r-vellum.github.io/vellumplot/reference/mark_area.md)
  and
  [`mark_segment()`](https://r-vellum.github.io/vellumplot/reference/mark_segment.md),
  `linewidth` may be **mapped** as well as set: see
  [`scale_linewidth()`](https://r-vellum.github.io/vellumplot/reference/scale_linewidth.md)
  for the tapered stroke that gives, and when it falls back to
  per-segment widths. Everywhere else `linewidth` is a constant. Every
  mark also takes `group`, which says which rows form one series without
  styling them: `mark_line(x = t, y = y, group = id)` draws one line per
  `id`. Without it, as in ggplot2, the interaction of every discrete
  `color`, `fill`, `pattern`, `linetype`, `shape`, `alpha`, `size` and
  `linewidth` mapping groups; a continuous colour never does, so a line
  mapped to one stays connected and changes colour along its length.
  `group = 1` makes the whole layer one group.

- size, shape:

  Convenience arguments for the point size (in mm) / shape; may be a
  constant or a mapped expression. A shape is a built-in marker
  (`"circle"`, `"square"`, `"triangle"`, `"diamond"`, `"plus"`,
  `"cross"`, `"triangle_down"`, `"star"`), its open or bordered variant
  (`"circle open"`, `"circle filled"`, ...; see
  [`scale_shape()`](https://r-vellum.github.io/vellumplot/reference/scale_shape.md)),
  a ggplot2 shape code (`shape = 1` is an open circle, `21` a circle
  filled with `fill` inside a `color` outline), or an **SVG icon** — a
  path `d` string or a `.svg` file — drawn as a crisp vector marker. Use
  a literal string (or `!!` a variable holding one) for a constant icon,
  map `shape` and pass icons to
  [`scale_shape()`](https://r-vellum.github.io/vellumplot/reference/scale_shape.md),
  or map a column of icons with
  [`scale_shape_identity()`](https://r-vellum.github.io/vellumplot/reference/scale_color_identity.md).
  A bare variable or column of SVG paths is a mapping like any other,
  and warns.

- position:

  Position adjustment: a string — `"identity"` (default), `"nudge"`,
  `"stack"`, `"fill"`, `"dodge"`, `"dodge2"`, `"jitter"`,
  `"jitterdodge"`, `"sina"` — or a parameterised
  [position](https://r-vellum.github.io/vellumplot/reference/position.md)
  object such as
  [`position_dodge()`](https://r-vellum.github.io/vellumplot/reference/position.md)
  or
  [`position_stack()`](https://r-vellum.github.io/vellumplot/reference/position.md).
  Every mark takes `"identity"` and `"nudge"`. `"stack"`/`"fill"` work
  on bars, areas, points, text and labels (so a label can sit on its
  stacked bar); `"dodge"`/`"dodge2"` on bars, points, text, labels,
  error bars, line/point ranges, cross-bars, box plots, violins and
  slab-intervals (so error bars dodge over dodged bars); `"jitter"` on
  points, text and labels; `"jitterdodge"` and `"sina"` on points. A
  position a mark does not support is an error naming the ones it does.

- auto:

  For `mark_point()`, `mark_line()`,
  [`mark_step()`](https://r-vellum.github.io/vellumplot/reference/mark_area.md),
  [`mark_segment()`](https://r-vellum.github.io/vellumplot/reference/mark_segment.md),
  and
  [`mark_edges()`](https://r-vellum.github.io/vellumplot/reference/mark_graph.md),
  when `TRUE` and the layer has very many rows, automatically render it
  as a datashaded density raster (see
  [`mark_datashade()`](https://r-vellum.github.io/vellumplot/reference/mark_datashade.md))
  instead of individual vector marks: points bin into a density grid
  ([`vellum::datashade()`](https://r-vellum.github.io/vellum/reference/datashade.html)),
  dense lines/steps rasterise as connected polylines
  ([`vellum::datashade_lines()`](https://r-vellum.github.io/vellum/reference/datashade_lines.html)),
  and segments/edges as independent segments
  ([`vellum::datashade_segments()`](https://r-vellum.github.io/vellum/reference/datashade_lines.html)).
  The datashaded line/segment output is `dynspread`-ed so thin marks
  stay visible (see the `spread` argument of
  [`mark_datashade()`](https://r-vellum.github.io/vellumplot/reference/mark_datashade.md)).
  The fallback is skipped under a warped coordinate system
  ([`coord_polar()`](https://r-vellum.github.io/vellumplot/reference/coord_polar.md)
  /
  [`coord_trans()`](https://r-vellum.github.io/vellumplot/reference/coord_trans.md)),
  which draws the vector marks instead.

- seed:

  For `mark_point(position = "jitter")`, an optional integer seed making
  the jitter reproducible. The global RNG stream is restored afterwards.

- blend:

  Optional blend mode for compositing this layer against what is already
  drawn beneath it (the panel and earlier layers), one of the CSS
  `mix-blend-mode` names, e.g. `"multiply"`, `"screen"`, `"darken"`. The
  whole layer composites as one isolated group (not per element).

- effects:

  A list of layer render effects applied to the mark at draw time —
  [`glow()`](https://r-vellum.github.io/vellumplot/reference/glow.md),
  [`outline()`](https://r-vellum.github.io/vellumplot/reference/outline.md),
  and
  [`shadow()`](https://r-vellum.github.io/vellumplot/reference/shadow.md).
  Available on stroked and point marks.

- sketch:

  A
  [`sketch()`](https://r-vellum.github.io/vellumplot/reference/sketch.md)
  spec giving this layer a hand-drawn look (wobbly outlines, hachure
  fills), `NA`/`FALSE` to force it crisp (overriding a plot-wide
  [`theme_sketch()`](https://r-vellum.github.io/vellumplot/reference/theme_sketch.md)),
  or `NULL` (default) to inherit. Geometry marks accept it; text,
  raster, hex and datashade marks do not.

- data:

  Optional layer data frame; overrides the plot data for this layer.

- window:

  For `mark_line()`, an optional window (rolling / cumulative / offset)
  transform of `y` computed per group over rows ordered by `x`, before
  the line is drawn. Either an op name (`"mean"`, `"sum"`, `"median"`,
  `"min"`, `"max"`, `"cumsum"`, `"cummean"`, `"cummax"`, `"cummin"`,
  `"lag"`, `"lead"`, `"rank"`) or a list
  `list(op=, k=, align=, partial=)`: `k` is the window size (rolling;
  default 7) or shift (`lag`/`lead`; default 1), `align` is `"right"`
  (trailing, default), `"left"`, or `"center"`, and `partial` (default
  `TRUE`) computes at the edges from the shorter available window. For
  example `window = list(op = "mean", k = 7)` is a 7-point trailing
  average.

## Value

The modified
[PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

## Details

`mark_bar()` draws bars from a zero baseline. With an explicit `y` it
uses the `y` values as heights; with no `y` it counts rows per category
(the `"count"` stat). When `color`/`fill` is mapped, grouped bars are
stacked by default; use `position = "dodge"` for side-by-side bars or
`"fill"` to normalise to 1. `width` sets the bar width in data units, as
in ggplot2 (a fraction of the band on a categorical axis, days on a date
axis); the default is 90% of the smallest gap between bars. Dodged bars
share it. `fill` fills the bars and a `color` given alongside it
outlines them (`fill = "steelblue", color = "white"`); a `color` alone
fills them. The count stat computes `count` and `prop`, the share of its
group (or of the layer, ungrouped), for use with
[`after_stat()`](https://r-vellum.github.io/vellumplot/reference/after_stat.md),
e.g. `y = after_stat(prop)`. `density` is an older name for `prop`.

## Mappings and settings

A mark argument is either a *mapping*, whose values go through a scale
and get a legend, or a *setting*, drawn as given with no legend:

- A literal (`color = "red"`, `size = 3`) is a setting.

- A bare name that is a column of the layer's data (`color = cyl`) is a
  mapping. The layer's data is its own `data =`, else the plot's.

- A bare name that is **not** a column, and holds a single value in the
  calling environment, is a setting, like the literal it holds:
  `ink <- "#f4eee0"; mark_line(color = ink)` draws in that colour with
  no legend. A non-column name holding a longer vector (one value per
  row) still maps. The check is made when the plot is drawn, against the
  data it is drawn with, so a column always wins over a variable of the
  same name.

- Any other expression (`color = factor(cyl)`) is a mapping.

- `I(x)` always sets: `color = I(ink)`. `!!x` injects the value, so it
  sets too.

- `.env$x` always maps an R value (`color = .env$label` puts `label` in
  a one-entry legend titled "label"), and `.data$x` always reads a
  column.

The name rule applies to the arguments that train a non-positional
scale: `color`, `fill`, `size`, `shape`, `alpha`, `linewidth`,
`linetype`, `pattern` and the graph `edge_*` channels. Positions,
`label` and the interactivity arguments (`tooltip`, `data_id`, ...) keep
their meaning.

## Interactivity

Any mark accepts reserved, per-row arguments (captured like encodings,
via tidy evaluation) that make its elements addressable — and stylable —
by an interactive host without changing what a static render draws:

- `data_id` — a per-element **data key** (e.g. `data_id = model`).
  Emitted by the SVG backend as `data-key` on each element and returned
  by
  [`vellum::scene_model()`](https://r-vellum.github.io/vellum/reference/scene_model.html);
  it is the join key a host uses to map a hover/click back to a datum,
  and to link the same datum across views.

- `tooltip` — per-element tooltip text (a column expression or a
  constant), surfaced in `scene_model()` metadata.

- `hover_group` — a field grouping elements for linked emphasis
  (consumed by a host in a later phase).

- `hover_color`, `selected_color` — per-element outline colours applied
  by the host when the element is hovered / selected (a constant, or
  mapped from a column so different marks highlight differently). They
  override the widget-wide theme set by
  `vellumwidget::as_widget(hover_color=, selected_color=)`.

These are inert for PNG/PDF and for an SVG opened without a JS host: a
plot with none of them compiles and renders exactly as before. Declaring
any of them without `data_id` gives the element a default key, so it is
still addressable. A default key names the panel, the layer and the row
(`"p1-1:l2:7"`: row 7 of layer 2 in the first panel), so it never
matches another layer's keys; give two layers the same `data_id` to link
them. They currently apply to `stat = "identity"` marks (points, bars,
tiles, segments, edges, hexbins, sf features, …); aggregating stats
(histogram/count/density) drop them, since rows no longer map 1:1 to
elements.

How these flow into the vellum scene (the `scene_model()` element table,
the SVG `data-key` / `data-vellum-*` attributes, and the reserved `meta`
key vocabulary) is described in vellum's "The scene contract" vignette
([`vignette("scene-contract", package = "vellum")`](https://r-vellum.github.io/vellum/articles/scene-contract.html)).

## Examples

``` r
vplot(mtcars) |> mark_point(x = wt, y = mpg, color = hp)


# Declare interactivity (inert on a static render):
df <- data.frame(wt = mtcars$wt, mpg = mtcars$mpg, model = rownames(mtcars))
vplot(df) |> mark_point(x = wt, y = mpg, tooltip = model, data_id = model)
```
