# Significance brackets

`mark_signif()` runs a pairwise test between `x` groups and draws a
significance **bracket** over each comparison with its p-value (or
stars) — the ggsignif / ggpubr idiom. Add it on top of a
[`mark_boxplot()`](https://r-vellum.github.io/vellumplot/reference/mark_boxplot.md)
/
[`mark_violin()`](https://r-vellum.github.io/vellumplot/reference/mark_violin.md);
the brackets stack above the data and the y-axis expands to fit them.

## Usage

``` r
mark_signif(
  plot,
  ...,
  position = "identity",
  comparisons = NULL,
  method = c("wilcox.test", "t.test"),
  label = c("p", "stars"),
  step = 0.12,
  tip_length = 0.03,
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

  Encodings (tidy-eval): a categorical `x` and the numeric `y`.

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

- comparisons:

  A list of length-2 character vectors naming the group pairs to test,
  e.g. `list(c("a", "b"), c("b", "c"))`. `NULL` (default) tests each
  adjacent pair of levels.

- method:

  The two-sample test: `"wilcox.test"` (default) or `"t.test"`.

- label:

  `"p"` (default, a formatted p-value) or `"stars"`
  (`*`/`**`/`***`/`****`/`ns`).

- step:

  Vertical gap between stacked brackets, as a fraction of the data's y
  range (default `0.12`).

- tip_length:

  Bracket down-tick length, as a fraction of the y range (default
  `0.03`).

- blend:

  Optional blend mode for compositing this layer against what is already
  drawn beneath it (the panel and earlier layers), one of the CSS
  `mix-blend-mode` names, e.g. `"multiply"`, `"screen"`, `"darken"`. The
  whole layer composites as one isolated group (not per element).

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

## Value

The modified
[PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

## Examples

``` r
set.seed(1)
d <- data.frame(g = rep(c("a", "b", "c"), each = 30),
                y = c(rnorm(30), rnorm(30, 1), rnorm(30, 0.4)))
vplot(d) |>
  mark_boxplot(x = g, y = y) |>
  mark_signif(x = g, y = y, comparisons = list(c("a", "b"), c("a", "c")))
```
