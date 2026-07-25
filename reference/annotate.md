# Add a one-off annotation

Draw a single mark (or a short vector of them) from values supplied
directly, rather than mapping a data column. The values become a small
inline layer `data` frame, so annotations are independent of the plot
data (and repeat on every facet panel). Supported `geom`s: `"text"`,
`"label"`, `"point"`, `"segment"`, `"rect"`, `"grob"`, and
`"sparkline"`.

## Usage

``` r
annotate(
  plot,
  geom,
  x = NULL,
  y = NULL,
  xend = NULL,
  yend = NULL,
  xmin = NULL,
  xmax = NULL,
  ymin = NULL,
  ymax = NULL,
  label = NULL,
  ...
)
```

## Arguments

- plot:

  A
  [PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

- geom:

  The annotation geometry: one of `"text"`, `"label"`, `"point"`,
  `"segment"`, `"rect"`, `"grob"`, `"sparkline"`.

- x, y:

  Position (text/label/point/grob/sparkline; segment start).

- xend, yend:

  Segment end.

- xmin, xmax, ymin, ymax:

  Rectangle extent.

- label:

  Text to draw (text/label).

- ...:

  Constant aesthetics passed to the mark (e.g. `color`, `fill`, `alpha`,
  `size`). For `"grob"`: `grob =` (a vellum grob or `PlotSpec`). For
  `"sparkline"`: `values =` plus any
  [`vsparkline()`](https://r-vellum.github.io/vellumplot/reference/vsparkline.md)
  argument. Both accept `width`/`height`/`units` (the box, default
  `20 x 6` mm) and `halign` (`"left"`/`"centre"`/`"right"`) / `valign`
  (`"top"`/`"centre"`/`"bottom"`) to anchor the box relative to
  `(x, y)`.

## Value

The modified
[PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

## Details

`"grob"` places an arbitrary vellum grob (or a
[PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md),
e.g. a
[`vsparkline()`](https://r-vellum.github.io/vellumplot/reference/vsparkline.md))
at data coordinate(s), in a box of physical size — a general "glyph /
chart in a panel" seam. `"sparkline"` is the convenience for the common
case: pass `values =` and it builds and places a
[`vsparkline()`](https://r-vellum.github.io/vellumplot/reference/vsparkline.md).

## Examples

``` r
vplot(mtcars) |>
  mark_point(x = wt, y = mpg) |>
  annotate("text", x = 4, y = 30, label = "note") |>
  annotate("rect", xmin = 3, xmax = 4, ymin = 15, ymax = 20, alpha = 0.2)

if (FALSE) { # \dontrun{
vplot(mtcars) |>
  mark_point(x = wt, y = mpg) |>
  annotate("sparkline", x = 4, y = 30, values = cumsum(rnorm(30)))
} # }
```
