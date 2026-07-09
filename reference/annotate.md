# Add a one-off annotation

Draw a single mark (or a short vector of them) from values supplied
directly, rather than mapping a data column. The values become a small
inline layer `data` frame, so annotations are independent of the plot
data (and repeat on every facet panel). Supported `geom`s: `"text"`,
`"label"`, `"point"`, `"segment"`, and `"rect"`.

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
  `"segment"`, `"rect"`.

- x, y:

  Position (text/label/point; segment start).

- xend, yend:

  Segment end.

- xmin, xmax, ymin, ymax:

  Rectangle extent.

- label:

  Text to draw (text/label).

- ...:

  Constant aesthetics passed to the mark (e.g. `color`, `fill`, `alpha`,
  `size`).

## Value

The modified
[PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

## Examples

``` r
vplot(mtcars) |>
  mark_point(x = wt, y = mpg) |>
  annotate("text", x = 4, y = 30, label = "note") |>
  annotate("rect", xmin = 3, xmax = 4, ymin = 15, ymax = 20, alpha = 0.2)
```
