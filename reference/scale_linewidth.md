# Line-width scale

Declare the scale for a mapped `linewidth` aesthetic on a stroked mark –
[`mark_line()`](https://r-vellum.github.io/vellumplot/reference/mark_point.md),
[`mark_step()`](https://r-vellum.github.io/vellumplot/reference/mark_area.md),
or
[`mark_segment()`](https://r-vellum.github.io/vellumplot/reference/mark_segment.md).
Data values are rescaled linearly onto a line-width `range` (in `lwd`
units, as everywhere else in the package) and a line-width legend is
drawn automatically.

## Usage

``` r
scale_linewidth(plot, range = NULL, limits = NULL, breaks = NULL, name = NULL)

scale_linewidth_continuous(
  plot,
  range = NULL,
  limits = NULL,
  breaks = NULL,
  name = NULL
)
```

## Arguments

- plot:

  A
  [PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

- range:

  Output line-width range `c(min, max)`, or `NULL` for the default
  `c(0.5, 4)`.

- limits:

  Data limits `c(min, max)`, or `NULL` to train from the data.

- breaks:

  Explicit legend breaks, or `NULL`.

- name:

  Legend title, or `NULL` to derive from the encoding.

## Value

The modified
[PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

## Details

On
[`mark_line()`](https://r-vellum.github.io/vellumplot/reference/mark_point.md)
and
[`mark_step()`](https://r-vellum.github.io/vellumplot/reference/mark_area.md),
a mapped `linewidth` draws each series as **one continuous stroke whose
width tapers smoothly** between the values at its vertices (via vellum's
`lines_grob(lwd_profile = )`). The stroke is built as a filled outline
at render time, so it looks the same in PNG, SVG and PDF and stays one
element per series in
[`vellum::scene_model()`](https://r-vellum.github.io/vellum/reference/scene_model.html)
and the SVG. A constant `linewidth =` is unaffected and still draws an
ordinary stroke.

A tapered stroke has one colour and no dash pattern, so a series falls
back to **one segment per span** – each at the mean of its two endpoint
widths, so the width steps at every vertex – when it also maps a
continuous colour (each segment then takes its own colour), uses a
dashed or dotted `linetype`, is drawn with a sketch theme, or has a
missing width. That fallback emits one path element per segment, worth
knowing before combining those with tens of thousands of rows.
[`mark_segment()`](https://r-vellum.github.io/vellumplot/reference/mark_segment.md)
is already one element per row and takes one width per segment.

For edges on a
[`vgraph()`](https://r-vellum.github.io/vellumplot/reference/vgraph.md)
plot, use
[`scale_edge_width()`](https://r-vellum.github.io/vellumplot/reference/scale_edge_width.md)
instead: an edge's width is trained and legended independently of the
line-width scale. That also means `guides(linewidth = )` and
`lims(linewidth = )` address *this* scale; use `guides(edge_width = )` /
`lims(edge_width = )` for a graph's edges.

A missing (`NA`) width takes its segment's width from whichever endpoint
is known, so one missing value thins the line rather than erasing the
two segments that meet at it. A segment missing a width at both ends is
dropped. (A series with a missing width is drawn per segment, as above.)

## See also

[`mark_line()`](https://r-vellum.github.io/vellumplot/reference/mark_point.md),
[`mark_step()`](https://r-vellum.github.io/vellumplot/reference/mark_area.md),
[`mark_segment()`](https://r-vellum.github.io/vellumplot/reference/mark_segment.md),
[`scale_edge_width()`](https://r-vellum.github.io/vellumplot/reference/scale_edge_width.md)

## Examples

``` r
vplot(pressure) |>
  mark_line(x = temperature, y = pressure, linewidth = pressure) |>
  scale_linewidth(range = c(0.5, 5))
```
