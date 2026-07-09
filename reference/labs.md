# Set plot titles and axis/legend labels

Add plot-level text — a `title`, `subtitle`, `caption`, and `tag` — and
override the titles of individual axes and legends (`x`, `y`, `color`,
`size`). The title, subtitle, and tag are drawn in a band above the
panels; the caption in a band below. Repeated `labs()` calls merge, with
later values winning.

## Usage

``` r
labs(
  plot,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  tag = NULL,
  x = NULL,
  y = NULL,
  color = NULL,
  colour = NULL,
  fill = NULL,
  size = NULL,
  alt = NULL,
  ...
)
```

## Arguments

- plot:

  A
  [PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

- title, subtitle, caption, tag:

  Plot-level text (or `NULL` to leave unset).

- x, y, size:

  Axis / legend title overrides for those aesthetics.

- color, colour, fill:

  Colour-scale title override; `colour` and `fill` are aliases for
  `color`.

- alt:

  A text alternative (alt text) describing the plot for screen readers
  and other assistive technology. Overrides the description vellumplot
  generates automatically; see
  [`plot_alt()`](https://r-vellum.github.io/vellumplot/reference/plot_alt.md).
  Emitted into the accessible SVG (`<desc>`) and tagged PDF (Figure
  `Alt`) by
  [`render_plot()`](https://r-vellum.github.io/vellumplot/reference/render_plot.md).

- ...:

  Reserved; passing anything here is an error.

## Value

The modified
[PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

## Details

Axis/legend overrides set here are used unless a matching
`scale_*(name = )` is also given, which takes precedence. With neither,
the title is derived from the mapping.

## Examples

``` r
vplot(mtcars) |>
  mark_point(x = wt, y = mpg) |>
  labs(title = "Fuel economy", x = "Weight", y = "MPG")
```
