# Colour scales

Declare a colour scale for the `color`/`fill` channel. Continuous data
get a perceptual ramp; discrete data get a qualitative palette.
`scale_*_manual()` sets exact colours, `scale_*_gradient()` a two-point
ramp. The `fill` variants are identical (colour and fill share one
scale). A legend is drawn automatically when colour is mapped.

## Usage

``` r
scale_color_continuous(
  plot,
  palette = NULL,
  breaks = NULL,
  labels = NULL,
  name = NULL
)

scale_color_discrete(
  plot,
  palette = NULL,
  breaks = NULL,
  labels = NULL,
  name = NULL
)

scale_color_manual(plot, values, name = NULL)

scale_color_gradient(plot, low = "#132B43", high = "#56B1F7", name = NULL)

scale_fill_continuous(
  plot,
  palette = NULL,
  breaks = NULL,
  labels = NULL,
  name = NULL
)

scale_fill_discrete(
  plot,
  palette = NULL,
  breaks = NULL,
  labels = NULL,
  name = NULL
)

scale_fill_manual(plot, values, name = NULL)

scale_fill_gradient(plot, low = "#132B43", high = "#56B1F7", name = NULL)

scale_colour_continuous(
  plot,
  palette = NULL,
  breaks = NULL,
  labels = NULL,
  name = NULL
)

scale_colour_discrete(
  plot,
  palette = NULL,
  breaks = NULL,
  labels = NULL,
  name = NULL
)

scale_colour_manual(plot, values, name = NULL)

scale_colour_gradient(plot, low = "#132B43", high = "#56B1F7", name = NULL)
```

## Arguments

- plot:

  A
  [PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

- palette:

  A vector of colours, or a single palette name passed to
  [`grDevices::hcl.colors()`](https://rdrr.io/r/grDevices/palettes.html)
  (e.g. `"Batlow"`, `"Blues"`, `"Set 2"`; matched
  case/space-insensitively). `NULL` uses a sensible default.

- breaks, labels:

  Explicit legend breaks / labels, or `NULL`.

- name:

  Legend title, or `NULL` to derive from the encoding.

- values:

  For `scale_*_manual()`, a vector of colours; if named, matched to data
  levels by name (unmatched levels draw grey).

- low, high:

  For `scale_*_gradient()`, the endpoint colours.

## Value

The modified
[PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

## Examples

``` r
vplot(mtcars) |>
  mark_point(x = wt, y = mpg, color = hp) |>
  scale_color_continuous(palette = "Batlow")
```
