# Encode a plot as a data URI

`plot_data_uri()` renders a plot to a self-contained **`data:` URI**
string — the bytes of the image inlined as base64 — ready to drop into
an HTML `<img src>`, a Markdown image, an email, or anywhere a URL is
expected without a separate file. `"svg"` (default) inlines the crisp
vector SVG; `"png"` inlines a raster render.

## Usage

``` r
plot_data_uri(
  plot,
  format = c("svg", "png"),
  width = NULL,
  height = NULL,
  dpi = NULL,
  ...
)
```

## Arguments

- plot:

  A
  [PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md)
  (or any object
  [`plot_svg()`](https://r-vellum.github.io/vellumplot/reference/plot_svg.md)
  /
  [`render_plot()`](https://r-vellum.github.io/vellumplot/reference/render_plot.md)
  accepts).

- format:

  `"svg"` (vector, default) or `"png"` (raster).

- width, height:

  Optional size override in inches.

- dpi:

  For `format = "png"`, the render resolution.

- ...:

  Passed to
  [`plot_svg()`](https://r-vellum.github.io/vellumplot/reference/plot_svg.md)
  (svg) or
  [`render_plot()`](https://r-vellum.github.io/vellumplot/reference/render_plot.md)
  (png).

## Value

A length-1 character `data:` URI.

## See also

[`plot_svg()`](https://r-vellum.github.io/vellumplot/reference/plot_svg.md),
[`render_plot()`](https://r-vellum.github.io/vellumplot/reference/render_plot.md)

## Examples

``` r
uri <- vplot(mtcars) |> mark_point(x = wt, y = mpg) |> plot_data_uri()
substr(uri, 1, 30)
#> [1] "data:image/svg+xml;base64,PD94"
```
