# Render a plot to a self-contained SVG string

`plot_svg()` compiles any vellumplot object (a
[PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md),
[`vtable()`](https://r-vellum.github.io/vellumplot/reference/vtable.md),
or a composition) to a stand-alone `<svg>` **string** rather than a file
— the way to embed a chart *inline* in HTML: a
[`vsparkline()`](https://r-vellum.github.io/vellumplot/reference/vsparkline.md)
in a `gt` / `reactable` / `DT` cell, a chart in a Quarto callout, or an
inline glyph in a report. The string carries a `viewBox`, so it scales.

## Usage

``` r
plot_svg(
  plot,
  width = NULL,
  height = NULL,
  scaling = c("fixed", "fit"),
  text = "native"
)
```

## Arguments

- plot:

  A
  [PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md),
  [`vtable()`](https://r-vellum.github.io/vellumplot/reference/vtable.md),
  or composition.

- width, height:

  Optional page-size override in **inches** (defaults to the object's
  own size).

- scaling:

  `"fixed"` (default) keeps the pixel size; `"fit"` sets the root
  `<svg>` `width`/`height` to `100%` so it fills its container (the
  `viewBox` preserves the aspect).

- text:

  How glyphs are emitted: `"native"` (default) or `"outline"` (paths,
  for maximum portability).

## Value

A length-1 character SVG string.

## See also

[`vsparkline()`](https://r-vellum.github.io/vellumplot/reference/vsparkline.md),
[`gt_vsparkline()`](https://r-vellum.github.io/vellumplot/reference/gt_vsparkline.md),
[`render_plot()`](https://r-vellum.github.io/vellumplot/reference/render_plot.md)

## Examples

``` r
svg <- plot_svg(vsparkline(cumsum(rnorm(20))))
substr(svg, 1, 40)
#> [1] "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<"
```
