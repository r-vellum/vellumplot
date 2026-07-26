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
  inline = FALSE,
  recolor = NULL,
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

- inline:

  `TRUE` strips the leading `<?xml …?>` prolog so the `<svg>` is a valid
  fragment to drop straight into an HTML paragraph / cell / Quarto
  inline expression. `FALSE` (default) keeps the stand-alone-file
  prolog.

- recolor:

  Optional named character vector mapping a **source colour** to a
  replacement written verbatim into the SVG — e.g.
  `c(grey30 = "currentColor")` makes a `grey30` sparkline follow the
  surrounding text colour (dark-mode-adaptive inline). Each name is
  resolved to its hex and swapped for the value; use for CSS keywords
  (`"currentColor"`) or `var(--x)` that R's colour engine can't emit
  itself.

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
# a transparent, prolog-free, dark-mode-adaptive inline sparkline:
plot_svg(vsparkline(1:9, color = "grey30"),
  inline = TRUE, recolor = c(grey30 = "currentColor")
)
#> [1] "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"76\" height=\"23\" viewBox=\"0 0 76 23\" role=\"img\" aria-labelledby=\"vl3860-d\"><desc id=\"vl3860-d\">A sparkline plot. It plots .v (vertical axis) against .i (horizontal axis). Based on 9 observations.</desc><defs></defs><g data-vellum-panel=\"plot\"><g data-vellum-panel=\"panel-area\"><g data-vellum-panel=\"panel-1-1\"><g data-vellum-pan=\"panel-1-1\"><g data-vellum-id=\"layer-1-sparkline-g1\"><path d=\"M0 19.78 L9.5 17.71 L19 15.64 L28.5 13.57 L38 11.5 L47.5 9.43 L57 7.36 L66.5 5.29 L76 3.22\" fill=\"none\" stroke=\"currentColor\" stroke-opacity=\"1\" stroke-width=\"1\" stroke-linecap=\"round\" stroke-linejoin=\"round\" stroke-miterlimit=\"10\"/></g><g data-vellum-id=\"layer-1-sparkline-g2\"><path d=\"M1 0 Q0.99999994 0.41421354 0.70710677 0.70710677 Q0.41421354 0.99999994 0 1 Q-0.41421354 0.99999994 -0.70710677 0.70710677 Q-0.99999994 0.41421354 -1 0 Q-0.99999994 -0.41421354 -0.70710677 -0.70710677 Q-0.41421354 -0.99999994 0 -1 Q0.41421354 -0.99999994 0.70710677 -0.70710677 Q0.99999994 -0.41421354 1 0 Z\" fill=\"#b22222\" fill-opacity=\"1\" transform=\"matrix(5.6692915 0 0 5.6692915 0 19.78)\"/><path d=\"M1 0 Q0.99999994 0.41421354 0.70710677 0.70710677 Q0.41421354 0.99999994 0 1 Q-0.41421354 0.99999994 -0.70710677 0.70710677 Q-0.99999994 0.41421354 -1 0 Q-0.99999994 -0.41421354 -0.70710677 -0.70710677 Q-0.41421354 -0.99999994 0 -1 Q0.41421354 -0.99999994 0.70710677 -0.70710677 Q0.99999994 -0.41421354 1 0 Z\" fill=\"none\" stroke=\"#b22222\" stroke-opacity=\"1\" stroke-width=\"0.17638889\" stroke-linecap=\"round\" stroke-linejoin=\"round\" stroke-miterlimit=\"10\" transform=\"matrix(5.6692915 0 0 5.6692915 0 19.78)\"/><path d=\"M1 0 Q0.99999994 0.41421354 0.70710677 0.70710677 Q0.41421354 0.99999994 0 1 Q-0.41421354 0.99999994 -0.70710677 0.70710677 Q-0.99999994 0.41421354 -1 0 Q-0.99999994 -0.41421354 -0.70710677 -0.70710677 Q-0.41421354 -0.99999994 0 -1 Q0.41421354 -0.99999994 0.70710677 -0.70710677 Q0.99999994 -0.41421354 1 0 Z\" fill=\"#b22222\" fill-opacity=\"1\" transform=\"matrix(5.6692915 0 0 5.6692915 76 3.22)\"/><path d=\"M1 0 Q0.99999994 0.41421354 0.70710677 0.70710677 Q0.41421354 0.99999994 0 1 Q-0.41421354 0.99999994 -0.70710677 0.70710677 Q-0.99999994 0.41421354 -1 0 Q-0.99999994 -0.41421354 -0.70710677 -0.70710677 Q-0.41421354 -0.99999994 0 -1 Q0.41421354 -0.99999994 0.70710677 -0.70710677 Q0.99999994 -0.41421354 1 0 Z\" fill=\"none\" stroke=\"#b22222\" stroke-opacity=\"1\" stroke-width=\"0.17638889\" stroke-linecap=\"round\" stroke-linejoin=\"round\" stroke-miterlimit=\"10\" transform=\"matrix(5.6692915 0 0 5.6692915 76 3.22)\"/></g></g></g></g></g></svg>\n"
```
