# Render a plot to a file

Compiles a
[PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md)
into a
[`vellum::vl_scene()`](https://r-vellum.github.io/vellum/reference/vl_scene.html)
and writes it. The output format is taken from the file extension
(`.png`, `.svg`, `.pdf` or `.pptx`).
[`vellum::render()`](https://r-vellum.github.io/vellum/reference/vl_scene.html)
also works on a plot directly, dispatching through the
`as_vellum_scene()` seam.

## Usage

``` r
render_plot(plot, path, text = "native", dpi = NULL, cvd = "none")
```

## Arguments

- plot:

  A
  [PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

- path:

  Output file path.

- text:

  For SVG and PowerPoint output, how text is written: `"native"`
  (default) for selectable, editable text, `"outline"` for glyph
  outlines (see
  [`vellum::render()`](https://r-vellum.github.io/vellum/reference/vl_scene.html)).
  Ignored for PNG and PDF.

- dpi:

  Output resolution in dots per inch. `NULL` (default) uses the plot's
  authored resolution (set on
  [`vplot()`](https://r-vellum.github.io/vellumplot/reference/vplot.md));
  a number overrides it for this render, so the PNG's pixel dimensions
  become `width * dpi` by `height * dpi`. Ignored for `.svg`, `.pdf` and
  `.pptx`, which are resolution-independent.

- cvd:

  Colour-vision-deficiency simulation for a `.png` render: one of
  `"none"` (default), `"protanopia"`, `"deuteranopia"`, `"tritanopia"`,
  or `"achromatopsia"`. Use it to preview how a palette reads for a
  colour-blind viewer (pair with
  [`plot_lint()`](https://r-vellum.github.io/vellumplot/reference/plot_lint.md),
  which flags low contrast, and
  [`scale_pattern()`](https://r-vellum.github.io/vellumplot/reference/scale_pattern.md)
  /
  [`pattern_hatch()`](https://r-vellum.github.io/vellumplot/reference/pattern_hatch.md),
  which add a redundant non-colour encoding). Simulation is raster-only:
  a `.svg`, `.pdf` or `.pptx` is written unsimulated, with a warning.

## Value

`path`, invisibly.

## Details

A `.pptx` path writes a one-slide PowerPoint file of editable shapes at
the plot's size (needs vellum 0.18.0 or later; no other package). For
several slides use
[`vellum::pptx_pages()`](https://r-vellum.github.io/vellum/reference/pptx_pages.html),
and to place a plot on a slide of an officer deck wrap it in
[`vellum::as_dml()`](https://r-vellum.github.io/vellum/reference/as_dml.html)
for `officer::ph_with()`.

## Examples

``` r
p <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
f <- tempfile(fileext = ".png")
render_plot(p, f)
render_plot(p, f, dpi = 300) # denser raster, same physical size
render_plot(p, f, cvd = "deuteranopia") # preview for red-green colour blindness
```
