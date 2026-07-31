# Render a plot to a file

Compiles a
[PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md)
into a
[`vellum::vl_scene()`](https://r-vellum.github.io/vellum/reference/vl_scene.html)
and writes it. The output format is taken from the file extension
(`.png`, `.svg`, `.pdf`).
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

  For SVG output, how text is written (see
  [`vellum::render()`](https://r-vellum.github.io/vellum/reference/vl_scene.html)).

- dpi:

  Output resolution in dots per inch. `NULL` (default) uses the plot's
  authored resolution (set on
  [`vplot()`](https://r-vellum.github.io/vellumplot/reference/vplot.md));
  a number overrides it for this render, so the PNG's pixel dimensions
  become `width * dpi` by `height * dpi`. Ignored for `.svg`/`.pdf`,
  which are resolution-independent.

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
  which add a redundant non-colour encoding). Simulation is raster-only;
  it is ignored for `.svg`/`.pdf`.

## Value

`path`, invisibly.

## Examples

``` r
p <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
f <- tempfile(fileext = ".png")
render_plot(p, f)
render_plot(p, f, dpi = 300) # denser raster, same physical size
render_plot(p, f, cvd = "deuteranopia") # preview for red-green colour blindness
```
