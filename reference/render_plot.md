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
render_plot(plot, path, text = "native", dpi = NULL)
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

## Value

`path`, invisibly.

## Examples

``` r
p <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
f <- tempfile(fileext = ".png")
render_plot(p, f)
render_plot(p, f, dpi = 300) # denser raster, same physical size
```
