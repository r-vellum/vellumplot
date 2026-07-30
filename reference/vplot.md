# Start a plot specification

`vplot()` begins a declarative, pipe-first plot. It captures the data
and page size and returns an inspectable
[PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md);
nothing is drawn until the spec is compiled (via
[`render_plot()`](https://r-vellum.github.io/vellumplot/reference/render_plot.md)
or
[`vellum::as_vellum_scene()`](https://r-vellum.github.io/vellum//reference/as_vellum_scene.html)).
Build the plot up with
[`mark_point()`](https://r-vellum.github.io/vellumplot/reference/mark_point.md)
/
[`mark_line()`](https://r-vellum.github.io/vellumplot/reference/mark_point.md)
/
[`mark_rule()`](https://r-vellum.github.io/vellumplot/reference/mark_point.md)
and the `scale_*()` functions.

## Usage

``` r
vplot(data, width = 6, height = 4, dpi = 96)
```

## Arguments

- data:

  A data frame. Encoding expressions in `mark_*()` are evaluated against
  it with tidy evaluation.

- width, height:

  Page size in inches.

- dpi:

  Output resolution in dots per inch. The exported PNG's pixel
  dimensions are `width * dpi` by `height * dpi`; raising `dpi` yields a
  denser image at the same physical size. Overridable at render time via
  [`render_plot()`](https://r-vellum.github.io/vellumplot/reference/render_plot.md)'s
  `dpi` argument.

## Value

A
[PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

## Examples

``` r
vplot(mtcars) |> mark_point(x = wt, y = mpg)
```
