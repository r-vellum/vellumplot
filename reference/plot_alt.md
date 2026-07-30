# Text alternative (alt text) for a plot

Returns the description used as the plot's text alternative for
assistive technology. If
[`labs()`](https://r-vellum.github.io/vellumplot/reference/labs.md) set
an explicit `alt`, that string is returned verbatim; otherwise
vellumplot generates a prose summary from the specification — the chart
type, what each axis / colour / size encodes, the number of
observations, and any faceting.

## Usage

``` r
plot_alt(x)
```

## Arguments

- x:

  A
  [PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md)
  or a plot composition.

## Value

A single string.

## Details

[`render_plot()`](https://r-vellum.github.io/vellumplot/reference/render_plot.md)
(and any compile through
[`vellum::as_vellum_scene()`](https://r-vellum.github.io/vellum//reference/as_vellum_scene.html))
passes this text to the scene's description, so the exported **SVG**
carries it in `<desc>` (with `role="img"`) and the exported **PDF** tags
the chart as a `Figure` whose `Alt` is this text. The plot **title**
(from `labs(title=)`) becomes the accessible name. See the
*Accessibility* article.

## See also

[`labs()`](https://r-vellum.github.io/vellumplot/reference/labs.md),
[`vellum::describe()`](https://r-vellum.github.io/vellum//reference/describe.html)

## Examples

``` r
p <- vplot(mtcars) |> mark_point(x = wt, y = mpg, color = hp)
plot_alt(p)
#> [1] "A scatter plot. It plots mpg (vertical axis) against wt (horizontal axis), where colour shows hp. Based on 32 observations."
plot_alt(labs(p, alt = "Heavier cars get fewer miles per gallon."))
#> [1] "Heavier cars get fewer miles per gallon."
```
