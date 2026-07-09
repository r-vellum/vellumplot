# Get started

vellumplot is a declarative, pipe-first grammar of graphics built on the
[vellum](https://r-vellum.github.io/vellum/) backend. You describe a
plot as an inspectable, serializable *spec*; nothing is drawn until the
spec is compiled into a vellum scene and rendered.

## Install

vellumplot compiles a Rust crate (inside `vellum`), so you need a Rust
toolchain (`cargo`/`rustc`) on your machine alongside R. With that in
place:

``` r

# install.packages("pak")
pak::pak("r-vellum/vellumplot")
```

## A first plot

Start with
[`vplot()`](https://r-vellum.github.io/vellumplot/reference/vplot.md) on
a data frame, then add a mark. Encodings (`x`, `y`, `color`, …) are
tidy-eval expressions evaluated against the data.

``` r

library(vellumplot)

vplot(mtcars) |>
  mark_point(x = wt, y = mpg, color = hp) |>
  scale_color_continuous()
```

![](vellumplot_files/figure-html/unnamed-chunk-3-1.png)

Printing a spec draws it into the plots pane (and embeds in a
knitr/Quarto chunk), like ggplot2.

## Layering marks

Add more marks to the same panel; scales train across every layer.

``` r

vplot(mtcars) |>
  mark_point(x = wt, y = mpg) |>
  mark_smooth(x = wt, y = mpg)
```

![](vellumplot_files/figure-html/unnamed-chunk-4-1.png)

## Faceting

Split into a grid of panels with shared or free scales.

``` r

vplot(mtcars) |>
  mark_point(x = wt, y = mpg) |>
  facet_wrap(~cyl)
```

![](vellumplot_files/figure-html/unnamed-chunk-5-1.png)

## The spec is just data

Nothing is drawn until the spec is compiled.
[`summary()`](https://rdrr.io/r/base/summary.html) shows its structure
without rendering.

``` r

summary(vplot(mtcars) |> mark_point(x = wt, y = mpg, color = hp))
#> <PlotSpec> 32x11 (11 columns), page 6x4 in
#> 
#> ── layers
#> • mark_point(x = wt, y = mpg, color = hp)
```

## Rendering to a file

[`render_plot()`](https://r-vellum.github.io/vellumplot/reference/render_plot.md)
writes the compiled scene; the format follows the file extension.

``` r

p <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
render_plot(p, "cars.png")
```

## Where to next

- **[Reference](https://r-vellum.github.io/vellumplot/reference/index.md)**:
  every mark, scale, coord, facet, theme, and effect.
- **[vellum](https://r-vellum.github.io/vellum/)**: the graphics backend
  vellumplot compiles into.
- **[vellumwidget](https://r-vellum.github.io/vellumwidget/)**: turn a
  scene into an interactive HTML widget.
- **[vellumverse](https://r-vellum.github.io/vellumverse/)**: install
  and attach the whole ecosystem in one step.
