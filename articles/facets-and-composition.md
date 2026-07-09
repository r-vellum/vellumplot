# Facets and composition

There are two different reasons to end up with a grid of panels, and
vellumplot keeps them separate. *Faceting* takes one plot and splits it
by a variable, so every panel shares the same encodings and (by default)
the same scales. *Composition* takes several independent plots, each its
own spec, and arranges them into a single figure. Use faceting for small
multiples of the same view; use composition for a dashboard of different
views.

## Faceting

[`facet_wrap()`](https://r-vellum.github.io/vellumplot/reference/facet_wrap.md)
lays panels out in a ribbon wrapped to `ncol` or `nrow`.

``` r

vplot(mtcars) |>
  mark_point(x = wt, y = mpg) |>
  facet_wrap(~cyl, ncol = 3)
```

![](facets-and-composition_files/figure-html/unnamed-chunk-2-1.png)

[`facet_grid()`](https://r-vellum.github.io/vellumplot/reference/facet_wrap.md)
puts panels on a 2-D grid defined by a `rows ~ cols` formula, which is
the right tool when you are crossing two variables.

``` r

vplot(mtcars) |>
  mark_point(x = wt, y = mpg) |>
  facet_grid(vs ~ am)
```

![](facets-and-composition_files/figure-html/unnamed-chunk-3-1.png)

### Shared or free scales

By default position scales are shared across panels so the axes line up
and panels are comparable. When panels cover very different ranges, free
them per panel with `scales = "free_x"`, `"free_y"`, or `"free"`.

``` r

vplot(mtcars) |>
  mark_point(x = wt, y = mpg) |>
  facet_wrap(~cyl, scales = "free")
```

![](facets-and-composition_files/figure-html/unnamed-chunk-4-1.png)

`scales = "free_y"` is sugar for the underlying
[`resolve_scale()`](https://r-vellum.github.io/vellumplot/reference/resolve_scale.md)
lattice, which lets you set the policy per aesthetic directly. These two
are equivalent:

``` r

vplot(mtcars) |> mark_point(x = wt, y = mpg) |>
  facet_wrap(~cyl, scales = "free_y")

vplot(mtcars) |> mark_point(x = wt, y = mpg) |>
  facet_wrap(~cyl) |> resolve_scale(y = "independent")
```

Colour and size scales (and their legends) stay shared across panels, so
one legend describes the whole figure.

## Composition

To place *different* plots side by side, build each one and combine
them.
[`concat()`](https://r-vellum.github.io/vellumplot/reference/concat.md)
(and its friends
[`hconcat()`](https://r-vellum.github.io/vellumplot/reference/concat.md)
and
[`vconcat()`](https://r-vellum.github.io/vellumplot/reference/concat.md))
arrange plots into a grid; by default they collect the legends into one
shared guide.

``` r

p1 <- vplot(mtcars) |> mark_point(x = wt, y = mpg, color = factor(cyl))
p2 <- vplot(mtcars) |> mark_boxplot(x = factor(cyl), y = mpg)
p3 <- vplot(mtcars) |> mark_histogram(x = mpg, bins = 10)

concat(p1, p2, p3, ncol = 3)
```

![](facets-and-composition_files/figure-html/unnamed-chunk-6-1.png)

Control the shape with `ncol`/`nrow`, the relative sizes with
`widths`/`heights`, and irregular layouts with a `design` string.
[`plot_spacer()`](https://r-vellum.github.io/vellumplot/reference/plot_spacer.md)
drops an empty cell, and
[`wrap_plots()`](https://r-vellum.github.io/vellumplot/reference/concat.md)
takes a list of plots when you have assembled them programmatically.

``` r

concat(p1, p2, ncol = 1, heights = c(2, 1))
```

![](facets-and-composition_files/figure-html/unnamed-chunk-7-1.png)

### Insets

[`inset()`](https://r-vellum.github.io/vellumplot/reference/inset.md)
floats one plot over another, positioned by fractional coordinates of a
reference box. Good for an overview-plus-detail figure.

``` r

main <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
mini <- vplot(mtcars) |> mark_histogram(x = mpg, bins = 8)
inset(main, mini, left = 0.6, bottom = 0.6, right = 0.98, top = 0.98)
```

![](facets-and-composition_files/figure-html/unnamed-chunk-8-1.png)

### Repeat over a variable

[`repeat_()`](https://r-vellum.github.io/vellumplot/reference/repeat_.md)
builds a small multiple by repeating one plot template across a set of
values, the composition-side counterpart to faceting when you want each
panel to be a fully independent plot rather than a shared-scale small
multiple.

Faceting and composition also compose with each other: a faceted plot is
still a spec, so it can go straight into a
[`concat()`](https://r-vellum.github.io/vellumplot/reference/concat.md).
Once your figure looks right, send it to a file the same way as any
plot, with
[`render_plot()`](https://r-vellum.github.io/vellumplot/reference/render_plot.md)
(see **[Get
started](https://r-vellum.github.io/vellumplot/articles/vellumplot.md)**).
