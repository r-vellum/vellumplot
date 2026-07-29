# A reproducibility manifest for a plot

`plot_manifest()` returns a small, serializable fingerprint of a plot: a
hash of its input data (order- and column-sensitive), the data's shape
and column names, a structural hash of the spec, and the number of
emitted elements. It is what
[`plot_svg()`](https://r-vellum.github.io/vellumplot/reference/plot_svg.md)
can embed (with `manifest = TRUE`) so a figure carries its own
provenance, and what
[`plot_verify()`](https://r-vellum.github.io/vellumplot/reference/plot_verify.md)
recomputes to confirm a figure still matches its data.

## Usage

``` r
plot_manifest(x)
```

## Arguments

- x:

  A
  [PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

## Value

A named list: `version`, `data` (a `hash`/`nrow`/`columns` record),
`spec_hash`, and `n_elements`.

## See also

[`plot_verify()`](https://r-vellum.github.io/vellumplot/reference/plot_verify.md),
[`plot_svg()`](https://r-vellum.github.io/vellumplot/reference/plot_svg.md),
[`provenance_join()`](https://r-vellum.github.io/vellumplot/reference/provenance_join.md)

## Examples

``` r
plot_manifest(vplot(mtcars) |> mark_point(x = wt, y = mpg))$data$hash
#> [1] "33f1aa2876d0ad2a1232bcd25a9a350e"
```
