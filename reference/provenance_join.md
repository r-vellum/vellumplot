# Join a plot's provenance to its rendered geometry

`provenance_join()` compiles `x` once and returns a tidy table with one
row per emitted mark element, tying each drawn primitive to **both** the
source data rows that produced it (`rows`, a list column) **and** its
device-pixel geometry (`x0`, `y0`, `x1`, `y1`, `x`, `y`, `w`, `h`) from
[`vellum::scene_model()`](https://r-vellum.github.io/vellum/reference/scene_model.html).
It is the consumer of the provenance table that
[`plot_provenance()`](https://r-vellum.github.io/vellumplot/reference/plot_provenance.md)
emits — the substrate for click-to-source interactivity, auditing "which
rows made this element?", and linked views.

## Usage

``` r
provenance_join(x)
```

## Arguments

- x:

  A
  [PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md)
  or composition.

## Value

A data frame: `id`, `layer`, `mark`, `kind`, `panel`, the pixel bbox
(`x0`, `y0`, `x1`, `y1`, `x`, `y`, `w`, `h`), `n_rows`, and `rows` (a
list column of integer source-row indices). Empty for a plot with no
mark grobs.

## See also

[`plot_provenance()`](https://r-vellum.github.io/vellumplot/reference/plot_provenance.md),
[`vellum::scene_model()`](https://r-vellum.github.io/vellum/reference/scene_model.html)

## Examples

``` r
df <- data.frame(wt = mtcars$wt, mpg = mtcars$mpg, cyl = factor(mtcars$cyl))
p <- vplot(df) |> mark_point(x = wt, y = mpg, color = cyl)
pj <- provenance_join(p)
pj[, c("id", "mark", "n_rows")]
#>                 id  mark n_rows
#> 1 layer-1-point-g1 point      7
#> 2 layer-1-point-g2 point     11
#> 3 layer-1-point-g3 point     14
```
