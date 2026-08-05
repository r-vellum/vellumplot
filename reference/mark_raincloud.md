# Raincloud plot

`mark_raincloud()` is a convenience that composes a **raincloud**: a
one-sided density "cloud"
([`mark_halfeye()`](https://r-vellum.github.io/vellumplot/reference/mark_halfeye.md))
with the raw observations as "rain" below it
([`mark_point()`](https://r-vellum.github.io/vellumplot/reference/mark_point.md)
with sina spread). It is exactly
`plot |> mark_halfeye(...) |> mark_point(..., position = position_sina())`,
so any `color`/`fill` mapping in `...` flows to both layers.

## Usage

``` r
mark_raincloud(plot, x, y, ..., width = 0.4, alpha = 0.5)
```

## Arguments

- plot:

  A
  [PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

- x, y:

  The categorical `x` and the sample `y` (tidy-eval).

- ...:

  Further shared encodings (e.g. `color`, `fill`) forwarded to both the
  slab and the points.

- width:

  Sina spread of the rain, as a fraction of the band (default `0.4`).

- alpha:

  Point opacity for the rain (default `0.5`).

## Value

The modified
[PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

## See also

[`mark_halfeye()`](https://r-vellum.github.io/vellumplot/reference/mark_halfeye.md),
[`position_sina()`](https://r-vellum.github.io/vellumplot/reference/position.md)

## Examples

``` r
set.seed(1)
d <- data.frame(g = rep(c("a", "b", "c"), each = 100),
                y = rnorm(300, rep(c(0, 1, 2), each = 100)))
vplot(d) |> mark_raincloud(x = g, y = y)
```
