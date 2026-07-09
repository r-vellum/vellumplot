# Coordinate systems

`coord_cartesian()` is the default Cartesian system; pass `xlim`/`ylim`
to **zoom** the view (out-of-range marks are clipped, not dropped —
unlike a `scale_*(limits=)`, which here behaves the same but is the
data-scale's job). `coord_flip()` swaps the x and y axes, e.g. for
horizontal bars. `coord_fixed()` / `coord_equal()` lock the aspect ratio
so one data unit on the y axis occupies `ratio` times the physical
length of one unit on x (the panel shrinks to fit and is centred).
Coordinate limits take precedence over scale limits.

## Usage

``` r
coord_cartesian(plot, xlim = NULL, ylim = NULL)

coord_flip(plot, xlim = NULL, ylim = NULL)

coord_fixed(plot, ratio = 1, xlim = NULL, ylim = NULL)

coord_equal(plot, ratio = 1, xlim = NULL, ylim = NULL)
```

## Arguments

- plot:

  A
  [PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

- xlim, ylim:

  Length-2 view-window limits, or `NULL` to use the trained range.

- ratio:

  Aspect ratio for `coord_fixed()`: the device length of one y unit
  relative to one x unit (default `1`).

## Value

The modified
[PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

## Examples

``` r
vplot(mtcars) |> mark_point(x = wt, y = mpg) |> coord_cartesian(xlim = c(2, 4))

vplot(mtcars) |> mark_bar(x = factor(cyl)) |> coord_flip()

vplot(mtcars) |> mark_point(x = wt, y = mpg) |> coord_fixed()
```
