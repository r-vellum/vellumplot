# Define a layout area for `design =`

An `area()` is a rectangular block of grid cells (1-based, inclusive)
that a sub-plot occupies. Pass a list of `area()`s (one per plot, in
order) as `concat(..., design = )` to place plots on an explicit,
possibly spanning, grid.

## Usage

``` r
area(t, l, b = t, r = l)
```

## Arguments

- t, l, b, r:

  Top, left, bottom, right cell indices (1-based, inclusive).

## Value

An `area` spec.

## Examples

``` r
a <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
b <- vplot(mtcars) |> mark_point(x = hp, y = mpg)
concat(a, b, design = list(area(1, 1, 1, 2), area(2, 1, 2, 1)))
```
