# Reserve an empty cell in a composition

Use inside
[`concat()`](https://r-vellum.github.io/vellumplot/reference/concat.md)
/
[`wrap_plots()`](https://r-vellum.github.io/vellumplot/reference/concat.md)
(or a `design`) to leave a gap.

## Usage

``` r
plot_spacer()
```

## Value

A spacer placeholder.

## Examples

``` r
a <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
concat(a, plot_spacer(), a, plot_spacer(), ncol = 2)
```
