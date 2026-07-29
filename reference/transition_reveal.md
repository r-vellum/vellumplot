# Reveal a plot progressively along a variable

`transition_reveal()` wipes the plot into view left to right — the
classic "line draws itself" animation. Unlike
[`transition_states()`](https://r-vellum.github.io/vellumplot/reference/transition_states.md),
it does not tween between states: the full plot is compiled and a clip
rectangle grows across the panel, revealing the marks in `along` order.
Best for a line or path over a continuous `along` (typically the x
variable, e.g. time).

## Usage

``` r
transition_reveal(plot, along)
```

## Arguments

- plot:

  A
  [PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md)
  (from
  [`vplot()`](https://r-vellum.github.io/vellumplot/reference/vplot.md)).

- along:

  The variable the reveal follows (mapped to x, increasing left to
  right).

## Value

The modified
[PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

## See also

[`transition_states()`](https://r-vellum.github.io/vellumplot/reference/transition_states.md),
[`transition_time()`](https://r-vellum.github.io/vellumplot/reference/transition_time.md),
[`animate()`](https://r-vellum.github.io/vellumplot/reference/animate.md)

## Examples

``` r
vplot(data.frame(t = 1:20, y = cumsum(rnorm(20)))) |>
  mark_line(x = t, y = y) |>
  transition_reveal(t)
```
