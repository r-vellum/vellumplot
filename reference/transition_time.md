# Animate a plot along a continuous time

`transition_time()` is like
[`transition_states()`](https://r-vellum.github.io/vellumplot/reference/transition_states.md)
but treats `time` as a continuous quantity: the states are its distinct
values and each transition is allocated frames **in proportion to its
time gap**, so the animation plays at a constant rate through unevenly
spaced times. There is no pause on a state and no wrap — time flows
once, start to finish.

## Usage

``` r
transition_time(plot, time)
```

## Arguments

- plot:

  A
  [PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md)
  (from
  [`vplot()`](https://r-vellum.github.io/vellumplot/reference/vplot.md)).

- time:

  A numeric column giving each row's time.

## Value

The modified
[PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

## See also

[`transition_states()`](https://r-vellum.github.io/vellumplot/reference/transition_states.md),
[`ease_aes()`](https://r-vellum.github.io/vellumplot/reference/ease_aes.md),
[`animate()`](https://r-vellum.github.io/vellumplot/reference/animate.md)

## Examples

``` r
if (requireNamespace("gapminder", quietly = TRUE)) {
  vplot(gapminder::gapminder) |>
    mark_point(x = gdpPercap, y = lifeExp, size = pop, color = continent) |>
    scale_x_continuous(trans = "log10") |>
    transition_time(year)
}
```
