# Animate a plot across the states of a variable

`transition_states()` turns a plot into a keyframe animation: the plot
is compiled once per level of `states` (a "keyframe"), and
[`animate()`](https://r-vellum.github.io/vellumplot/reference/animate.md)
tweens the frames between successive keyframes. The scales are trained
**once over all states and frozen**, so axis breaks, colour ramps and
sizes stay put across the whole animation (the interpolation is
non-reactive: the states are fixed at author time, nothing retrains in
response to anything).

## Usage

``` r
transition_states(
  plot,
  states,
  transition_length = 1,
  state_length = 1,
  wrap = TRUE
)
```

## Arguments

- plot:

  A
  [PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md)
  (from
  [`vplot()`](https://r-vellum.github.io/vellumplot/reference/vplot.md)).

- states:

  A column (bare or a string) whose distinct values, in level order, are
  the animation's states.

- transition_length:

  Relative duration of the moving segments between states.

- state_length:

  Relative duration of the held pause on each state.

- wrap:

  Loop the last state back to the first?

## Value

The modified
[PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

## Details

The step only records the intent; call
[`animate()`](https://r-vellum.github.io/vellumplot/reference/animate.md)
to build the animation and
[`anim_save()`](https://r-vellum.github.io/vellumplot/reference/anim_save.md)
to write it. A plain (non-animated) render ignores it.

## See also

[`ease_aes()`](https://r-vellum.github.io/vellumplot/reference/ease_aes.md),
[`animate()`](https://r-vellum.github.io/vellumplot/reference/animate.md),
[`anim_save()`](https://r-vellum.github.io/vellumplot/reference/anim_save.md)

## Examples

``` r
if (requireNamespace("gapminder", quietly = TRUE)) {
  library(gapminder)
  p <- vplot(gapminder) |>
    mark_point(x = gdpPercap, y = lifeExp, size = pop, color = continent) |>
    scale_x_continuous(trans = "log10") |>
    transition_states(year)
}
```
