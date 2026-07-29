# Set the easing of an animation's frames

`ease_aes()` chooses the easing function that shapes how the
interpolation fraction moves through each transition — e.g.
`"cubic-in-out"` starts and ends gently. Applies to every interpolated
aesthetic.

## Usage

``` r
ease_aes(plot, ease = "linear")
```

## Arguments

- plot:

  A
  [PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md)
  carrying a
  [`transition_states()`](https://r-vellum.github.io/vellumplot/reference/transition_states.md).

- ease:

  An easing name: `"linear"`, or a family
  (`quad`/`cubic`/`quart`/`quint`/`sine`/`expo`/`circ`/`back`/`elastic`/`bounce`)
  with a direction suffix (`-in`, `-out`, `-in-out`), e.g.
  `"cubic-in-out"`.

## Value

The modified
[PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

## See also

[`transition_states()`](https://r-vellum.github.io/vellumplot/reference/transition_states.md),
[`animate()`](https://r-vellum.github.io/vellumplot/reference/animate.md)

## Examples

``` r
vplot(mtcars) |>
  mark_point(x = wt, y = mpg) |>
  transition_states(cyl) |>
  ease_aes("cubic-in-out")
```
