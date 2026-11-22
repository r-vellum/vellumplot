# Reveal a plot progressively along a variable

`transition_reveal()` draws the data in, in the order of `along` – the
classic "line draws itself" animation, as in gganimate. Each frame shows
the rows whose `along` value has been reached: a point appears when the
reveal reaches it, and a line, path, area, ribbon or step grows through
its rows with an interpolated leading point, so it advances smoothly
between data points rather than jumping from one to the next.

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

  The variable the reveal follows: numeric, `Date` or `POSIXct` (often
  time, but any ordering column, such as a path's parameter).

## Value

The modified
[PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

## Details

The reveal follows `along`, not the screen: a spiral revealed along its
parameter winds outwards, a
[`coord_flip()`](https://r-vellum.github.io/vellumplot/reference/coord_cartesian.md)
series grows up the vertical axis, and a reversed scale grows from the
right. Only the data layers change; the panel background, gridlines,
axes and legends are drawn whole on every frame, and the scales are
trained once over all the data and frozen, so nothing moves as the data
arrive. A layer whose own `data` lacks the `along` variable (a reference
line, say) is drawn whole throughout.

`animate(nframes = )` frames are spread evenly over the range of `along`
(the
[`ease_aes()`](https://r-vellum.github.io/vellumplot/reference/ease_aes.md)
position easing shapes the pace). Each frame is compiled from its own
rows, so a statistic (a smooth) is recomputed on the data revealed so
far. The leading point is interpolated on the data values, before any
scale transformation, and among the rows of each group (the `group`
aesthetic, else the layer's discrete colour/fill/linetype/... mappings).

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
