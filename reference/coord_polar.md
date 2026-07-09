# Polar coordinates

`coord_polar()` projects the panel into polar space: one position
aesthetic becomes the angle and the other the radius. With `theta = "x"`
(the default) the x aesthetic maps to angle and y to radius — a
categorical bar chart becomes a wind-rose / coxcomb, a line becomes a
radar/spider trace, a point cloud is positioned by `(angle, radius)`.
With `theta = "y"` a stacked bar becomes a pie (see also the
[`mark_pie()`](https://r-vellum.github.io/vellumplot/reference/mark_pie.md)
/
[`mark_donut()`](https://r-vellum.github.io/vellumplot/reference/mark_pie.md)
shortcuts). The panel is locked to a square. Lines, areas, and ribbons
are interpolated into smooth arcs.

## Usage

``` r
coord_polar(plot, theta = "x", start = 0, direction = 1)
```

## Arguments

- plot:

  A
  [PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

- theta:

  Which position aesthetic drives the angle: `"x"` (default) or `"y"`.

- start:

  Angular offset of the zero position, in radians (`0` places the first
  value at twelve o'clock).

- direction:

  Winding direction: `1` for clockwise (default), `-1` for
  counter-clockwise.

## Value

The modified
[PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

## See also

[`mark_pie()`](https://r-vellum.github.io/vellumplot/reference/mark_pie.md),
[`mark_donut()`](https://r-vellum.github.io/vellumplot/reference/mark_pie.md)

## Examples

``` r
vplot(mtcars) |> mark_bar(x = factor(cyl)) |> coord_polar(theta = "x")
```
