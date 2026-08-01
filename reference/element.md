# Theme elements

Typed building blocks for
[`theme()`](https://r-vellum.github.io/vellumplot/reference/theme_gray.md).
Each describes how a family of theme slots is drawn; any property left
`NULL` is inherited from the slot's parent in the theme tree.
`element_blank()` draws nothing.

## Usage

``` r
element_text(
  family = NULL,
  face = NULL,
  colour = NULL,
  color = NULL,
  size = NULL,
  cex = NULL,
  hjust = NULL,
  vjust = NULL,
  angle = NULL,
  lineheight = NULL,
  margin = NULL
)

element_line(
  colour = NULL,
  color = NULL,
  linewidth = NULL,
  linetype = NULL,
  lineend = NULL,
  sketch = NULL
)

element_rect(
  fill = NULL,
  colour = NULL,
  color = NULL,
  linewidth = NULL,
  linetype = NULL,
  sketch = NULL
)

element_blank()
```

## Arguments

- family, face, size, cex, colour, color, hjust, vjust, angle,
  lineheight, margin:

  Text properties. `color` is an alias for `colour`; `size` is in points
  and `cex` is a relative multiplier on the inherited `size`; `margin`
  is a numeric vector of millimetres (recycled to length 4).

- linewidth, linetype, lineend:

  Line properties.

- sketch:

  For `element_line()` / `element_rect()`, a
  [`sketch()`](https://r-vellum.github.io/vellumplot/reference/sketch.md)
  spec giving that theme element (gridlines, axis lines, ticks,
  panel/strip/legend backgrounds) a hand-drawn look, `NA` to force it
  crisp, or `NULL` (default) to inherit from its parent element / the
  plot-wide
  [`theme_sketch()`](https://r-vellum.github.io/vellumplot/reference/theme_sketch.md)
  default. Text elements are never sketched.

- fill:

  Fill colour (rectangles).

## Value

An element object for use in
[`theme()`](https://r-vellum.github.io/vellumplot/reference/theme_gray.md).

## Examples

``` r
vplot(mtcars) |>
  mark_point(x = wt, y = mpg) |>
  theme(plot.title = element_text(size = 16), panel.grid.minor = element_blank())
```
