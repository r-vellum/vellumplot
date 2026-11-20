# A 2-D legend for a bivariate scale

`guide_bivariate()` draws a bivariate colour scale as a square grid of
swatches: the `x` variable's classes run left to right along the bottom,
the `y` variable's classes bottom to top up the side, each axis with its
title and an arrow pointing towards "more". When the classes come from
[`bi_class()`](https://r-vellum.github.io/vellumplot/reference/bi_class.md),
each axis is labelled with its class breaks at the swatch edges.
`rotate = TRUE` turns the grid 45 degrees into a diamond, low-low at the
bottom and high-high at the top. It is the default guide of
[`scale_fill_bivariate()`](https://r-vellum.github.io/vellumplot/reference/scale_fill_bivariate.md);
use it through
[`guides()`](https://r-vellum.github.io/vellumplot/reference/guides.md)
to retitle the axes or resize the swatches without respelling the scale.
Asking for it on a fill that maps a
[`bi_class()`](https://r-vellum.github.io/vellumplot/reference/bi_class.md)-style
column makes that scale bivariate.

## Usage

``` r
guide_bivariate(
  title = NULL,
  xlab = NULL,
  ylab = NULL,
  key.size = NULL,
  labels = TRUE,
  rotate = FALSE,
  position = NULL
)
```

## Arguments

- title:

  A title above the grid, or `NULL` (default) for none.

- xlab, ylab:

  The axis titles, or `NULL` to keep the scale's (or the variable names
  recorded by
  [`bi_class()`](https://r-vellum.github.io/vellumplot/reference/bi_class.md)).

- key.size:

  Side of one swatch, in millimetres. `NULL` (default) uses 1.5 x the
  theme's `legend.key.size`, widened to fit the break labels.

- labels:

  Label the swatch edges with the class breaks
  [`bi_class()`](https://r-vellum.github.io/vellumplot/reference/bi_class.md)
  recorded (default `TRUE`). A scale without recorded breaks (a
  hand-made class column) has none to show.

- rotate:

  Turn the grid 45 degrees, so the classes read as a diamond with
  high-high at the top (default `FALSE`).

- position:

  Where this legend goes: `"right"`, `"left"`, `"top"`, `"bottom"` or
  `"inside"` (over the panels, at `theme(legend.position.inside = )` /
  `legend.justification`). `NULL` (default) follows
  `theme(legend.position = )`. A guide's own position overrides the
  theme for that legend only, as in ggplot2 3.5, so legends can sit on
  different sides of one plot; legends at one position stack as usual. A
  legend at the top or bottom is laid out horizontally.
  `theme(legend.position = "none")` still hides every legend. Two
  legends that would merge (one variable, one title) only merge when
  they share a position.

## Value

A guide specification for
[`guides()`](https://r-vellum.github.io/vellumplot/reference/guides.md).

## See also

[`scale_fill_bivariate()`](https://r-vellum.github.io/vellumplot/reference/scale_fill_bivariate.md)

Other bivariate:
[`bi_class()`](https://r-vellum.github.io/vellumplot/reference/bi_class.md),
[`bi_pal()`](https://r-vellum.github.io/vellumplot/reference/bi_pal.md),
[`scale_fill_bivariate()`](https://r-vellum.github.io/vellumplot/reference/scale_fill_bivariate.md)

## Examples

``` r
d <- bi_class(mtcars, hp, wt)
vplot(d) |>
  mark_point(x = hp, y = wt, color = bi_class, size = 2) |>
  guides(color = guide_bivariate(xlab = "More power", ylab = "More weight"))


# the diamond form
vplot(d) |>
  mark_point(x = hp, y = wt, color = bi_class, size = 2) |>
  guides(color = guide_bivariate(rotate = TRUE))
```
