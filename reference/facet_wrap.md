# Facet a plot into a grid of panels

`facet_wrap()` lays panels out in a ribbon wrapped into `ncol`/`nrow`;
`facet_grid()` arranges them on a 2-D grid defined by a `rows ~ cols`
formula. Position scales are shared across panels by default (so axes
align); pass `scales = "free_x"`, `"free_y"`, or `"free"` to train them
per panel. Colour, size and the other non-position scales are shared too
(one legend); `resolve_scale(color = "independent")` gives each panel
its own.

## Usage

``` r
facet_wrap(
  plot,
  facets,
  ncol = NULL,
  nrow = NULL,
  scales = c("fixed", "free_x", "free_y", "free"),
  labeller = NULL
)

facet_grid(
  plot,
  facets,
  scales = c("fixed", "free_x", "free_y", "free"),
  labeller = NULL
)
```

## Arguments

- plot:

  A
  [PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

- facets:

  A faceting formula. For `facet_wrap()`, one-sided such as `~ cyl` (one
  or more `+`-separated variables); for `facet_grid()`, a two-sided
  `rows ~ cols` (use `.` for no faceting on a side).

- ncol, nrow:

  Number of columns/rows for `facet_wrap()`.

- scales:

  One of `"fixed"` (default), `"free_x"`, `"free_y"`, `"free"`.

- labeller:

  How to label the facet strips: a labeller such as
  [`label_both()`](https://r-vellum.github.io/vellumplot/reference/labellers.md),
  [`label_value()`](https://r-vellum.github.io/vellumplot/reference/labellers.md)
  or
  [`label_wrap_gen()`](https://r-vellum.github.io/vellumplot/reference/labellers.md),
  a named character vector looking values up (`c("4" = "Four")`), or a
  function from values to labels (see
  [`as_labeller()`](https://r-vellum.github.io/vellumplot/reference/labellers.md)).
  The default, `NULL`, shows the values, several variables on one side
  joined by `", "`.

## Value

The modified
[PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

## See also

[labellers](https://r-vellum.github.io/vellumplot/reference/labellers.md)

## Examples

``` r
vplot(mtcars) |> mark_point(x = wt, y = mpg) |> facet_wrap(~cyl)

vplot(mtcars) |> mark_point(x = wt, y = mpg) |> facet_grid(am ~ cyl)

vplot(mtcars) |>
  mark_point(x = wt, y = mpg) |>
  facet_grid(am ~ cyl, labeller = label_both)
```
