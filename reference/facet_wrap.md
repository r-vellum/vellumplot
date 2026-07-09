# Facet a plot into a grid of panels

`facet_wrap()` lays panels out in a ribbon wrapped into `ncol`/`nrow`;
`facet_grid()` arranges them on a 2-D grid defined by a `rows ~ cols`
formula. Position scales are shared across panels by default (so axes
align); pass `scales = "free_x"`, `"free_y"`, or `"free"` to train them
per panel. Colour and size scales (and their legends) are always shared
in this version.

## Usage

``` r
facet_wrap(
  plot,
  facets,
  ncol = NULL,
  nrow = NULL,
  scales = c("fixed", "free_x", "free_y", "free")
)

facet_grid(plot, facets, scales = c("fixed", "free_x", "free_y", "free"))
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

## Value

The modified
[PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

## Examples

``` r
vplot(mtcars) |> mark_point(x = wt, y = mpg) |> facet_wrap(~cyl)

vplot(mtcars) |> mark_point(x = wt, y = mpg) |> facet_grid(am ~ cyl)
```
