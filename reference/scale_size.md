# Size scale

Declare the scale for a mapped `size` aesthetic: data values map
linearly to a point-size `range` (in mm). A size legend is drawn
automatically.

`scale_size_area()` maps the data value to the marker's **area** rather
than its radius, with value `0` at size `0` — the perceptually honest
default for bubble charts (a value twice as large looks twice as big in
ink). `max_size` is the size (mm) of the largest value.

## Usage

``` r
scale_size(plot, range = NULL, limits = NULL, breaks = NULL, name = NULL)

scale_size_area(plot, max_size = 6, limits = NULL, breaks = NULL, name = NULL)
```

## Arguments

- plot:

  A
  [PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

- range:

  Numeric length-2 output size range in mm, or `NULL` for the default
  `c(1, 4)`.

- limits:

  Numeric length-2 data domain, or `NULL` to train from the data.

- breaks:

  Explicit legend breaks, or `NULL`.

- name:

  Legend title, or `NULL` to derive from the encoding.

- max_size:

  For `scale_size_area()`, the point size (mm) of the largest value.

## Value

The modified
[PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

## Examples

``` r
vplot(mtcars) |> mark_point(x = wt, y = mpg, size = hp) |> scale_size(range = c(1, 8))

vplot(mtcars) |>
  mark_point(x = wt, y = mpg, size = hp) |>
  scale_size_area(max_size = 10)
```
