# Position scales

Declare a position scale to override the trained default.
`scale_x_continuous()` / `scale_y_continuous()` handle numeric (and
date/time) axes; `scale_x_discrete()` / `scale_y_discrete()` handle
categorical (band) axes and let you set the level order via `limits`.

## Usage

``` r
scale_x_continuous(
  plot,
  limits = NULL,
  trans = "identity",
  breaks = NULL,
  labels = NULL,
  name = NULL,
  sec.axis = NULL
)

scale_y_continuous(
  plot,
  limits = NULL,
  trans = "identity",
  breaks = NULL,
  labels = NULL,
  name = NULL,
  sec.axis = NULL
)

scale_x_discrete(plot, limits = NULL, name = NULL)

scale_y_discrete(plot, limits = NULL, name = NULL)
```

## Arguments

- plot:

  A
  [PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

- limits:

  For continuous scales a numeric length-2 domain `c(min, max)`; for
  discrete scales a character vector of levels (sets order / subset).
  `NULL` trains from the data.

- trans:

  Transformation: `"identity"` (default), `"log10"`, `"sqrt"`,
  `"reverse"`, or a
  [`scales::transform_log10()`](https://scales.r-lib.org/reference/transform_log.html)-style
  transform object.

- breaks, labels:

  Explicit break positions (data units) and their labels, or `NULL` to
  compute them.

- name:

  Axis title, or `NULL` to derive from the encoding.

- sec.axis:

  A secondary axis from
  [`sec_axis()`](https://r-vellum.github.io/vellumplot/reference/sec_axis.md)
  /
  [`dup_axis()`](https://r-vellum.github.io/vellumplot/reference/sec_axis.md),
  drawn on the opposite edge, or `NULL` for none. Continuous Cartesian
  plots only (see
  [`sec_axis()`](https://r-vellum.github.io/vellumplot/reference/sec_axis.md)
  for the current limitations).

## Value

The modified
[PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

## Examples

``` r
vplot(mtcars) |> mark_point(x = wt, y = mpg) |> scale_x_continuous(limits = c(0, 6))

vplot(mtcars) |> mark_point(x = wt, y = mpg) |> scale_y_continuous(trans = "sqrt")
```
