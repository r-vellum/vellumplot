# Binned position scales

`scale_x_binned()` / `scale_y_binned()` cut a continuous position
variable into bins: axis ticks fall at the bin **boundaries** and each
datum is drawn at its bin **centre**. Breaks use the same classification
as the binned colour scales (`style`/`n`; `classInt` for jenks/fisher/…
— a Suggest). Handy to summarise a dense continuous axis, or to place
[`mark_bar()`](https://r-vellum.github.io/vellumplot/reference/mark_point.md)
on a binned continuous `x`.

## Usage

``` r
scale_x_binned(
  plot,
  style = "pretty",
  n = 10,
  breaks = NULL,
  labels = NULL,
  name = NULL
)

scale_y_binned(
  plot,
  style = "pretty",
  n = 10,
  breaks = NULL,
  labels = NULL,
  name = NULL
)
```

## Arguments

- plot:

  A
  [PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

- style:

  Binning style: `"pretty"` (default), `"equal"`, `"quantile"`, or a
  `classInt` style.

- n:

  Target number of bins.

- breaks:

  Explicit bin boundaries (overrides `style`/`n`), or `NULL`.

- labels:

  Explicit boundary labels, or `NULL` to format the boundaries.

- name:

  Axis title, or `NULL` to derive from the encoding.

## Value

The modified
[PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

## Examples

``` r
vplot(mtcars) |> mark_point(x = wt, y = mpg) |> scale_x_binned(n = 6)
```
