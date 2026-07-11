# Add marginal distributions to a plot

`add_marginal()` draws a distribution of the panel's `x` variable along
the top edge and/or of its `y` variable along the right edge, each
sharing the main panel's axis so it lines up with the scatter (the
vellumplot analogue of `ggExtra::ggMarginal()`). It is a plot-level
modifier, like
[`facet_wrap()`](https://r-vellum.github.io/vellumplot/reference/facet_wrap.md)
or
[`coord_flip()`](https://r-vellum.github.io/vellumplot/reference/coord_cartesian.md):
it takes no encoding of its own and instead reads `x`, `y` (and, with
`group = TRUE`, `color`) from the first layer that maps a numeric `x`
and `y` (typically your
[`mark_point()`](https://r-vellum.github.io/vellumplot/reference/mark_point.md)).

## Usage

``` r
add_marginal(
  plot,
  type = c("density", "histogram"),
  sides = "tr",
  size = 0.15,
  adjust = 1,
  bins = 30,
  group = FALSE
)
```

## Arguments

- plot:

  A
  [PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md)
  (from
  [`vplot()`](https://r-vellum.github.io/vellumplot/reference/vplot.md))
  with at least one `x`/`y` layer.

- type:

  The marginal distribution: `"density"` (a kernel-density curve, the
  default) or `"histogram"` (binned counts).

- sides:

  Which edges to draw, as a string of `"t"` (top, the `x` distribution)
  and/or `"r"` (right, the `y` distribution). Default `"tr"`.

- size:

  The marginal size as a fraction of the panel, in `(0, 1)`.

- adjust:

  Bandwidth multiplier for `type = "density"` (see
  [`mark_density()`](https://r-vellum.github.io/vellumplot/reference/mark_tile.md)).

- bins:

  Number of bins for `type = "histogram"` (see
  [`mark_histogram()`](https://r-vellum.github.io/vellumplot/reference/mark_histogram.md)).

- group:

  When `TRUE` and the plot maps `color`/`fill` to a discrete variable,
  draw one distribution per group in the matching colour (like
  `ggMarginal(groupColour = TRUE)`). The scatter's legend already covers
  them, so no extra legend is added.

## Value

The modified
[PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

## Details

The marginals are drawn without their own axes or background. This
version supports a single panel only: combining it with
[`facet_wrap()`](https://r-vellum.github.io/vellumplot/reference/facet_wrap.md)
/
[`facet_grid()`](https://r-vellum.github.io/vellumplot/reference/facet_wrap.md),
a non-Cartesian coordinate system
([`coord_flip()`](https://r-vellum.github.io/vellumplot/reference/coord_cartesian.md),
[`coord_polar()`](https://r-vellum.github.io/vellumplot/reference/coord_polar.md),
[`coord_fixed()`](https://r-vellum.github.io/vellumplot/reference/coord_cartesian.md),
[`coord_sf()`](https://r-vellum.github.io/vellumplot/reference/coord_sf.md)),
or a locked aspect ratio is an error.

## See also

[`mark_density()`](https://r-vellum.github.io/vellumplot/reference/mark_tile.md),
[`mark_histogram()`](https://r-vellum.github.io/vellumplot/reference/mark_histogram.md),
[`facet_wrap()`](https://r-vellum.github.io/vellumplot/reference/facet_wrap.md)

## Examples

``` r
vplot(faithful) |>
  mark_point(x = eruptions, y = waiting) |>
  add_marginal()


vplot(faithful) |>
  mark_point(x = eruptions, y = waiting) |>
  add_marginal(type = "histogram", sides = "t")
```
