# Binned (classed) colour scales

`scale_fill_binned()` / `scale_color_binned()` cut a continuous
aesthetic into classes and colour each class with one step of a
sequential palette — the standard choropleth scale. Class breaks are
chosen by a classification `style`. `quantile`, `equal`, and `pretty`
need no extra packages; other styles (e.g. `"jenks"`, `"fisher"`,
`"kmeans"`, `"headtails"`) delegate to the optional `classInt` package
(in `Suggests`) and error if it is absent.

## Usage

``` r
scale_fill_binned(
  plot,
  style = "quantile",
  n = 5,
  breaks = NULL,
  palette = NULL,
  labels = NULL,
  na_value = "grey80",
  name = NULL
)

scale_color_binned(
  plot,
  style = "quantile",
  n = 5,
  breaks = NULL,
  palette = NULL,
  labels = NULL,
  na_value = "grey80",
  name = NULL
)

scale_color_steps(
  plot,
  style = "quantile",
  n = 5,
  breaks = NULL,
  palette = NULL,
  labels = NULL,
  na_value = "grey80",
  name = NULL
)

scale_colour_steps(
  plot,
  style = "quantile",
  n = 5,
  breaks = NULL,
  palette = NULL,
  labels = NULL,
  na_value = "grey80",
  name = NULL
)

scale_fill_steps(
  plot,
  style = "quantile",
  n = 5,
  breaks = NULL,
  palette = NULL,
  labels = NULL,
  na_value = "grey80",
  name = NULL
)
```

## Arguments

- plot:

  A
  [PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

- style:

  Classification style: one of `"quantile"`, `"equal"`, `"pretty"` (base
  R), or any
  [`classInt::classIntervals()`](https://r-spatial.github.io/classInt/reference/classIntervals.html)
  style. Default `"quantile"`.

- n:

  Number of classes (default `5`). Ignored when `breaks` is supplied.

- breaks:

  Explicit class boundaries (length = number of classes + 1), overriding
  `style`/`n`.

- palette:

  A sequential palette: `NULL` (batlow), an
  [`grDevices::hcl.pals()`](https://rdrr.io/r/grDevices/palettes.html)
  name, or a vector of colours interpolated across the classes.

- labels:

  Optional class labels (one per class); defaults to interval ranges
  like `"[0, 10)"`.

- na_value:

  Fill colour for `NA` values (shown as a distinct legend swatch).
  Default `"grey80"`.

- name:

  Legend title, or `NULL` to derive from the encoding.

## Value

The modified
[PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

## Details

`scale_color_steps()` / `scale_fill_steps()` are aliases of the binned
colour scales, for ggplot2 users who reach for the `_steps` name — a
continuous variable cut into steps with an interval-key legend.

## See also

[`mark_sf()`](https://r-vellum.github.io/vellumplot/reference/mark_sf.md),
[`scale_fill_continuous()`](https://r-vellum.github.io/vellumplot/reference/scale_color_continuous.md)

## Examples

``` r
if (FALSE) { # \dontrun{
nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
vplot(nc) |>
  mark_sf(fill = SID74) |>
  scale_fill_binned(style = "quantile", n = 5) |>
  coord_sf()
} # }
```
