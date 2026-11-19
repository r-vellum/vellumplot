# Binned (classed) colour scales

`scale_fill_binned()` / `scale_color_binned()` cut a continuous
aesthetic into classes and colour each class with one step of a
sequential palette — the standard choropleth scale. Class breaks are
chosen by a classification `style`. `quantile`, `equal`, and `pretty`
need no extra packages; other styles (e.g. `"jenks"`, `"fisher"`,
`"kmeans"`, `"headtails"`) delegate to the optional `classInt` package
(in `Suggests`) and error if it is absent.

The ggplot2-named binned colour scales follow ggplot2's binning: without
`breaks`, about `n.breaks` "nice" class boundaries inside the data
range, the outer classes widened to whole bins (with `limits`, the
limits are the outer boundaries); classes close on the right, `(a, b]`.

- `scale_*_steps2()` — a diverging `low`–`mid`–`high` ramp centred on
  `midpoint`, each class taking the ramp's colour at its centre.

- `scale_*_stepsn()` — an n-colour ramp `colours`, sampled at each class
  centre.

- `scale_*_fermenter()` — a ColorBrewer palette's k-class colours (as
  [`scale_color_distiller()`](https://r-vellum.github.io/vellumplot/reference/scale_viridis.md),
  but binned).

- `scale_*_viridis_b()` — k evenly spaced viridis colours.

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

scale_color_steps2(
  plot,
  low = "#832424",
  mid = "#FFFFFF",
  high = "#3A3A98",
  midpoint = 0,
  name = NULL,
  breaks = NULL,
  labels = NULL,
  limits = NULL,
  n.breaks = NULL,
  na.value = "grey50",
  oob = NULL
)

scale_color_stepsn(
  plot,
  colours,
  name = NULL,
  breaks = NULL,
  labels = NULL,
  limits = NULL,
  n.breaks = NULL,
  na.value = "grey50",
  oob = NULL
)

scale_color_fermenter(
  plot,
  palette = 1,
  type = "seq",
  direction = -1,
  name = NULL,
  breaks = NULL,
  labels = NULL,
  limits = NULL,
  n.breaks = NULL,
  na.value = "grey50",
  oob = NULL
)

scale_color_viridis_b(
  plot,
  option = "viridis",
  direction = 1,
  name = NULL,
  breaks = NULL,
  labels = NULL,
  limits = NULL,
  n.breaks = NULL,
  na.value = "grey50",
  oob = NULL
)

scale_fill_steps2(
  plot,
  low = "#832424",
  mid = "#FFFFFF",
  high = "#3A3A98",
  midpoint = 0,
  name = NULL,
  breaks = NULL,
  labels = NULL,
  limits = NULL,
  n.breaks = NULL,
  na.value = "grey50",
  oob = NULL
)

scale_fill_stepsn(
  plot,
  colours,
  name = NULL,
  breaks = NULL,
  labels = NULL,
  limits = NULL,
  n.breaks = NULL,
  na.value = "grey50",
  oob = NULL
)

scale_fill_fermenter(
  plot,
  palette = 1,
  type = "seq",
  direction = -1,
  name = NULL,
  breaks = NULL,
  labels = NULL,
  limits = NULL,
  n.breaks = NULL,
  na.value = "grey50",
  oob = NULL
)

scale_fill_viridis_b(
  plot,
  option = "viridis",
  direction = 1,
  name = NULL,
  breaks = NULL,
  labels = NULL,
  limits = NULL,
  n.breaks = NULL,
  na.value = "grey50",
  oob = NULL
)

scale_colour_steps2(
  plot,
  low = "#832424",
  mid = "#FFFFFF",
  high = "#3A3A98",
  midpoint = 0,
  name = NULL,
  breaks = NULL,
  labels = NULL,
  limits = NULL,
  n.breaks = NULL,
  na.value = "grey50",
  oob = NULL
)

scale_colour_stepsn(
  plot,
  colours,
  name = NULL,
  breaks = NULL,
  labels = NULL,
  limits = NULL,
  n.breaks = NULL,
  na.value = "grey50",
  oob = NULL
)

scale_colour_fermenter(
  plot,
  palette = 1,
  type = "seq",
  direction = -1,
  name = NULL,
  breaks = NULL,
  labels = NULL,
  limits = NULL,
  n.breaks = NULL,
  na.value = "grey50",
  oob = NULL
)

scale_colour_viridis_b(
  plot,
  option = "viridis",
  direction = 1,
  name = NULL,
  breaks = NULL,
  labels = NULL,
  limits = NULL,
  n.breaks = NULL,
  na.value = "grey50",
  oob = NULL
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

- low, mid, high, midpoint:

  For `scale_*_steps2()`, the end and midpoint colours and the data
  value at `mid`.

- limits:

  For the ggplot2-named scales, a length-2 numeric `c(min, max)`: the
  outer class boundaries.

- n.breaks:

  For the ggplot2-named scales, the approximate number of class
  boundaries when `breaks` is `NULL` (default 5).

- na.value:

  For the ggplot2-named scales, the colour of missing values.

- oob:

  For the ggplot2-named scales, what to do with values outside the outer
  boundaries: `"squish"` (the default, as in ggplot2) colours them as
  the outer class, `"censor"` with `na.value`; or a
  `function(x, range)`.

- colours:

  For `scale_*_stepsn()`, the colours of the ramp.

- type, direction:

  For `scale_*_fermenter()`, the ColorBrewer family a numeric `palette`
  indexes and its order (see
  [`scale_color_brewer()`](https://r-vellum.github.io/vellumplot/reference/scale_viridis.md));
  for `scale_*_viridis_b()`, `direction = -1` reverses the map.

- option:

  For `scale_*_viridis_b()`, the viridis map (see
  [`scale_color_viridis_c()`](https://r-vellum.github.io/vellumplot/reference/scale_viridis.md)).

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
vplot(faithful) |>
  mark_point(x = eruptions, y = waiting, color = waiting - 70) |>
  scale_color_steps2()
```
