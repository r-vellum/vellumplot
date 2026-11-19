# Binned (classed) colour scales

`scale_fill_binned()` / `scale_color_binned()` cut a continuous
aesthetic into classes and colour each class with one step of a
sequential palette — the standard choropleth scale. By default the
classes follow ggplot2's binning (`style = "extended"`): about `n`
"nice" class boundaries inside the data range, the outer classes widened
to whole bins, and classes closed on the right, `(a, b]`. A
classification `style` chooses other breaks: `quantile`, `equal`, and
`pretty` need no extra packages; other styles (e.g. `"jenks"`,
`"fisher"`, `"kmeans"`, `"headtails"`) delegate to the optional
`classInt` package (in `Suggests`) and error if it is absent. Those
classes close on the left, `[a, b)`.

The ggplot2-named binned colour scales follow ggplot2's binning: without
`breaks`, about `n.breaks` "nice" class boundaries inside the data
range, the outer classes widened to whole bins (with `limits`, the
limits are the outer boundaries); classes close on the right, `(a, b]`.

- `scale_*_steps()` — a two-colour `low`–`high` ramp, each class taking
  the ramp's colour at its centre.

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
  style = "extended",
  n = NULL,
  breaks = NULL,
  palette = NULL,
  labels = NULL,
  na_value = "grey80",
  name = NULL
)

scale_color_binned(
  plot,
  style = "extended",
  n = NULL,
  breaks = NULL,
  palette = NULL,
  labels = NULL,
  na_value = "grey80",
  name = NULL
)

scale_color_steps(
  plot,
  low = "#132B43",
  high = "#56B1F7",
  name = NULL,
  breaks = NULL,
  labels = NULL,
  limits = NULL,
  n.breaks = NULL,
  na.value = "grey50",
  oob = NULL
)

scale_colour_steps(
  plot,
  low = "#132B43",
  high = "#56B1F7",
  name = NULL,
  breaks = NULL,
  labels = NULL,
  limits = NULL,
  n.breaks = NULL,
  na.value = "grey50",
  oob = NULL
)

scale_fill_steps(
  plot,
  low = "#132B43",
  high = "#56B1F7",
  name = NULL,
  breaks = NULL,
  labels = NULL,
  limits = NULL,
  n.breaks = NULL,
  na.value = "grey50",
  oob = NULL
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
  values = NULL,
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
  values = NULL,
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
  values = NULL,
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

  Classification style: `"extended"` (the default, ggplot2's breaks),
  `"quantile"`, `"equal"`, `"pretty"` (base R), or any
  [`classInt::classIntervals()`](https://r-spatial.github.io/classInt/reference/classIntervals.html)
  style.

- n:

  For `"extended"`, the approximate number of class boundaries (default
  5); for the other styles, the number of classes (default 5). Ignored
  when `breaks` is supplied.

- breaks:

  Explicit class boundaries, overriding `style`/`n`. Under `"extended"`
  the data range is added as the outer boundaries (as ggplot2 adds its
  limits); under the other styles they are every boundary (length =
  number of classes + 1).

- palette:

  A sequential palette: `NULL` (batlow), an
  [`grDevices::hcl.pals()`](https://rdrr.io/r/grDevices/palettes.html)
  name, or a vector of colours interpolated across the classes.

- labels:

  Optional class labels (one per class); defaults to interval ranges
  like `"(0, 10]"`.

- na_value:

  Fill colour for `NA` values (shown as a distinct legend swatch).
  Default `"grey80"`.

- name:

  Legend title, or `NULL` to derive from the encoding.

- low, mid, high, midpoint:

  For `scale_*_steps()`, the end colours `low` and `high`; for
  `scale_*_steps2()`, the end and midpoint colours and the data value at
  `mid`.

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

- values:

  For `scale_*_stepsn()`, the positions (between 0 and 1, one per
  colour, increasing) of `colours` along the ramp, or `NULL` to space
  them evenly.

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

A colour vector `palette` is a ramp, sampled at each class's centre as
in ggplot2; a named palette (and the default) gives one evenly spaced
colour per class.

## See also

[`mark_sf()`](https://r-vellum.github.io/vellumplot/reference/mark_sf.md),
[`scale_fill_continuous()`](https://r-vellum.github.io/vellumplot/reference/scale_color_continuous.md)

## Examples

``` r
vplot(faithful) |>
  mark_point(x = eruptions, y = waiting, color = waiting) |>
  scale_color_binned()

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
