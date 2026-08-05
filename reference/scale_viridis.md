# Viridis and ColorBrewer scales

Named convenience constructors for popular palettes, easing migration
from ggplot2. They are thin wrappers over the `palette =` argument of
[`scale_color_continuous()`](https://r-vellum.github.io/vellumplot/reference/scale_color_continuous.md)
/
[`scale_color_discrete()`](https://r-vellum.github.io/vellumplot/reference/scale_color_continuous.md)
(any
[`grDevices::hcl.colors()`](https://rdrr.io/r/grDevices/palettes.html)
name works there directly).

## Usage

``` r
scale_color_viridis_c(
  plot,
  option = "viridis",
  breaks = NULL,
  labels = NULL,
  name = NULL
)

scale_color_viridis_d(
  plot,
  option = "viridis",
  breaks = NULL,
  labels = NULL,
  name = NULL
)

scale_fill_viridis_c(
  plot,
  option = "viridis",
  breaks = NULL,
  labels = NULL,
  name = NULL
)

scale_fill_viridis_d(
  plot,
  option = "viridis",
  breaks = NULL,
  labels = NULL,
  name = NULL
)

scale_color_brewer(
  plot,
  palette = "Blues",
  breaks = NULL,
  labels = NULL,
  name = NULL
)

scale_fill_brewer(
  plot,
  palette = "Blues",
  breaks = NULL,
  labels = NULL,
  name = NULL
)

scale_color_distiller(
  plot,
  palette = "Blues",
  breaks = NULL,
  labels = NULL,
  name = NULL
)

scale_fill_distiller(
  plot,
  palette = "Blues",
  breaks = NULL,
  labels = NULL,
  name = NULL
)

scale_colour_viridis_c(
  plot,
  option = "viridis",
  breaks = NULL,
  labels = NULL,
  name = NULL
)

scale_colour_viridis_d(
  plot,
  option = "viridis",
  breaks = NULL,
  labels = NULL,
  name = NULL
)

scale_colour_brewer(
  plot,
  palette = "Blues",
  breaks = NULL,
  labels = NULL,
  name = NULL
)

scale_colour_distiller(
  plot,
  palette = "Blues",
  breaks = NULL,
  labels = NULL,
  name = NULL
)
```

## Arguments

- plot:

  A
  [PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

- option:

  Which viridis map (see above).

- name, breaks, labels:

  As for
  [`scale_color_continuous()`](https://r-vellum.github.io/vellumplot/reference/scale_color_continuous.md).

- palette:

  A
  [`grDevices::hcl.colors()`](https://rdrr.io/r/grDevices/palettes.html)
  palette name.

## Value

The modified
[PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

## Details

- `scale_*_viridis_c()` / `scale_*_viridis_d()` — the
  perceptually-uniform, colour-vision-deficient-safe viridis maps,
  continuous (`_c`) or discrete (`_d`). `option` picks the map:
  `"viridis"`, `"plasma"`, `"inferno"`, `"cividis"`, `"rocket"`, or
  `"mako"` (unknown options fall back to viridis).

- `scale_*_brewer()` — a discrete ColorBrewer-style
  qualitative/sequential palette; `scale_*_distiller()` — the same ramp
  interpolated for continuous data. `palette` is any
  [`hcl.colors()`](https://rdrr.io/r/grDevices/palettes.html) name (e.g.
  `"Blues"`, `"Set 2"`).

## See also

[`scale_color_continuous()`](https://r-vellum.github.io/vellumplot/reference/scale_color_continuous.md)

## Examples

``` r
vplot(mtcars) |> mark_point(x = wt, y = mpg, color = hp) |> scale_color_viridis_c()

vplot(mtcars) |>
  mark_point(x = wt, y = mpg, color = factor(cyl)) |>
  scale_color_brewer(palette = "Set 2")
```
