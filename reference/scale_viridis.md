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
  name = NULL,
  type = "seq",
  direction = 1
)

scale_fill_brewer(
  plot,
  palette = "Blues",
  breaks = NULL,
  labels = NULL,
  name = NULL,
  type = "seq",
  direction = 1
)

scale_color_distiller(
  plot,
  palette = "Blues",
  breaks = NULL,
  labels = NULL,
  name = NULL,
  type = "seq",
  direction = -1
)

scale_fill_distiller(
  plot,
  palette = "Blues",
  breaks = NULL,
  labels = NULL,
  name = NULL,
  type = "seq",
  direction = -1
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
  name = NULL,
  type = "seq",
  direction = 1
)

scale_colour_distiller(
  plot,
  palette = "Blues",
  breaks = NULL,
  labels = NULL,
  name = NULL,
  type = "seq",
  direction = -1
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

  A ColorBrewer palette name or index (see above), or a
  [`grDevices::hcl.colors()`](https://rdrr.io/r/grDevices/palettes.html)
  palette name.

- type:

  When `palette` is a number, which ColorBrewer family it indexes:
  `"seq"` (sequential), `"div"` (diverging) or `"qual"` (qualitative).

- direction:

  `1` keeps a ColorBrewer palette's order, `-1` reverses it.
  `scale_*_distiller()` defaults to `-1`, as in ggplot2, so high values
  are light. Ignored for non-ColorBrewer palette names.

## Value

The modified
[PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

## Details

- `scale_*_viridis_c()` / `scale_*_viridis_d()` — the
  perceptually-uniform, colour-vision-deficient-safe viridis maps,
  continuous (`_c`) or discrete (`_d`). `option` picks the map:
  `"viridis"`, `"plasma"`, `"inferno"`, `"cividis"`, `"rocket"`, or
  `"mako"` (unknown options fall back to viridis).

- `scale_*_brewer()` — a discrete ColorBrewer palette (sequential,
  diverging or qualitative, via
  [`scales::pal_brewer()`](https://scales.r-lib.org/reference/pal_brewer.html));
  `scale_*_distiller()` — the same palette's 7-class ramp interpolated
  for continuous data. `palette` is a ColorBrewer name (`"Set1"`,
  `"Dark2"`, `"Paired"`, `"Blues"`, `"RdBu"`, `"Spectral"`, ...; matched
  ignoring case, spaces and punctuation, so `"Set 2"` is `"Set2"`) or an
  index into the palettes of `type`. Any other
  [`grDevices::hcl.colors()`](https://rdrr.io/r/grDevices/palettes.html)
  name (e.g. `"Batlow"`) is passed through unchanged, as by
  [`scale_color_discrete()`](https://r-vellum.github.io/vellumplot/reference/scale_color_continuous.md)
  /
  [`scale_color_continuous()`](https://r-vellum.github.io/vellumplot/reference/scale_color_continuous.md).

## See also

[`scale_color_continuous()`](https://r-vellum.github.io/vellumplot/reference/scale_color_continuous.md)

## Examples

``` r
vplot(mtcars) |> mark_point(x = wt, y = mpg, color = hp) |> scale_color_viridis_c()

vplot(mtcars) |>
  mark_point(x = wt, y = mpg, color = factor(cyl)) |>
  scale_color_brewer(palette = "Set1")

vplot(mtcars) |>
  mark_point(x = wt, y = mpg, color = hp) |>
  scale_color_distiller(palette = "Spectral")
```
