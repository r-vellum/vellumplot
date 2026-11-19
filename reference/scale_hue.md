# Hue, grey and ordinal colour scales

ggplot2's other discrete colour scales, for code migrating from it.

## Usage

``` r
scale_color_hue(
  plot,
  h = c(0, 360) + 15,
  c = 100,
  l = 65,
  h.start = 0,
  direction = 1,
  name = NULL,
  breaks = NULL,
  labels = NULL,
  limits = NULL,
  na.value = "grey50"
)

scale_colour_hue(
  plot,
  h = c(0, 360) + 15,
  c = 100,
  l = 65,
  h.start = 0,
  direction = 1,
  name = NULL,
  breaks = NULL,
  labels = NULL,
  limits = NULL,
  na.value = "grey50"
)

scale_fill_hue(
  plot,
  h = c(0, 360) + 15,
  c = 100,
  l = 65,
  h.start = 0,
  direction = 1,
  name = NULL,
  breaks = NULL,
  labels = NULL,
  limits = NULL,
  na.value = "grey50"
)

scale_color_grey(
  plot,
  start = 0.2,
  end = 0.8,
  name = NULL,
  breaks = NULL,
  labels = NULL,
  limits = NULL,
  na.value = "red"
)

scale_colour_grey(
  plot,
  start = 0.2,
  end = 0.8,
  name = NULL,
  breaks = NULL,
  labels = NULL,
  limits = NULL,
  na.value = "red"
)

scale_fill_grey(
  plot,
  start = 0.2,
  end = 0.8,
  name = NULL,
  breaks = NULL,
  labels = NULL,
  limits = NULL,
  na.value = "red"
)

scale_color_ordinal(
  plot,
  option = "viridis",
  name = NULL,
  breaks = NULL,
  labels = NULL,
  limits = NULL,
  na.value = "grey50"
)

scale_colour_ordinal(
  plot,
  option = "viridis",
  name = NULL,
  breaks = NULL,
  labels = NULL,
  limits = NULL,
  na.value = "grey50"
)

scale_fill_ordinal(
  plot,
  option = "viridis",
  name = NULL,
  breaks = NULL,
  labels = NULL,
  limits = NULL,
  na.value = "grey50"
)
```

## Arguments

- plot:

  A
  [PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

- h:

  Range of hues, in degrees on the colour wheel.

- c, l:

  Chroma and luminance of the hues.

- h.start:

  Hue to start at.

- direction:

  `1` runs clockwise round the wheel, `-1` anticlockwise.

- name:

  Legend title, or `NULL` to derive from the encoding.

- breaks, labels:

  The levels the legend shows and their labels (one per shown level, a
  vector named by level, or a function), or `NULL`.

- limits:

  The levels the scale covers, or `NULL` for those in the data.

- na.value:

  Colour for missing values (and levels outside `limits`).

- start, end:

  Grey levels of the first and last colour, in `[0, 1]`.

- option:

  For `scale_*_ordinal()`, the viridis map (see
  [`scale_color_viridis_d()`](https://r-vellum.github.io/vellumplot/reference/scale_viridis.md)).

## Value

The modified
[PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

## Details

- `scale_*_hue()` — evenly spaced hues around the HCL colour wheel
  (ggplot2's default discrete palette, via
  [`scales::pal_hue()`](https://scales.r-lib.org/reference/pal_hue.html)).

- `scale_*_grey()` — a grey ramp from `start` to `end` (0 is black, 1
  white), via
  [`scales::pal_grey()`](https://scales.r-lib.org/reference/pal_grey.html).

- `scale_*_ordinal()` — the viridis palette ggplot2 uses for an ordered
  factor (as
  [`scale_color_viridis_d()`](https://r-vellum.github.io/vellumplot/reference/scale_viridis.md)).

The colours depend on the number of levels, so they are drawn when the
plot is compiled.

## See also

[`scale_color_discrete()`](https://r-vellum.github.io/vellumplot/reference/scale_color_continuous.md),
[scale_viridis](https://r-vellum.github.io/vellumplot/reference/scale_viridis.md)

## Examples

``` r
vplot(mtcars) |>
  mark_point(x = wt, y = mpg, color = factor(cyl)) |>
  scale_color_hue(l = 45)

vplot(mtcars) |>
  mark_bar(x = factor(gear), fill = factor(cyl)) |>
  scale_fill_grey()
```
