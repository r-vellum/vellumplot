# Shape scale

Declare the scale for a mapped (discrete) `shape` aesthetic. Levels
cycle through a default set of marker shapes unless `values` is given. A
shape legend is drawn automatically.

## Usage

``` r
scale_shape(
  plot,
  values = NULL,
  name = NULL,
  breaks = NULL,
  labels = NULL,
  limits = NULL,
  na.value = NULL,
  drop = TRUE
)

scale_shape_manual(
  plot,
  values,
  breaks = NULL,
  labels = NULL,
  name = NULL,
  limits = NULL,
  na.value = NULL,
  drop = TRUE
)

scale_shape_discrete(
  plot,
  values = NULL,
  name = NULL,
  breaks = NULL,
  labels = NULL,
  limits = NULL,
  na.value = NULL,
  drop = TRUE
)
```

## Arguments

- plot:

  A
  [PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

- values:

  Character vector of shapes, or `NULL` for the default. Each is either
  a built-in marker (`"circle"`, `"square"`, `"triangle"`, `"diamond"`,
  `"plus"`, `"cross"`, `"triangle_down"`, `"star"`) **or an SVG icon** —
  a path `d` string (what icon sets ship) or a path to a `.svg` file.
  SVG values are drawn as crisp vector markers (via
  [`vellum::svg_grob()`](https://r-vellum.github.io/vellum/reference/vl_svg_path.html))
  and the legend shows the icon; the `size` aesthetic scales them.

  For `scale_shape_manual()` `values` is required, and may also be
  ggplot2's numeric shape codes (`16`, `17`, ...), which map to the
  nearest marker (open, filled and bordered variants of a shape draw
  alike). Named values are matched to levels by name.

- name:

  Legend title, or `NULL` to derive from the encoding.

- breaks:

  The levels the legend shows (and their order), or `NULL` for all of
  them.

- labels:

  Legend labels: one per shown level, a vector named by level, or a
  function of the levels; `NULL` keeps the levels.

- limits:

  The levels the scale covers (a character vector), or `NULL` for the
  levels in the data. Data outside `limits` draws as `na.value`.

- na.value:

  The shape drawn for missing values (default a circle).

- drop:

  For the discrete scales, drop factor levels that no row uses (default
  `TRUE`, as in ggplot2): they get no legend key and no palette entry,
  so the colours go to the levels that remain. `FALSE` keeps every
  level. `limits` overrides it.

## Value

The modified
[PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

## Examples

``` r
vplot(mtcars) |> mark_point(x = wt, y = mpg, shape = factor(cyl))

# SVG icon markers, one per level:
star <- "M12 2l3 7h7l-5.5 4.5 2 7-6.5-4.5-6.5 4.5 2-7L2 9h7z"
vplot(mtcars) |>
  mark_point(x = wt, y = mpg, shape = factor(cyl)) |>
  scale_shape(values = c(star, star, star))

vplot(mtcars) |>
  mark_point(x = wt, y = mpg, shape = factor(cyl)) |>
  scale_shape_manual(values = c("4" = 16, "6" = 17, "8" = 15))
```
