# Shape scale

Declare the scale for a mapped (discrete) `shape` aesthetic. Levels
cycle through a default set of marker shapes unless `values` is given. A
shape legend is drawn automatically.

## Usage

``` r
scale_shape(plot, values = NULL, name = NULL)
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

- name:

  Legend title, or `NULL` to derive from the encoding.

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
```
