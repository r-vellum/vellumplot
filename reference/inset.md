# Overlay a plot as an inset

Place `plot` as an inset over `base`, positioned by fractional
coordinates (0–1) of the chosen reference box. Returns a 1-cell
`PlotComposition` carrying the inset (compose it further or render
directly).

## Usage

``` r
inset(
  base,
  plot,
  left = 0.6,
  bottom = 0.6,
  right = 0.98,
  top = 0.98,
  align_to = c("panel", "plot", "full"),
  on_top = TRUE,
  shadow = NULL
)
```

## Arguments

- base:

  A
  [PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md)
  or `PlotComposition` to draw underneath.

- plot:

  The
  [PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md)
  to overlay.

- left, bottom, right, top:

  Inset position as fractions (0–1).

- align_to:

  Reference box: `"panel"`, `"plot"`, or `"full"` (currently the whole
  `base` cell).

- on_top:

  Draw the inset above (`TRUE`) or below the base.

- shadow:

  Optional drop shadow cast by the inset, from
  [`vellum::vl_shadow()`](https://r-vellum.github.io/vellum/reference/vl_shadow.html)
  (e.g. `vl_shadow(dx = 2, dy = 3, blur = 6)`).

## Value

A `PlotComposition`.

## Details

A plot with a locked aspect ratio
([`coord_fixed()`](https://r-vellum.github.io/vellumplot/reference/coord_cartesian.md),
[`coord_sf()`](https://r-vellum.github.io/vellumplot/reference/coord_sf.md),
[`coord_polar()`](https://r-vellum.github.io/vellumplot/reference/coord_polar.md),
or `theme(aspect.ratio =)`) is fitted inside the inset box: the box
shrinks in one direction to the plot's own shape, keeping the side
nearest the edge it sits on (a box in the right half keeps its right
edge, one in the top half its top edge), so its background and border
hug the panel rather than framing empty bands.

Each inset's viewport is named `"inset-<k>"` (the k-th inset of the
composition), so it can be found with
[`vellum::node_names()`](https://r-vellum.github.io/vellum/reference/node_names.html).

## Examples

``` r
a <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
b <- vplot(mtcars) |> mark_histogram(x = mpg, bins = 8)
inset(a, b, left = 0.55, bottom = 0.55, right = 0.98, top = 0.98)
```
