# Resolve scales as shared or independent across panels

The scale-resolution lattice (after Vega-Lite): for each aesthetic,
choose whether faceted panels share one trained scale or train
independent ones. Every scale defaults to `"shared"`;
`facet_*(scales = "free_*")` is sugar for setting `x`/`y` to
`"independent"`.

## Usage

``` r
resolve_scale(plot, ...)
```

## Arguments

- plot:

  A
  [PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

- ...:

  Named arguments mapping an aesthetic (`x`, `y`, `color`, `size`, ...)
  to `"shared"` or `"independent"`.

## Value

The modified
[PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

## Details

A non-position aesthetic (`color`, `size`, `alpha`, `shape`, `linetype`,
`linewidth`, `pattern`, ...) set to `"independent"` is trained in each
panel from that panel's data alone, so each panel spends the whole
palette or range on its own values, and each panel gets its own legend.
The legends are collected in the legend area, titled with the scale
title and the panel's strip text (`"hp (cyl: 4)"`). Scales left shared
keep their single legend. `colour` and `fill` both name the colour
scale. A plot with a single panel ignores the setting.

## Examples

``` r
vplot(mtcars) |> mark_point(x = wt, y = mpg) |> facet_wrap(~cyl) |>
  resolve_scale(y = "independent")


# one colour legend per panel
vplot(mtcars) |>
  mark_point(x = wt, y = mpg, color = factor(gear)) |>
  facet_wrap(~cyl) |>
  resolve_scale(color = "independent")
```
