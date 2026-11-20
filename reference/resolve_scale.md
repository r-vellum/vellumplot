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

A non-position aesthetic (`color`, `fill`, `size`, `alpha`, `shape`,
`linetype`, `linewidth`, `pattern`, ...) set to `"independent"` is
trained in each panel from that panel's data alone, so each panel spends
the whole palette or range on its own values, and each panel gets its
own legend, drawn **beside its panel** (the guide position `"panel"`):
right of each panel by default, or on the side
`theme(legend.position = )` names (left of, above or below each panel,
or inside it). Each column of panels (or row, for legends above or
below) gets a legend track as wide (tall) as its largest legend. To
collect them in the legend area instead, give the guide a position, e.g.
`guides(color = guide_legend(position = "right"))`; they are then titled
with the scale title and the panel's strip text (`"hp (cyl: 4)"`).
Scales left shared keep their single legend. `colour` is an alias for
`color`; `fill` is its own scale. A plot with a single panel ignores the
setting.

## Examples

``` r
vplot(mtcars) |> mark_point(x = wt, y = mpg) |> facet_wrap(~cyl) |>
  resolve_scale(y = "independent")


# one colour legend beside each panel
vplot(mtcars) |>
  mark_point(x = wt, y = mpg, color = factor(gear)) |>
  facet_wrap(~cyl) |>
  resolve_scale(color = "independent")


# the per-panel legends collected on the right instead
vplot(mtcars) |>
  mark_point(x = wt, y = mpg, color = factor(gear)) |>
  facet_wrap(~cyl) |>
  resolve_scale(color = "independent") |>
  guides(color = guide_legend(position = "right"))
```
