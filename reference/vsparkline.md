# Sparklines — tiny word-sized charts

`vsparkline()` builds a compact, axis-free chart of a single numeric
series, sized in physical units (mm by default) so it reads as a
*word-sized graphic* (Tufte's sparkline) — for a table cell, a caption,
or a dashboard tile. It is a self-contained
[PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md):
render it with
[`render_plot()`](https://r-vellum.github.io/vellumplot/reference/render_plot.md),
drop it into a composition with
[`inset()`](https://r-vellum.github.io/vellumplot/reference/inset.md),
or (soon) a table cell.

## Usage

``` r
vsparkline(
  values,
  type = c("line", "bar", "winloss"),
  color = "grey30",
  points = c("extremes", "last", "none"),
  point_color = "firebrick",
  baseline = 0,
  win_color = "#2c7fb8",
  loss_color = "#d7301f",
  linewidth = 1,
  point_size = 1.5,
  width = 20,
  height = 6,
  units = "mm",
  dpi = 96
)
```

## Arguments

- values:

  A numeric vector (the series), length \>= 2.

- type:

  `"line"` (default), `"bar"`, or `"winloss"`.

- color:

  Trend / bar colour. Default `"grey30"`.

- points:

  For `type = "line"`, which points get a dot: `"extremes"` (default –
  the min and max), `"last"`, or `"none"`.

- point_color:

  Dot colour (default `"firebrick"`).

- baseline:

  For `"bar"`, the value bars grow from (default `0`); for `"winloss"`,
  the threshold separating a win (`>=`) from a loss (default `0`).

- win_color, loss_color:

  For `"winloss"`, the up / down bar colours (defaults blue / red).

- linewidth:

  Trend line width (default `1`).

- point_size:

  Dot diameter in mm (default `1.5`).

- width, height, units:

  Physical size of the sparkline; `units` is one of `"mm"` (default),
  `"cm"`, `"in"`, `"pt"`. Default `20 x 6` mm.

- dpi:

  Resolution for raster output.

## Value

A
[PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

## Details

Three shapes via `type`: a `"line"` trend (with optional dots on its
extremes or last point), a `"bar"` column micro-chart, and a `"winloss"`
chart of equal up/down bars about a baseline (for streaks of
wins/losses, gains/drops).

The chart fills its box with no axes, gridlines, labels, or legend.

## See also

[`render_plot()`](https://r-vellum.github.io/vellumplot/reference/render_plot.md),
[`inset()`](https://r-vellum.github.io/vellumplot/reference/inset.md)

## Examples

``` r
set.seed(1)
vsparkline(cumsum(rnorm(30)))

vsparkline(rpois(20, 5), type = "bar")

vsparkline(sample(c(-1, 1), 20, replace = TRUE), type = "winloss")
```
