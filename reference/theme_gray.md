# Plot themes

Control the non-data look of a plot. `theme_gray()` is the default (grey
panel, white gridlines); `theme_minimal()` drops the panel fill for
light gridlines on the page; `theme_bw()` is a white panel with light
grey gridlines; `theme_classic()` has axis lines and no gridlines;
`theme_void()` strips everything but the marks, legend, and titles;
`theme_cyberpunk()` is a dark neon theme (see Details).

## Usage

``` r
theme_gray(plot, base_size = 11, base_family = "")

theme_minimal(plot, base_size = 11, base_family = "")

theme_bw(plot, base_size = 11, base_family = "")

theme_classic(plot, base_size = 11, base_family = "")

theme_void(plot, base_size = 11, base_family = "")

theme_light(plot, base_size = 11, base_family = "")

theme_dark(plot, base_size = 11, base_family = "")

theme_linedraw(plot, base_size = 11, base_family = "")

theme_cyberpunk(plot, base_size = 11, base_family = "")

theme(plot, ...)

set_theme(
  plot,
  panel_bg = NULL,
  grid_col = NULL,
  label_col = NULL,
  strip_bg = NULL
)
```

## Arguments

- plot:

  A
  [PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).
  `theme()` also accepts a `PlotComposition`, setting the figure-level
  chrome (title bands, collected legend, panel spacing, tags).

- base_size:

  Base text size in points (default 11). Every text element is sized
  relative to it, so `theme_gray(base_size = 14)` scales titles, axis
  text, legend text and strips together.

- base_family:

  Base font family for all text; `""` (default) keeps the system
  default.

- ...:

  Named theme elements, e.g. `plot.title = element_text(size = 16)`,
  `panel.grid.minor = element_blank()`, or settings like
  `legend.position`, one of `"right"` (default), `"left"`, `"top"`,
  `"bottom"`, `"none"`, or `"inside"`. An inside legend sits over the
  panels at `legend.position.inside = c(x, y)` (fractions of the panel
  area; the older `legend.position = c(x, y)` works too), its
  `legend.justification` point (`c(x, y)` or words such as
  `c("right", "top")`, defaulting to the position) on that spot. Legends
  drawn beside each facet panel (`guide_legend(position = "panel")`) sit
  on the side `legend.position` names, justified along the panel edge by
  `legend.justification` (e.g. `"top"` aligns a legend right of a panel
  with the panel's top). Legend geometry is tunable via
  `legend.key.size` (key/swatch side, mm), `legend.spacing` (gap between
  stacked guides, mm), and `legend.margin` (inset around the legend
  block, one or four millimetres, `t, r, b, l`).

- panel_bg, grid_col, label_col, strip_bg:

  Colours (or `NA` to draw nothing) for the panel background, gridlines,
  axis-label/legend text, and facet strip background.

## Value

The modified
[PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

## Details

`theme()` overrides individual elements on top of the current theme
using
[`element_text()`](https://r-vellum.github.io/vellumplot/reference/element.md)
/
[`element_line()`](https://r-vellum.github.io/vellumplot/reference/element.md)
/
[`element_rect()`](https://r-vellum.github.io/vellumplot/reference/element.md)
/
[`element_blank()`](https://r-vellum.github.io/vellumplot/reference/element.md).
`set_theme()` is a small back-compatible shortcut for the most common
colours.

`theme_cyberpunk()` sets a dark canvas with dim neon gridlines and a
bright neon default palette (both discrete and continuous), in the
spirit of [mplcyberpunk](https://github.com/dhaitz/mplcyberpunk). It
pairs with the
[`glow()`](https://r-vellum.github.io/vellumplot/reference/glow.md)
layer effect and
[`linear_gradient()`](https://r-vellum.github.io/vellumplot/reference/gradients.md)
fills for the full neon look; the palette is only a *default*, so
`scale_*` overrides still win.

## Examples

``` r
vplot(mtcars) |> mark_point(x = wt, y = mpg) |> theme_minimal()

vplot(mtcars) |>
  mark_point(x = wt, y = mpg) |>
  theme(panel.grid.minor = element_blank(), legend.position = "none")


# a legend tucked into the top-right corner of the panel
vplot(mtcars) |>
  mark_point(x = wt, y = mpg, color = factor(cyl)) |>
  theme(legend.position = "inside", legend.justification = c(1, 1))
```
