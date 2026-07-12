# Plot themes

Control the non-data look of a plot. `theme_gray()` is the default (grey
panel, white gridlines); `theme_minimal()` drops the panel fill for
light gridlines on the page; `theme_bw()` is a white panel with light
grey gridlines; `theme_classic()` has axis lines and no gridlines;
`theme_void()` strips everything but the marks, legend, and titles;
`theme_cyberpunk()` is a dark neon theme (see Details).

## Usage

``` r
theme_gray(plot)

theme_minimal(plot)

theme_bw(plot)

theme_classic(plot)

theme_void(plot)

theme_cyberpunk(plot)

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

- ...:

  Named theme elements, e.g. `plot.title = element_text(size = 16)`,
  `panel.grid.minor = element_blank()`, or settings like
  `legend.position`, one of `"right"` (default), `"left"`, `"top"`,
  `"bottom"`, or `"none"`. Legend geometry is tunable via
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
```
