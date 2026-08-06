# Control a scale's legend

`guides()` overrides the legend (guide) for one or more aesthetics
without respelling the whole `scale_*()`. Pass `"none"` (or
`guide_none()`) to hide a legend, `guide_legend()` to tweak a keyed
legend (reverse the key order, override the title, restyle the keys with
`override.aes`), `guide_colourbar()` to size and style a continuous
colour bar, or `guide_coloursteps()` to draw a **binned** colour scale
as a segmented bar instead of swatches. Applies to the non-position
legends (`color`/`fill`, `size`, `shape`, `alpha`, `linetype`); position
axes are unaffected.

## Usage

``` r
guides(plot, ...)

guide_none()

guide_legend(title = NULL, reverse = FALSE, override.aes = NULL)

guide_colourbar(
  title = NULL,
  barwidth = NULL,
  barheight = NULL,
  ticks = TRUE,
  ticks.colour = "white",
  label.position = NULL,
  reverse = FALSE
)

guide_colorbar(
  title = NULL,
  barwidth = NULL,
  barheight = NULL,
  ticks = TRUE,
  ticks.colour = "white",
  label.position = NULL,
  reverse = FALSE
)

guide_coloursteps(
  title = NULL,
  barwidth = NULL,
  barheight = NULL,
  ticks = FALSE,
  ticks.colour = "white",
  label.position = NULL,
  reverse = FALSE
)

guide_colorsteps(
  title = NULL,
  barwidth = NULL,
  barheight = NULL,
  ticks = FALSE,
  ticks.colour = "white",
  label.position = NULL,
  reverse = FALSE
)
```

## Arguments

- plot:

  A
  [PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

- ...:

  Named by aesthetic, e.g.
  `guides(color = "none", shape = guide_legend(reverse = TRUE))`.

- title:

  An axis/legend title override, or `NULL` to keep the default.

- reverse:

  Reverse the order of the legend keys / the colour bar.

- override.aes:

  A named list of aesthetics to force on the legend **keys**,
  independent of the plotted data — the classic "make faint, small
  points legible in the key" fix. Recognised names: `size` (mm),
  `alpha`, `colour`/`color`, `fill`, `shape`, and `linewidth`. For
  example `override.aes = list(size = 5, alpha = 1)` draws big, opaque
  keys over a scatter of tiny translucent points. `NULL` (default)
  leaves the keys as drawn from the data.

- barwidth, barheight:

  The colour bar's own width and height, in millimetres
  (`guide_colourbar()`). `barheight` sets the bar's **length** on the
  default vertical bar (and its thickness on a horizontal one);
  `barwidth` its thickness (and the bar length on a horizontal legend).
  `NULL` auto-sizes.

- ticks:

  Draw the break ticks on the colour bar? (Default `TRUE` for
  `guide_colourbar()`, `FALSE` for the segmented `guide_coloursteps()`.)

- ticks.colour:

  Colour of the break ticks (default `"white"`).

- label.position:

  Which side of a **vertical** colour bar the labels sit, `"right"`
  (default) or `"left"`.

## Value

`guides()`: the modified
[PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).
`guide_none()` / `guide_legend()`: a guide specification for use inside
`guides()`.

## Examples

``` r
vplot(mtcars) |>
  mark_point(x = wt, y = mpg, color = factor(cyl)) |>
  guides(color = "none")


vplot(mtcars) |>
  mark_point(x = wt, y = mpg, color = factor(cyl)) |>
  guides(color = guide_legend(reverse = TRUE))


# legible keys over faint, tiny points
vplot(mtcars) |>
  mark_point(x = wt, y = mpg, color = factor(cyl), alpha = 0.15, size = 0.6) |>
  guides(color = guide_legend(override.aes = list(size = 5, alpha = 1)))


# a taller, wider colour bar with left-side labels and no ticks
vplot(mtcars) |>
  mark_point(x = wt, y = mpg, color = hp) |>
  guides(color = guide_colourbar(
    barwidth = 8, barheight = 60, ticks = FALSE, label.position = "left"
  ))


# a binned colour scale as a segmented bar
vplot(mtcars) |>
  mark_point(x = wt, y = mpg, color = hp) |>
  scale_color_binned() |>
  guides(color = guide_coloursteps())
```
