# Control a scale's legend

`guides()` overrides the legend (guide) for one or more aesthetics
without respelling the whole `scale_*()`. Pass `"none"` (or
`guide_none()`) to hide a legend, or `guide_legend()` to tweak it
(reverse the key order, override the title). Applies to the non-position
legends (`color`/`fill`, `size`, `shape`, `alpha`, `linetype`); position
axes are unaffected.

## Usage

``` r
guides(plot, ...)

guide_none()

guide_legend(title = NULL, reverse = FALSE, override.aes = NULL)
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

  Reverse the order of the legend keys (discrete legends).

- override.aes:

  A named list of aesthetics to force on the legend **keys**,
  independent of the plotted data — the classic "make faint, small
  points legible in the key" fix. Recognised names: `size` (mm),
  `alpha`, `colour`/`color`, `fill`, `shape`, and `linewidth`. For
  example `override.aes = list(size = 5, alpha = 1)` draws big, opaque
  keys over a scatter of tiny translucent points. `NULL` (default)
  leaves the keys as drawn from the data.

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
```
