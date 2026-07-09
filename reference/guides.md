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

guide_legend(title = NULL, reverse = FALSE)
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
```
