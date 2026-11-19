# Position scales

Declare a position scale to override the trained default.
`scale_x_continuous()` / `scale_y_continuous()` handle numeric (and
date/time) axes; `scale_x_discrete()` / `scale_y_discrete()` handle
categorical (band) axes and let you set the level order via `limits`.

`scale_x_log10()`, `scale_x_sqrt()` and `scale_x_reverse()` (and their
`y` forms) are shortcuts for `scale_x_continuous(trans = "log10")` and
so on, as in ggplot2; `...` takes the other arguments of
`scale_x_continuous()`.

## Usage

``` r
scale_x_continuous(
  plot,
  limits = NULL,
  trans = "identity",
  breaks = NULL,
  labels = NULL,
  name = NULL,
  expand = NULL,
  sec.axis = NULL,
  oob = NULL,
  guide = NULL
)

scale_y_continuous(
  plot,
  limits = NULL,
  trans = "identity",
  breaks = NULL,
  labels = NULL,
  name = NULL,
  expand = NULL,
  sec.axis = NULL,
  oob = NULL,
  guide = NULL
)

scale_x_log10(plot, ...)

scale_x_sqrt(plot, ...)

scale_x_reverse(plot, ...)

scale_y_log10(plot, ...)

scale_y_sqrt(plot, ...)

scale_y_reverse(plot, ...)

scale_x_discrete(
  plot,
  limits = NULL,
  name = NULL,
  breaks = NULL,
  labels = NULL,
  drop = TRUE,
  guide = NULL
)

scale_y_discrete(
  plot,
  limits = NULL,
  name = NULL,
  breaks = NULL,
  labels = NULL,
  drop = TRUE,
  guide = NULL
)
```

## Arguments

- plot:

  A
  [PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

- limits:

  For continuous scales a numeric length-2 domain `c(min, max)`; for
  discrete scales a character vector of levels (sets order / subset).
  `NULL` trains from the data.

- trans:

  Transformation: `"identity"` (default), `"log10"`, `"sqrt"`,
  `"symlog"` (symmetric log — linear through zero, logarithmic in the
  tails, so signed data and zero/negatives read on one axis),
  `"reverse"`, or a
  [`scales::transform_log10()`](https://scales.r-lib.org/reference/transform_log.html)-style
  transform object.

- breaks, labels:

  Explicit break positions (data units) and their labels, or `NULL` to
  compute them. On a discrete scale `breaks` is the levels that get a
  tick, and `labels` a character vector (one per break, or named by
  level) or a function of the levels.

- name:

  Axis title, or `NULL` to derive from the encoding.

- expand:

  Axis padding beyond the data. `NULL` (default) keeps the usual 5%
  breathing room; a numeric `c(mult, add)` sets a proportion of the
  range plus a constant (either may be `0`), applied to both ends. Both
  act in the scale's *transformed* space, so on a `trans = "log10"` axis
  `add` is in log units (matching how the axis is drawn). Use
  `expand = c(0, 0)` to clamp the axis exactly to the data (e.g. a bar
  chart's baseline), or `expand = c(0.1, 0)` for a wider margin.

- sec.axis:

  A secondary axis from
  [`sec_axis()`](https://r-vellum.github.io/vellumplot/reference/sec_axis.md)
  /
  [`dup_axis()`](https://r-vellum.github.io/vellumplot/reference/sec_axis.md),
  drawn on the opposite edge, or `NULL` for none. Continuous Cartesian
  plots only (see
  [`sec_axis()`](https://r-vellum.github.io/vellumplot/reference/sec_axis.md)
  for the current limitations).

- oob:

  What to do with values outside `limits`, as in
  [`scales::oob_censor()`](https://scales.r-lib.org/reference/oob.html)
  and friends: `"censor"` drops them (they become `NA` and are not
  drawn), `"squish"` moves them onto the nearest limit, `"keep"` draws
  them where they fall, clipped by the panel. A function
  `function(x, range)` such as
  [`scales::oob_squish_any()`](https://scales.r-lib.org/reference/oob.html)
  also works. `NULL` (default) is `"keep"` on a position axis. Infinite
  values are never out of bounds: `x = Inf` sits on the panel edge.

- guide:

  The axis guide:
  [`guide_axis()`](https://r-vellum.github.io/vellumplot/reference/guide_axis.md)
  to stagger, rotate or thin the labels, `"none"` for no labels or
  ticks, or `NULL` (default) for the plain axis. Same as `guides(x = )`
  / `guides(y = )`.

- ...:

  For the transform shortcuts, further arguments to
  `scale_x_continuous()` / `scale_y_continuous()` (not `trans`).

- drop:

  For discrete scales, drop factor levels that do not occur in the data
  (default `TRUE`, as in ggplot2). `limits` overrides it.

## Value

The modified
[PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

## Examples

``` r
vplot(mtcars) |> mark_point(x = wt, y = mpg) |> scale_x_continuous(limits = c(0, 6))

vplot(mtcars) |> mark_point(x = wt, y = mpg) |> scale_y_continuous(trans = "sqrt")
```
