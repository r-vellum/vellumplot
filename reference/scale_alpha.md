# Alpha (opacity) scale

Declare the scale for a mapped `alpha` aesthetic: data values map to an
opacity `range` in `[0, 1]`. An alpha legend is drawn automatically.

`scale_alpha_manual()` sets the opacity of each level of a discrete
variable directly via `values` (named values match levels by name).
`scale_alpha_discrete()` / `scale_alpha_ordinal()` spread the levels
evenly over `range`; the discrete form warns that alpha is not advised
for an unordered variable, the ordinal form does not.

## Usage

``` r
scale_alpha(
  plot,
  range = NULL,
  limits = NULL,
  breaks = NULL,
  name = NULL,
  oob = NULL
)

scale_alpha_continuous(
  plot,
  range = NULL,
  limits = NULL,
  breaks = NULL,
  name = NULL,
  oob = NULL
)

scale_alpha_manual(
  plot,
  values,
  breaks = NULL,
  labels = NULL,
  name = NULL,
  limits = NULL,
  na.value = NULL,
  drop = TRUE
)

scale_alpha_discrete(
  plot,
  range = NULL,
  breaks = NULL,
  labels = NULL,
  name = NULL,
  limits = NULL,
  na.value = NULL,
  drop = TRUE
)

scale_alpha_ordinal(
  plot,
  range = NULL,
  breaks = NULL,
  labels = NULL,
  name = NULL,
  limits = NULL,
  na.value = NULL,
  drop = TRUE
)
```

## Arguments

- plot:

  A
  [PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

- range:

  Numeric length-2 output opacity range, or `NULL` for the default
  `c(0.1, 1)`.

- limits:

  Numeric length-2 data domain, or `NULL` to train from the data.

- breaks:

  Explicit legend breaks, or `NULL`.

- name:

  Legend title, or `NULL` to derive from the encoding.

- oob:

  What to do with values outside `limits`: `"censor"` (they get no
  opacity, so the mark is not drawn), `"squish"` (the opacity of the
  nearest limit), `"keep"` (extrapolated past the range), or a
  `function(x, range)`. `NULL` (default) keeps them.

- values:

  For `scale_alpha_manual()`, opacities in `[0, 1]`, one per level (or
  named by level).

- labels:

  For the discrete forms, legend labels (one per key, a vector named by
  level, or a function), or `NULL`.

- na.value:

  For the discrete forms, the opacity drawn for missing values, or
  `NULL` to drop them.

- drop:

  For the discrete forms, drop factor levels that no row uses (default
  `TRUE`, as in ggplot2): they get no legend key and no opacity, so the
  range is spread over the levels that remain. `FALSE` keeps every
  level. `limits` overrides it.

## Value

The modified
[PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

## Details

A factor, character or logical mapped to `alpha` gets a discrete scale,
its levels spread evenly over `range`, with a warning for an unordered
variable (as in ggplot2).

## Examples

``` r
vplot(mtcars) |> mark_point(x = wt, y = mpg, alpha = hp) |> scale_alpha(range = c(0.2, 1))

vplot(mtcars) |>
  mark_point(x = wt, y = mpg, alpha = factor(cyl)) |>
  scale_alpha_manual(values = c(0.2, 0.5, 1))
```
