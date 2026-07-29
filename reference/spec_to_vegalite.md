# Convert between a vellumplot spec and a Vega-Lite specification

`spec_to_vegalite()` translates a
[PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md)
into a [Vega-Lite](https://vega.github.io/vega-lite/) specification;
`spec_from_vegalite()` translates one back. Both are layered grammars of
graphics, so the mapping is direct for the common cases — marks,
encodings, scales (domain / log-sqrt type / categorical range / title),
`bin` + `count`, faceting, inline data, and the plot title.

## Usage

``` r
spec_to_vegalite(plot, json = FALSE)

spec_from_vegalite(vl, data = NULL, env = globalenv())
```

## Arguments

- plot:

  A
  [PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

- json:

  For `spec_to_vegalite()`, return a JSON string instead of a list.

- vl:

  A Vega-Lite spec as a parsed list or a JSON string.

- data:

  Optional data frame for a Vega-Lite spec whose data is external.

- env:

  Environment channel expressions are re-quoted in (for
  `spec_from_vegalite()`).

## Value

`spec_to_vegalite()` returns a Vega-Lite spec (a list, or JSON string if
`json = TRUE`); `spec_from_vegalite()` returns a
[PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

## Details

The bridge covers a **documented subset** (see the "Specs, agents, and
interoperability" article). Anything it cannot map — polar/flipped
coordinates, layer effects, patterns, statistical marks without a
Vega-Lite equivalent — is **reported** via a warning listing the dropped
features, never silently diverged.

## See also

[`as_spec()`](https://r-vellum.github.io/vellumplot/reference/as_spec.md),
[`spec_to_json()`](https://r-vellum.github.io/vellumplot/reference/spec_to_json.md)

## Examples

``` r
p <- vplot(mtcars) |> mark_point(x = wt, y = mpg, color = cyl)
vl <- spec_to_vegalite(p)
vl$mark
#> $type
#> [1] "point"
#> 
```
