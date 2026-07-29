# Serialize a plot to / from a JSON spec string

`spec_to_json()` renders a
[PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md)
as a JSON string (via
[`as_spec()`](https://r-vellum.github.io/vellumplot/reference/as_spec.md));
`spec_from_json()` parses one back into a `PlotSpec` (via
[`from_spec()`](https://r-vellum.github.io/vellumplot/reference/as_spec.md)).
This is the portable wire format for the LLM / agent tooling and the
Vega-Lite bridge. See
[`as_spec()`](https://r-vellum.github.io/vellumplot/reference/as_spec.md)
for the serializable subset and its guarantees.

## Usage

``` r
spec_to_json(plot, pretty = TRUE)

spec_from_json(json, data = NULL, env = globalenv())
```

## Arguments

- plot:

  A
  [PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

- pretty:

  Whether to pretty-print (default `TRUE`).

- json:

  A JSON spec string (or a path to a `.json` file).

- data, env:

  Passed to
  [`from_spec()`](https://r-vellum.github.io/vellumplot/reference/as_spec.md)
  — a data frame for a by-reference spec, and the environment channel
  expressions are re-quoted in.

## Value

`spec_to_json()` returns a length-1 JSON string; `spec_from_json()`
returns a
[PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

## See also

[`as_spec()`](https://r-vellum.github.io/vellumplot/reference/as_spec.md),
[`spec_schema()`](https://r-vellum.github.io/vellumplot/reference/spec_schema.md),
[`vplot_from_spec()`](https://r-vellum.github.io/vellumplot/reference/spec_diagnose.md)

## Examples

``` r
p <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
json <- spec_to_json(p)
p2 <- spec_from_json(json)
```
