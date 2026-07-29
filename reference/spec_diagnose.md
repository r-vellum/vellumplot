# Diagnose a spec against its data

`spec_diagnose()` checks a spec (a list, JSON string, or path) without
throwing: it parses it, validates every referenced field against the
data, and dry-run compiles it, returning a structured verdict.
`vplot_from_spec()` is the strict form — it returns the
[PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md)
on success and raises a classed `vellumplot_spec_invalid` error
(carrying the same diagnostics) on failure.

## Usage

``` r
spec_diagnose(spec, data = NULL, env = globalenv())

vplot_from_spec(spec, data = NULL, env = globalenv())
```

## Arguments

- spec:

  A spec list (from
  [`as_spec()`](https://r-vellum.github.io/vellumplot/reference/as_spec.md)),
  a JSON string, or a `.json` path.

- data:

  The data frame the spec is drawn from. Required when the spec stores
  data by reference; optional (for field validation) when inlined.

- env:

  Environment channel expressions are re-quoted in.

## Value

`spec_diagnose()` returns a list with `ok` (logical), `plot` (the
[PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md)
or `NULL`), and `diagnostics` (a list of records with `severity`,
`field`, `message`, `hint`). `vplot_from_spec()` returns a
[PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

## Details

This is the agent repair loop: a failure comes back as data an agent can
act on (which field is unknown, the nearest real field) rather than a
traceback.

## See also

[`spec_fields()`](https://r-vellum.github.io/vellumplot/reference/spec_fields.md),
[`spec_schema()`](https://r-vellum.github.io/vellumplot/reference/spec_schema.md),
[`vplot_ask()`](https://r-vellum.github.io/vellumplot/reference/vplot_ask.md)

## Examples

``` r
spec <- as_spec(vplot(mtcars) |> mark_point(x = wt, y = mpg))
d <- spec_diagnose(spec, data = mtcars)
d$ok
#> [1] TRUE
```
