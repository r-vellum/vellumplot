# Serialize a plot to a plain spec (and back)

`as_spec()` turns a
[PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md)
into a plain, nested, **serializable** R list (data, layers, encodings,
scales, coordinates, facets, labels, page size); `from_spec()` rebuilds
a `PlotSpec` from that list. Together with
[`spec_to_json()`](https://r-vellum.github.io/vellumplot/reference/spec_to_json.md)
/
[`spec_from_json()`](https://r-vellum.github.io/vellumplot/reference/spec_to_json.md)
this makes a vellumplot plot a portable *document* — the substrate for
the LLM / agent tooling
([`spec_fields()`](https://r-vellum.github.io/vellumplot/reference/spec_fields.md),
[`vplot_from_spec()`](https://r-vellum.github.io/vellumplot/reference/spec_diagnose.md))
and the Vega-Lite bridge
([`spec_to_vegalite()`](https://r-vellum.github.io/vellumplot/reference/spec_to_vegalite.md)).

## Usage

``` r
as_spec(plot)

from_spec(spec, data = NULL, env = globalenv())
```

## Arguments

- plot:

  A
  [PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

- spec:

  A spec list from `as_spec()`.

- data:

  Optional data frame to attach when `spec` stores its data by
  reference. Ignored when the spec inlines its data.

- env:

  The environment channel expressions are re-quoted in (default the
  global environment).

## Value

`as_spec()` returns a named list; `from_spec()` returns a
[PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

## Details

The spec is a **subset** of the full grammar — the encoding-level state
that a portable document can carry faithfully. State that a JSON
document cannot represent (custom transform *functions*, paint/pattern
fills, hand-drawn
[`sketch()`](https://r-vellum.github.io/vellumplot/reference/sketch.md)
geometry, secondary axes, `sf` CRS objects, per-layer data) is
**refused** with a classed `vellumplot_unserializable` error naming the
exact slot — it is never silently dropped. The single exception is theme
element customisation, which is styling orthogonal to the data spec: the
theme *preset* name and its scalar settings round-trip, but custom
`element_*()` overrides are dropped with a warning.

Channel expressions are stored as text (a column name, or an expression
like `log(x)`); the quosure's captured environment is dropped, so an
encoding that closes over a local variable will not round-trip.

Data is **inlined** when small (at most a few thousand cells and
all-atomic columns) and otherwise stored **by reference** (name +
content hash + column schema); a by-reference spec needs
`from_spec(spec, data = )` to recompile.

## See also

[`spec_to_json()`](https://r-vellum.github.io/vellumplot/reference/spec_to_json.md),
[`spec_to_vegalite()`](https://r-vellum.github.io/vellumplot/reference/spec_to_vegalite.md),
[`vplot_from_spec()`](https://r-vellum.github.io/vellumplot/reference/spec_diagnose.md)

## Examples

``` r
p <- vplot(mtcars) |> mark_point(x = wt, y = mpg, color = cyl)
spec <- as_spec(p)
spec$layers[[1]]$mark
#> [1] "point"
p2 <- from_spec(spec)
```
