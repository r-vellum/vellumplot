# The vellumplot spec JSON Schema

Returns the bundled JSON Schema (Draft 2020-12) describing a
serializable vellumplot spec — the input contract for
[`spec_from_json()`](https://r-vellum.github.io/vellumplot/reference/spec_to_json.md)
and the MCP `render_spec` / `get_schema` tools. An agent can validate a
generated spec against it before rendering.

## Usage

``` r
spec_schema(as = c("string", "list", "path"))
```

## Arguments

- as:

  A character scalar: `"string"` (raw JSON, the default), `"list"`
  (parsed R list), or `"path"` (the file path in the installed package).

## Value

The schema as a string, a list, or a file path.

## See also

[`spec_to_json()`](https://r-vellum.github.io/vellumplot/reference/spec_to_json.md),
[`spec_fields()`](https://r-vellum.github.io/vellumplot/reference/spec_fields.md)

## Examples

``` r
schema <- spec_schema(as = "list")
schema$title
#> [1] "vellumplot spec (v1)"
```
