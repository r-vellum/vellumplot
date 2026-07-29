# Generate a plot from a natural-language request

`vplot_ask()` is a thin, **model-agnostic** convenience: it assembles
the grounding payload (the JSON
[`spec_schema()`](https://r-vellum.github.io/vellumplot/reference/spec_schema.md)
plus
[`spec_fields()`](https://r-vellum.github.io/vellumplot/reference/spec_fields.md)
for `data` and the user's `prompt`), hands it to a `responder` function
you supply — any function that takes the payload and returns a JSON spec
string (wrapping the Claude API, an MCP client, a local model, or a
canned fixture) — and validates the result with
[`vplot_from_spec()`](https://r-vellum.github.io/vellumplot/reference/spec_diagnose.md).

## Usage

``` r
vplot_ask(prompt, data, responder, env = globalenv())
```

## Arguments

- prompt:

  A natural-language description of the desired plot.

- data:

  The data frame to plot.

- responder:

  A function `function(payload) -> json_string`. `payload` is a list
  with `prompt`, `fields` (from
  [`spec_fields()`](https://r-vellum.github.io/vellumplot/reference/spec_fields.md)),
  and `schema`.

- env:

  Environment channel expressions are re-quoted in.

## Value

A
[PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

## Details

vellumplot deliberately ships no built-in model client: the value is the
schema, the grounding, and the validated repair loop, not a hard
dependency on one provider. For a ready-to-run agent integration, point
an MCP client at the bundled server
(`system.file("mcp/server.R", package = "vellumplot")`).

## See also

[`spec_fields()`](https://r-vellum.github.io/vellumplot/reference/spec_fields.md),
[`vplot_from_spec()`](https://r-vellum.github.io/vellumplot/reference/spec_diagnose.md),
[`spec_schema()`](https://r-vellum.github.io/vellumplot/reference/spec_schema.md)

## Examples

``` r
# a canned responder standing in for a model call:
responder <- function(payload) {
  spec_to_json(vplot(mtcars) |> mark_point(x = wt, y = mpg))
}
p <- vplot_ask("scatter of weight vs mpg", mtcars, responder)
```
