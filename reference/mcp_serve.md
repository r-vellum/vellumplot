# Run the vellumplot MCP server

Starts a Model Context Protocol server on stdio so an agent can generate
validated vellumplot specs. It exposes three tools — `get_schema`,
`list_fields`, and `render_spec` — that delegate to
[`spec_schema()`](https://r-vellum.github.io/vellumplot/reference/spec_schema.md),
[`spec_fields()`](https://r-vellum.github.io/vellumplot/reference/spec_fields.md),
and
[`spec_diagnose()`](https://r-vellum.github.io/vellumplot/reference/spec_diagnose.md) +
[`render_plot()`](https://r-vellum.github.io/vellumplot/reference/render_plot.md).
A `render_spec` failure returns structured diagnostics (unknown field,
compile error) so the agent can repair and retry, rather than a
traceback.

## Usage

``` r
mcp_serve(input = "stdin", output = stdout())
```

## Arguments

- input, output:

  Connections to read requests from / write responses to (defaults:
  standard input / output).

## Value

Invisibly `NULL`, when the input stream closes.

## Details

The server is pure R (newline-delimited JSON-RPC 2.0 over stdio) and
needs no Node or SDK. Point an MCP client at it with:
`Rscript -e 'vellumplot::mcp_serve()'`, or the bundled launcher
`system.file("mcp/server.R", package = "vellumplot")`.

## See also

[`spec_schema()`](https://r-vellum.github.io/vellumplot/reference/spec_schema.md),
[`spec_fields()`](https://r-vellum.github.io/vellumplot/reference/spec_fields.md),
[`vplot_from_spec()`](https://r-vellum.github.io/vellumplot/reference/spec_diagnose.md)
