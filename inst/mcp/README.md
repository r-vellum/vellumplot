# vellumplot MCP server

A [Model Context Protocol](https://modelcontextprotocol.io) server that lets an
agent generate **validated** vellumplot charts from data. It is pure R (no Node,
no SDK) — newline-delimited JSON-RPC 2.0 over stdio — and delegates entirely to
the package's serializer, so a generated chart is *data* (a spec), never executed
code.

## Tools

| tool | purpose |
|---|---|
| `get_schema` | Return the JSON Schema a spec must satisfy. |
| `list_fields` | List a CSV's columns with inferred encoding types + examples — call first to ground field names. |
| `render_spec` | Validate a spec against the data and render it; on failure returns structured diagnostics (unknown field, compile error) to repair and retry. |

## Run it

```sh
Rscript -e 'vellumplot::mcp_serve()'
```

## Configure a client

Point your MCP client at the launcher. For example, in Claude Code:

```sh
claude mcp add vellumplot -- Rscript -e 'vellumplot::mcp_serve()'
```

Or reference the bundled launcher path:

```r
system.file("mcp/server.R", package = "vellumplot")
```

## Typical agent loop

1. `list_fields(data_path=…)` → learn the real column names and types.
2. Generate a spec that validates against `get_schema`.
3. `render_spec(spec=…, data_path=…, out_path=…)` → an image, or diagnostics to
   fix and retry.

The spec is a portable document: it round-trips through
`vellumplot::spec_from_json()` and can be re-rendered identically later.
