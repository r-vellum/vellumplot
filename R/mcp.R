#' @include spec-fields.R
NULL

# ---------------------------------------------------------------------------
# MCP adapter (GAPS-HORIZON Feature 1c).
#
# A thin, pure-R Model Context Protocol server so an agent (Claude Code, an SDK
# client, ...) can generate *validated* vellumplot specs. It speaks newline-
# delimited JSON-RPC 2.0 over stdio and holds NO logic of its own: the three
# tools delegate to spec_fields() / spec_schema() / spec_diagnose() + render.
#
# The dispatch core (`.mcp_dispatch`) is a pure function of a request list, so it
# is unit-testable without spawning a process; `mcp_serve()` is the stdio loop.
# Run it via:  Rscript -e 'vellumplot::mcp_serve()'  (see inst/mcp/server.R).
# ---------------------------------------------------------------------------

.MCP_PROTOCOL <- "2024-11-05"

# The three tool definitions advertised to the client.
.mcp_tools <- function() {
  list(
    list(
      name = "get_schema",
      description = "Return the JSON Schema for a vellumplot spec. Generate specs that validate against it, then render with render_spec.",
      inputSchema = list(
        type = "object",
        properties = structure(list(), names = character(0))
      )
    ),
    list(
      name = "list_fields",
      description = "List the columns of a CSV dataset with inferred encoding types (quantitative/nominal/ordinal/temporal) and examples. Call this first to ground a spec in the real field names.",
      inputSchema = list(
        type = "object",
        properties = list(
          data_path = list(type = "string", description = "Path to a CSV file.")
        ),
        required = list("data_path")
      )
    ),
    list(
      name = "render_spec",
      description = "Validate a vellumplot spec against the data and render it. Returns the output file path on success, or structured diagnostics (unknown field, compile error) to repair and retry.",
      inputSchema = list(
        type = "object",
        properties = list(
          spec = list(
            type = "string",
            description = "The vellumplot spec as a JSON string."
          ),
          data_path = list(
            type = "string",
            description = "Path to a CSV file (if the spec does not inline data)."
          ),
          out_path = list(
            type = "string",
            description = "Output file path; extension picks png/svg/pdf. Defaults to a temp PNG."
          )
        ),
        required = list("spec")
      )
    )
  )
}

.mcp_text <- function(text, is_error = FALSE) {
  list(content = list(list(type = "text", text = text)), isError = is_error)
}

# Read a CSV data path argument, or NULL. Errors surface as a tool error result.
.mcp_read_data <- function(args) {
  path <- args$data_path
  if (is.null(path)) {
    return(NULL)
  }
  utils::read.csv(path, stringsAsFactors = FALSE, check.names = TRUE)
}

# Execute one tool call; returns an MCP tool-result list.
.mcp_call_tool <- function(name, args) {
  # A `tools/call` without a `params.name` would make `switch(name, ...)` throw
  # an internal "EXPR must be a length 1 vector" error; return a clean tool error.
  if (!is.character(name) || length(name) != 1L) {
    return(.mcp_text("Missing tool 'name'.", is_error = TRUE))
  }
  tryCatch(
    switch(
      name,
      get_schema = .mcp_text(spec_schema(as = "string")),
      list_fields = {
        data <- .mcp_read_data(args)
        if (is.null(data)) {
          return(.mcp_text(
            "Provide 'data_path' (a CSV file).",
            is_error = TRUE
          ))
        }
        .mcp_text(jsonlite::toJSON(
          spec_fields(data),
          dataframe = "rows",
          auto_unbox = TRUE,
          pretty = TRUE
        ))
      },
      render_spec = {
        data <- .mcp_read_data(args)
        diag <- spec_diagnose(args$spec, data = data)
        if (!diag$ok) {
          return(.mcp_text(
            jsonlite::toJSON(
              list(ok = FALSE, diagnostics = diag$diagnostics),
              auto_unbox = TRUE,
              pretty = TRUE,
              null = "null"
            ),
            is_error = TRUE
          ))
        }
        out <- args$out_path %||% tempfile(fileext = ".png")
        render_plot(diag$plot, out)
        .mcp_text(jsonlite::toJSON(
          list(ok = TRUE, path = out),
          auto_unbox = TRUE,
          pretty = TRUE
        ))
      },
      .mcp_text(paste0("Unknown tool: ", name), is_error = TRUE)
    ),
    error = function(e) .mcp_text(conditionMessage(e), is_error = TRUE)
  )
}

# Dispatch one JSON-RPC request list -> a response list, or NULL for a
# notification (no `id`, no reply). Pure function: no I/O.
.mcp_dispatch <- function(req) {
  if (!is.list(req)) {
    return(NULL) # not a JSON object -- no id to reply to; drop it
  }
  id <- req$id
  method <- req$method
  reply <- function(result) list(jsonrpc = "2.0", id = id, result = result)
  err <- function(code, message) {
    list(jsonrpc = "2.0", id = id, error = list(code = code, message = message))
  }
  if (is.null(id)) {
    return(NULL) # a notification (e.g. notifications/initialized)
  }
  # A malformed request (no/blank/non-scalar `method`) must return a JSON-RPC
  # error, not throw -- `switch(NULL, ...)` would error and, unguarded, take the
  # whole `mcp_serve()` loop down with it.
  if (
    !is.character(method) ||
      length(method) != 1L ||
      is.na(method) ||
      !nzchar(method)
  ) {
    return(err(-32600, "Invalid Request: missing or malformed 'method'."))
  }
  switch(
    method,
    initialize = reply(list(
      protocolVersion = .MCP_PROTOCOL,
      capabilities = list(tools = structure(list(), names = character(0))),
      serverInfo = list(
        name = "vellumplot",
        version = as.character(utils::packageVersion("vellumplot"))
      )
    )),
    "tools/list" = reply(list(tools = .mcp_tools())),
    "tools/call" = reply(.mcp_call_tool(
      req$params$name,
      req$params$arguments %||% list()
    )),
    "ping" = reply(structure(list(), names = character(0))),
    err(-32601, paste0("Method not found: ", method))
  )
}

#' Run the vellumplot MCP server
#'
#' Starts a Model Context Protocol server on stdio so an agent can generate
#' validated vellumplot specs. It exposes three tools — `get_schema`,
#' `list_fields`, and `render_spec` — that delegate to [spec_schema()],
#' [spec_fields()], and [spec_diagnose()] + [render_plot()]. A `render_spec`
#' failure returns structured diagnostics (unknown field, compile error) so the
#' agent can repair and retry, rather than a traceback.
#'
#' The server is pure R (newline-delimited JSON-RPC 2.0 over stdio) and needs no
#' Node or SDK. Point an MCP client at it with:
#' `Rscript -e 'vellumplot::mcp_serve()'`, or the bundled launcher
#' `system.file("mcp/server.R", package = "vellumplot")`.
#'
#' @param input,output Connections to read requests from / write responses to
#'   (defaults: standard input / output).
#' @return Invisibly `NULL`, when the input stream closes.
#' @seealso [spec_schema()], [spec_fields()], [vplot_from_spec()]
#' @export
mcp_serve <- function(input = "stdin", output = stdout()) {
  .need_pkg("jsonlite", "mcp_serve()")
  con <- if (identical(input, "stdin")) file("stdin") else input
  # Open the connection ONCE and hold it across the loop. A connection left
  # unopened is opened-and-closed by each `readLines()`, which re-reads from the
  # top of the stream every pass -- so the server saw EOF after the first line and
  # a real MCP client died right after `initialize`. Close only a connection we
  # opened (an already-open one the caller passed is theirs to manage).
  if (inherits(con, "connection") && !isOpen(con)) {
    open(con, "r")
    on.exit(close(con), add = TRUE)
  }
  repeat {
    line <- readLines(con, n = 1L, warn = FALSE)
    if (!length(line)) {
      break # EOF
    }
    if (!nzchar(trimws(line))) {
      next
    }
    req <- tryCatch(
      jsonlite::fromJSON(line, simplifyVector = FALSE),
      error = function(e) NULL
    )
    if (is.null(req)) {
      next
    }
    # Never let one request kill the session: any unexpected dispatch error
    # becomes an internal-error reply (or is dropped when there is no id).
    resp <- tryCatch(
      .mcp_dispatch(req),
      error = function(e) {
        rid <- tryCatch(req$id, error = function(e2) NULL)
        if (is.null(rid)) {
          return(NULL)
        }
        list(
          jsonrpc = "2.0",
          id = rid,
          error = list(code = -32603, message = conditionMessage(e))
        )
      }
    )
    if (!is.null(resp)) {
      writeLines(
        jsonlite::toJSON(resp, auto_unbox = TRUE, null = "null"),
        output
      )
      flush(output)
    }
  }
  invisible(NULL)
}
