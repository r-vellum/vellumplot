#' @include spec-serialize.R
NULL

# ---------------------------------------------------------------------------
# JSON marshalling of the spec IR (GAPS-HORIZON Phase A / Feature 1).
#
# `spec_to_json()` / `spec_from_json()` are thin adapters over `as_spec()` /
# `from_spec()` and `jsonlite`. The JSON *is* the public contract consumed by the
# MCP tools and any external agent; `spec_schema()` returns the bundled JSON
# Schema so a caller can validate a spec before `from_spec()`.
# ---------------------------------------------------------------------------

# Read a JSON spec -- a JSON string, a character vector of lines, or a path to a
# file -- into a plain IR list. A single string with no `{`/newline that names an
# existing file is read from disk. `simplifyDataFrame`/`Matrix` are OFF so nested
# records stay lists (the shape the `*_from_ir` readers expect). Single source
# for `spec_from_json()`, `spec_diagnose()`, and `spec_from_vegalite()`.
.read_json_spec <- function(x, what) {
  .need_pkg("jsonlite", what)
  if (length(x) == 1 && !grepl("[{\n]", x) && file.exists(x)) {
    x <- readLines(x, warn = FALSE)
  }
  jsonlite::fromJSON(
    paste(x, collapse = "\n"),
    simplifyVector = TRUE,
    simplifyDataFrame = FALSE,
    simplifyMatrix = FALSE
  )
}

#' Serialize a plot to / from a JSON spec string
#'
#' `spec_to_json()` renders a [PlotSpec] as a JSON string (via [as_spec()]);
#' `spec_from_json()` parses one back into a `PlotSpec` (via [from_spec()]). This
#' is the portable wire format for the LLM / agent tooling and the Vega-Lite
#' bridge. See [as_spec()] for the serializable subset and its guarantees.
#'
#' @param plot A [PlotSpec].
#' @param json A JSON spec string (or a path to a `.json` file).
#' @param pretty Whether to pretty-print (default `TRUE`).
#' @param data,env Passed to [from_spec()] — a data frame for a by-reference
#'   spec, and the environment channel expressions are re-quoted in.
#' @return `spec_to_json()` returns a length-1 JSON string; `spec_from_json()`
#'   returns a [PlotSpec].
#' @seealso [as_spec()], [spec_schema()], [vplot_from_spec()]
#' @examplesIf requireNamespace("jsonlite", quietly = TRUE)
#' p <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
#' json <- spec_to_json(p)
#' p2 <- spec_from_json(json)
#' @export
spec_to_json <- function(plot, pretty = TRUE) {
  .need_pkg("jsonlite", "spec_to_json()")
  jsonlite::toJSON(
    as_spec(plot),
    auto_unbox = TRUE,
    null = "null",
    na = "null",
    pretty = pretty,
    digits = NA
  )
}

#' @rdname spec_to_json
#' @export
spec_from_json <- function(json, data = NULL, env = globalenv()) {
  spec <- .read_json_spec(json, "spec_from_json()")
  from_spec(spec, data = data, env = env)
}

#' The vellumplot spec JSON Schema
#'
#' Returns the bundled JSON Schema (Draft 2020-12) describing a serializable
#' vellumplot spec — the input contract for [spec_from_json()] and the MCP
#' `render_spec` / `get_schema` tools. An agent can validate a generated spec
#' against it before rendering.
#'
#' @param as A character scalar: `"string"` (raw JSON, the default), `"list"`
#'   (parsed R list), or `"path"` (the file path in the installed package).
#' @return The schema as a string, a list, or a file path.
#' @seealso [spec_to_json()], [spec_fields()]
#' @examplesIf requireNamespace("jsonlite", quietly = TRUE)
#' schema <- spec_schema(as = "list")
#' schema$title
#' @export
spec_schema <- function(as = c("string", "list", "path")) {
  as <- match.arg(as)
  path <- system.file(
    "schema",
    "vellumplot-spec-v1.json",
    package = "vellumplot"
  )
  if (!nzchar(path)) {
    cli::cli_abort(
      "The bundled spec schema was not found in the installed package."
    )
  }
  if (identical(as, "path")) {
    return(path)
  }
  txt <- paste(readLines(path, warn = FALSE), collapse = "\n")
  if (identical(as, "string")) {
    return(txt)
  }
  .need_pkg("jsonlite", "spec_schema(as = \"list\")")
  jsonlite::fromJSON(txt, simplifyVector = TRUE, simplifyDataFrame = FALSE)
}
