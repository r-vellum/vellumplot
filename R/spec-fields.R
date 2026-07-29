#' @include spec-json.R
NULL

# ---------------------------------------------------------------------------
# LLM / agent tooling (GAPS-HORIZON Feature 1).
#
# `spec_fields()` grounds a model in the actual data (column names + inferred
# encoding types) before it generates. `spec_diagnose()` / `vplot_from_spec()`
# turn a generated spec into a plot, returning *structured* diagnostics (unknown
# field, compile failure) rather than a traceback -- the machine-readable repair
# loop. `vplot_ask()` is a thin, model-agnostic convenience over the two.
# ---------------------------------------------------------------------------

# Map an R column to a Vega-Lite-style encoding type.
.field_type <- function(col) {
  if (inherits(col, c("Date", "POSIXct", "POSIXlt"))) {
    "temporal"
  } else if (is.ordered(col)) {
    "ordinal"
  } else if (is.numeric(col)) {
    "quantitative"
  } else {
    "nominal"
  }
}

#' Summarise a data frame's fields for a model
#'
#' `spec_fields()` returns one row per column of `data` with its name, inferred
#' encoding `type` (`quantitative` / `nominal` / `ordinal` / `temporal`), R
#' class, distinct-value count, and a few example values. It is the *grounding*
#' step for LLM plot generation ([vplot_ask()], the MCP `list_fields` tool):
#' handing a model the real vocabulary up front is what prevents hallucinated
#' column names, and [vplot_from_spec()] then validates against exactly these.
#'
#' @param data A data frame.
#' @param n_examples How many example values to show per field (default 3).
#' @return A data frame with columns `name`, `type`, `class`, `n_unique`,
#'   `examples`.
#' @seealso [vplot_from_spec()], [spec_schema()]
#' @examples
#' spec_fields(mtcars)
#' @export
spec_fields <- function(data, n_examples = 3L) {
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame.")
  }
  ex <- function(col) {
    vals <- utils::head(unique(col[!is.na(col)]), n_examples)
    paste(format(vals, trim = TRUE), collapse = ", ")
  }
  data.frame(
    name = names(data),
    type = vapply(data, .field_type, character(1)),
    class = vapply(data, function(col) class(col)[1], character(1)),
    n_unique = vapply(data, function(col) length(unique(col)), integer(1)),
    examples = vapply(data, ex, character(1)),
    row.names = NULL,
    stringsAsFactors = FALSE
  )
}

# All bare-field references in a spec's channels + facets (the names that must
# exist in the data). Expression / after_stat channels are skipped -- they are
# not simple field lookups.
.spec_referenced_fields <- function(spec) {
  fields <- character(0)
  for (L in spec$layers %||% list()) {
    for (ch in L$encoding %||% list()) {
      if (!is.null(ch$field)) {
        fields <- c(fields, ch$field)
      }
    }
  }
  if (!is.null(spec$facet)) {
    fields <- c(
      fields,
      unlist(spec$facet$rows, use.names = FALSE),
      unlist(spec$facet$cols, use.names = FALSE)
    )
  }
  unique(fields)
}

#' Diagnose a spec against its data
#'
#' `spec_diagnose()` checks a spec (a list, JSON string, or path) without
#' throwing: it parses it, validates every referenced field against the data,
#' and dry-run compiles it, returning a structured verdict. `vplot_from_spec()`
#' is the strict form — it returns the [PlotSpec] on success and raises a classed
#' `vellumplot_spec_invalid` error (carrying the same diagnostics) on failure.
#'
#' This is the agent repair loop: a failure comes back as data an agent can act
#' on (which field is unknown, the nearest real field) rather than a traceback.
#'
#' @param spec A spec list (from [as_spec()]), a JSON string, or a `.json` path.
#' @param data The data frame the spec is drawn from. Required when the spec
#'   stores data by reference; optional (for field validation) when inlined.
#' @param env Environment channel expressions are re-quoted in.
#' @return `spec_diagnose()` returns a list with `ok` (logical), `plot` (the
#'   [PlotSpec] or `NULL`), and `diagnostics` (a list of records with `severity`,
#'   `field`, `message`, `hint`). `vplot_from_spec()` returns a [PlotSpec].
#' @seealso [spec_fields()], [spec_schema()], [vplot_ask()]
#' @examples
#' spec <- as_spec(vplot(mtcars) |> mark_point(x = wt, y = mpg))
#' d <- spec_diagnose(spec, data = mtcars)
#' d$ok
#' @export
spec_diagnose <- function(spec, data = NULL, env = globalenv()) {
  diags <- list()
  add <- function(
    severity,
    message,
    field = NA_character_,
    hint = NA_character_
  ) {
    diags[[length(diags) + 1]] <<- list(
      severity = severity,
      field = field,
      message = message,
      hint = hint
    )
  }
  # 1. parse the spec (JSON string / path / list) into an IR list.
  if (is.character(spec)) {
    spec <- tryCatch(
      {
        .need_pkg("jsonlite", "spec_diagnose()")
        if (length(spec) == 1 && !grepl("[{\n]", spec) && file.exists(spec)) {
          spec <- readLines(spec, warn = FALSE)
        }
        jsonlite::fromJSON(
          paste(spec, collapse = "\n"),
          simplifyVector = TRUE,
          simplifyDataFrame = FALSE,
          simplifyMatrix = FALSE
        )
      },
      error = function(e) {
        add("error", paste0("Invalid JSON: ", conditionMessage(e)))
        NULL
      }
    )
    if (is.null(spec)) {
      return(list(ok = FALSE, plot = NULL, diagnostics = diags))
    }
  }
  # 2. field validation against the data (the common hallucination).
  known <- names(data)
  if (!is.null(known)) {
    for (f in .spec_referenced_fields(spec)) {
      if (!f %in% known) {
        near <- known[utils::adist(f, known)[1, ] <= 2]
        hint <- if (length(near)) {
          paste0("Did you mean: ", paste(near, collapse = ", "), "?")
        } else {
          paste0("Available fields: ", paste(known, collapse = ", "), ".")
        }
        add("error", paste0("Unknown field '", f, "'."), field = f, hint = hint)
      }
    }
  }
  # 3. build + dry-run compile.
  plot <- tryCatch(
    from_spec(spec, data = data, env = env),
    error = function(e) {
      cls <- setdiff(class(e), c("rlang_error", "error", "condition"))[1]
      add(
        "error",
        conditionMessage(e),
        hint = if (!is.na(cls)) paste0("(", cls, ")")
      )
      NULL
    }
  )
  if (
    !is.null(plot) &&
      !any(vapply(diags, function(d) d$severity == "error", logical(1)))
  ) {
    ok_compile <- tryCatch(
      {
        vellum::as_vellum_scene(plot)
        TRUE
      },
      error = function(e) {
        add("error", paste0("Compile failed: ", conditionMessage(e)))
        FALSE
      }
    )
  } else {
    ok_compile <- FALSE
  }
  ok <- ok_compile &&
    !any(vapply(diags, function(d) d$severity == "error", logical(1)))
  list(ok = ok, plot = if (ok) plot else NULL, diagnostics = diags)
}

#' @rdname spec_diagnose
#' @export
vplot_from_spec <- function(spec, data = NULL, env = globalenv()) {
  res <- spec_diagnose(spec, data = data, env = env)
  if (!res$ok) {
    msgs <- vapply(
      res$diagnostics,
      function(d) {
        m <- d$message
        if (!is.na(d$hint)) {
          m <- paste0(m, " ", d$hint)
        }
        m
      },
      character(1)
    )
    cli::cli_abort(
      c("Invalid spec:", stats::setNames(msgs, rep("x", length(msgs)))),
      class = "vellumplot_spec_invalid"
    )
  }
  res$plot
}

#' Generate a plot from a natural-language request
#'
#' `vplot_ask()` is a thin, **model-agnostic** convenience: it assembles the
#' grounding payload (the JSON [spec_schema()] plus [spec_fields()] for `data`
#' and the user's `prompt`), hands it to a `responder` function you supply — any
#' function that takes the payload and returns a JSON spec string (wrapping the
#' Claude API, an MCP client, a local model, or a canned fixture) — and validates
#' the result with [vplot_from_spec()].
#'
#' vellumplot deliberately ships no built-in model client: the value is the
#' schema, the grounding, and the validated repair loop, not a hard dependency on
#' one provider. For a ready-to-run agent integration, point an MCP client at the
#' bundled server (`system.file("mcp/server.R", package = "vellumplot")`).
#'
#' @param prompt A natural-language description of the desired plot.
#' @param data The data frame to plot.
#' @param responder A function `function(payload) -> json_string`. `payload` is a
#'   list with `prompt`, `fields` (from [spec_fields()]), and `schema`.
#' @param env Environment channel expressions are re-quoted in.
#' @return A [PlotSpec].
#' @seealso [spec_fields()], [vplot_from_spec()], [spec_schema()]
#' @examplesIf requireNamespace("jsonlite", quietly = TRUE)
#' # a canned responder standing in for a model call:
#' responder <- function(payload) {
#'   spec_to_json(vplot(mtcars) |> mark_point(x = wt, y = mpg))
#' }
#' p <- vplot_ask("scatter of weight vs mpg", mtcars, responder)
#' @export
vplot_ask <- function(prompt, data, responder, env = globalenv()) {
  if (!is.function(responder)) {
    cli::cli_abort(c(
      "{.arg responder} must be a function {.code function(payload) -> json}.",
      "i" = "vellumplot ships no model client; supply one, or use the bundled MCP server."
    ))
  }
  payload <- list(
    prompt = prompt,
    fields = spec_fields(data),
    schema = spec_schema(as = "string")
  )
  json <- responder(payload)
  if (!is.character(json) || length(json) != 1) {
    cli::cli_abort("{.arg responder} must return a length-1 JSON string.")
  }
  vplot_from_spec(json, data = data, env = env)
}
