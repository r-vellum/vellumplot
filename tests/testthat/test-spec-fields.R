# Feature 1: LLM / agent tooling (spec_fields, spec_diagnose, vplot_from_spec,
# vplot_ask, and the MCP dispatch core).

test_that("spec_fields infers encoding types", {
  df <- data.frame(
    q = 1:3,
    n = c("a", "b", "c"),
    o = ordered(c("lo", "hi", "lo"), levels = c("lo", "hi")),
    t = as.Date("2020-01-01") + 0:2
  )
  f <- spec_fields(df)
  expect_identical(f$type[f$name == "q"], "quantitative")
  expect_identical(f$type[f$name == "n"], "nominal")
  expect_identical(f$type[f$name == "o"], "ordinal")
  expect_identical(f$type[f$name == "t"], "temporal")
})

test_that("spec_diagnose accepts a valid spec", {
  spec <- as_spec(vplot(mtcars) |> mark_point(x = wt, y = mpg))
  d <- spec_diagnose(spec, data = mtcars)
  expect_true(d$ok)
  expect_s7_class(d$plot, PlotSpec)
  expect_length(d$diagnostics, 0)
})

test_that("spec_diagnose flags an unknown field with a suggestion, no throw", {
  spec <- as_spec(vplot(mtcars) |> mark_point(x = wt, y = mpg))
  # corrupt a field name to a near-miss of a real column
  spec$layers[[1]]$encoding$x$field <- "wtt"
  d <- spec_diagnose(spec, data = mtcars)
  expect_false(d$ok)
  errs <- Filter(function(x) x$severity == "error", d$diagnostics)
  expect_true(any(vapply(
    errs,
    function(e) identical(e$field, "wtt"),
    logical(1)
  )))
  expect_true(any(grepl("wt", vapply(errs, function(e) e$hint, character(1)))))
})

test_that("vplot_from_spec returns a plot or throws classed error", {
  good <- as_spec(vplot(mtcars) |> mark_point(x = wt, y = mpg))
  expect_s7_class(vplot_from_spec(good, data = mtcars), PlotSpec)

  bad <- good
  bad$layers[[1]]$encoding$y$field <- "nope"
  expect_error(
    vplot_from_spec(bad, data = mtcars),
    class = "vellumplot_spec_invalid"
  )
})

test_that("vplot_from_spec parses a JSON string", {
  skip_if_not_installed("jsonlite")
  json <- spec_to_json(vplot(mtcars) |> mark_point(x = wt, y = mpg))
  expect_s7_class(vplot_from_spec(json), PlotSpec)
})

test_that("vplot_ask runs the grounding + validate loop with a stub responder", {
  skip_if_not_installed("jsonlite")
  responder <- function(payload) {
    expect_true("wt" %in% payload$fields$name)
    expect_true(grepl("layers", payload$schema))
    spec_to_json(vplot(mtcars) |> mark_point(x = wt, y = mpg))
  }
  p <- vplot_ask("scatter of wt vs mpg", mtcars, responder)
  expect_s7_class(p, PlotSpec)
})

test_that("MCP dispatch handles initialize, tools/list, and notifications", {
  skip_if_not_installed("jsonlite")
  init <- .mcp_dispatch(list(jsonrpc = "2.0", id = 1, method = "initialize"))
  expect_identical(init$result$serverInfo$name, "vellumplot")

  tools <- .mcp_dispatch(list(jsonrpc = "2.0", id = 2, method = "tools/list"))
  names <- vapply(tools$result$tools, function(t) t$name, character(1))
  expect_setequal(names, c("get_schema", "list_fields", "render_spec"))

  # a notification (no id) yields no response
  expect_null(.mcp_dispatch(list(
    jsonrpc = "2.0",
    method = "notifications/initialized"
  )))
})

test_that("MCP render_spec returns diagnostics on a bad spec", {
  skip_if_not_installed("jsonlite")
  csv <- tempfile(fileext = ".csv")
  utils::write.csv(mtcars, csv, row.names = FALSE)
  spec <- as_spec(vplot(mtcars) |> mark_point(x = wt, y = mpg))
  spec$layers[[1]]$encoding$x$field <- "nope" # a hallucinated column
  bad <- jsonlite::toJSON(spec, auto_unbox = TRUE, null = "null")
  resp <- .mcp_dispatch(list(
    jsonrpc = "2.0",
    id = 3,
    method = "tools/call",
    params = list(
      name = "render_spec",
      arguments = list(spec = bad, data_path = csv)
    )
  ))
  expect_true(resp$result$isError)
  expect_match(resp$result$content[[1]]$text, "Unknown field")
})

test_that("MCP render_spec renders a valid spec to a file", {
  skip_if_not_installed("jsonlite")
  out <- tempfile(fileext = ".svg")
  spec <- spec_to_json(vplot(mtcars) |> mark_point(x = wt, y = mpg))
  resp <- .mcp_dispatch(list(
    jsonrpc = "2.0",
    id = 4,
    method = "tools/call",
    params = list(
      name = "render_spec",
      arguments = list(spec = spec, out_path = out)
    )
  ))
  expect_false(isTRUE(resp$result$isError))
  expect_true(file.exists(out))
})

test_that("from_spec() uses supplied data when the spec has no data block", {
  spec <- as_spec(vplot(mtcars) |> mark_point(x = wt, y = mpg))
  spec$data <- NULL # encoding-only spec
  # supplied data wins -> compiles
  expect_no_error(vellum::as_vellum_scene(from_spec(spec, data = mtcars)))
  # no data anywhere -> a diagnostic naming the missing references, not a cryptic
  # low-level compile error
  expect_error(from_spec(spec), "references data field.*wt.*mpg|no data")
})
