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

# -- compile report + pre-compile checks (spec_diagnose) ---------------------

diag_msgs <- function(d, severity) {
  hits <- Filter(function(x) identical(x$severity, severity), d$diagnostics)
  vapply(hits, function(x) x$message, character(1))
}

test_that("spec_diagnose accepts a PlotSpec and reports a clean plot", {
  d <- spec_diagnose(vplot(mtcars) |> mark_point(x = wt, y = mpg))
  expect_s3_class(d, "vellumplot_diagnosis")
  expect_true(d$ok)
  expect_s7_class(d$plot, PlotSpec)
  expect_length(d$diagnostics, 0)
  expect_identical(d$report$rows, 32L)
  expect_identical(d$report$dropped, 0L)
})

test_that("the report counts rows not shown per layer, and why", {
  df <- data.frame(
    x = c(1, 2, 3, NA, 5, 6),
    y = c(-1, 1, 10, 4, 100, 5)
  )
  p <- vplot(df) |>
    mark_point(x = x, y = y) |>
    scale_y_continuous(trans = "log10", limits = c(1, 50)) |>
    mark_line(x = x, y = x)
  d <- suppressWarnings(spec_diagnose(p))
  expect_true(d$ok) # rows not shown are a warning, not a failure
  r <- d$report
  expect_identical(r$layer, 1:2)
  expect_identical(r$mark, c("point", "line"))
  # layer 1: row 4 has no x; row 1 is <= 0 on the log axis; row 5 is above the
  # limits -- each row counted once, under its first reason
  expect_identical(r$missing, c(1L, 1L))
  expect_identical(r$transform, c(1L, 0L))
  expect_identical(r$limits, c(1L, 0L))
  expect_identical(r$dropped, c(3L, 1L))
  w <- diag_msgs(d, "warning")
  expect_length(w, 2L)
  expect_match(w[1], "Layer 1 (point): 3 of 6 rows", fixed = TRUE)
  expect_match(w[1], "1 outside the scale limits", fixed = TRUE)
})

test_that("partial limits bound only the end they name; coord limits zoom", {
  df <- data.frame(x = c(-5, 1, 2, 30), y = 1:4)
  part <- spec_diagnose(vplot(df) |> mark_point(x = x, y = y) |> xlim(0, NA))
  expect_identical(part$report$limits, 1L)
  zoom <- spec_diagnose(
    vplot(df) |> mark_point(x = x, y = y) |> coord_cartesian(xlim = c(0, 3))
  )
  expect_identical(zoom$report$dropped, 0L)
})

test_that("a missing facet or discrete position value is drawn, so not counted", {
  df <- data.frame(x = 1:4, y = 1:4, g = c("a", "b", NA, "a"))
  d <- spec_diagnose(vplot(df) |> mark_point(x = x, y = y) |> facet_wrap(~g))
  expect_identical(d$report$dropped, 0L)
  expect_false("facet" %in% names(d$report))
  d2 <- spec_diagnose(vplot(df) |> mark_point(x = g, y = y))
  expect_identical(d2$report$missing, 0L)
})

test_that("a missing non-position value is drawn, so it is not counted", {
  df <- data.frame(x = 1:3, y = 1:3, g = c("a", NA, "b"))
  d <- spec_diagnose(vplot(df) |> mark_point(x = x, y = y, color = g))
  expect_identical(d$report$dropped, 0L)
})

test_that("a facet variable missing from a layer's data is caught", {
  ann <- data.frame(x = 3, y = 30)
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    mark_text(data = ann, x = x, y = y, label = "note") |>
    facet_wrap(~cyl)
  d <- spec_diagnose(p)
  expect_true(d$ok) # legal (annotation in every panel), but flagged
  w <- diag_msgs(d, "warning")
  expect_length(w, 1L)
  expect_match(w, "Layer 2's data has no facet variable 'cyl'", fixed = TRUE)

  # from the plot data itself it is an error, caught before compiling
  bad <- spec_diagnose(
    vplot(mtcars) |> mark_point(x = wt, y = mpg) |> facet_wrap(~nope)
  )
  expect_false(bad$ok)
  e <- diag_msgs(bad, "error")
  expect_length(e, 1L)
  expect_match(e, "Facet variable 'nope'", fixed = TRUE)
})

test_that("an unknown facet field in a spec is reported once", {
  spec <- as_spec(
    vplot(mtcars) |> mark_point(x = wt, y = mpg) |> facet_wrap(~cyl)
  )
  spec$facet$cols[[1]] <- "cyll"
  d <- spec_diagnose(spec, data = mtcars)
  expect_false(d$ok)
  fields <- vapply(d$diagnostics, function(x) x$field, character(1))
  expect_identical(sum(fields == "cyll", na.rm = TRUE), 1L)
})

test_that("color and fill of different types sharing a legend are caught", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, color = factor(cyl)) |>
    mark_bar(x = wt, y = mpg, fill = hp)
  d <- spec_diagnose(p)
  expect_false(d$ok)
  e <- diag_msgs(d, "error")
  # one specific finding, not a second generic "Compile failed" for the same
  expect_length(e, 1L)
  expect_match(e, "layer 2 maps fill to a continuous variable", fixed = TRUE)
  expect_match(e, "layer 1 maps color to a nominal one", fixed = TRUE)

  # with the scale kind set explicitly the compile decides; the clash itself is
  # demoted to a warning (here a discrete scale, so `hp` becomes categories)
  ex <- spec_diagnose(p |> scale_color_discrete())
  expect_true(ex$ok)
  w <- diag_msgs(ex, "warning")
  expect_true(any(grepl("share one colour scale", w, fixed = TRUE)))
})

test_that("a manual palette on a continuous channel is caught", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, color = hp) |>
    scale_color_manual(values = c("red", "blue"))
  d <- spec_diagnose(p)
  # the compile now refuses it, as ggplot2 does (VP-11); the pre-compile check
  # still explains why
  expect_false(d$ok)
  w <- diag_msgs(d, "warning")
  expect_length(w, 1L)
  expect_match(w, "manual/discrete colour palette", fixed = TRUE)
  expect_match(
    diag_msgs(d, "error"),
    "Continuous values supplied to a discrete",
    fixed = TRUE
  )
  # a manual palette on a discrete channel is what it is for
  ok <- spec_diagnose(
    vplot(mtcars) |>
      mark_point(x = wt, y = mpg, color = factor(am)) |>
      scale_color_manual(values = c("red", "blue"))
  )
  expect_length(ok$diagnostics, 0L)
})

test_that("a diagnosis prints a summary", {
  df <- data.frame(x = c(1, NA, 3), y = 1:3)
  d <- spec_diagnose(vplot(df) |> mark_point(x = x, y = y))
  out <- capture.output(print(d))
  expect_match(out[1], "<vellumplot diagnosis> ok: 0 errors, 1 warning")
  expect_true(any(grepl("1 missing a position value", out, fixed = TRUE)))
  expect_true(any(grepl("Rows not shown, per layer", out, fixed = TRUE)))
})
