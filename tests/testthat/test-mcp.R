# MCP adapter: the pure dispatch core (`.mcp_dispatch`, a function of a request
# list) plus the stdio `mcp_serve()` loop. Most of this needs no process.

test_that(".mcp_tools advertises the three tools", {
  tools <- vellumplot:::.mcp_tools()
  expect_length(tools, 3L)
  expect_setequal(
    vapply(tools, function(t) t$name, character(1)),
    c("get_schema", "list_fields", "render_spec")
  )
})

test_that("initialize returns the protocol version and server info", {
  resp <- vellumplot:::.mcp_dispatch(
    list(jsonrpc = "2.0", id = 1, method = "initialize")
  )
  expect_equal(resp$id, 1)
  expect_identical(resp$result$protocolVersion, vellumplot:::.MCP_PROTOCOL)
  expect_identical(resp$result$serverInfo$name, "vellumplot")
})

test_that("tools/list echoes the tool definitions", {
  resp <- vellumplot:::.mcp_dispatch(list(id = 2, method = "tools/list"))
  expect_length(resp$result$tools, 3L)
})

test_that("ping returns an empty result", {
  resp <- vellumplot:::.mcp_dispatch(list(id = 3, method = "ping"))
  expect_length(resp$result, 0L)
})

test_that("a notification (no id) gets no reply", {
  expect_null(
    vellumplot:::.mcp_dispatch(list(method = "notifications/initialized"))
  )
})

test_that("an unknown method returns -32601", {
  resp <- vellumplot:::.mcp_dispatch(list(id = 4, method = "no/such"))
  expect_equal(resp$error$code, -32601)
})

test_that("a malformed request (missing method) returns -32600, not a crash", {
  resp <- vellumplot:::.mcp_dispatch(list(jsonrpc = "2.0", id = 5))
  expect_equal(resp$error$code, -32600)
})

test_that("a non-list request is dropped rather than erroring", {
  expect_null(vellumplot:::.mcp_dispatch(42))
  expect_null(vellumplot:::.mcp_dispatch("garbage"))
})

test_that("get_schema returns the schema as text content", {
  skip_if_not_installed("jsonlite")
  resp <- vellumplot:::.mcp_dispatch(list(
    id = 6,
    method = "tools/call",
    params = list(name = "get_schema", arguments = list())
  ))
  res <- resp$result
  expect_false(isTRUE(res$isError))
  expect_identical(res$content[[1]]$type, "text")
  expect_true(nzchar(res$content[[1]]$text))
})

test_that("list_fields without a data_path returns a tool error", {
  resp <- vellumplot:::.mcp_dispatch(list(
    id = 7,
    method = "tools/call",
    params = list(name = "list_fields", arguments = list())
  ))
  expect_true(resp$result$isError)
})

test_that("an unknown tool name returns a tool error", {
  resp <- vellumplot:::.mcp_dispatch(list(
    id = 8,
    method = "tools/call",
    params = list(name = "nope", arguments = list())
  ))
  expect_true(resp$result$isError)
  expect_match(resp$result$content[[1]]$text, "Unknown tool")
})

test_that("render_spec validates and renders an inlined spec", {
  skip_if_not_installed("jsonlite")
  p <- vplot(mtcars[1:10, ]) |> mark_point(x = wt, y = mpg)
  json <- spec_to_json(p)
  out <- local_tempfile(fileext = ".png")
  resp <- vellumplot:::.mcp_dispatch(list(
    id = 9,
    method = "tools/call",
    params = list(
      name = "render_spec",
      arguments = list(spec = json, out_path = out)
    )
  ))
  res <- resp$result
  expect_false(isTRUE(res$isError))
  parsed <- jsonlite::fromJSON(res$content[[1]]$text)
  expect_true(parsed$ok)
  expect_true(file.exists(out))
})

test_that("render_spec surfaces a bad spec as a tool error, not a crash", {
  skip_if_not_installed("jsonlite")
  resp <- vellumplot:::.mcp_dispatch(list(
    id = 10,
    method = "tools/call",
    params = list(name = "render_spec", arguments = list(spec = "{ not json"))
  ))
  expect_true(resp$result$isError)
})

test_that("mcp_serve survives a malformed request and keeps serving", {
  skip_if_not_installed("jsonlite")
  lines <- c(
    '{"jsonrpc":"2.0","id":1,"method":"initialize"}',
    "this is not json at all",
    '{"jsonrpc":"2.0","id":2}', # a request with no method
    '{"jsonrpc":"2.0","id":3,"method":"ping"}'
  )
  input <- textConnection(lines)
  on.exit(try(close(input), silent = TRUE), add = TRUE)
  outcon <- textConnection("captured", open = "w", local = TRUE)
  on.exit(try(close(outcon), silent = TRUE), add = TRUE)

  expect_no_error(mcp_serve(input = input, output = outcon))
  close(outcon)

  # Three replies: initialize, the -32600 for the missing-method line, and ping.
  # The non-JSON line is silently skipped; nothing kills the loop.
  resps <- lapply(captured, function(l) jsonlite::fromJSON(l))
  ids <- vapply(resps, function(r) r$id, numeric(1))
  expect_setequal(ids, c(1, 2, 3))
  expect_equal(resps[[which(ids == 2)]]$error$code, -32600)
})
