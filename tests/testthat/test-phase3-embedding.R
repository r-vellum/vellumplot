# Phase 3: plot_data_uri() + adaptive knit_print.

p <- vplot(mtcars) |> mark_point(x = wt, y = mpg, color = factor(cyl))

test_that("plot_data_uri encodes a valid svg data URI", {
  uri <- plot_data_uri(p)
  expect_match(uri, "^data:image/svg\\+xml;base64,")
  svg <- rawToChar(jsonlite::base64_dec(sub(
    "^data:image/svg\\+xml;base64,",
    "",
    uri
  )))
  expect_match(svg, "<svg")
})

test_that("plot_data_uri encodes a valid png data URI", {
  skip_if_not_installed("png")
  uri <- plot_data_uri(p, format = "png", dpi = 72)
  expect_match(uri, "^data:image/png;base64,")
  bytes <- jsonlite::base64_dec(sub("^data:image/png;base64,", "", uri))
  # PNG magic number
  expect_equal(as.integer(bytes[1:4]), c(137L, 80L, 78L, 71L))
})

test_that("adaptive knit_print emits inline SVG for HTML output", {
  skip_if_not_installed("knitr")
  testthat::local_mocked_bindings(
    is_html_output = function(...) TRUE,
    .package = "knitr"
  )
  out <- vellumplot:::.knit_print_plot(p)
  expect_s3_class(out, "knit_asis")
  expect_match(as.character(out), "<svg")
})

test_that("adaptive knit_print falls back to the device render otherwise", {
  skip_if_not_installed("knitr")
  testthat::local_mocked_bindings(
    is_html_output = function(...) FALSE,
    .package = "knitr"
  )
  pdf(NULL)
  on.exit(dev.off())
  expect_no_error(vellumplot:::.knit_print_plot(p))
})
