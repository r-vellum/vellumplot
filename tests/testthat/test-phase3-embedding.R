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

# VP-165: knit `x` to HTML with the chunk options in `...`, under the
# document defaults `doc` (`knitr::opts_chunk`). As knitr does, a `fig.asp`
# (the chunk's, else the document's) sets the chunk's `fig.height`.
knit_html <- function(x, ..., doc = list()) {
  testthat::local_mocked_bindings(
    is_html_output = function(...) TRUE,
    .package = "knitr"
  )
  old_doc <- knitr::opts_chunk$get()
  old <- knitr::opts_current$get()
  on.exit(knitr::opts_chunk$restore(old_doc), add = TRUE)
  on.exit(knitr::opts_current$restore(old), add = TRUE)
  if (length(doc)) {
    do.call(knitr::opts_chunk$set, doc)
  }
  opts <- utils::modifyList(knitr::opts_chunk$get(), list(...))
  if (is.numeric(opts$fig.asp)) {
    opts$fig.height <- opts$fig.width * opts$fig.asp
  }
  do.call(knitr::opts_current$set, opts)
  as.character(vellumplot:::.knit_print_plot(x))
}
svg_root <- function(svg) regmatches(svg, regexpr("<svg[^>]*>", svg))

test_that("a plot without its own size takes the chunk's fig-width / fig-height (VP-165)", {
  skip_if_not_installed("knitr")
  # no fig size in the chunk: it fills the column, as before
  root <- svg_root(knit_html(p))
  expect_match(root, 'width="100%" height="100%"', fixed = TRUE)
  expect_no_match(root, "style=")
  expect_match(root, 'viewBox="0 0 576 384"', fixed = TRUE)
  # fig-width 3 and fig-height 2: laid out at 3 x 2 in, and no wider than that
  root <- svg_root(knit_html(p, fig.width = 3, fig.height = 2))
  expect_match(root, 'viewBox="0 0 288 192"', fixed = TRUE)
  expect_match(root, 'style="max-width:288px"', fixed = TRUE)
  expect_match(root, 'width="100%" height="100%"', fixed = TRUE)
  # only fig-width: the height stays the plot's own
  root <- svg_root(knit_html(p, fig.width = 8))
  expect_match(root, 'viewBox="0 0 768 384"', fixed = TRUE)
  expect_match(root, 'style="max-width:768px"', fixed = TRUE)
  # only fig-height: laid out taller, no width cap
  root <- svg_root(knit_html(p, fig.height = 6))
  expect_match(root, 'viewBox="0 0 576 576"', fixed = TRUE)
  expect_no_match(root, "style=")
  # a composition of unsized plots counts as unsized too
  comp <- hconcat(p, p)
  root <- svg_root(knit_html(comp, fig.width = 9, fig.height = 3))
  expect_match(root, 'viewBox="0 0 864 288"', fixed = TRUE)
  # a dimension given to vplot() keeps its value; the other follows the chunk
  wide <- vplot(mtcars, width = 5) |> mark_point(x = wt, y = mpg)
  root <- svg_root(knit_html(wide, fig.width = 3, fig.height = 2))
  expect_match(root, 'viewBox="0 0 480 192"', fixed = TRUE)
  expect_no_match(root, "max-width")
})

test_that("fig.asp sets the height from the width (VP-165)", {
  skip_if_not_installed("knitr")
  # a document-wide fig.asp leaves unsized plots alone
  root <- svg_root(knit_html(p, doc = list(fig.asp = 0.618)))
  expect_match(root, 'viewBox="0 0 576 384"', fixed = TRUE)
  expect_no_match(root, "style=")
  # ... until a chunk sets the width: the document's aspect then applies
  root <- svg_root(knit_html(p, fig.width = 5, doc = list(fig.asp = 0.5)))
  expect_match(root, 'viewBox="0 0 480 240"', fixed = TRUE)
  # a chunk fig.asp alone: the plot's own width times the aspect
  root <- svg_root(knit_html(p, fig.asp = 0.5))
  expect_match(root, 'viewBox="0 0 576 288"', fixed = TRUE)
  expect_no_match(root, "style=")
  # chunk fig.asp with fig.width
  root <- svg_root(knit_html(p, fig.width = 4, fig.asp = 0.25))
  expect_match(root, 'viewBox="0 0 384 96"', fixed = TRUE)
  expect_match(root, 'style="max-width:384px"', fixed = TRUE)
})

test_that("the width cap is in CSS pixels at any dpi (VP-165)", {
  skip_if_not_installed("knitr")
  hi <- vplot(mtcars, dpi = 300) |> mark_point(x = wt, y = mpg)
  root <- svg_root(knit_html(hi, fig.width = 4))
  expect_match(root, 'style="max-width:384px"', fixed = TRUE)
})

test_that("out.width becomes the figure's CSS width (VP-165)", {
  skip_if_not_installed("knitr")
  root <- svg_root(knit_html(p, out.width = "50%"))
  expect_match(root, 'style="width:50%"', fixed = TRUE)
  root <- svg_root(knit_html(p, out.width = "50%", fig.width = 3))
  expect_match(root, 'style="width:50%;max-width:288px"', fixed = TRUE)
  # also for a plot with its own size
  sized <- vplot(mtcars, width = 5, height = 2) |> mark_point(x = wt, y = mpg)
  root <- svg_root(knit_html(sized, out.width = 300))
  expect_match(root, 'style="width:300px"', fixed = TRUE)
  # knitr's own fill-in under fig.retina (fig.width * dpi) is not the author's
  root <- svg_root(knit_html(p, fig.retina = 2, dpi = 192, out.width = 7 * 96))
  expect_no_match(root, "style=")
})

test_that("a plot with its own size ignores the chunk's fig size (VP-165)", {
  skip_if_not_installed("knitr")
  sized <- vplot(mtcars, width = 5, height = 2) |> mark_point(x = wt, y = mpg)
  root <- svg_root(knit_html(sized, fig.width = 3, fig.height = 3))
  expect_match(root, 'viewBox="0 0 480 192"', fixed = TRUE)
  expect_no_match(root, "style=")
  # a size given as the default values still counts as given
  six <- vplot(mtcars, width = 6, height = 4) |> mark_point(x = wt, y = mpg)
  root <- svg_root(knit_html(six, fig.width = 3))
  expect_match(root, 'viewBox="0 0 576 384"', fixed = TRUE)
  expect_no_match(root, "style=")
  comp <- hconcat(p, p, width = 10, height = 3)
  root <- svg_root(knit_html(comp, fig.width = 3))
  expect_match(root, 'viewBox="0 0 960 288"', fixed = TRUE)
  expect_no_match(root, "style=")
  # a composition of sized plots is sized
  root <- svg_root(knit_html(hconcat(six, six), fig.width = 3))
  expect_match(root, 'viewBox="0 0 1152 384"', fixed = TRUE)
  # a chunk value equal to the document default sets nothing
  root <- svg_root(knit_html(p, fig.width = knitr::opts_chunk$get("fig.width")))
  expect_no_match(root, "style=")
})
