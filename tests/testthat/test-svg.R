# plot_svg() (inline SVG string) + gt_vsparkline() (gt bridge).

test_that("plot_svg() returns a self-contained <svg> string", {
  svg <- plot_svg(vsparkline(cumsum(rnorm(20))))
  expect_type(svg, "character")
  expect_length(svg, 1L)
  expect_match(svg, "^<\\?xml|^<svg", perl = TRUE)
  expect_match(svg, "viewBox")
})

test_that('scaling = "fit" makes the root svg fluid but keeps the viewBox', {
  fit <- plot_svg(vsparkline(1:10), scaling = "fit")
  # the root <svg ...> element carries width/height 100%
  root <- regmatches(fit, regexpr("<svg[^>]*>", fit))
  expect_match(root, 'width="100%"')
  expect_match(root, 'height="100%"')
  expect_match(fit, "viewBox")
  # fixed keeps a pixel size
  fixed <- plot_svg(vsparkline(1:10))
  froot <- regmatches(fixed, regexpr("<svg[^>]*>", fixed))
  expect_no_match(froot, "100%")
})

test_that("plot_svg() coerces a vtable and a composition too", {
  df <- data.frame(a = c("x", "y"))
  df$v <- list(1:5, 6:10)
  expect_match(plot_svg(vtable(df, spark = list(v = "line"))), "<svg")
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
  expect_match(plot_svg(p, width = 3, height = 2), "<svg")
})

test_that("gt_vsparkline() embeds a sparkline SVG per row", {
  skip_if_not_installed("gt")
  df <- data.frame(metric = c("A", "B", "C"))
  df$trend <- list(1:6, c(3, 1, 4, 1, 5), c(9, 7, 8, 6))
  g <- gt::gt(df) |> gt_vsparkline(trend, type = "line")
  html <- as.character(gt::as_raw_html(g))
  # one embedded <svg> per row
  expect_equal(length(gregexpr("<svg", html)[[1]]), 3L)
})

test_that("gt_vsparkline() accepts a string column and forwards the type", {
  skip_if_not_installed("gt")
  df <- data.frame(m = c("A", "B"))
  df$v <- list(1:8, 8:1)
  g <- gt::gt(df) |> gt_vsparkline("v", type = "bar")
  html <- as.character(gt::as_raw_html(g))
  expect_match(html, "<svg")
})

test_that("gt_vsparkline() errors on a missing or non-list column", {
  skip_if_not_installed("gt")
  df <- data.frame(m = c("A", "B"), n = c(1, 2))
  df$v <- list(1:3, 4:6)
  expect_error(gt::gt(df) |> gt_vsparkline(nope), "no column")
  expect_error(gt::gt(df) |> gt_vsparkline(n), "must be a list-column")
})

# --- inline embedding (transparent, prolog-free, recolor) --------------------

test_that("inline = TRUE strips the XML prolog", {
  s <- plot_svg(vsparkline(1:10), inline = TRUE)
  expect_false(startsWith(s, "<?xml"))
  expect_true(startsWith(sub("^\\s*", "", s), "<svg"))
  # default keeps the stand-alone prolog
  expect_true(startsWith(plot_svg(vsparkline(1:10)), "<?xml"))
})

test_that("a sparkline has a transparent background (no white fill)", {
  s <- plot_svg(vsparkline(cumsum(rnorm(20))))
  expect_false(grepl("fill=\"#ffffff\"", s, fixed = TRUE))
})

test_that("an ordinary plot keeps its white background (no regression)", {
  s <- plot_svg(vplot(mtcars) |> mark_point(x = wt, y = mpg))
  expect_true(grepl("fill=\"#ffffff\"", s, fixed = TRUE))
})

test_that("recolor swaps a source colour for a verbatim token, leaving others", {
  s <- plot_svg(
    vsparkline(cumsum(rnorm(20)), color = "grey30"),
    recolor = c(grey30 = "currentColor")
  )
  expect_true(grepl("currentColor", s, fixed = TRUE))
  expect_false(grepl("#4d4d4d", s, fixed = TRUE)) # grey30 hex gone
  expect_true(grepl("b22222", tolower(s), fixed = TRUE)) # firebrick dots untouched
})

test_that("recolor accepts a hex source and requires names", {
  s <- plot_svg(
    vsparkline(1:8, color = "#4d4d4d"),
    recolor = c("#4d4d4d" = "red")
  )
  expect_true(grepl("stroke=\"red\"", s, fixed = TRUE))
  expect_error(
    plot_svg(vsparkline(1:8), recolor = "currentColor"),
    "must be a named vector"
  )
})
