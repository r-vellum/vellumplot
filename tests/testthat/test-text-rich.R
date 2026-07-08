# mark_text() now carries rich (per-datum md()) and multi-line ("\n") labels
# through to vellum instead of flattening them with as.character(). Phase 1.

render_ok <- function(p) {
  f <- tempfile(fileext = ".svg")
  on.exit(unlink(f), add = TRUE)
  expect_no_error(render_plot(p, f))
}

test_that("mark_text() accepts a per-datum md() label column", {
  df <- data.frame(x = 1:3, y = 1:3, lab = c("aa", "bb", "cc"))
  p <- vplot(df) |> mark_text(x = x, y = y, label = md(sprintf("**%s**", lab)))
  render_ok(p)
})

test_that("mark_text() accepts a single md() label and multi-line plain labels", {
  df <- data.frame(x = 1:2, y = 1:2)
  render_ok(vplot(df) |> mark_text(x = x, y = y, label = md("R^2^")))
  render_ok(vplot(df) |> mark_text(x = x, y = y, label = "line one\nline two"))
})

test_that("a plain single-line label is unchanged (regression)", {
  df <- data.frame(x = 1:3, y = 1:3, lab = letters[1:3])
  p <- vplot(df) |> mark_text(x = x, y = y, label = lab)
  render_ok(p)
})
