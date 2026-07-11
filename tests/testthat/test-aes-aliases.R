# British spelling: mark_*(colour = ) must behave exactly like color = ; before
# the fix the mapping was silently dropped and no colour scale was trained.
# (Housekeeping audit P0 #1.)

distinct_fills <- function(p) {
  f <- tempfile(fileext = ".svg")
  on.exit(unlink(f), add = TRUE)
  render_plot(p, f)
  s <- paste(readLines(f), collapse = "")
  length(unique(unlist(regmatches(s, gregexpr('fill="#[0-9a-fA-F]{6}"', s)))))
}

test_that("mark_*(colour=) trains a colour scale identically to color=", {
  us <- vplot(mtcars) |> mark_point(x = wt, y = mpg, color = factor(cyl))
  gb <- vplot(mtcars) |> mark_point(x = wt, y = mpg, colour = factor(cyl))
  n <- distinct_fills(us)
  expect_gt(n, 1L) # colour is actually mapped (multiple category colours)
  expect_equal(distinct_fills(gb), n) # British spelling is identical
})

test_that("colour is normalised to the color channel (provenance, alt text)", {
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg, colour = factor(cyl))
  chans <- plot_provenance(p)[[1]]$channels
  expect_true("color" %in% names(chans))
  expect_false("colour" %in% names(chans))
  # the alt text now truthfully reports the colour mapping it actually drew
  expect_match(plot_alt(p), "colour shows factor(cyl)", fixed = TRUE)
})

test_that("scale_colour_* are aliases of scale_color_*", {
  expect_identical(scale_colour_continuous, scale_color_continuous)
  expect_identical(scale_colour_discrete, scale_color_discrete)
  expect_identical(scale_colour_manual, scale_color_manual)
  expect_identical(scale_colour_gradient, scale_color_gradient)
  expect_identical(scale_colour_identity, scale_color_identity)
})

test_that("scale_colour_continuous() declares a colour scale", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, colour = hp) |>
    scale_colour_continuous(palette = "Blues")
  expect_true(any(vapply(
    p@scales,
    function(s) s@aesthetic == "color",
    logical(1)
  )))
})
