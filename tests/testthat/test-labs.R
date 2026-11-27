# ---- spec storage ----------------------------------------------------------

test_that("labs() stores plot-level labels", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    labs(title = "T", subtitle = "S", caption = "C", tag = "A")
  expect_identical(p@labels$title, "T")
  expect_identical(p@labels$subtitle, "S")
  expect_identical(p@labels$caption, "C")
  expect_identical(p@labels$tag, "A")
})

test_that("repeated labs() calls merge, last value winning", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    labs(title = "a") |>
    labs(subtitle = "b") |>
    labs(title = "c")
  expect_identical(p@labels$title, "c")
  expect_identical(p@labels$subtitle, "b")
})

test_that("colour folds into the color key; fill keeps its own (VP-48)", {
  p1 <- vplot(mtcars) |> mark_point(x = wt, y = mpg) |> labs(colour = "X")
  p2 <- vplot(mtcars) |> mark_point(x = wt, y = mpg) |> labs(fill = "X")
  expect_identical(p1@labels$color, "X")
  expect_identical(p2@labels$fill, "X")
  expect_null(p2@labels$color)
  expect_null(p1@labels$colour)
  expect_null(p1@labels$fill)
})

test_that("labs() validates its inputs", {
  expect_error(labs(1, title = "x"), "PlotSpec")
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
  expect_error(labs(p, foo = "x"))
})

# ---- title-override wiring & precedence -------------------------------------

test_that("labs() axis/legend overrides reach the trained scales", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, color = hp) |>
    labs(x = "Weight", y = "Miles/gal", color = "Power")
  sc <- vellumplot:::.train_scales(p, vellumplot:::.resolve_layers(p))
  expect_identical(sc$x$name, "Weight")
  expect_identical(sc$y$name, "Miles/gal")
  expect_identical(sc$color$name, "Power")
})

test_that("labs(fill =) titles the fill scale, not the colour scale", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, color = hp, fill = hp) |>
    labs(fill = "Power")
  sc <- vellumplot:::.train_scales(p, vellumplot:::.resolve_layers(p))
  expect_identical(sc$fill$name, "Power")
  expect_identical(sc$color$name, "hp")
})

test_that("scale_*(name =) takes precedence over labs()", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    scale_x_continuous(name = "FromScale") |>
    labs(x = "FromLabs")
  sc <- vellumplot:::.train_scales(p, vellumplot:::.resolve_layers(p))
  expect_identical(sc$x$name, "FromScale")
})

test_that("labs(y =) overrides the bar 'count' default title", {
  base <- vplot(mtcars) |> mark_bar(x = factor(cyl))
  sc0 <- vellumplot:::.train_scales(base, vellumplot:::.resolve_layers(base))
  expect_identical(sc0$y$name, "count")

  withlab <- base |> labs(y = "Frequency")
  sc1 <- vellumplot:::.train_scales(
    withlab,
    vellumplot:::.resolve_layers(withlab)
  )
  expect_identical(sc1$y$name, "Frequency")
})

# ---- layout bands ----------------------------------------------------------

test_that("absent labels add no band tracks", {
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
  lay <- vellumplot:::.build_layout(
    vellumplot:::.build_panels(p),
    list(),
    p@labels
  )
  expect_true(is.na(lay$title_row))
  expect_true(is.na(lay$subtitle_row))
  expect_true(is.na(lay$caption_row))
  expect_true(is.na(lay$tag_row))
  expect_identical(lay$ncol_total, length(lay$widths))
})

test_that("present labels add bands that span the full width", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    labs(title = "T", caption = "C")
  lay <- vellumplot:::.build_layout(
    vellumplot:::.build_panels(p),
    list(),
    p@labels
  )
  expect_false(is.na(lay$title_row))
  expect_false(is.na(lay$caption_row))
  expect_true(is.na(lay$subtitle_row))
  # the title band sits above every panel row; caption below
  expect_lt(lay$title_row, min(lay$panel_row))
  expect_gt(lay$caption_row, max(lay$panel_row))
})

test_that("a tag sits in its own row above the title", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    labs(title = "T", tag = "A")
  lay <- vellumplot:::.build_layout(
    vellumplot:::.build_panels(p),
    list(),
    p@labels
  )
  expect_false(is.na(lay$tag_row))
  expect_lt(lay$tag_row, lay$title_row)
  expect_lt(lay$title_row, min(lay$panel_row))
})

# ---- rendering -------------------------------------------------------------

test_that("a title renders ink in the top band", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    labs(title = "A Title")
  img <- render_px(p)
  expect_true(has_ink(img, rows = c(0, 0.08), cols = c(0, 0.6)))
})

test_that("a caption renders ink in the bottom band", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    labs(caption = "a caption")
  img <- render_px(p)
  # caption sits in the bottom band, just above the plot.margin
  expect_true(has_ink(img, rows = c(0.85, 0.92), cols = c(0.4, 1)))
})

test_that("faceted plots with labs render", {
  f <- local_tempfile(fileext = ".png")
  render_plot(
    vplot(mtcars) |>
      mark_point(x = wt, y = mpg, color = hp) |>
      facet_wrap(~cyl) |>
      labs(title = "T", x = "Weight", color = "Power"),
    f
  )
  expect_gt(file.info(f)$size, 0)
})

test_that("concat carries per-plot labs", {
  a <- vplot(mtcars) |> mark_point(x = wt, y = mpg) |> labs(title = "A")
  b <- vplot(mtcars) |> mark_histogram(x = mpg, bins = 8) |> labs(title = "B")
  f <- local_tempfile(fileext = ".png")
  render_plot(hconcat(a, b), f)
  expect_gt(file.info(f)$size, 0)
})

# ---- box text: long title/subtitle/caption wrap to the page width ----------

# First row (top-down fraction) carrying any near-black ink across the width --
# the top of the drawn content (axis labels, ticks, points). A band that wraps
# to more lines reserves more height and pushes this down.
first_ink_row <- function(img, thresh = 0.3) {
  ink <- apply(img[,, 1:3, drop = FALSE] <= thresh, c(1, 2), all)
  which(apply(ink, 1, any))[1] / dim(img)[1]
}

test_that("a long subtitle wraps and reserves height (pushes content down)", {
  long <- paste(rep("wrap", 60), collapse = " ")
  base <- vplot(mtcars, width = 6, height = 4) |> mark_point(x = wt, y = mpg)
  short_top <- first_ink_row(render_px(base |> labs(subtitle = "short")))
  long_top <- first_ink_row(render_px(base |> labs(subtitle = long)))
  # the wrapped (multi-line) subtitle reserves noticeably more vertical space
  expect_gt(long_top, short_top + 0.05)
})

test_that("a long caption wraps upward, reserving height below the panel", {
  long <- paste(rep("wrap", 60), collapse = " ")
  base <- vplot(mtcars, width = 6, height = 4) |> mark_point(x = wt, y = mpg)
  # a long caption itself inks low, so measure the panel's grey floor: its
  # bottom edge rises when the caption band grows. Detect it as the last
  # *mostly-grey* row (the full-width panel band) rather than sampling one
  # column -- sparse caption text overlapping a single column varies with the
  # rendering font and gave a false floor low in the image.
  panel_floor <- function(sub) {
    img <- render_px(base |> labs(caption = sub))
    grey <- abs(img[,, 1] - img[,, 2]) < 0.03 &
      img[,, 1] > 0.85 &
      img[,, 1] < 0.97
    rows <- which(rowMeans(grey) > 0.5)
    rows[length(rows)] / dim(img)[1]
  }
  expect_lt(panel_floor(long), panel_floor("short") - 0.03)
})

test_that("labs(x = NULL) blanks the axis title; an unpassed arg is untouched", {
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
  expect_identical(labs(p, x = NULL)@labels$x, "") # explicit NULL -> blank
  expect_identical(labs(p, x = "")@labels$x, "") # "" also blanks
  expect_identical(labs(p, x = "Weight")@labels$x, "Weight")
  expect_null(labs(p, y = "MPG")@labels$x) # x not passed -> not set
  expect_identical(labs(p, colour = NULL)@labels$color, "") # alias, explicit NULL
})

test_that("labs() typesets a plotmath expression via plotmath() (VP-163)", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    labs(x = expression(alpha^2), y = quote(beta[i]), title = "Plain")
  expect_s3_class(p@labels$x, "vellum::vellum_label")
  expect_identical(p@labels$x@tex, vellum::plotmath(expression(alpha^2))@tex)
  expect_identical(p@labels$y@tex, vellum::plotmath(expression(beta[i]))@tex)
  expect_identical(p@labels$title, "Plain")
  # The drawn axis title is the typeset label, not the deparsed source.
  svg <- plot_svg(p)
  # The alt text, <title> and <desc> read a plain rendering, not TeX source.
  expect_false(grepl("\\alpha", svg, fixed = TRUE))
  expect_match(plot_alt(p), "\u03b1^2 (horizontal axis)", fixed = TRUE)
  expect_match(plot_alt(p), "\u03b2_i (vertical axis)", fixed = TRUE)
  t <- plot_svg(p |> labs(title = expression(paste("Effect of ", mu))))
  expect_match(t, "<title[^>]*>Effect of \u03bc</title>")
})

test_that("labs() refuses an expression for alt and warns on a long one", {
  b <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
  expect_error(labs(b, alt = expression(alpha)), "must be plain text")
  expect_warning(
    p <- labs(b, x = expression(a, b)),
    "uses only the first element"
  )
  expect_identical(p@labels$x@text, "a")
  expect_error(
    spec_to_json(labs(b, x = expression(alpha))),
    "plotmath"
  )
})

test_that(".plotmath_plain() reads common plotmath (VP-163)", {
  pp <- vellumplot:::.plotmath_plain
  expect_identical(pp(quote(alpha^2)), "\u03b1^2")
  expect_identical(pp(quote(beta[i])), "\u03b2_i")
  expect_identical(pp(quote(R^2 == 0.75)), "R^2 = 0.75")
  expect_identical(pp(quote(list(a, b))), "a, b")
  expect_identical(pp(quote(hat(mu) * "/s")), "\u03bc/s")
  expect_identical(pp(quote(frac(1, 2))), "1/2")
  expect_identical(pp(quote(-x)), "-x")
})

# VP-171: a scale name or guide title given as an expression was drawn as its
# source text; it is typeset as labs() typesets one.
test_that("scale names and guide titles typeset plotmath (VP-171)", {
  b <- vplot(mtcars) |> mark_point(x = wt, y = mpg, color = factor(cyl))
  p <- b |> scale_x_continuous(name = expression(alpha^2))
  nm <- p@scales[[1]]@name
  expect_s3_class(nm, "vellum::vellum_label")
  expect_identical(nm@tex, vellum::plotmath(expression(alpha^2))@tex)
  expect_identical(nm@text, "\u03b1^2")
  expect_match(plot_alt(p), "\u03b1^2 (horizontal axis)", fixed = TRUE)
  # A quoted call too, and the guide title by guides() or the scale's guide.
  q <- b |> scale_y_continuous(name = quote(beta[i]))
  expect_identical(q@scales[[1]]@name@text, "\u03b2_i")
  g <- b |> guides(color = guide_legend(title = expression(gamma)))
  expect_s3_class(g@scales[[1]]@guide$title, "vellum::vellum_label")
  g2 <- b |>
    scale_x_continuous(guide = guide_axis(title = quote(gamma^2)))
  expect_identical(g2@scales[[1]]@guide$title@text, "\u03b3^2")
  a <- b |> guides(x = guide_axis(title = quote(delta)))
  expect_identical(a@scales[[1]]@guide$title@text, "\u03b4")
  s <- sec_axis(~ . * 2, name = expression(mu))
  expect_identical(s@name@text, "\u03bc")
  # The drawn title is the typeset label, not the source text.
  svg <- plot_svg(p |> guides(color = guide_legend(title = expression(gamma))))
  expect_false(grepl("alpha^2", svg, fixed = TRUE))
  expect_false(grepl(">gamma<", svg, fixed = TRUE))
  # Plain names are untouched; only the first element of an expression draws.
  expect_identical(
    (b |> scale_x_continuous(name = "Weight"))@scales[[1]]@name,
    "Weight"
  )
  expect_warning(
    w <- b |> scale_x_continuous(name = expression(a, b)),
    "first element"
  )
  expect_identical(w@scales[[1]]@name@text, "a")
})

# VP-172: a tex() title leaked its TeX source into the alt text and the SVG's
# <title>/<desc>; they read it as plain text now.
test_that("tex() titles read as plain text in alt text and SVG (VP-172)", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    labs(
      title = tex("\\alpha^{2} + \\frac{a}{b}"),
      x = tex("\\mathrm{wt}\\ (10^3\\,\\mathrm{lb})"),
      y = md("Fit $\\hat{y} = \\beta_{0}$ **now**")
    )
  alt <- plot_alt(p)
  expect_match(alt, "wt (10^3 lb) (horizontal axis)", fixed = TRUE)
  expect_match(alt, "Fit y = \u03b2_0 now (vertical axis)", fixed = TRUE)
  svg <- plot_svg(p)
  expect_match(svg, ">\u03b1^2 + a/b</title>", fixed = TRUE)
  expect_false(grepl("\\alpha", svg, fixed = TRUE))
  expect_false(grepl("\\mathrm", svg, fixed = TRUE))
  # plotmath() labels read the same way.
  q <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    labs(x = plotmath("alpha^2"))
  expect_match(plot_alt(q), "\u03b1^2 (horizontal axis)", fixed = TRUE)
})

test_that(".tex_plain() reads common TeX (VP-172)", {
  tp <- vellumplot:::.tex_plain
  expect_identical(tp("\\alpha^{2}"), "\u03b1^2")
  expect_identical(tp("\\beta_{i}"), "\u03b2_i")
  expect_identical(tp("x^{ab}"), "x^(ab)")
  expect_identical(tp("\\frac{1}{n}"), "1/n")
  expect_identical(tp("\\frac{x+1}{2}"), "(x+1)/2")
  expect_identical(tp("\\sqrt{x}"), "\u221a(x)")
  expect_identical(tp("\\hat{y} = \\bar{x}"), "y = x")
  expect_identical(tp("a \\leq b \\times c"), "a \u2264 b \u00d7 c")
  expect_identical(tp("\\int_0^\\infty"), "\u222b_0^\u221e")
  expect_identical(tp("\\text{mean}\\,x"), "mean x")
  expect_identical(tp("\\{x\\}"), "{x}")
  expect_identical(tp("E = mc^2"), "E = mc^2")
  expect_identical(tp("\\left( a \\right)"), "( a )")
  expect_identical(tp("\\unknown x"), "unknown x")
})
