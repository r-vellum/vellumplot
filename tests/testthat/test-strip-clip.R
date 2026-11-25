# Facet strip text is clipped to its strip (VP-134), and label_wrap_gen()
# strips reserve their lines.

long_lab <- "A facet label far longer than its panel is wide, WWWWWWWW"

# The scene-spec node whose viewport is named `name`.
find_vp <- function(node, name) {
  if (!is.list(node)) {
    return(NULL)
  }
  nm <- node$vp$name
  if (is.list(nm)) {
    nm <- nm$v
  }
  if (identical(nm, name)) {
    return(node)
  }
  for (k in node) {
    hit <- find_vp(k, name)
    if (!is.null(hit)) {
      return(hit)
    }
  }
  NULL
}

# Classes of the grobs below a node that sit in a clipping viewport.
clipped_classes <- function(node, clipped = FALSE) {
  if (!is.list(node)) {
    return(character(0))
  }
  here <- clipped || isTRUE(node$vp$clip)
  out <- if (here && is.character(node$class)) node$class else character(0)
  for (k in node$children$items) {
    out <- c(out, clipped_classes(k, here))
  }
  out
}

test_that("strip text is clipped to its strip; the background is not", {
  d <- data.frame(x = 1:4, y = 1:4, g = rep(c(long_lab, "short"), 2))
  p <- vplot(d, width = 4, height = 3) |>
    mark_point(x = x, y = y) |>
    facet_wrap(~g)
  ss <- vellum::as_scene_spec(vellum::as_vellum_scene(p))
  for (nm in c("strip-1-1", "strip-1-2")) {
    strip <- find_vp(ss$root, nm)
    expect_false(is.null(strip))
    expect_false(isTRUE(strip$vp$clip))
    cls <- clipped_classes(strip)
    expect_true("grob_text" %in% cls)
    expect_false("grob_rect" %in% cls)
  }
})

test_that("grid row and column strips clip their text too", {
  p <- vplot(mtcars, width = 4, height = 3) |>
    mark_point(x = wt, y = mpg) |>
    facet_grid(am ~ cyl)
  ss <- vellum::as_scene_spec(vellum::as_vellum_scene(p))
  for (nm in c("strip-col-1", "strip-row-1")) {
    expect_true("grob_text" %in% clipped_classes(find_vp(ss$root, nm)))
  }
})

test_that("label_wrap_gen() strips reserve every wrapped line", {
  d <- data.frame(x = 1:4, y = 1:4, g = rep(c(long_lab, "short"), 2))
  base <- vplot(d, width = 4, height = 3) |> mark_point(x = x, y = y)
  strip_h <- function(p) {
    pl <- vellumplot:::.build_panels(p)
    rt <- vellumplot:::.resolve_theme(vellumplot:::.theme_of(p))
    lay <- vellumplot:::.build_layout(pl, list(), p@labels, rt)
    vellum::vl_convert(lay$heights[lay$wrapstrip_row[1]], "mm")
  }
  one <- strip_h(base |> facet_wrap(~g))
  wrapped <- base |> facet_wrap(~g, labeller = label_wrap_gen(20))
  n <- length(strwrap(long_lab, 20))
  expect_gt(n, 2L)
  rt <- vellumplot:::.resolve_theme(vellumplot:::.theme_default())
  line_h <- vellumplot:::.el_strheight("Ag", rt[["strip.text"]])
  expect_gte(strip_h(wrapped), one + (n - 1) * line_h * 0.9)
  expect_no_error(vellum::as_vellum_scene(wrapped))
})
