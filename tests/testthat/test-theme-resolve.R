# The theme resolver: inheritance, override, and blank propagation.

resolve <- function(theme) vellumplot:::.resolve_theme(theme)
gray <- function() vellumplot:::.theme_gray_complete()

test_that("leaves inherit through the element tree", {
  rt <- resolve(gray())
  # axis.title.x -> axis.title -> title -> text (size 11, colour black)
  expect_identical(rt[["axis.title.x"]]@size, 11)
  expect_identical(rt[["axis.title.x"]]@colour, "black")
  # panel.grid.major.x -> panel.grid -> line (white, lwd 1)
  expect_identical(rt[["panel.grid.major.x"]]@colour, "white")
  expect_identical(rt[["panel.grid.major.x"]]@linewidth, 1)
})

test_that("an intermediate override reaches both child leaves but keeps siblings", {
  th <- vellumplot:::.merge_theme(
    gray(),
    list(axis.title = element_text(size = 20))
  )
  rt <- resolve(th)
  expect_identical(rt[["axis.title.x"]]@size, 20)
  expect_identical(rt[["axis.title.y"]]@size, 20)
  expect_identical(rt[["axis.title.x"]]@colour, "black") # colour still inherited
  expect_identical(rt[["axis.text.x"]]@size, 9) # sibling untouched
})

test_that("element_blank on a leaf short-circuits only that leaf", {
  th <- vellumplot:::.merge_theme(
    gray(),
    list(panel.grid.minor = element_blank())
  )
  rt <- resolve(th)
  expect_true(vellumplot:::.is_blank(rt[["panel.grid.minor.x"]]))
  expect_false(vellumplot:::.is_blank(rt[["panel.grid.major.x"]]))
})

test_that("element_blank on an intermediate propagates to all descendants", {
  th <- vellumplot:::.merge_theme(gray(), list(panel.grid = element_blank()))
  rt <- resolve(th)
  expect_true(vellumplot:::.is_blank(rt[["panel.grid.major.x"]]))
  expect_true(vellumplot:::.is_blank(rt[["panel.grid.minor.y"]]))
})

test_that("a theme() element under a blank parent is drawn, as in ggplot2", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    theme_void() |>
    theme(axis.text.x = element_text(colour = "red"))
  rt <- resolve(p@theme)
  # drawn, with its unset properties from the nearest non-blank ancestor
  # (axis.text is blank in theme_void, so text: base size 11)
  expect_false(vellumplot:::.is_blank(rt[["axis.text.x"]]))
  expect_identical(rt[["axis.text.x"]]@colour, "red")
  expect_identical(rt[["axis.text.x"]]@size, 11)
  # the sibling the user did not set stays blank
  expect_true(vellumplot:::.is_blank(rt[["axis.text.y"]]))
  # and the labels reach the scene
  sc <- vellum::as_vellum_scene(p)
  expect_true(any(grepl("#ff0000", vellum::scene_svg(sc), fixed = TRUE)))
})

test_that("a preset's own children stay blank under a blank parent", {
  # theme_classic / theme_void blank panel.grid; the gray base's
  # panel.grid.minor element must not resurface
  for (preset in list(theme_classic, theme_void)) {
    rt <- resolve(preset(vplot(mtcars))@theme)
    expect_true(vellumplot:::.is_blank(rt[["panel.grid.minor.x"]]))
  }
  # a theme() element set blank again is blank
  p <- vplot(mtcars) |>
    theme_void() |>
    theme(axis.text.x = element_text(colour = "red")) |>
    theme(axis.text.x = element_blank())
  expect_true(vellumplot:::.is_blank(resolve(p@theme)[["axis.text.x"]]))
})

test_that("presets resolve every drawn leaf to blank or the right element", {
  cls <- list(
    text = vellumplot:::.element_text,
    line = vellumplot:::.element_line,
    rect = vellumplot:::.element_rect
  )
  for (preset in list(
    theme_gray,
    theme_minimal,
    theme_bw,
    theme_classic,
    theme_void
  )) {
    p <- preset(vplot(mtcars) |> mark_point(x = wt, y = mpg))
    rt <- resolve(p@theme)
    for (slot in vellumplot:::.DRAWN_LEAVES) {
      el <- rt[[slot]]
      ok <- vellumplot:::.is_blank(el) ||
        S7::S7_inherits(el, cls[[vellumplot:::.slot_root(slot)]])
      expect_true(ok, info = paste(slot, "must be blank or its element class"))
    }
    # the key drawn text/line/rect properties are actually populated in gray
    if (identical(preset, theme_gray)) {
      expect_identical(rt[["axis.text.x"]]@size, 9)
      expect_identical(rt[["panel.grid.major.x"]]@colour, "white")
      expect_identical(rt[["panel.background"]]@fill, "grey92")
    }
  }
})

# VP-10: text size cascades through the tree via rel().
text_sizes <- function(rt) {
  leaves <- c(
    "plot.title",
    "plot.subtitle",
    "plot.caption",
    "plot.tag",
    "axis.title.x",
    "axis.title.y",
    "legend.title",
    "axis.text.x",
    "axis.text.y",
    "legend.text",
    "strip.text"
  )
  vapply(leaves, function(s) rt[[s]]@size, numeric(1))
}

test_that("default text sizes are unchanged by the rel() cascade (VP-10)", {
  expect_identical(
    text_sizes(resolve(gray())),
    c(
      plot.title = 14,
      plot.subtitle = 11,
      plot.caption = 9,
      plot.tag = 12,
      axis.title.x = 11,
      axis.title.y = 11,
      legend.title = 10,
      axis.text.x = 9,
      axis.text.y = 9,
      legend.text = 9,
      strip.text = 9
    )
  )
})

test_that("theme(text = size) scales every text element (VP-10)", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    theme(text = element_text(size = 22))
  sz <- text_sizes(resolve(p@theme))
  expect_equal(sz, text_sizes(resolve(gray())) * 2)
})

test_that("base_size scales a complete theme (VP-10)", {
  for (preset in list(theme_gray, theme_minimal, theme_bw, theme_cyberpunk)) {
    p <- vplot(mtcars) |> mark_point(x = wt, y = mpg) |> preset(base_size = 22)
    expect_equal(text_sizes(resolve(p@theme)), text_sizes(resolve(gray())) * 2)
  }
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    theme_bw(base_family = "serif")
  expect_identical(resolve(p@theme)[["axis.text.x"]]@family, "serif")
  expect_snapshot(error = TRUE, theme_gray(vplot(mtcars), base_size = -1))
})

test_that("rel() resolves against the parent element (VP-10)", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    theme(
      text = element_text(size = 20),
      axis.text = element_text(size = rel(0.5)),
      axis.text.x = element_text(size = rel(2))
    )
  rt <- resolve(p@theme)
  expect_equal(rt[["axis.text.y"]]@size, 10)
  expect_equal(rt[["axis.text.x"]]@size, 20) # rel(2) of axis.text's 10
  expect_equal(rt[["plot.title"]]@size, 20 * 14 / 11) # title still cascades
  # rel() over an absolute size scales it
  p2 <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    theme(text = element_text(size = rel(2)))
  expect_equal(resolve(p2@theme)[["axis.title.x"]]@size, 22)
  expect_s3_class(rel(1.5), "rel")
  expect_snapshot(error = TRUE, rel("a"))
})

test_that("a text colour still inherits where no preset colour is set (VP-10)", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    theme(text = element_text(colour = "red"))
  rt <- resolve(p@theme)
  expect_identical(rt[["axis.title.x"]]@colour, "red")
  expect_identical(rt[["legend.title"]]@colour, "red")
})
