# The theme resolver: inheritance, override, and blank propagation.

resolve <- function(theme) quill:::.resolve_theme(theme)
gray <- function() quill:::.theme_gray_complete()

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
  th <- quill:::.merge_theme(
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
  th <- quill:::.merge_theme(
    gray(),
    list(panel.grid.minor = element_blank())
  )
  rt <- resolve(th)
  expect_true(quill:::.is_blank(rt[["panel.grid.minor.x"]]))
  expect_false(quill:::.is_blank(rt[["panel.grid.major.x"]]))
})

test_that("element_blank on an intermediate propagates to all descendants", {
  th <- quill:::.merge_theme(gray(), list(panel.grid = element_blank()))
  rt <- resolve(th)
  expect_true(quill:::.is_blank(rt[["panel.grid.major.x"]]))
  expect_true(quill:::.is_blank(rt[["panel.grid.minor.y"]]))
})

test_that("presets resolve every drawn leaf to blank or the right element", {
  cls <- list(
    text = quill:::.element_text,
    line = quill:::.element_line,
    rect = quill:::.element_rect
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
    for (slot in quill:::.DRAWN_LEAVES) {
      el <- rt[[slot]]
      ok <- quill:::.is_blank(el) ||
        S7::S7_inherits(el, cls[[quill:::.slot_root(slot)]])
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
