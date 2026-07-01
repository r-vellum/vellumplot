# The style-grouping rule (mirrors vellum's per-element gpar split): rows are
# grouped by gpar-borne aesthetics only; geometry-borne ones do not force a
# split.

test_that(".style_groups splits by gpar-borne fields", {
  # three distinct colours -> three groups
  g <- quill:::.style_groups(3, list(col = c("a", "a", "b")))
  expect_length(g, 2)
  expect_setequal(unlist(g), 1:3)
})

test_that("no gpar-borne fields means a single group", {
  expect_length(quill:::.style_groups(5, list()), 1)
})

test_that("size and shape do NOT force a style split", {
  # all one colour, but varying size/shape -> still one group
  g <- quill:::.style_groups(4, list(col = rep("red", 4)))
  expect_length(g, 1)
})

test_that("alpha forces a split", {
  g <- quill:::.style_groups(
    3,
    list(col = rep("red", 3), alpha = c(1, 1, 0.5))
  )
  expect_length(g, 2)
})
