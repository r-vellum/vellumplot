# Element constructors + helpers.

test_that("element_text aliases color to colour and recycles margin", {
  e <- element_text(color = "red", size = 12, margin = 2)
  expect_identical(e@colour, "red")
  expect_identical(e@size, 12)
  expect_identical(e@margin, rep(2, 4))
  expect_null(element_text()@colour)
})

test_that("element_line / element_rect alias color to colour", {
  expect_identical(element_line(color = "blue")@colour, "blue")
  expect_identical(element_rect(color = "blue", fill = "grey")@colour, "blue")
  expect_identical(element_rect(fill = "grey")@fill, "grey")
})

test_that(".is_blank distinguishes element_blank from elements", {
  expect_true(vellumplot:::.is_blank(element_blank()))
  expect_false(vellumplot:::.is_blank(element_text()))
  expect_false(vellumplot:::.is_blank(element_line()))
})

test_that(".merge_element fills NULL child props from the parent", {
  parent <- element_text(colour = "black", size = 11, family = "serif")
  child <- element_text(size = 20)
  m <- vellumplot:::.merge_element(parent, child)
  expect_identical(m@size, 20) # child wins
  expect_identical(m@colour, "black") # inherited
  expect_identical(m@family, "serif") # inherited
})
