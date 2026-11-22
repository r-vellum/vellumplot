# VP-75: two composition cells that condition the same aesthetic on the same
# selection keep apart: each cell's tag takes its cell id, in the elements and
# in interaction_model(). Tags no other cell shares stay as they are.

vp75_df <- data.frame(
  x = c(1, 2, 3, 4),
  y = c(2, 1, 4, 3),
  g = c("a", "b", "a", "b"),
  id = paste0("p", 1:4)
)

vp75_cell <- function(if_false, select = FALSE, aes = "color") {
  enc <- list(x = quote(x), y = quote(y), data_id = quote(id))
  enc[[aes]] <- rlang::call2("condition", "s", quote(g), if_false)
  p <- rlang::inject(mark_point(vplot(vp75_df), !!!enc))
  if (select) select_point(p, "s") else p
}

elem_tags <- function(comp) {
  m <- vellum::scene_model(vellum::as_vellum_scene(comp), meta = "columns")
  k <- !is.na(m$elements$key) & !startsWith(m$elements$key, "legend")
  sort(unique(unlist(m$elements$meta$cond[k])))
}

model_tags <- function(comp) {
  vapply(interaction_model(comp)$conditions, function(c) c$tag, "")
}

test_that("a tag two cells share takes each cell's id (aligned cells)", {
  comp <- hconcat(vp75_cell("grey80", select = TRUE), vp75_cell("tomato"))
  expect_identical(model_tags(comp), c("cell-1:s:color", "cell-2:s:color"))
  expect_identical(elem_tags(comp), c("cell-1:s:color", "cell-2:s:color"))
  ifs <- vapply(interaction_model(comp)$conditions, function(c) c$if_false, "")
  expect_identical(ifs, c("grey80", "tomato"))
})

test_that("a cell drawn as a whole plot is prefixed the same way", {
  comp <- hconcat(
    vp75_cell("grey80", select = TRUE),
    vp75_cell("tomato") |> facet_wrap(~g)
  )
  expect_identical(model_tags(comp), c("cell-1:s:color", "cell-2:s:color"))
  expect_identical(elem_tags(comp), c("cell-1:s:color", "cell-2:s:color"))
})

test_that("tags no other cell shares are unchanged", {
  comp <- hconcat(vp75_cell("grey80", select = TRUE), vp75_cell(1, aes = "shape"))
  expect_identical(model_tags(comp), c("s:color", "s:shape"))
  expect_identical(elem_tags(comp), c("s:color", "s:shape"))
  # a lone plot is untouched
  expect_identical(model_tags(vp75_cell("grey80", select = TRUE)), "s:color")
})

test_that("the cell id joins a VP-72 layer suffix", {
  two <- vp75_cell("grey80", select = TRUE) |>
    mark_text(x = x, y = y, label = g, data_id = id, color = condition("s", g, "red"))
  comp <- hconcat(two, vp75_cell("tomato"))
  expect_identical(
    model_tags(comp),
    c("s:color@1", "s:color@2", "s:color")
  )
  # "s:color" is in both cells only once the suffixed ones are set apart:
  # cell 1 has s:color@1 / s:color@2, cell 2 has s:color -- no collision
  comp2 <- hconcat(two, two)
  expect_identical(
    model_tags(comp2),
    c(
      "cell-1:s:color@1", "cell-1:s:color@2",
      "cell-2:s:color@1", "cell-2:s:color@2"
    )
  )
  expect_identical(elem_tags(comp2), sort(model_tags(comp2)))
})
