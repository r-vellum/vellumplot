# VW-46: a per-row `if_false` of a colour aesthetic reaches the element meta
# (`cond_value`) as the colours a host paints non-members with.

cond_values <- function(p) {
  m <- vellum::scene_model(vellum::as_vellum_scene(p), meta = "columns")
  el <- m$elements
  keep <- !is.na(el$key) & !startsWith(el$key, "legend")
  cv <- el$meta$cond_value[keep]
  stats::setNames(
    vapply(cv, function(e) if (is.null(e)) NA_character_ else as.character(e[[1]]), ""),
    el$key[keep]
  )
}

cv_data <- data.frame(
  x = 1:6,
  y = 1:6,
  g = c("a", "b", "c", "a", "b", "c"),
  g2 = c("b", "c", "a", "c", "a", "b"),
  col = c("grey80", "red", "#00ff0080", "blue", "nope", NA),
  v = c(1, 2, 3, 4, 5, 6),
  id = paste0("p", 1:6)
)

test_that("a column of colours is carried as the colours", {
  p <- vplot(cv_data) |>
    mark_point(x = x, y = y, data_id = id, color = condition("s", g, col)) |>
    select_point("s")
  cv <- cond_values(p)[cv_data$id]
  expect_equal(
    unname(cv),
    c("grey80", "red", "#00ff0080", "blue", NA, NA)
  )
})

test_that("a column of the scale's levels is mapped through the scale", {
  p <- vplot(cv_data) |>
    mark_point(x = x, y = y, data_id = id, color = condition("s", g, g2)) |>
    select_point("s")
  cv <- cond_values(p)[cv_data$id]
  # g2 pairs p1 with p6, p2 with p4 and p3 with p5
  sc <- vellumplot:::.cond_colours(
    cv_data$g2,
    list(type = "discrete", levels = c("a", "b", "c"), map = function(x) {
      unname(c(a = "A", b = "B", c = "C")[as.character(x)])
    })
  )
  expect_equal(sc, c("B", "C", "A", "C", "A", "B"))
  expect_equal(cv[["p1"]], cv[["p6"]])
  expect_equal(cv[["p2"]], cv[["p4"]])
  expect_equal(cv[["p3"]], cv[["p5"]])
  expect_length(unique(cv), 3L)
  expect_true(all(grepl("^#", cv)))
})

test_that("numbers map through a continuous colour scale", {
  p <- vplot(cv_data) |>
    mark_point(x = x, y = y, data_id = id, color = condition("s", v, rev(v))) |>
    select_point("s")
  cv <- cond_values(p)[cv_data$id]
  expect_true(all(grepl("^#", cv)))
  # the reversed column gives p1 the colour p6 is drawn with, and so on
  p_plain <- vplot(cv_data) |>
    mark_point(x = x, y = y, data_id = id, color = condition("s", v, v)) |>
    select_point("s")
  cv_plain <- cond_values(p_plain)[cv_data$id]
  expect_equal(unname(cv), rev(unname(cv_plain)))
})

test_that("values that are neither levels nor colours are NA", {
  p <- vplot(cv_data) |>
    mark_point(x = x, y = y, data_id = id, color = condition("s", v, g)) |>
    select_point("s")
  expect_true(all(is.na(cond_values(p))))
})

test_that("other aesthetics and constants are unchanged", {
  p <- vplot(cv_data) |>
    mark_point(x = x, y = y, data_id = id, size = condition("s", v, rev(v))) |>
    select_point("s")
  m <- vellum::scene_model(vellum::as_vellum_scene(p), meta = "columns")
  k <- match(cv_data$id, m$elements$key)
  expect_equal(
    vapply(m$elements$meta$cond_value[k], function(e) e[[1]], 1),
    rev(cv_data$v)
  )
  p2 <- vplot(cv_data) |>
    mark_point(x = x, y = y, data_id = id, color = condition("s", g, "grey80")) |>
    select_point("s")
  m2 <- vellum::scene_model(vellum::as_vellum_scene(p2), meta = "columns")
  expect_null(m2$elements$meta$cond_value)
  expect_equal(interaction_model(p2)$conditions[[1]]$if_false, "grey80")
})
