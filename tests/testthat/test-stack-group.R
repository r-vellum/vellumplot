# Stacks follow the group key, whichever aesthetic defines it (VP-38).

us_exp <- function() {
  u <- datasets::USPersonalExpenditure
  data.frame(
    category = factor(rep(rownames(u), ncol(u)), levels = rownames(u)),
    year = factor(rep(colnames(u), each = nrow(u))),
    value = as.vector(u)
  )
}

# Bottom-to-top category order of the stack at the first year.
stack_order <- function(p) {
  r <- .resolve_layers(p)[[1]]
  first <- r$values$x == levels(r$values$x)[1]
  key <- r$values$pattern %||% r$values$linetype %||% r$values$fill
  as.character(key[first][order(r$values$ymin[first])])
}

test_that("a stack grouped by pattern runs in legend order, like fill", {
  d <- us_exp()
  by_fill <- stack_order(
    vplot(d) |> mark_bar(x = year, y = value, fill = category)
  )
  by_pattern <- stack_order(
    vplot(d) |> mark_bar(x = year, y = value, pattern = category)
  )
  # first level on top, as ggplot2
  expect_identical(by_fill, rev(levels(d$category)))
  expect_identical(by_pattern, by_fill)
})

test_that("pattern, linetype and shape group a layer without color or fill", {
  d <- data.frame(x = 1:4, g = c("a", "b", "a", "b"))
  for (aes in c("pattern", "linetype", "shape")) {
    L <- .resolve_layers(
      vplot(d) |> mark_point(x = x, y = x, !!rlang::sym(aes) := g)
    )[[1]]
    expect_identical(as.character(.group_key(L)), d$g)
  }
  # with color too, the key is their interaction (VP-52)
  L <- .resolve_layers(
    vplot(d) |> mark_point(x = x, y = x, color = x > 2, shape = g)
  )[[1]]
  expect_identical(
    as.character(.group_key(L)),
    c("FALSE:a", "FALSE:b", "TRUE:a", "TRUE:b")
  )
})
