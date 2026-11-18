# The `group` aesthetic and the one grouping key (`.group_key()`) every stat,
# position and connected mark derives series from.

# Provenance entries (one per emitted grob) of the given mark type.
prov_of <- function(p, mark) {
  pr <- attr(as_vellum_scene(p), "vellumplot_provenance")
  Filter(function(e) identical(e$mark, mark), pr)
}

# The first layer's values after its stat and position ran.
layer_values <- function(p) vellumplot:::.resolve_layers(p)[[1]]$values

grp_df <- data.frame(
  t = rep(1:5, 3),
  y = c(1:5, 5:1, rep(3, 5)),
  id = rep(c("a", "b", "c"), each = 5)
)

test_that(".group_key: explicit group wins, continuous colour never groups", {
  gk <- vellumplot:::.group_key
  L <- list(values = list(group = c("a", "b"), color = c("x", "y")), n = 2)
  expect_equal(gk(L), c("a", "b"))
  # a continuous colour is skipped in favour of a discrete fill
  L <- list(values = list(color = c(1.5, 2.5), fill = c("p", "q")), n = 2)
  expect_equal(gk(L), c("p", "q"))
  L <- list(values = list(color = c(1.5, 2.5)), n = 2)
  expect_null(gk(L))
  # a factor is discrete even when its labels look numeric
  L <- list(values = list(color = factor(c(4, 6))), n = 2)
  expect_equal(as.character(gk(L)), c("4", "6"))
  # a constant `group = 1` makes the whole layer one group
  L <- list(values = list(color = c("x", "y")), params = list(group = 1), n = 2)
  expect_equal(gk(L), c(1, 1))
})

test_that("mark_line(group =) draws one polyline per group", {
  p <- vplot(grp_df) |> mark_line(x = t, y = y, group = id)
  lines <- prov_of(p, "line")
  expect_length(lines, 3L)
  # each polyline keys exactly its own group's rows, in x order
  expect_equal(lapply(lines, `[[`, "rows"), list(1:5, 6:10, 11:15))
  # ungrouped it is one zig-zag through every row, as before
  expect_length(prov_of(vplot(grp_df) |> mark_line(x = t, y = y), "line"), 1L)
})

test_that("group splits step, area, ribbon and text_path series", {
  expect_length(
    prov_of(vplot(grp_df) |> mark_step(x = t, y = y, group = id), "step"),
    3L
  )
  expect_length(
    prov_of(
      vplot(grp_df) |>
        mark_area(x = t, y = y, group = id, position = "identity"),
      "area"
    ),
    3L
  )
  expect_length(
    prov_of(
      vplot(grp_df) |>
        mark_ribbon(x = t, ymin = y - 1, ymax = y + 1, group = id),
      "ribbon"
    ),
    3L
  )
  d <- transform(grp_df, lab = "path label")
  expect_length(
    prov_of(
      vplot(d) |> mark_text_path(x = t, y = y, label = lab, group = id),
      "text_path"
    ),
    3L
  )
})

test_that("a discrete colour varying within a group still splits it by style", {
  d <- transform(grp_df, half = rep(rep(c("u", "v"), c(3, 2)), 3))
  p <- vplot(d) |> mark_line(x = t, y = y, group = id, color = half)
  expect_length(prov_of(p, "line"), 6L)
})

test_that("a continuous colour keeps a line connected, coloured per segment", {
  p <- vplot(pressure) |>
    mark_line(x = temperature, y = pressure, color = pressure)
  lines <- prov_of(p, "line")
  # one connected series: a single segments grob over every consecutive pair,
  # not one invisible single-point series per quantised colour
  expect_length(lines, 1L)
  expect_length(lines[[1]]$rows, nrow(pressure) - 1L)
  svg <- vellum::scene_svg(as_vellum_scene(p))
  strokes <- unique(regmatches(svg, gregexpr('stroke="#[0-9A-Fa-f]{6}', svg))[[
    1
  ]])
  expect_gt(length(strokes), 5L)
  # the same holds for a step
  st <- prov_of(
    vplot(pressure) |>
      mark_step(x = temperature, y = pressure, color = pressure),
    "step"
  )
  expect_length(st, 1L)
})

test_that("stats run per group and keep each group's own colour", {
  p <- vplot(grp_df) |>
    mark_smooth(x = t, y = y, group = id, method = "lm", se = FALSE)
  expect_length(prov_of(p, "smooth"), 3L)
  # group = id, colour = a per-id treatment shared by two ids: three fits, each
  # painted by its own treatment, not by the id label
  d <- transform(grp_df, trt = rep(c("ctl", "drug", "drug"), each = 5))
  L <- layer_values(
    vplot(d) |>
      mark_smooth(x = t, y = y, group = id, color = trt, method = "lm")
  )
  expect_setequal(unique(as.character(L$color)), c("ctl", "drug"))
  per_group <- tapply(as.character(L$color), as.character(L$group), unique)
  expect_equal(as.vector(per_group), c("ctl", "drug", "drug"))
  # `group = 1` pools a coloured layer into one fit
  p1 <- vplot(iris) |>
    mark_smooth(
      x = Sepal.Length,
      y = Sepal.Width,
      color = Species,
      group = 1,
      method = "lm",
      se = FALSE
    )
  expect_length(prov_of(p1, "smooth"), 1L)
})

test_that("position = 'stack' stacks by group", {
  d <- data.frame(
    x = c("a", "a", "b", "b"),
    y = c(1, 2, 3, 4),
    g = c(1, 2, 1, 2)
  )
  L <- layer_values(vplot(d) |> mark_bar(x = x, y = y, group = factor(g)))
  expect_equal(L$ymax, c(1, 3, 3, 7))
})
