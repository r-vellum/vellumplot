# VP-86: a constant `if_true` in `condition()` is a setting, not a mapping: no
# scale, no legend, drawn at its value; a mapped `if_true` still gets its scale.

# The scene without the interactivity meta (`keys` / `meta`), which a
# selection adds to the conditioned layer only.
cc_scene <- function(p) {
  strip <- function(x) {
    if (!is.list(x)) {
      return(x)
    }
    x[c("keys", "meta")] <- NULL
    lapply(x, strip)
  }
  strip(vellum::as_scene_spec(vellum::as_vellum_scene(p)))
}

cc_point_meta <- function(p) {
  m <- vellum::scene_model(vellum::as_vellum_scene(p), meta = "columns")
  el <- m$elements
  keep <- !is.na(el$key) & el$mark == "point"
  list(cond = el$meta$cond[keep], cond_value = el$meta$cond_value[keep])
}

has_legend <- function(p) {
  m <- vellum::scene_model(vellum::as_vellum_scene(p), meta = "columns")
  any(m$panels$name == "legend")
}

test_that("a constant alpha if_true adds no alpha legend and draws at its value", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, alpha = condition("s", 1, 0.2)) |>
    select_point("s")
  expect_false(has_legend(p))
  plain <- vplot(mtcars) |> mark_point(x = wt, y = mpg, alpha = 1)
  expect_identical(cc_scene(p), cc_scene(plain))
})

test_that("a constant alpha if_true next to a mapped colour keeps only the colour legend", {
  p <- vplot(mtcars) |>
    mark_point(
      x = wt,
      y = mpg,
      color = factor(cyl),
      alpha = condition("s", 0.6, 0.2)
    ) |>
    select_point("s")
  plain <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, color = factor(cyl), alpha = 0.6)
  expect_identical(cc_scene(p), cc_scene(plain))
})

test_that("constant colour, fill and size if_true values are settings too", {
  cases <- list(
    color = list(
      vplot(mtcars) |>
        mark_point(x = wt, y = mpg, color = condition("s", "red", "grey80")),
      vplot(mtcars) |> mark_point(x = wt, y = mpg, color = "red")
    ),
    fill = list(
      vplot(mtcars) |>
        mark_bar(x = factor(cyl), fill = condition("s", "steelblue")),
      vplot(mtcars) |> mark_bar(x = factor(cyl), fill = "steelblue")
    ),
    size = list(
      vplot(mtcars) |>
        mark_point(x = wt, y = mpg, size = condition("s", 4, 1)),
      vplot(mtcars) |> mark_point(x = wt, y = mpg, size = 4)
    ),
    # a computed constant (no data column) counts as a constant
    rgb = list(
      vplot(mtcars) |>
        mark_point(x = wt, y = mpg, color = condition("s", rgb(1, 0, 0))),
      vplot(mtcars) |> mark_point(x = wt, y = mpg, color = "#FF0000")
    )
  )
  for (nm in names(cases)) {
    cnd <- cases[[nm]][[1]] |> select_point("s")
    expect_false(has_legend(cnd), label = nm)
    expect_identical(cc_scene(cnd), cc_scene(cases[[nm]][[2]]), label = nm)
  }
})

test_that("a mapped if_true still trains its scale and legend", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, alpha = condition("s", hp, 0.2)) |>
    select_point("s")
  plain <- vplot(mtcars) |> mark_point(x = wt, y = mpg, alpha = hp)
  expect_true(has_legend(p))
  expect_identical(cc_scene(p), cc_scene(plain))
  q <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, color = condition("s", factor(cyl), "grey80")) |>
    select_point("s")
  expect_identical(
    cc_scene(q),
    cc_scene(vplot(mtcars) |> mark_point(x = wt, y = mpg, color = factor(cyl)))
  )
})

test_that("a constant if_true keeps the condition for the host", {
  d <- mtcars
  d$id <- rownames(d)
  d$af <- ifelse(d$am == 1, 0.1, 0.5)
  p <- vplot(d) |>
    mark_point(x = wt, y = mpg, data_id = id, alpha = condition("s", 1, af)) |>
    select_point("s")
  im <- interaction_model(p)
  expect_length(im$conditions, 1L)
  expect_identical(im$conditions[[1]]$aes, "alpha")
  expect_identical(im$conditions[[1]]$tag, "s:alpha")
  meta <- cc_point_meta(p)
  expect_true(all(vapply(meta$cond, identical, logical(1), "s:alpha")))
  # the per-row if_false numbers are carried as they are (VW-56)
  expect_equal(
    vapply(meta$cond_value, function(e) e[["s:alpha"]], numeric(1)),
    d$af
  )
  # a constant if_false on a constant if_true is the plot-level value
  q <- vplot(d) |>
    mark_point(x = wt, y = mpg, data_id = id, alpha = condition("s", 1, 0.2)) |>
    select_point("s")
  expect_equal(interaction_model(q)$conditions[[1]]$if_false, 0.2)
})

test_that("a constant if_true survives a spec round trip", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, alpha = condition("s", 1, 0.2)) |>
    select_point("s")
  # (a declared selection is not serializable yet: round-trip the layer)
  p@selections <- list()
  q <- spec_from_json(spec_to_json(p), data = mtcars)
  expect_false(has_legend(q))
  expect_identical(cc_scene(q), cc_scene(p))
})
