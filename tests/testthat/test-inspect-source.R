# inspect_source(): the click-to-source opt-in, surfaced through interaction_model().

test_that("inspect_source sets a SourceSpec on the plot", {
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg) |> inspect_source()
  expect_s7_class(p@source, SourceSpec)
  expect_identical(p@source@on, "click")
  expect_false(p@source@values)

  p2 <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    inspect_source(on = "hover", values = TRUE)
  expect_identical(p2@source@on, "hover")
  expect_true(p2@source@values)
})

test_that("inspect_source surfaces in the interaction model", {
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg) |> inspect_source()
  im <- interaction_model(p)
  expect_false(is.null(im))
  expect_identical(im$source$on, "click")
  expect_false(im$source$values)
})

test_that("a plot with only inspect_source still has a non-null interaction model", {
  # (guards .parts_empty accounting for source)
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg) |> inspect_source()
  expect_false(is.null(interaction_model(p)))

  plain <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
  expect_null(interaction_model(plain))
})

test_that("inspect_source pairs with the provenance payload a host consumes", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, color = factor(cyl)) |>
    inspect_source()
  im <- interaction_model(p)
  pl <- provenance_payload(p, values = im$source$values)
  # every element id the payload exposes is a real scene id
  sc <- vellum::as_vellum_scene(p)
  ids <- unique(stats::na.omit(vellum::scene_model(sc)$elements$id))
  expect_true(all(vapply(pl$elements, function(e) e$id, character(1)) %in% ids))
})

test_that("inspect_source round-trips through a composition's interaction model", {
  p1 <- vplot(mtcars) |> mark_point(x = wt, y = mpg) |> inspect_source()
  p2 <- vplot(mtcars) |> mark_point(x = wt, y = qsec)
  im <- interaction_model(concat(p1, p2))
  expect_identical(im$source$on, "click")
})
