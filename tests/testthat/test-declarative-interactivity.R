# Declarative interactivity: selections, conditional encodings, filters, binds.
# The core guarantee is that every node is inert on a static render.

df <- data.frame(x = 1:20, y = seq_len(20) / 20, g = rep(c("a", "b"), 10))

test_that("select_point / select_interval build and attach (piped)", {
  p <- vplot(df) |>
    mark_point(x = x, y = y) |>
    select_point("hi", on = "hover")
  expect_length(p@selections, 1L)
  expect_s3_class(p@selections[[1]], "vellumplot::SelectionSpec")
  expect_identical(p@selections[[1]]@name, "hi")
  expect_identical(p@selections[[1]]@kind, "point")
  expect_identical(p@selections[[1]]@on, "hover")

  p2 <- vplot(df) |> mark_point(x = x, y = y) |> select_interval("b", on = "x")
  expect_identical(p2@selections[[1]]@kind, "interval")
  expect_identical(p2@selections[[1]]@on, "x")
})

test_that("select_* free-standing returns a SelectionSpec, add_selection attaches it", {
  sel <- select_interval("brush", on = "x")
  expect_s3_class(sel, "vellumplot::SelectionSpec")
  expect_identical(sel@name, "brush")
  p <- vplot(df) |> mark_point(x = x, y = y) |> add_selection(sel)
  expect_length(p@selections, 1L)
})

test_that("group_by is an alias for fields", {
  s <- select_point("g", group_by = "g")
  expect_identical(s@fields, "g")
})

test_that("condition() cannot be called directly", {
  expect_error(condition("s", 1, 2), "inside a mark encoding")
})

test_that("condition() is transparent: static render identical to the plain encoding", {
  r <- function(p) {
    f <- withr::local_tempfile(fileext = ".png")
    render_plot(p, f)
    readBin(f, "raw", 1e7)
  }
  plain <- r(vplot(df) |> mark_point(x = x, y = y, color = g))
  cond <- r(
    vplot(df) |>
      mark_point(x = x, y = y, color = condition("hi", g, "grey80")) |>
      select_point("hi", on = "hover")
  )
  expect_identical(plain, cond)
  # if_false omitted (theme dim) is equally transparent
  cond2 <- r(
    vplot(df) |>
      mark_point(x = x, y = y, color = condition("hi", g)) |>
      select_point("hi", on = "hover")
  )
  expect_identical(plain, cond2)
})

test_that("adding selections / filters / binds does not change a static render", {
  r <- function(p) {
    f <- withr::local_tempfile(fileext = ".png")
    render_plot(p, f)
    readBin(f, "raw", 1e7)
  }
  base <- r(vplot(df) |> mark_point(x = x, y = y))
  decorated <- r(
    vplot(df) |>
      mark_point(x = x, y = y) |>
      select_interval("b", on = "x") |>
      filter_by("b")
  )
  expect_identical(base, decorated)
})

test_that("interaction_model() returns the declaration block, or NULL", {
  expect_null(interaction_model(vplot(df) |> mark_point(x = x, y = y)))
  im <- interaction_model(
    vplot(df) |>
      mark_point(x = x, y = y, color = condition("hi", g, "grey80")) |>
      select_point("hi", on = "hover")
  )
  expect_length(im$selections, 1L)
  expect_length(im$conditions, 1L)
  expect_identical(im$conditions[[1]]$aes, "color")
  expect_identical(im$conditions[[1]]$selection, "hi")
  expect_identical(im$conditions[[1]]$if_false, "grey80")
  expect_true(im$selections[[1]]$empty)
})

test_that("interaction_model() errors on a dangling selection reference", {
  expect_error(
    interaction_model(
      vplot(df) |> mark_point(x = x, y = y, color = condition("nope", g))
    ),
    "undeclared selection"
  )
  expect_error(
    interaction_model(
      vplot(df) |> mark_point(x = x, y = y) |> filter_by("ghost")
    ),
    "undeclared selection"
  )
})

test_that("the compiled scene carries condition membership tags + keys", {
  p <- vplot(df) |>
    mark_point(x = x, y = y, color = condition("hi", g, "grey80")) |>
    select_point("hi", on = "hover")
  m <- vellum::scene_model(vellum::as_vellum_scene(p))
  el <- m$elements
  keyed <- el[!is.na(el$key), , drop = FALSE]
  expect_equal(nrow(keyed), nrow(df))
  expect_identical(keyed$meta[[1]]$cond, "hi:color")
  # a conditioned layer is interactive but declares no tooltip
  expect_null(keyed$meta[[1]]$tooltip)
})

test_that("bind_scale records a domain bind", {
  p <- vplot(df) |>
    mark_point(x = x, y = y) |>
    add_selection(select_interval("ov", on = "x")) |>
    bind_scale("ov", aes = "x")
  expect_length(p@binds, 1L)
  expect_identical(p@binds[[1]]@aes, "x")
  expect_identical(p@binds[[1]]@selection, "ov")
})
