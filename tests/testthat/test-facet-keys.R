# VW-11: default element keys (an interactive plot with no data_id) must be
# unique across facet panels. They used to be row positions within each panel's
# layer, so they repeated: hovering or brushing in one panel lit up the marks
# sharing those positions in every other panel.

.fk_keyed <- function(p) {
  el <- vellum::scene_model(vellum::as_vellum_scene(p))$elements
  el[!is.na(el$key), , drop = FALSE]
}

.fk_join <- function(el) {
  vapply(el$meta, function(m) m$join %||% NA_character_, character(1))
}

test_that("default keys are unique across facet_wrap panels", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    facet_wrap(~cyl) |>
    select_point("s")
  el <- .fk_keyed(p)
  expect_equal(nrow(el), nrow(mtcars))
  expect_false(anyDuplicated(el$key) > 0L)
  # "<panel>:<source row>", and the source row rides along as `join`
  expect_true(all(grepl("^panel-\\d+-\\d+:\\d+$", el$key)))
  expect_setequal(.fk_join(el), as.character(seq_len(nrow(mtcars))))
  expect_identical(sub("^.*:", "", el$key), .fk_join(el))
})

test_that("default keys are unique across facet_grid panels", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, color = condition("s", "red", "grey")) |>
    facet_grid(am ~ gear) |>
    select_point("s")
  el <- .fk_keyed(p)
  expect_gte(nrow(el), nrow(mtcars))
  expect_false(anyDuplicated(el$key) > 0L)
})

test_that("a layer repeated in every panel still gets unique keys", {
  # the second layer's data has no facet column, so every panel draws all of it
  ref <- data.frame(wt = c(2, 4), mpg = c(15, 30))
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    mark_point(data = ref, x = wt, y = mpg, color = "red") |>
    facet_wrap(~cyl) |>
    select_point("s")
  el <- .fk_keyed(p)
  ref_el <- el[el$id != el$id[[1L]] & grepl("layer-2", el$id), , drop = FALSE]
  expect_equal(nrow(ref_el), 2L * 3L)
  expect_false(anyDuplicated(ref_el$key) > 0L)
  # ... while the same reference row keeps one join id in every panel
  expect_setequal(.fk_join(ref_el), c("1", "2"))
})

test_that("a single-panel plot keeps plain row keys and no join", {
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg) |> select_point("s")
  el <- .fk_keyed(p)
  expect_identical(el$key, as.character(seq_len(nrow(mtcars))))
  expect_true(all(is.na(.fk_join(el))))
})

test_that("a declared data_id is used as given in a faceted plot", {
  df <- data.frame(wt = mtcars$wt, mpg = mtcars$mpg, cyl = mtcars$cyl)
  df$model <- rownames(mtcars)
  p <- vplot(df) |>
    mark_point(x = wt, y = mpg, data_id = model) |>
    facet_wrap(~cyl)
  el <- .fk_keyed(p)
  expect_setequal(el$key, df$model)
  expect_true(all(is.na(.fk_join(el))))
})

test_that("an aggregated layer in facets gets unique keys without a join", {
  p <- vplot(mtcars) |>
    mark_bar(x = factor(gear), stat = "count") |>
    facet_wrap(~am) |>
    select_point("s")
  el <- .fk_keyed(p)
  skip_if(nrow(el) == 0L, "bars not keyed")
  expect_false(anyDuplicated(el$key) > 0L)
  expect_true(all(is.na(.fk_join(el))))
})

test_that("faceted composition cells prefix their keys with the cell id", {
  # VP-51: each faceted cell names its panels `panel-r-c`, so without the cell
  # id two cells shared "panel-1-1:3" and a hover in one lit up the other.
  fp <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    facet_wrap(~am) |>
    select_point("s")
  el <- .fk_keyed(hconcat(fp, fp))
  expect_equal(nrow(el), 2L * nrow(mtcars))
  expect_false(anyDuplicated(el$key) > 0L)
  expect_true(all(grepl("^cell-[12]:panel-1-[12]:\\d+$", el$key)))
  # the source row is still the join, shared by both cells
  expect_identical(sub("^.*:", "", el$key), .fk_join(el))
  expect_setequal(
    .fk_join(el)[startsWith(el$key, "cell-2:")],
    as.character(seq_len(nrow(mtcars)))
  )
})

test_that("an unaligned single-panel cell next to a faceted one is prefixed too", {
  sp <- vplot(mtcars) |> mark_point(x = wt, y = mpg, tooltip = cyl)
  fp <- sp |> facet_wrap(~am)
  el <- .fk_keyed(hconcat(sp, fp))
  el <- el[!grepl("^legend:", el$key), , drop = FALSE]
  one <- el[startsWith(el$key, "cell-1:"), , drop = FALSE]
  expect_equal(nrow(one), nrow(mtcars))
  expect_identical(one$key, paste0("cell-1:", .fk_join(one)))
  expect_true(all(startsWith(el$key, "cell-")))
})

test_that("a lone faceted plot's keys are unchanged by the cell prefix", {
  fp <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    facet_wrap(~am) |>
    select_point("s")
  el <- .fk_keyed(fp)
  expect_true(all(grepl("^panel-1-[12]:\\d+$", el$key)))
})
