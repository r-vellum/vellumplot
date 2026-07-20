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

test_that("a plot that declares interaction keys its marks (for a host to address)", {
  # filter_by / a selection with no per-mark condition or tooltip still needs keys
  p <- vplot(df) |>
    mark_point(x = x, y = y) |>
    select_interval("br", on = "x") |>
    filter_by("br")
  m <- vellum::scene_model(vellum::as_vellum_scene(p))
  expect_equal(sum(!is.na(m$elements$key)), nrow(df))
  # a plot with no declared interaction stays unkeyed (SVG unchanged)
  m0 <- vellum::scene_model(vellum::as_vellum_scene(
    vplot(df) |> mark_point(x = x, y = y)
  ))
  expect_equal(sum(!is.na(m0$elements$key)), 0L)
})

test_that("a composition resolves cross-cell selection references", {
  # selection defined in cell A (add_selection), referenced by filter_by in cell B
  sel <- select_interval("br", on = "x")
  comp <- vconcat(
    vplot(df) |> mark_point(x = x, y = y) |> add_selection(sel),
    vplot(df) |> mark_point(x = x, y = y) |> filter_by(sel)
  )
  im <- interaction_model(comp)
  expect_length(im$selections, 1L)
  expect_length(im$filters, 1L)
  expect_identical(im$filters[[1]]$selection, "br")
})

test_that("composition cells get unique keys + a join id; single plots do not", {
  # single plot: keys are the row/data ids, no join
  sp <- vellum::scene_model(vellum::as_vellum_scene(
    vplot(df) |> mark_point(x = x, y = y, tooltip = x)
  ))$elements
  sp <- sp[!is.na(sp$key), , drop = FALSE]
  expect_false(any(grepl(":", sp$key)))
  expect_null(sp$meta[[1]]$join)

  # composition: cells share row identity via `join`, but keys are unique per cell
  sel <- select_interval("br", on = "x")
  comp <- vconcat(
    vplot(df) |> mark_point(x = x, y = y) |> add_selection(sel),
    vplot(df) |> mark_point(x = x, y = y) |> filter_by(sel)
  )
  kc <- vellum::scene_model(vellum::as_vellum_scene(comp))$elements
  kc <- kc[!is.na(kc$key), , drop = FALSE]
  expect_equal(length(unique(kc$key)), nrow(kc)) # all keys unique
  expect_true(all(grepl("^subplot-", kc$key)))
  joins <- vapply(kc$meta, function(m) m$join, character(1))
  expect_setequal(joins, as.character(rep(seq_len(nrow(df)), 2))) # shared identity
})

test_that("a cross-view filter tags only the filtering cell's elements", {
  sel <- select_interval("br", on = "x")
  comp <- vconcat(
    vplot(df) |> mark_point(x = x, y = y) |> add_selection(sel), # source
    vplot(df) |> mark_point(x = x, y = y) |> filter_by(sel) # filtered
  )
  m <- vellum::scene_model(vellum::as_vellum_scene(comp))
  keyed <- m$elements[!is.na(m$elements$key), , drop = FALSE]
  # both cells keyed (shared row-index keys), only the filtered cell carries `filt`
  expect_equal(nrow(keyed), 2L * nrow(df))
  has_filt <- vapply(
    keyed$meta,
    function(mm) !is.null(mm[["filt"]]) && "br" %in% mm[["filt"]],
    logical(1)
  )
  expect_equal(sum(has_filt), nrow(df))
})

test_that("select_point(group_by=) emits the field value as hover_group", {
  # group_by lets a host link the whole group on hover/click via the existing
  # hover-group machinery (no widget change). One hover_group per element, = its
  # group value; inert on a static render.
  d <- data.frame(x = 1:6, y = 1:6, g = rep(c("a", "b", "c"), 2))
  p <- vplot(d) |>
    mark_point(x = x, y = y, color = condition("hi", g, "grey80")) |>
    select_point("hi", on = "hover", group_by = "g")
  m <- vellum::scene_model(vellum::as_vellum_scene(p))
  el <- m$elements[!is.na(m$elements$key), , drop = FALSE]
  hg <- vapply(
    el$meta,
    function(mm) if (is.null(mm$hover_group)) NA_character_ else mm$hover_group,
    character(1)
  )
  expect_setequal(hg, c("a", "b", "c"))
  expect_equal(sum(hg == "a"), 2L) # each group has its members

  # transparent to the static render
  r <- function(pl) {
    f <- withr::local_tempfile(fileext = ".png")
    render_plot(pl, f)
    readBin(f, "raw", 1e7)
  }
  expect_identical(
    r(vplot(d) |> mark_point(x = x, y = y, color = g)),
    r(p)
  )
  # a user-declared hover_group is not overridden by group_by
  p2 <- vplot(d) |>
    mark_point(x = x, y = y, hover_group = y) |>
    select_point("hi", on = "hover", group_by = "g")
  m2 <- vellum::scene_model(vellum::as_vellum_scene(p2))
  el2 <- m2$elements[!is.na(m2$elements$key), , drop = FALSE]
  hg2 <- vapply(
    el2$meta,
    function(mm) as.character(mm$hover_group),
    character(1)
  )
  expect_setequal(hg2, as.character(1:6)) # the user's y-based hover_group wins
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
