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
  expect_identical(im$conditions[[1]]$tag, "hi:color")
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

test_that("two layers conditioning one aesthetic on one selection keep apart (VP-72)", {
  lab <- data.frame(x = c(1, 2), y = c(3, 4), g = c("a", "b"), l = c("p", "q"))
  p <- vplot(df) |>
    mark_point(x = x, y = y, color = condition("pick", g, "grey85")) |>
    mark_text(
      data = lab,
      x = x,
      y = y,
      label = l,
      color = condition("pick", g, "tomato")
    ) |>
    select_point("pick", on = "click")
  im <- interaction_model(p)
  tags <- vapply(im$conditions, function(cnd) cnd$tag, character(1))
  falses <- vapply(im$conditions, function(cnd) cnd$if_false, character(1))
  expect_identical(tags, c("pick:color@1", "pick:color@2"))
  expect_identical(falses, c("grey85", "tomato"))
  # each layer's elements carry their own layer's tag, so a host styles a
  # point with the point layer's `if_false` and a label with the text layer's
  el <- vellum::scene_model(vellum::as_vellum_scene(p))$elements
  keyed <- el[!is.na(el$key), , drop = FALSE]
  cond <- vapply(keyed$meta, function(m) paste(m$cond, collapse = ","), "")
  expect_setequal(unique(cond), c("pick:color@1", "pick:color@2"))
  expect_equal(sum(cond == "pick:color@1"), nrow(df))
  expect_equal(sum(cond == "pick:color@2"), nrow(lab))
  # an unshared tag stays "<sel>:<aes>", as before
  one <- vplot(df) |>
    mark_point(x = x, y = y, color = condition("pick", g, "grey85")) |>
    mark_text(data = lab, x = x, y = y, label = l, size = condition("pick", 5, 3)) |>
    select_point("pick", on = "click")
  im1 <- interaction_model(one)
  expect_identical(
    vapply(im1$conditions, function(cnd) cnd$tag, character(1)),
    c("pick:color", "pick:size")
  )
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
  # single plot: default keys are "p1-1:l<layer>:<row>" (VP-55), no join
  sp <- vellum::scene_model(vellum::as_vellum_scene(
    vplot(df) |> mark_point(x = x, y = y, tooltip = x)
  ))$elements
  sp <- sp[!is.na(sp$key), , drop = FALSE]
  expect_true(all(startsWith(sp$key, "p1-1:l1:")))
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
  expect_true(all(grepl("^cell-", kc$key)))
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

# --- select_interval(encodings =): spec record + per-element data values -------

test_that("select_interval() takes encodings, defaulting to x and y", {
  s <- select_interval("b")
  expect_identical(s@kind, "interval")
  expect_identical(s@encodings, c("x", "y"))
  expect_identical(select_interval("b", encodings = "x")@encodings, "x")
  expect_identical(select_interval("b", encodings = "y")@encodings, "y")
  # canonical x-then-y order, duplicates dropped
  expect_identical(
    select_interval("b", encodings = c("y", "x", "y"))@encodings,
    c("x", "y")
  )
  # legacy `on` still works
  expect_identical(select_interval("b", on = "xy")@encodings, c("x", "y"))
  expect_identical(select_interval("b", on = "y")@encodings, "y")
  expect_error(select_interval("b", encodings = "z"), "encodings")
  expect_error(select_interval("b", encodings = character()), "encodings")
})

test_that("an interval selection serialises as {name, type, encodings}", {
  p <- vplot(df) |>
    mark_point(x = x, y = y, color = condition("br", g, "grey80")) |>
    select_interval("br", encodings = "x")
  rec <- interaction_model(p)$selections[[1]]
  expect_identical(rec$name, "br")
  expect_identical(rec$type, "interval")
  # a list, so a single encoding stays a JSON array
  expect_identical(rec$encodings, list("x"))
  expect_identical(rec$on, "x")
  json <- jsonlite::toJSON(rec, auto_unbox = TRUE)
  expect_match(json, '"encodings":["x"]', fixed = TRUE)
  # point selections carry no encodings
  pp <- vplot(df) |> mark_point(x = x, y = y) |> select_point("p")
  expect_null(interaction_model(pp)$selections[[1]]$encodings)
})

interval_meta <- function(p) {
  m <- vellum::scene_model(vellum::as_vellum_scene(p))
  keyed <- !is.na(m$elements$key)
  list(meta = m$elements$meta[keyed], panels = m$panels)
}

test_that("keyed elements carry their data values on the brushed channels", {
  d <- data.frame(
    x = c(1, 10, 100),
    y = c(2, 3, 4),
    g = c("b", "a", "c"),
    day = as.Date("2020-01-01") + c(0, 10, 20)
  )
  # continuous (log axis): raw data values, not transformed ones
  im <- interval_meta(
    vplot(d) |>
      mark_point(x = x, y = y) |>
      scale_x_continuous(trans = "log10") |>
      select_interval("b", encodings = "x")
  )
  expect_equal(vapply(im$meta, `[[`, numeric(1), "ix"), d$x)
  expect_null(im$meta[[1]]$iy) # x-only: no y values
  # date: the epoch number
  im <- interval_meta(
    vplot(d) |> mark_point(x = day, y = y) |> select_interval("b")
  )
  expect_equal(vapply(im$meta, `[[`, numeric(1), "ix"), as.numeric(d$day))
  # discrete: the category label; both channels for an xy selection
  im <- interval_meta(
    vplot(d) |>
      mark_bar(x = g, y = y) |>
      select_interval("b", encodings = c("x", "y"))
  )
  expect_setequal(vapply(im$meta, `[[`, character(1), "ixd"), d$g)
  expect_setequal(vapply(im$meta, `[[`, numeric(1), "iy"), d$y)
})

test_that("a series element spans the range of its rows", {
  d <- data.frame(x = c(3, 1, 7), y = c(1, 2, 3))
  im <- interval_meta(
    vplot(d) |>
      mark_line(x = x, y = y, data_id = "s") |>
      select_interval("b")
  )
  expect_length(im$meta, 1L)
  expect_identical(im$meta[[1]]$ix, 1)
  expect_identical(im$meta[[1]]$ixh, 7)
})

test_that("panel axis descriptors name their aesthetic and discrete levels", {
  d <- data.frame(g = c("b", "a", "c"), y = 1:3)
  im <- interval_meta(
    vplot(d) |> mark_bar(x = g, y = y) |> select_interval("b")
  )
  sc <- Filter(Negate(is.null), lapply(im$panels$meta, `[[`, "scales"))[[1]]
  expect_identical(sc$x$aesthetic, "x")
  expect_identical(sc$x$levels, c("a", "b", "c"))
  expect_null(sc$y$levels)
  # coord_flip: the horizontal axis shows the y aesthetic
  imf <- interval_meta(
    vplot(d) |> mark_bar(x = g, y = y) |> coord_flip() |> select_interval("b")
  )
  scf <- Filter(Negate(is.null), lapply(imf$panels$meta, `[[`, "scales"))[[1]]
  expect_identical(scf$x$aesthetic, "y")
  expect_identical(scf$y$aesthetic, "x")
})

test_that("data values are emitted only for plots with an interval selection", {
  m <- vellum::scene_model(vellum::as_vellum_scene(
    vplot(df) |> mark_point(x = x, y = y) |> select_point("p")
  ))
  meta <- m$elements$meta[!is.na(m$elements$key)]
  expect_false(any(vapply(meta, function(r) "ix" %in% names(r), logical(1))))
})

test_that("select_interval() leaves the static render unchanged", {
  r <- function(p) {
    f <- withr::local_tempfile(fileext = ".png")
    render_plot(p, f)
    readBin(f, "raw", 1e7)
  }
  plain <- r(vplot(df) |> mark_point(x = x, y = y, color = g))
  brushed <- r(
    vplot(df) |>
      mark_point(x = x, y = y, color = condition("br", g, "grey80")) |>
      select_interval("br", encodings = c("x", "y"))
  )
  expect_identical(plain, brushed)
})

test_that("range marks carry the span of their extent", {
  d <- data.frame(a = c(1, 5), b = c(3, 4), c = c(0, 0), e = c(1, 1))
  im <- interval_meta(
    vplot(d) |>
      mark_rect(xmin = a, xmax = b, ymin = c, ymax = e) |>
      select_interval("b")
  )
  expect_identical(vapply(im$meta, `[[`, numeric(1), "ix"), c(1, 4))
  expect_identical(vapply(im$meta, `[[`, numeric(1), "ixh"), c(3, 5))
})
