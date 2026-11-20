# annotate() + mark_segment.

base <- function() vplot(mtcars) |> mark_point(x = wt, y = mpg)
lastlayer <- function(p) p@layers[[length(p@layers)]]

test_that("each geom builds an own-data layer with the right mark", {
  p <- base() |> annotate("text", x = 4, y = 30, label = "a")
  expect_identical(lastlayer(p)@mark, "text")
  expect_false(is.null(lastlayer(p)@data))

  expect_identical(
    lastlayer(base() |> annotate("point", x = 1, y = 2))@mark,
    "point"
  )
  expect_identical(
    lastlayer(
      base() |> annotate("segment", x = 1, y = 1, xend = 2, yend = 2)
    )@mark,
    "segment"
  )
  expect_identical(
    lastlayer(
      base() |> annotate("rect", xmin = 1, xmax = 2, ymin = 1, ymax = 2)
    )@mark,
    "rect"
  )
})

test_that("annotate errors on unknown geom and missing aesthetics", {
  expect_error(base() |> annotate("wobble", x = 1, y = 1))
  expect_error(base() |> annotate("text", x = 1, y = 1), "label") # no label
  expect_error(base() |> annotate("segment", x = 1, y = 1), "xend") # no xend/yend
})

test_that("annotate geoms render (and flipped)", {
  builds <- list(
    function(p) annotate(p, "text", x = 4, y = 30, label = "n"),
    function(p) annotate(p, "point", x = 2, y = 15, color = "red"),
    function(p) annotate(p, "segment", x = 2, y = 30, xend = 5, yend = 12),
    function(p) {
      annotate(p, "rect", xmin = 3, xmax = 4, ymin = 15, ymax = 20, alpha = 0.3)
    }
  )
  for (b in builds) {
    f <- local_tempfile(fileext = ".png")
    render_plot(b(base()), f)
    expect_gt(file.info(f)$size, 0)
    f2 <- local_tempfile(fileext = ".png")
    render_plot(b(base()) |> coord_flip(), f2)
    expect_gt(file.info(f2)$size, 0)
  }
})

test_that(".bound_native resolves infinite bounds to the panel edge (#29)", {
  sc <- list(domain = c(0, 10), map = function(v) v * 2)
  # -Inf/Inf -> low/high domain edge; finite bounds map normally (never NaN/Inf).
  expect_identical(
    vellumplot:::.bound_native(c(-Inf, 3, Inf), sc),
    c(0, 6, 10)
  )
  # A reversed (decreasing) domain still edges to min()/max(), not domain[1].
  scr <- list(domain = c(10, 0), map = function(v) v)
  b <- vellumplot:::.bound_native(c(-Inf, Inf), scr)
  expect_true(all(is.finite(b)))
  expect_identical(b, c(0, 10))
})

test_that("annotate('rect') with infinite bounds renders a full-panel band (#29)", {
  # Previously the -Inf/Inf rect collapsed to x = NaN, width = Inf and was
  # dropped silently. Both a fully-infinite and a single-sided rect must render.
  builds <- list(
    function(p) {
      annotate(
        p,
        "rect",
        xmin = -Inf,
        xmax = Inf,
        ymin = 25,
        ymax = 30,
        alpha = 0.4
      )
    },
    function(p) {
      annotate(
        p,
        "rect",
        xmin = -Inf,
        xmax = 5,
        ymin = 10,
        ymax = 20,
        alpha = 0.4
      )
    }
  )
  for (b in builds) {
    f <- local_tempfile(fileext = ".png")
    render_plot(b(base()), f)
    expect_gt(file.info(f)$size, 0)
  }
})

test_that("an annotation appears on every facet panel", {
  p <- base() |>
    facet_wrap(~cyl) |>
    annotate("text", x = 4, y = 30, label = "*")
  built <- vellumplot:::.build_panels(p)
  ns <- vapply(built$panels, function(pp) pp$resolved[[2]]$n, integer(1))
  expect_true(all(ns == 1L))
})

# mark_segment standalone

test_that("mark_segment draws x/y -> xend/yend", {
  d <- data.frame(x = 1:3, y = 1:3, xend = 4:6, yend = 3:1)
  expect_identical(
    (vplot(d) |> mark_segment(x = x, y = y, xend = xend, yend = yend))@layers[[
      1
    ]]@mark,
    "segment"
  )
  f <- local_tempfile(fileext = ".png")
  render_plot(
    vplot(d) |> mark_segment(x = x, y = y, xend = xend, yend = yend),
    f
  )
  expect_gt(file.info(f)$size, 0)
})

# --- grob / sparkline annotations -------------------------------------------

test_that('annotate("grob") adds a grob layer carrying the grob + box spec', {
  g <- vellum::circle_grob(r = 0.4)
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    annotate(
      "grob",
      x = 3,
      y = 20,
      grob = g,
      width = 8,
      height = 8,
      halign = "left"
    )
  L <- p@layers[[length(p@layers)]]
  expect_identical(L@mark, "grob")
  expect_identical(L@stat_params$grob, g)
  expect_equal(L@stat_params$width, 8)
  expect_identical(L@stat_params$halign, "left")
})

test_that('annotate("sparkline") builds and places a vsparkline', {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    annotate("sparkline", x = 4, y = 30, values = 1:20, type = "bar")
  L <- p@layers[[length(p@layers)]]
  expect_identical(L@mark, "grob")
  expect_s3_class(L@stat_params$grob, "vellumplot::PlotSpec")
  expect_identical(
    L@stat_params$grob@layers[[1]]@stat_params$type,
    "bar"
  )
})

test_that("the grob/sparkline annotations validate their payload", {
  base <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
  expect_error(annotate(base, "grob", x = 1, y = 1), "needs a")
  expect_error(annotate(base, "sparkline", x = 1, y = 1), "needs")
})

test_that("grob and sparkline annotations render", {
  base <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
  plots <- list(
    base |>
      annotate("grob", x = 3, y = 20, grob = vellum::circle_grob(r = 0.4)),
    base |> annotate("sparkline", x = 4, y = 30, values = cumsum(rnorm(20))),
    # vectorised: a glyph at several points
    base |>
      annotate(
        "grob",
        x = c(2, 4),
        y = c(15, 30),
        grob = vellum::circle_grob(r = 0.3)
      )
  )
  for (p in plots) {
    f <- local_tempfile(fileext = ".png")
    expect_no_error(render_plot(p, f))
    expect_gt(file.info(f)$size, 0)
  }
})

test_that("a nested plot does not leak its context into the outer layers (VP-12)", {
  # An inset sub-plot is drawn through `.draw_plot()`, which sets the interaction
  # context for itself. Panel 1's inset is drawn before panel 2's points, which
  # must keep the outer plot's context: the outer plot filters on "br", so every
  # point stays keyed and tagged `filt`.
  df <- data.frame(x = 1:6, y = c(2, 4, 3, 5, 1, 2), g = rep(c("a", "b"), 3))
  p <- vplot(df) |>
    mark_point(x = x, y = y) |>
    annotate("sparkline", x = 2, y = 3, values = 1:10) |>
    facet_wrap(~g) |>
    add_selection(select_interval("br", on = "x")) |>
    filter_by("br")
  scene <- vellum::as_vellum_scene(p)
  m <- vellum::scene_model(scene)
  keyed <- m$elements[!is.na(m$elements$key), , drop = FALSE]
  expect_equal(nrow(keyed), nrow(df))
  expect_setequal(keyed$panel, c("panel-1-1", "panel-1-2"))
  has_filt <- vapply(
    keyed$meta,
    function(mm) "br" %in% mm[["filt"]],
    logical(1)
  )
  expect_true(all(has_filt))
  # the outer point layer's provenance names the outer panels
  prov <- attr(scene, "vellumplot_provenance")
  pts <- Filter(function(e) startsWith(e$id, "layer-1-point"), prov)
  expect_setequal(
    vapply(pts, function(e) e$panel, character(1)),
    c("panel-1-1", "panel-1-2")
  )
})

test_that("annotations get no default keys from a plot-level selection (VW-24)", {
  df <- data.frame(x = 1:3, y = 1:3, g = c("a", "b", "a"))
  p <- vplot(df) |>
    mark_point(x = x, y = y, color = condition("pick", g, "grey80")) |>
    select_point("pick") |>
    annotate("text", x = 2, y = 3, label = "note") |>
    annotate("rect", xmin = 1, xmax = 2, ymin = 1, ymax = 2)
  expect_identical(
    vapply(p@layers, function(l) l@annotation, logical(1)),
    c(FALSE, TRUE, TRUE)
  )
  sc <- as_vellum_scene(p)
  ids <- vellum::scene_model(sc)$elements$key
  ids <- ids[!is.na(ids)]
  expect_length(ids, 3L)
})

test_that("an annotation with its own data_id is still keyed", {
  p <- vplot(data.frame(x = 1:2, y = 1:2)) |>
    mark_point(x = x, y = y) |>
    select_point("pick") |>
    annotate("text", x = 1, y = 2, label = "note", data_id = "n1")
  ids <- vellum::scene_model(as_vellum_scene(p))$elements$key
  expect_true("n1" %in% ids)
})
