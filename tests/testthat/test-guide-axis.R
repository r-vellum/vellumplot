# guide_axis() (F-17): staggered, rotated and thinned tick labels, minor ticks,
# a capped axis line, and guides(x = "none").

cars <- data.frame(model = rownames(mtcars), mpg = mtcars$mpg)
bars <- function() vplot(cars) |> mark_bar(x = model, y = mpg)

# The laid-out geometry (track sizes) of a plot, captured from the compile.
layout_of <- function(p) {
  lay <- NULL
  orig <- vellumplot:::.build_layout
  testthat::with_mocked_bindings(
    vellum::as_vellum_scene(p),
    .build_layout = function(...) {
      lay <<- orig(...)
      lay
    },
    .package = "vellumplot"
  )
  lay
}
# Height (mm) of the x tick-label row and width (mm) of the y label gutter.
x_row_mm <- function(p) {
  lay <- layout_of(p)
  vellum::vl_convert(lay$heights[lay$xlabels_row[1]], "mm")
}
y_col_mm <- function(p) {
  lay <- layout_of(p)
  vellum::vl_convert(lay$widths[lay$ylabels_col[1]], "mm")
}
# The tick labels the axis drawer places (label, anchor, rotation): the
# `.axis_text_grob()` calls positioned at a native break (measurement calls are
# not).
axis_text <- function(p, axis = "x") {
  calls <- list()
  orig <- vellumplot:::.axis_text_grob
  testthat::with_mocked_bindings(
    vellum::as_vellum_scene(p),
    .axis_text_grob = function(
      label,
      el,
      just,
      x = vellum::vl_unit(0.5, "npc"),
      y = vellum::vl_unit(0.5, "npc"),
      rot = 0,
      wrap_mm = NA_real_
    ) {
      along <- if (axis == "x") x else y
      if (grepl("native", format(along), fixed = TRUE)) {
        calls[[length(calls) + 1L]] <<- data.frame(
          label = label,
          pos = as.numeric(sub("native", "", format(along))),
          off = format(if (axis == "x") y else x),
          rot = rot
        )
      }
      orig(label, el, just, x = x, y = y, rot = rot, wrap_mm = wrap_mm)
    },
    .package = "vellumplot"
  )
  if (length(calls)) do.call(rbind, calls) else data.frame()
}

test_that("guide_axis() validates and records its options", {
  g <- guide_axis(n.dodge = 2, angle = 45, check.overlap = TRUE, cap = TRUE)
  expect_identical(g$kind, "axis")
  expect_identical(g$n.dodge, 2L)
  expect_identical(g$cap, "both")
  expect_identical(guide_axis(cap = FALSE)$cap, "none")
  expect_error(guide_axis(n.dodge = 0), "n.dodge")
  expect_error(guide_axis(angle = "a"), "angle")
  expect_error(guide_axis(cap = "middle"), "cap")
  expect_error(
    vplot(mtcars) |> scale_x_continuous(guide = guide_legend()),
    "guide"
  )
})

test_that("guides(x =) and scale_*(guide =) reach the trained scale", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    guides(x = guide_axis(n.dodge = 2, title = "Weight"))
  sc <- vellumplot:::.build_panels(p)$scales
  expect_identical(sc$x$axis_guide$n.dodge, 2L)
  expect_identical(sc$x$name, "Weight")
  q <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    scale_y_continuous(guide = guide_axis(minor.ticks = TRUE))
  expect_true(vellumplot:::.build_panels(q)$scales$y$axis_guide$minor.ticks)
  # a later scale declaration keeps a guide set with guides()
  r <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    guides(x = guide_axis(angle = 30)) |>
    scale_x_continuous(limits = c(0, 6))
  expect_identical(vellumplot:::.build_panels(r)$scales$x$axis_guide$angle, 30)
})

test_that("n.dodge staggers the labels and the gutter grows to fit", {
  short <- vplot(data.frame(k = letters[1:5], v = 1:5)) |>
    mark_bar(x = k, y = v)
  one <- x_row_mm(short)
  two <- x_row_mm(short |> guides(x = guide_axis(n.dodge = 2)))
  expect_gt(two, one * 1.4)
  tx <- axis_text(bars() |> guides(x = guide_axis(n.dodge = 2)))
  expect_identical(nrow(tx), 32L)
  # two distinct rows, alternating
  expect_length(unique(tx$off), 2L)
  expect_false(tx$off[1] == tx$off[2])
  expect_identical(tx$off[1], tx$off[3])
  # y dodging widens the left gutter
  pts <- vplot(cars) |> mark_point(y = model, x = mpg)
  expect_gt(
    y_col_mm(pts |> guides(y = guide_axis(n.dodge = 2))),
    y_col_mm(pts) * 1.4
  )
})

test_that("angle rotates the labels and the row fits the rotated extent", {
  flat <- x_row_mm(bars() |> guides(x = guide_axis(angle = 0)))
  slanted <- x_row_mm(bars() |> guides(x = guide_axis(angle = 45)))
  vertical <- x_row_mm(bars() |> guides(x = guide_axis(angle = 90)))
  expect_gt(slanted, flat)
  expect_gt(vertical, slanted)
  tx <- axis_text(bars() |> guides(x = guide_axis(angle = 90)))
  expect_true(all(tx$rot == 90))
})

test_that("check.overlap keeps the first, last and middle labels", {
  expect_equal(vellumplot:::.axis_label_priority(5), c(1, 5, 3, 2, 4))
  keep <- vellumplot:::.overlap_keep(1:5, rep(1.5, 5))
  expect_identical(keep, c(TRUE, FALSE, TRUE, FALSE, TRUE))
  tx <- axis_text(bars() |> guides(x = guide_axis(check.overlap = TRUE)))
  expect_lt(nrow(tx), 32L)
  expect_gt(nrow(tx), 2L)
  # the first and last labels always survive
  expect_true(all(c("AMC Javelin", "Volvo 142E") %in% tx$label))
  # y too
  ty <- axis_text(
    vplot(cars) |>
      mark_point(y = model, x = mpg) |>
      guides(y = guide_axis(check.overlap = TRUE)),
    "y"
  )
  expect_lt(nrow(ty), 32L)
})

test_that("minor ticks, a capped axis line and guides(x = 'none')", {
  base <- vplot(mtcars) |> mark_point(x = wt, y = mpg) |> theme_classic()
  segs <- function(p) {
    el <- vellum::scene_model(vellum::as_vellum_scene(p))$elements
    el[el$mark == "segment", ]
  }
  n0 <- nrow(segs(base))
  n1 <- nrow(segs(base |> guides(y = guide_axis(minor.ticks = TRUE))))
  expect_gt(n1, n0)
  # the capped x axis line spans the outermost breaks, not the panel
  line_w <- function(p) {
    s <- segs(p)
    s <- s[startsWith(s$panel, "axis-x-"), ]
    max(s$w)
  }
  expect_lt(line_w(base |> guides(x = guide_axis(cap = "both"))), line_w(base))
  none <- base |> guides(x = "none")
  expect_identical(nrow(axis_text(none)), 0L)
  expect_lt(x_row_mm(none), x_row_mm(base))
})

test_that("wrapped tick labels reserve the height of the tallest wrap", {
  lay <- layout_of(bars())
  skip_if_not(is.finite(lay$xwrap_mm))
  el <- vellumplot:::.resolve_theme(vellumplot:::.theme_default())[[
    "axis.text.x"
  ]]
  h <- function(l) {
    vellum::vl_convert(
      vellum::grobheight(vellumplot:::.axis_text_grob(
        l,
        el,
        c("centre", "top"),
        wrap_mm = lay$xwrap_mm
      )),
      "mm"
    )
  }
  tallest <- max(vapply(cars$model, h, numeric(1)))
  row_mm <- vellum::vl_convert(lay$heights[lay$xlabels_row[1]], "mm")
  expect_gte(row_mm, tallest)
})

test_that("guide_axis_logticks() draws long, mid and short log10 ticks", {
  d <- data.frame(x = c(1, 1000), y = 1:2)
  p <- vplot(d) |>
    mark_point(x = x, y = y) |>
    scale_x_continuous(trans = "log10", guide = guide_axis_logticks())
  sc <- vellumplot:::.build_panels(p)$scales$x
  expect_false(is.null(sc$axis_guide$logticks))
  t <- vellumplot:::.log_ticks(sc, sc$axis_guide$logticks)
  # every 1..9 x 10^k inside the (expanded) domain
  v <- as.vector(outer(1:9, 10^(-1:3)))
  expect_equal(10^t$pos, v[v >= 10^sc$domain[1] & v <= 10^sc$domain[2]])
  expect_equal(t$len[10^t$pos %in% c(1, 10, 100, 1000)], rep(2.25, 4))
  expect_equal(unique(t$len[round(10^t$pos) %in% c(5, 50, 500)]), 1.5)
  expect_equal(unique(t$len[round(10^t$pos) %in% c(2, 30, 700)]), 0.75)
  # pre-transformed data
  q <- vplot(data.frame(x = log10(c(1, 1000)), y = 1:2)) |>
    mark_point(x = x, y = y) |>
    guides(x = guide_axis_logticks(prescale.base = 10))
  sq <- vellumplot:::.build_panels(q)$scales$x
  tq <- vellumplot:::.log_ticks(sq, sq$axis_guide$logticks)
  expect_true(all(c(1, 10, 100, 1000) %in% round(10^tq$pos, 6)))
  expect_no_error(vellum::as_vellum_scene(p))
  expect_error(guide_axis_logticks(long = -1), "long")
  expect_error(guide_axis_logticks(prescale.base = 1), "prescale")
})
