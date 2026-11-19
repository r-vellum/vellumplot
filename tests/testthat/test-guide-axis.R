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

test_that("date, datetime, time and binned position scales take guide =", {
  days <- data.frame(day = as.Date("2020-01-01") + 0:364, y = sin(1:365 / 30))
  base <- vplot(days) |> mark_line(x = day, y = y)
  p <- base |>
    scale_x_date(
      date_breaks = "1 month",
      date_labels = "%B",
      guide = guide_axis(n.dodge = 2, title = "Day")
    )
  sc <- vellumplot:::.build_panels(p)$scales
  expect_identical(sc$x$axis_guide$n.dodge, 2L)
  expect_identical(sc$x$name, "Day")
  tx <- axis_text(p)
  expect_length(unique(tx$off), 2L)
  expect_gt(x_row_mm(p), x_row_mm(base) * 1.4)

  stamps <- data.frame(
    t = as.POSIXct("2020-01-01", tz = "UTC") + 3600 * 0:47,
    y = 1:48
  )
  q <- vplot(stamps) |>
    mark_point(x = t, y = y) |>
    scale_x_datetime(guide = guide_axis(angle = 90))
  expect_true(all(axis_text(q)$rot == 90))
  r <- vplot(stamps) |>
    mark_point(y = t, x = y) |>
    scale_y_datetime(guide = "none")
  expect_identical(nrow(axis_text(r, "y")), 0L)

  tod <- data.frame(t = as.difftime(seq(0, 86400, by = 3600), units = "secs"))
  tod$y <- seq_along(tod$t)
  s <- vplot(tod) |>
    mark_point(x = t, y = y) |>
    scale_x_time(guide = guide_axis(angle = 45))
  expect_identical(vellumplot:::.build_panels(s)$scales$x$axis_guide$angle, 45)

  b <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    scale_x_binned(n = 6, guide = guide_axis(angle = 90)) |>
    scale_y_binned(guide = guide_axis(minor.ticks = TRUE))
  sb <- vellumplot:::.build_panels(b)$scales
  expect_identical(sb$x$axis_guide$angle, 90)
  expect_true(sb$y$axis_guide$minor.ticks)
  expect_true(all(axis_text(b)$rot == 90))
  expect_error(scale_x_date(base, guide = guide_legend()), "guide")
  expect_error(scale_y_binned(base, guide = "legend"), "guide")
})

test_that("check.overlap thins free facet scales, fixed aspect and marginals", {
  labs <- sprintf("category %02d", 1:30)
  df <- data.frame(
    g = rep(c("a", "b"), each = 30),
    lab = rep(labs, 2),
    y = rep(1:30, 2)
  )
  # free scales: each panel's axis is thinned on its own
  free <- vplot(df) |>
    mark_point(x = lab, y = y) |>
    facet_wrap(~g, scales = "free_x") |>
    guides(x = guide_axis(check.overlap = TRUE))
  lay <- layout_of(free)
  expect_null(lay$x_keep)
  expect_length(lay$x_keep_panels, 2L)
  for (k in lay$x_keep_panels) {
    expect_lt(sum(k), 30L)
    expect_true(k[1] && k[30])
  }
  tx <- axis_text(free)
  expect_lt(nrow(tx), 60L)
  expect_gt(nrow(tx), 4L)
  # free y too
  fy <- vplot(df) |>
    mark_point(y = lab, x = y) |>
    facet_wrap(~g, scales = "free_y") |>
    guides(y = guide_axis(check.overlap = TRUE)) |>
    theme(axis.text.y = element_text(size = 30))
  expect_true(all(vapply(layout_of(fy)$y_keep_panels, sum, 1) < 30))

  # fixed aspect: the panel's mm width comes from the aspect-locked track
  one <- df[df$g == "a", ]
  fixed <- vplot(one) |>
    mark_point(x = lab, y = y) |>
    coord_fixed() |>
    guides(x = guide_axis(check.overlap = TRUE))
  k <- layout_of(fixed)$x_keep
  expect_length(k, 30L)
  expect_lt(sum(k), 30L)
  # a square panel is narrower than the free-aspect one, so it keeps fewer
  wide <- vplot(one) |>
    mark_point(x = lab, y = y) |>
    guides(x = guide_axis(check.overlap = TRUE))
  expect_lt(sum(k), sum(layout_of(wide)$x_keep))

  # marginal plots
  marg <- vplot(one) |>
    mark_point(x = y, y = y) |>
    add_marginal(sides = "tr") |>
    scale_x_continuous(breaks = 1:30, labels = labs) |>
    guides(x = guide_axis(check.overlap = TRUE))
  km <- layout_of(marg)$x_keep
  expect_length(km, 30L)
  expect_lt(sum(km), 30L)
})

test_that(".panel_mm() shares the free space by null weight and respects aspect", {
  u <- list(vellum::vl_unit(10, "mm"), vellum::vl_unit(1, "null"))
  rt <- list(plot.margin = 0)
  mm <- vellumplot:::.panel_mm(u, u, 110 / 25.4, 60 / 25.4, rt)
  expect_equal(unname(mm), c(100, 50))
  mm <- vellumplot:::.panel_mm(u, u, 110 / 25.4, 60 / 25.4, rt, respect = TRUE)
  expect_equal(unname(mm), c(50, 50))
  w <- list(vellum::vl_unit(1, "null"), vellum::vl_unit(3, "null"))
  expect_equal(
    unname(vellumplot:::.panel_mm(w, u, 100 / 25.4, 60 / 25.4, rt)[["w"]]),
    25
  )
  expect_identical(vellumplot:::.null_weight(vellum::vl_unit(2.5, "null")), 2.5)
  expect_identical(vellumplot:::.null_weight(vellum::vl_unit(2, "mm")), 0)
})
