# End tick labels that hang past the panel get room when the plot margin
# cannot hold them (VP-138).

mm <- function(u) vellum::vl_convert(u, "mm")

edge_plot <- function(margin) {
  vplot(mtcars, width = 4, height = 2.5) |>
    mark_point(x = wt, y = mpg) |>
    scale_x_continuous(limits = c(1, 6), breaks = 1:6, expand = c(0, 0)) |>
    scale_y_continuous(
      limits = c(10, 35),
      breaks = c(10, 20, 35),
      expand = c(0, 0)
    ) |>
    theme(plot.margin = margin, axis.title.y = element_blank())
}

lay_of <- function(p) {
  pl <- vellumplot:::.build_panels(p)
  rt <- vellumplot:::.resolve_theme(vellumplot:::.theme_of(p))
  vellumplot:::.build_layout(
    pl,
    list(),
    p@labels,
    rt,
    page_width = p@width,
    page_height = p@height
  )
}

test_that("a zero margin gets a spacer for the top and right end labels", {
  lay <- lay_of(edge_plot(0))
  rt <- vellumplot:::.resolve_theme(vellumplot:::.theme_default())
  half_h <- mm(vellum::grobheight(
    vellumplot:::.axis_text_grob(
      "35",
      rt[["axis.text.y"]],
      c("right", "centre")
    )
  )) /
    2
  half_w <- mm(vellum::grobwidth(
    vellumplot:::.axis_text_grob("6", rt[["axis.text.x"]], c("centre", "top"))
  )) /
    2
  # the panel moved down / in by one spacer track
  expect_identical(lay$panel_row, 2L)
  expect_equal(mm(lay$heights[1]), half_h, tolerance = 1e-6)
  n <- length(lay$widths)
  expect_equal(mm(lay$widths[n]), half_w, tolerance = 1e-6)
  # every other index followed the shift
  expect_identical(lay$xlabels_row, 3L)
  expect_identical(lay$nrow_total, length(lay$heights))
  expect_no_error(vellum::as_vellum_scene(edge_plot(0)))
})

test_that("the default margin absorbs the overhang: no spacer", {
  lay <- lay_of(edge_plot(5.5))
  plain <- lay_of(
    vplot(mtcars, width = 4, height = 2.5) |>
      mark_point(x = wt, y = mpg) |>
      theme(axis.title.y = element_blank())
  )
  expect_identical(lay$panel_row, plain$panel_row)
  expect_identical(length(lay$widths), length(plain$widths))
  expect_identical(length(lay$heights), length(plain$heights))
})

test_that("an end break inside the panel needs less room", {
  p <- vplot(mtcars, width = 4, height = 2.5) |>
    mark_point(x = wt, y = mpg) |>
    theme(plot.margin = 0, axis.title.y = element_blank())
  lay <- lay_of(p)
  # default expansion keeps the top break well clear of the edge
  expect_identical(lay$panel_row, 1L)
})

test_that(".shift_tracks() leaves NA and unused zero indices alone", {
  lay <- list(
    panel_row = c(2L, 4L),
    wrapstrip_row = c(0L, 0L),
    tag_row = NA_integer_,
    panel_col = 3L,
    legend_cols = c(left = NA_integer_, right = 5L)
  )
  out <- vellumplot:::.shift_tracks(lay, 1L, 1L)
  expect_identical(out$panel_row, c(3L, 5L))
  expect_identical(out$wrapstrip_row, c(0L, 0L))
  expect_identical(out$tag_row, NA_integer_)
  expect_identical(out$panel_col, 4L)
  expect_identical(out$legend_cols, c(left = NA_integer_, right = 6L))
})

test_that("coord_trans places the end breaks where the warped axis draws them", {
  d <- data.frame(x = c(1, 10, 100, 1000), y = 1:4)
  p <- vplot(d, width = 5, height = 3) |>
    mark_point(x = x, y = y) |>
    scale_x_continuous(
      breaks = c(1, 10, 100, 900),
      labels = c("1", "10", "100", "900000"),
      expand = c(0, 0)
    ) |>
    theme(plot.margin = 0)
  right_spacer <- function(q) {
    pl <- vellumplot:::.build_panels(q)
    rt <- vellumplot:::.resolve_theme(vellumplot:::.theme_of(q))
    lay <- vellumplot:::.build_layout(
      pl,
      list(),
      q@labels,
      rt,
      coord = vellumplot:::.coord_of(q),
      page_width = q@width,
      page_height = q@height
    )
    n <- length(lay$widths)
    if (n > max(lay$panel_col)) mm(lay$widths[n]) else 0
  }
  # untransformed, 900 sits a tenth of the panel in from the edge: no spacer
  expect_identical(right_spacer(p), 0)
  # on a log10 axis it sits almost at the edge, so "900000" needs room
  expect_gt(right_spacer(coord_trans(p, x = "log10")), 0)
})

# VP-140: opposite sides solved together, budgets refreshed, composition cells.

conv_mm <- function(u) {
  vapply(
    seq_along(u),
    function(i) tryCatch(mm(u[i]), error = function(e) NA_real_),
    numeric(1)
  )
}

test_that("top and bottom spacers are solved together: no residual clip", {
  p <- vplot(mtcars, width = 4, height = 1.6) |>
    mark_point(x = wt, y = mpg) |>
    scale_y_continuous(
      limits = c(10, 35),
      breaks = c(10, 20, 30, 35),
      expand = c(0.02, 0)
    ) |>
    theme(
      plot.margin = 0,
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title = element_blank(),
      axis.text.y = element_text(size = 30)
    )
  pl <- vellumplot:::.build_panels(p)
  rt <- vellumplot:::.resolve_theme(vellumplot:::.theme_of(p))
  lay <- lay_of(p)
  h <- conv_mm(lay$heights)
  n <- length(h)
  expect_identical(lay$panel_row, 2L)
  expect_gt(h[1], 0)
  expect_gt(h[n], 0)
  panel <- p@height * 25.4 - sum(h, na.rm = TRUE)
  ex <- vellumplot:::.end_tick_extent(
    pl$scales$y,
    rt[["axis.text.y"]],
    0,
    c("right", "centre"),
    "y"
  )
  # each end label fits in its spacer plus the end gap of the shrunk panel
  expect_lte(ex$e[1] - (h[n] + ex$g[1] * panel), 1e-3)
  expect_lte(ex$e[2] - (h[1] + ex$g[2] * panel), 1e-3)
})

test_that(".solve_overhang() grows monotonically and stops when settled", {
  calls <- 0L
  s <- vellumplot:::.solve_overhang(function(s) {
    calls <<- calls + 1L
    # each side needs 2 mm plus a tenth of the opposite side's spacer
    c(t = 2 + 0.1 * s[["b"]], r = 0, b = 2 + 0.1 * s[["t"]], l = 0)
  })
  # iterated until no side grows by more than 1e-3 mm
  expect_equal(s[["t"]], 2 / 0.9, tolerance = 1e-3)
  expect_lte(calls, 6L)
  one <- 0L
  vellumplot:::.solve_overhang(function(s) {
    one <<- one + 1L
    c(t = 1, r = 0, b = 0, l = 0)
  })
  expect_identical(one, 2L)
})

test_that("a left / right spacer narrows the x wrap budget", {
  p <- vplot(data.frame(x = 1:4, y = 1:4), width = 3.2, height = 2.5) |>
    mark_point(x = x, y = y) |>
    scale_x_continuous(
      breaks = 1:4,
      labels = c(
        "A long label one",
        "Another long label",
        "Third long label here",
        "End label WWWWWW"
      ),
      expand = c(0.02, 0)
    ) |>
    theme(
      plot.margin = 0,
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title = element_blank()
    )
  lay <- lay_of(p)
  w <- conv_mm(lay$widths)
  spacers <- w[1] + w[length(w)]
  expect_gt(spacers, 0)
  expect_equal(
    lay$xwrap_mm,
    (p@width * 25.4 - spacers) / 4,
    tolerance = 1e-6
  )
})

test_that("an aligned composition gives edge cells their end-tick room", {
  edge <- function(q) {
    q |>
      scale_y_continuous(
        limits = c(10, 35),
        breaks = c(10, 20, 35),
        expand = c(0, 0)
      )
  }
  a <- vplot(mtcars) |> mark_point(x = wt, y = mpg) |> edge()
  b <- vplot(mtcars) |>
    mark_point(x = hp, y = mpg) |>
    scale_x_continuous(
      limits = c(50, 350),
      breaks = c(50, 200, 350),
      expand = c(0, 0)
    )
  panels <- function(x) {
    p <- vellum::scene_model(vellum::as_vellum_scene(x))$panels
    p[grepl("panel-", p$name), , drop = FALSE]
  }
  tight <- hconcat(a, b) |> theme(plot.margin = 0)
  pc <- panels(tight)
  # the top y label and the last x label get room at the page edge
  expect_gt(min(pc$py0), 1)
  expect_gt(tight@width * tight@dpi - max(pc$px1), 1)
  # the default margin holds them: the layout is unchanged
  loose <- panels(hconcat(a, b))
  plo <- panels(hconcat(
    vplot(mtcars) |> mark_point(x = wt, y = mpg),
    vplot(mtcars) |> mark_point(x = hp, y = mpg)
  ))
  expect_equal(min(loose$py0), min(plo$py0), tolerance = 1e-6)
})

test_that("an uncomputable need keeps the spacers found so far", {
  s <- vellumplot:::.solve_overhang(function(s) {
    if (s[["t"]] > 0) {
      c(t = NA, r = NA, b = NA, l = NA)
    } else {
      c(t = 3, r = 0, b = 1, l = 0)
    }
  })
  expect_identical(s, c(t = 3, r = 0, b = 1, l = 0))
})

test_that("spacers that eat the page never error (VP-140 review)", {
  p <- vplot(mtcars, width = 3, height = 0.75) |>
    mark_point(x = wt, y = mpg) |>
    theme(plot.margin = 0, axis.text = element_text(size = 30))
  expect_no_error(vellum::as_vellum_scene(p))
  # a compact sweep of small pages, big text and facets at zero margin
  for (fac in c("none", "wrap", "grid")) {
    for (h in c(0.6, 0.9)) {
      for (sz in c(30, 45)) {
        q <- vplot(mtcars, width = 1.5, height = h) |>
          mark_point(x = wt, y = mpg) |>
          theme(plot.margin = 0, axis.text = element_text(size = sz))
        q <- switch(
          fac,
          none = q,
          wrap = facet_wrap(q, ~am),
          grid = facet_grid(q, am ~ vs)
        )
        expect_no_error(vellum::as_vellum_scene(q))
      }
    }
  }
  tiny <- hconcat(
    vplot(mtcars) |> mark_point(x = wt, y = mpg),
    vplot(mtcars) |> mark_point(x = hp, y = mpg),
    width = 2,
    height = 0.6
  ) |>
    theme(plot.margin = 0, axis.text = element_text(size = 40))
  expect_no_error(vellum::as_vellum_scene(tiny))
})

test_that("end breaks well inside the panel converge with no residual clip", {
  # end breaks a third of the way in (g ~ 0.36): each side's spacer shrinks
  # the panel and so the other side's end gap a lot; a fixed 3-pass cap left
  # ~0.2 mm clipped here
  p <- vplot(mtcars, width = 4, height = 1.4) |>
    mark_point(x = wt, y = mpg) |>
    scale_y_continuous(
      limits = c(10, 35),
      breaks = c(19, 26),
      expand = c(0, 0)
    ) |>
    theme(
      plot.margin = 0,
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title = element_blank(),
      axis.text.y = element_text(size = 80)
    )
  pl <- vellumplot:::.build_panels(p)
  rt <- vellumplot:::.resolve_theme(vellumplot:::.theme_of(p))
  lay <- lay_of(p)
  h <- conv_mm(lay$heights)
  n <- length(h)
  # both spacers present and a real panel left (text widths vary by platform)
  expect_identical(lay$panel_row, 2L)
  panel <- p@height * 25.4 - sum(h, na.rm = TRUE)
  expect_gt(panel, 3)
  ex <- vellumplot:::.end_tick_extent(
    pl$scales$y,
    rt[["axis.text.y"]],
    0,
    c("right", "centre"),
    "y"
  )
  expect_gt(ex$g[1], 0.3)
  expect_lte(ex$e[1] - (h[n] + ex$g[1] * panel), 2e-3)
  expect_lte(ex$e[2] - (h[1] + ex$g[2] * panel), 2e-3)
})

# VP-146: a spacer that narrowed the wrap budget shrinks back to what the
# wrapped end labels need.

test_that(".solve_overhang() shrinks to the need when it falls as it grows", {
  # a wrap-like need: the bigger the spacer, the narrower the labels
  need <- function(s) c(t = 0, r = 10 - 0.5 * s[["r"]], b = 0, l = 0)
  s <- vellumplot:::.solve_overhang(need)
  # the fixed point s = 10 - s / 2 (to 0.05 mm, from above), not the first
  # pass's 10
  expect_gte(s[["r"]], 20 / 3 - 1e-3)
  expect_lte(s[["r"]], 20 / 3 + 0.05)
  expect_lte(need(s)[["r"]], s[["r"]] + 1e-3)
  # two sides sharing one budget: the tight side grows a little so the other
  # can shrink a lot (each needs 8 - (l + r) / 4, and the first pass leaves
  # r far above it)
  calls <- 0L
  shared <- function(s) {
    calls <<- calls + 1L
    lr <- s[["l"]] + s[["r"]]
    c(
      t = 0,
      r = if (lr == 0) 9 else 8 - lr / 4,
      b = 0,
      l = if (lr == 0) 4 else 8 - lr / 4
    )
  }
  s2 <- vellumplot:::.solve_overhang(shared)
  expect_equal(s2[["l"]], 16 / 3, tolerance = 0.05)
  expect_equal(s2[["r"]], 16 / 3, tolerance = 0.05)
  expect_true(all(shared(s2) <= s2 + 1e-3))
  expect_lte(calls, 12L)
  # a need the shrunk spacer covers outright is taken as it is
  step <- function(s) c(t = 0, r = if (s[["r"]] > 0) 3 else 5, b = 0, l = 0)
  expect_identical(vellumplot:::.solve_overhang(step)[["r"]], 3)
})

test_that("wrapped end labels get the spacer they need, no more (VP-146)", {
  p <- vplot(data.frame(x = 1:4, y = 1:4), width = 3.2, height = 2.5) |>
    mark_point(x = x, y = y) |>
    scale_x_continuous(
      breaks = 1:4,
      labels = c(
        "A long label one",
        "Another long label",
        "Third long label here",
        "End label WWWWWW"
      ),
      expand = c(0.02, 0)
    ) |>
    theme(
      plot.margin = 0,
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title = element_blank()
    )
  lay <- lay_of(p)
  w <- conv_mm(lay$widths)
  r <- w[length(w)]
  panel <- p@width * 25.4 - w[1] - r
  pl <- vellumplot:::.build_panels(p)
  rt <- vellumplot:::.resolve_theme(vellumplot:::.theme_of(p))
  ex <- vellumplot:::.end_tick_extent(
    pl$scales$x,
    rt[["axis.text.x"]],
    0,
    c("centre", "top"),
    "x",
    wrap_mm = lay$xwrap_mm
  )
  # the last label, wrapped at the final budget, fits its spacer plus the end
  # gap exactly: not clipped, and not ~2 mm over (as the first pass left it)
  expect_equal(ex$e[2], r + ex$g[2] * panel, tolerance = 1e-2)
})

test_that("long end labels at both ends share the wrap budget (VP-146 review)", {
  labs <- c(
    "A long label one",
    "Another long label",
    "Third long label here",
    "End label WWWWWW"
  )
  p <- vplot(data.frame(x = 1:4, y = 1:4), width = 5.1, height = 2.5) |>
    mark_point(x = x, y = y) |>
    scale_x_continuous(breaks = 1:4, labels = labs, expand = c(0, 0)) |>
    theme(plot.margin = 0)
  lay <- lay_of(p)
  w <- conv_mm(lay$widths)
  pc <- lay$panel_col[1]
  # the room each end label has outside the panel: every track on its side
  l <- sum(w[seq_len(pc - 1L)], na.rm = TRUE)
  r <- sum(w[-seq_len(pc)], na.rm = TRUE)
  pl <- vellumplot:::.build_panels(p)
  rt <- vellumplot:::.resolve_theme(vellumplot:::.theme_of(p))
  panel <- p@width * 25.4 - sum(w, na.rm = TRUE)
  ex <- vellumplot:::.end_tick_extent(
    pl$scales$x,
    rt[["axis.text.x"]],
    0,
    c("centre", "top"),
    "x",
    wrap_mm = lay$xwrap_mm
  )
  # both end labels fit, and neither side is more than 0.1 mm over (the
  # grow-then-shrink-down search left r ~2.3 mm over here)
  slack <- c(l + ex$g[1] * panel - ex$e[1], r + ex$g[2] * panel - ex$e[2])
  expect_true(all(slack >= -1e-2))
  expect_true(all(slack <= 0.1))
})

# VP-153: a wrapped end label measures as its wrap box, but its lines are
# centred in the box, so it reaches half its widest line, not half the box.
test_that("a wrapped end label reaches half its widest line (VP-153)", {
  rt <- vellumplot:::.resolve_theme(vellumplot:::.theme_default())
  el <- rt[["axis.text.x"]]
  lab <- "last break here wrapped"
  wrap <- 25
  lw <- vellumplot:::.wrapped_line_mm(lab, el, wrap)
  # greedy wrap: "last break here" / "wrapped"
  expect_equal(
    lw,
    vellumplot:::.el_strwidth("last break here", el, features = vellumplot:::.TNUM)
  )
  expect_lt(lw, wrap)
  # Not wrapped, or not a plain string: nothing to measure.
  expect_true(is.na(vellumplot:::.wrapped_line_mm(lab, el, NA_real_)))
  expect_true(is.na(vellumplot:::.wrapped_line_mm(NA_character_, el, wrap)))
  # A word wider than the box is broken to fit it (VP-174).
  long <- "Antidisestablishmentarianism x"
  expect_lte(vellumplot:::.wrapped_line_mm(long, el, 15), 15)
  sc <- list(breaks = c(1, 10), labels = c("first", lab), domain = c(1, 10))
  ex <- vellumplot:::.end_tick_extent(
    sc,
    el,
    0,
    c("centre", "top"),
    "x",
    wrap_mm = wrap
  )
  expect_equal(ex$e[2], lw / 2)
  # A word wider than the box is broken to fit it, so it reaches no further
  # than half the box (VP-174).
  big <- "Supercalifragilisticexpialidocious"
  sc2 <- list(breaks = c(1, 10), labels = c("first", big), domain = c(1, 10))
  ex2 <- vellumplot:::.end_tick_extent(
    sc2,
    el,
    0,
    c("centre", "top"),
    "x",
    wrap_mm = 10
  )
  expect_lte(ex2$e[2], 10 / 2)
  # Unwrapped, the reach is half the label as before.
  ex0 <- vellumplot:::.end_tick_extent(sc, el, 0, c("centre", "top"), "x")
  expect_equal(
    ex0$e[2],
    vellumplot:::.el_strwidth(lab, el, features = vellumplot:::.TNUM) / 2,
    tolerance = 1e-3
  )
})

# VP-174: a word wider than its wrap box ran into the neighbouring tick's
# label. It is broken across lines (at its own hyphen, else between letters
# with a hyphen added) so every piece fits the box.
test_that("an overlong word in a wrapped tick label is hyphenated (VP-174)", {
  rt <- vellumplot:::.resolve_theme(vellumplot:::.theme_default())
  el <- rt[["axis.text.x"]]
  w <- function(x) vellumplot:::.el_strwidth(x, el, features = vellumplot:::.TNUM)
  hy <- function(x, mm) vellumplot:::.hyphenate_label(x, el, mm)
  big <- "Antidisestablishment of the old regime"
  out <- hy(big, 12)
  pieces <- strsplit(out, "\n")[[1]]
  expect_gt(length(pieces), 1L)
  expect_true(endsWith(pieces[1], "-"))
  # the pieces put back together are the word
  first <- strsplit(out, " ")[[1]][1]
  expect_identical(gsub("-\n", "", first), "Antidisestablishment")
  expect_true(all(w(strsplit(first, "\n")[[1]]) <= 12))
  # the rest of the label is left to the word wrap
  expect_match(out, " of the old regime$")
  # An existing hyphen is the preferred break.
  hw <- hy("well-established", w("established") + 1)
  expect_identical(hw, "well-\nestablished")
  # Labels whose words fit, numbers and codes are unchanged.
  expect_identical(hy("North region sales", 12), "North region sales")
  expect_identical(hy("1000000000", 3), "1000000000")
  expect_identical(hy("ABC123XYZ789", 3), "ABC123XYZ789")
  expect_identical(hy(NA_character_, 3), NA_character_)
  # Rich labels are left alone.
  m <- md("**Supercalifragilistic**")
  expect_identical(hy(m, 3), m)
})

test_that("wrapped tick labels with an overlong word no longer overlap (VP-174)", {
  d <- data.frame(
    k = c("Antidisestablishment of the old regime", "b", "c", "d", "e", "f"),
    v = 1:6
  )
  p <- vplot(d, width = 4, height = 3) |> mark_bar(x = k, y = v)
  s <- vellum::as_scene_spec(vellum::as_vellum_scene(p))
  labs <- character()
  walk <- function(node) {
    if (identical(node$class, "grob_text")) {
      v <- node$label
      if (is.list(v) && !is.null(v$v)) v <- v$v
      labs <<- c(labs, unlist(v))
    }
    for (ch in node) if (is.list(ch)) walk(ch)
  }
  walk(s$root)
  expect_true(any(grepl("^Antidis[a-z]*-\n", labs)))
  # A label that fits draws as before.
  d2 <- data.frame(k = c("North region sales", "South region", "East"), v = 1:3)
  p2 <- vplot(d2, width = 3, height = 3) |> mark_bar(x = k, y = v)
  s2 <- vellum::as_scene_spec(vellum::as_vellum_scene(p2))
  labs <- character()
  walk(s2$root)
  expect_false(any(grepl("-\n", labs)))
})
