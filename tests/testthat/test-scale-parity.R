# ggplot2 parity for scales and legends (VP-11, F-10).

panels <- function(p) .build_panels(p)

# --- VP-11 ---------------------------------------------------------------

test_that("coord_cartesian(xlim) zooms a discrete axis in level positions", {
  d <- data.frame(g = letters[1:5], v = 1:5)
  p <- vplot(d) |>
    mark_bar(x = g, y = v, stat = "identity") |>
    coord_cartesian(xlim = c(2, 4))
  x <- panels(p)$scales$x
  expect_equal(x$domain, c(1.5, 4.5))
  expect_identical(x$labels, c("b", "c", "d"))
  expect_identical(x$levels, letters[1:5])
  expect_no_error(vellum::as_vellum_scene(p))
})

test_that("a discrete colour scale on a numeric variable errors", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, color = hp) |>
    scale_color_manual(values = c("red", "blue"))
  expect_error(panels(p), "Continuous values supplied to a discrete")
  p2 <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, color = factor(cyl)) |>
    scale_color_manual(values = c("red", "blue", "green"))
  expect_no_error(panels(p2))
})

test_that("infinite positions sit on the panel edge", {
  d <- data.frame(x = c(1, 2, 3, Inf, -Inf), y = c(1, 2, 3, 2, Inf))
  sc <- panels(vplot(d) |> mark_point(x = x, y = y))$scales
  dom <- sc$x$domain
  expect_equal(sc$x$map(c(Inf, -Inf)), c(max(dom), min(dom)))
  expect_equal(sc$y$map(Inf), max(sc$y$domain))
  # the data range ignores them
  expect_equal(sc$x$data_range, c(1, 3))
  # a reversed axis mirrors them: Inf is on the left, where the (descending)
  # domain starts, as under ggplot2's scale_x_reverse()
  rv <- panels(
    vplot(d) |>
      mark_point(x = x, y = y) |>
      scale_x_continuous(trans = "reverse")
  )$scales$x
  expect_equal(rv$map(Inf), rv$domain[1])
  # a value a transform cannot show (0 on log10) is still dropped, not moved
  lg <- panels(
    vplot(data.frame(x = c(0, 1, 10), y = 1:3)) |>
      mark_point(x = x, y = y) |>
      scale_x_continuous(trans = "log10")
  )$scales$x
  expect_false(is.finite(lg$map(0)))
})

test_that("an all-missing continuous aesthetic draws no legend", {
  d <- data.frame(x = 1:3, y = 1:3, z = NA_real_)
  # (no row has a size, so none draws, with a warning; VR-38)
  b <- suppressWarnings(
    panels(vplot(d) |> mark_point(x = x, y = y, color = z, size = z)),
    classes = "vellumplot_missing_rows"
  )
  expect_true(b$scales$color$no_guide)
  expect_true(b$scales$size$no_guide)
  expect_length(.legend_guides(b$scales), 0L)
  # with limits there is something to show
  b2 <- panels(
    vplot(d) |>
      mark_point(x = x, y = y, color = z) |>
      scale_color_continuous(limits = c(0, 10))
  )
  expect_length(.legend_guides(b2$scales), 1L)
})

# --- F-10: discrete position scales --------------------------------------

test_that("scale_x_discrete() takes breaks, labels and drop", {
  d <- data.frame(
    g = factor(c("a", "b", "c"), levels = c("a", "b", "c", "z")),
    v = 1:3
  )
  base <- vplot(d) |> mark_point(x = g, y = v)
  # unused levels drop by default, as in ggplot2
  expect_identical(panels(base)$scales$x$levels, c("a", "b", "c"))
  kept <- panels(base |> scale_x_discrete(drop = FALSE))$scales$x
  expect_identical(kept$levels, c("a", "b", "c", "z"))
  # breaks pick the ticks
  br <- panels(base |> scale_x_discrete(breaks = c("a", "c")))$scales$x
  expect_equal(br$breaks, c(1, 3))
  expect_identical(br$labels, c("a", "c"))
  # labels: a vector, a named vector, or a function
  lab <- panels(base |> scale_x_discrete(labels = c("A", "B", "C")))$scales$x
  expect_identical(lab$labels, c("A", "B", "C"))
  nm <- panels(base |> scale_x_discrete(labels = c(b = "Bee")))$scales$x
  expect_identical(nm$labels, c("a", "Bee", "c"))
  fn <- panels(base |> scale_x_discrete(labels = toupper))$scales$x
  expect_identical(fn$labels, c("A", "B", "C"))
  both <- panels(
    base |> scale_x_discrete(breaks = c("a", "c"), labels = c("first", "last"))
  )$scales$x
  expect_identical(both$labels, c("first", "last"))
  expect_error(
    panels(base |> scale_x_discrete(labels = c("A", "B"))),
    "one entry per break"
  )
  expect_error(scale_y_discrete(base, drop = NA), "drop")
})

# --- F-10: oob ------------------------------------------------------------

test_that("oob censors, squishes or keeps out-of-limits positions", {
  base <- vplot(data.frame(x = c(1, 5, 10), y = 1:3)) |>
    mark_point(x = x, y = y)
  map_x <- function(oob) {
    # (censoring warns about the removed rows; tested below)
    suppressWarnings(
      panels(base |> scale_x_continuous(limits = c(2, 8), oob = oob)),
      classes = "vellumplot_missing_rows"
    )$scales$x$map
  }
  expect_equal(map_x("censor")(c(1, 5, 10)), c(NA, 5, NA))
  expect_equal(map_x("squish")(c(1, 5, 10)), c(2, 5, 8))
  expect_equal(map_x("keep")(c(1, 5, 10)), c(1, 5, 10))
  expect_equal(map_x(scales::oob_squish)(c(1, 5, 10)), c(2, 5, 8))
  # the default keeps them (drawn, clipped by the panel)
  expect_equal(
    panels(base |> scale_x_continuous(limits = c(2, 8)))$scales$x$map(1),
    1
  )
  # Inf is never out of bounds: it still goes to the edge
  expect_equal(
    map_x("censor")(Inf),
    max(panels(base |> scale_x_continuous(limits = c(2, 8)))$scales$x$domain)
  )
  expect_error(scale_x_continuous(base, oob = "clip"), "oob")
  censored <- base |> scale_x_continuous(limits = c(2, 8), oob = "censor")
  expect_warning(
    vellum::as_vellum_scene(censored),
    "Removed 2 rows containing missing values or values outside the scale range (layer 1, `mark_point()`)",
    fixed = TRUE
  )
})

test_that("censored positions count into the one removed-rows warning (VP-41)", {
  d <- data.frame(x = c(1, 5, 10, NA), y = c(1, 2, 3, 4))
  p <- vplot(d) |>
    mark_point(x = x, y = y) |>
    scale_x_continuous(limits = c(2, 8), oob = "censor")
  w <- character()
  b <- withCallingHandlers(
    .build_panels(p),
    warning = function(cnd) {
      w <<- c(w, conditionMessage(cnd))
      invokeRestart("muffleWarning")
    }
  )
  # one warning, counting the missing row and the two censored ones
  expect_length(w, 1L)
  expect_match(w, "Removed 3 rows containing missing values or values outside")
  # the censored rows are gone before the stat, as in ggplot2
  expect_equal(b$panels[[1]]$resolved[[1]]$n, 1L)
  # a censoring function counts too; squish and keep drop nothing
  fun <- vplot(d[1:3, ]) |>
    mark_point(x = x, y = y) |>
    scale_y_continuous(limits = c(0, 2.5), oob = scales::oob_censor)
  expect_warning(.build_panels(fun), "Removed 1 row containing")
  for (oob in c("squish", "keep")) {
    expect_no_warning(
      .build_panels(
        vplot(d[1:3, ]) |>
          mark_point(x = x, y = y) |>
          scale_x_continuous(limits = c(2, 8), oob = oob)
      )
    )
  }
  # without limits nothing is out of bounds
  expect_no_warning(
    .build_panels(
      vplot(d[1:3, ]) |>
        mark_point(x = x, y = y) |>
        scale_x_continuous(oob = "censor")
    )
  )
  # a stat layer summarises only the rows inside the limits
  bx <- vplot(data.frame(g = "a", v = c(1:9, 100))) |>
    mark_boxplot(x = g, y = v) |>
    scale_y_continuous(limits = c(0, 50), oob = "censor")
  expect_warning(bb <- .build_panels(bx), "Removed 1 row")
  expect_equal(max(bb$panels[[1]]$resolved[[1]]$values$y), 9)
})

test_that("oob on colour and size scales", {
  d <- data.frame(x = 1:3, y = 1:3, v = c(0, 5, 10))
  col <- function(oob) {
    panels(
      vplot(d) |>
        mark_point(x = x, y = y, color = v) |>
        scale_color_continuous(
          limits = c(2, 8),
          oob = oob,
          palette = c("black", "white")
        )
    )$scales$color
  }
  cc <- col("censor")
  expect_identical(cc$map(0), cc$na_value)
  sq <- col("squish")
  expect_identical(sq$map(0), sq$map(2))
  # (censored sizes draw no point, with a warning; VR-38)
  sz <- function(oob) {
    suppressWarnings(
      panels(
        vplot(d) |>
          mark_point(x = x, y = y, size = v) |>
          scale_size(limits = c(2, 8), range = c(1, 4), oob = oob)
      ),
      classes = "vellumplot_missing_rows"
    )$scales$size
  }
  expect_true(is.na(sz("censor")$map(0)))
  expect_equal(sz("squish")$map(10), 4)
  expect_equal(sz(NULL)$map(10), 5) # extrapolated, as before
})

test_that("the named continuous colour scales take limits and oob (VP-41)", {
  d <- data.frame(x = 1:3, y = 1:3, v = c(0, 5, 10))
  ctors <- list(
    gradient = function(p, ...) scale_color_gradient(p, ...),
    gradient2 = function(p, ...) scale_fill_gradient2(p, midpoint = 5, ...),
    gradientn = function(p, ...) {
      scale_colour_gradientn(p, colours = c("red", "white", "blue"), ...)
    },
    viridis_c = function(p, ...) scale_fill_viridis_c(p, ...),
    distiller = function(p, ...) scale_color_distiller(p, ...)
  )
  for (nm in names(ctors)) {
    # a fill scale sets the fill scale only (VP-48)
    aes <- if (nm %in% c("gradient2", "viridis_c")) "fill" else "color"
    sc <- function(...) {
      p <- vplot(d) |> mark_point(x = x, y = y, color = v, fill = v)
      panels(ctors[[nm]](p, ...))$scales[[aes]]
    }
    # the default is unchanged: no limits, the data range, nothing out of bounds
    expect_equal(sc()$range, c(0, 10), info = nm)
    cens <- sc(limits = c(2, 8), oob = "censor", na.value = "magenta")
    expect_equal(cens$range, c(2, 8), info = nm)
    expect_identical(cens$map(c(0, 10)), c("magenta", "magenta"), info = nm)
    sq <- sc(limits = c(2, 8), oob = "squish")
    expect_identical(sq$map(0), sq$map(2), info = nm)
    # NULL oob keeps the old behaviour: out-of-range values take the end colour
    expect_identical(sc(limits = c(2, 8))$map(10), sq$map(8), info = nm)
  }
  expect_error(
    vplot(d) |> scale_color_gradient(oob = "clip"),
    "oob"
  )
  expect_error(
    vplot(d) |> scale_color_gradient(limits = 1),
    "limits"
  )
})

test_that("oob round-trips through the spec serializer", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    scale_x_continuous(limits = c(2, 4), oob = scales::oob_squish) |>
    scale_y_discrete(drop = FALSE)
  s <- p@scales[[1]]
  expect_identical(s@oob, "squish")
  expect_identical(.scale_from_ir(.scale_to_ir(s))@oob, "squish")
})

# --- F-10: inside legends -------------------------------------------------

test_that("legend.position = 'inside' / c(x, y) places the legend over the panels", {
  base <- vplot(mtcars) |> mark_point(x = wt, y = mpg, color = factor(cyl))
  rt <- function(p) .resolve_theme(p@theme)
  inside <- base |>
    theme(legend.position = "inside", legend.justification = c(1, 1))
  expect_equal(.legend_inside(rt(inside)), list(pos = c(1, 1), just = c(1, 1)))
  old <- base |> theme(legend.position = c(0.1, 0.9))
  expect_equal(
    .legend_inside(rt(old)),
    list(pos = c(0.1, 0.9), just = c(0.1, 0.9))
  )
  words <- base |>
    theme(
      legend.position = "inside",
      legend.position.inside = c(0.5, 0.5),
      legend.justification = c("left", "top")
    )
  expect_equal(
    .legend_inside(rt(words)),
    list(pos = c(0.5, 0.5), just = c(0, 1))
  )
  expect_null(.legend_inside(rt(base)))
  # the legend takes no track: the panel is as wide as with no legend
  svg <- plot_svg(inside)
  expect_match(svg, "legend-inside", fixed = TRUE)
  expect_error(theme(base, legend.position = c(2, NA)), "legend.position")
  expect_error(theme(base, legend.justification = "middle"), "justification")
})

test_that("each composition cell honours its own legend position (VP-41)", {
  mt <- transform(mtcars, fcyl = factor(cyl), fam = factor(am))
  inside <- function(p, at) {
    theme(p, legend.position = "inside", legend.position.inside = at)
  }
  p1 <- vplot(mt) |>
    mark_point(x = wt, y = mpg, color = fcyl) |>
    inside(c(1, 1))
  p2 <- vplot(mt) |>
    mark_point(x = hp, y = qsec, color = fam) |>
    inside(c(0, 0))
  p3 <- vplot(mt) |> mark_point(x = disp, y = mpg, shape = fam)
  p4 <- vplot(mt) |>
    mark_point(x = drat, y = mpg, color = fcyl) |>
    theme(legend.position = "none")
  plans <- lapply(list(p1, p2, p3, p4), .plan_plot)
  mode <- function(plan) {
    lg <- .subplot_legend_groups(plan)
    if (length(lg$inside)) {
      "inside"
    } else if (length(.subplot_legend_sides(lg))) {
      "side"
    } else {
      "none"
    }
  }
  expect_identical(
    vapply(plans, mode, ""),
    c("inside", "inside", "side", "none")
  )
  # only an outside legend takes a right-side band
  bands <- lapply(plans, .plot_bands, collect = FALSE)
  zero <- vapply(
    bands,
    function(b) vellum::vl_convert(b$R_legend, "mm") == 0,
    logical(1)
  )
  expect_identical(zero, c(TRUE, TRUE, FALSE, TRUE))
  # collecting pools only the outside legends; inside ones stay in their cell
  pooled <- .pool_guides(plans)
  expect_length(pooled, 1L)
  expect_identical(pooled[[1]]$kind, "shape")
  count_inside <- function(svg) {
    lengths(regmatches(svg, gregexpr("legend-inside", svg, fixed = TRUE)))
  }
  for (guides in c("keep", "collect")) {
    comp <- wrap_plots(list(p1, p2, p3, p4), ncol = 2, guides = guides)
    expect_identical(count_inside(plot_svg(comp)), 2L, info = guides)
  }
})
