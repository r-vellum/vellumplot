# VP-104: an error bar's `width` is in data units, as `mark_bar(width =)` is
# since VP-98 and as in ggplot2's geom_errorbar(), defaulting to 90% of the
# band, so bars and their error bars line up, dodged or not, on a discrete or
# a continuous x.

# The cap segments (x0, x1, native units) of a plot's error bars.
eb_caps <- function(p) {
  s <- vellum::as_scene_spec(vellum::as_vellum_scene(p))
  val <- function(x) if (is.list(x)) x$v else x
  out <- list()
  walk <- function(node) {
    id <- val(node$id)
    if (
      identical(node$class, "grob_segments") &&
        is.character(id) &&
        grepl("errorbar", id)
    ) {
      x0 <- val(node$x0$value)
      x1 <- val(node$x1$value)
      cap <- x0 != x1
      out[[length(out) + 1L]] <<- data.frame(x0 = x0[cap], x1 = x1[cap])
    }
    for (ch in node) {
      if (is.list(ch)) walk(ch)
    }
  }
  walk(s$root)
  out <- do.call(rbind, out)
  out[!duplicated(out), ]
}

eb <- data.frame(x = c(10, 20, 30), y = c(2, 4, 3))
eb$lo <- eb$y - 1
eb$hi <- eb$y + 1

test_that("errorbar width is in data units on a continuous x", {
  caps <- eb_caps(vplot(eb) |> mark_errorbar(x = x, ymin = lo, ymax = hi, width = 4))
  expect_equal(caps$x1 - caps$x0, rep(4, 3))
  expect_equal((caps$x0 + caps$x1) / 2, c(10, 20, 30))
  # the default is 90% of the resolution, a default bar's width
  caps <- eb_caps(vplot(eb) |> mark_errorbar(x = x, ymin = lo, ymax = hi))
  expect_equal(caps$x1 - caps$x0, rep(9, 3))
})

test_that("the caps train the continuous axis, as bars do", {
  p <- vplot(eb) |> mark_errorbar(x = x, ymin = lo, ymax = hi, width = 8)
  dom <- vellumplot:::.build_panels(p)$scales$x$domain
  expect_lte(dom[1], 6)
  expect_gte(dom[2], 34)
})

test_that("dodged error bars line up with dodged bars", {
  d <- data.frame(
    x = rep(c(1, 2, 4), 2),
    y = c(3, 5, 2, 4, 1, 3),
    g = rep(c("a", "b"), each = 3)
  )
  d$lo <- d$y - 0.5
  d$hi <- d$y + 0.5
  d$f <- factor(d$x)
  for (xx in c("x", "f")) {
    p <- vplot(d) |>
      mark_bar(x = .data[[xx]], y = y, fill = g, position = "dodge") |>
      mark_errorbar(
        x = .data[[xx]],
        ymin = lo,
        ymax = hi,
        group = g,
        position = "dodge"
      )
    caps <- eb_caps(p)
    caps <- caps[order(caps$x0), ]
    # bars: 0.9 wide per x, two per x, so each is 0.45 wide; the caps span them
    expect_equal(caps$x1 - caps$x0, rep(0.45, 6), info = xx)
    pos <- if (xx == "x") c(1, 2, 4) else 1:3
    expect_equal(
      (caps$x0 + caps$x1) / 2,
      sort(c(pos - 0.225, pos + 0.225)),
      info = xx
    )
  }
  # a narrower cap keeps its slot's centre and shares its width
  p <- vplot(d) |>
    mark_bar(x = f, y = y, fill = g, position = "dodge") |>
    mark_errorbar(x = f, ymin = lo, ymax = hi, group = g, position = "dodge", width = 0.3)
  caps <- eb_caps(p)
  expect_equal(caps$x1 - caps$x0, rep(0.15, 6))
})

test_that("bar and error-bar geometry agree at the defaults", {
  L <- list(
    values = list(g = c("a", "b", "a", "b")),
    params = list(),
    stat_params = list(),
    position = "dodge",
    n = 4L,
    mark = "errorbar"
  )
  xp <- c(1, 1, 2, 2)
  local_mocked_bindings(.group_key = function(L) L$values$g)
  b <- vellumplot:::.bar_geometry(L, xp, 1, 4L)
  e <- vellumplot:::.errorbar_geometry(L, xp, 1, 4L)
  expect_equal(e, b)
})
