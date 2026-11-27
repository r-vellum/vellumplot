# vvenn(): Venn / Euler diagrams whose disjoint regions are drawn as solid
# boolean geometry via vellum::vl_path_op().

venn_of <- function(p) p@layers[[length(p@layers)]]@params$venn
render_ok <- function(p) {
  f <- withr::local_tempfile(fileext = ".png")
  render_plot(p, f)
  expect_gt(file.info(f)$size, 0)
}

test_that("region counts are the exactly-this-subset memberships", {
  v <- venn_of(vvenn(list(
    Coffee = c("Ann", "Bo", "Cy", "Di", "Ed"),
    Tea = c("Bo", "Di", "Ed", "Fi", "Gy", "Hu")
  )))
  r <- v$regions
  only_coffee <- r$count[r$Coffee & !r$Tea]
  only_tea <- r$count[!r$Coffee & r$Tea]
  both <- r$count[r$Coffee & r$Tea]
  expect_equal(only_coffee, 2L) # Ann, Cy
  expect_equal(only_tea, 3L) # Fi, Gy, Hu
  expect_equal(both, 3L) # Bo, Di, Ed
})

test_that("a data frame of logical columns is read as set membership", {
  #        r:  T  T  F  T  F     py: F  T  T  T  F
  # -> r-only = row1 (1); both = rows 2,4 (2); py-only = row3 (1)
  df <- data.frame(
    r = c(TRUE, TRUE, FALSE, TRUE, FALSE),
    py = c(FALSE, TRUE, TRUE, TRUE, FALSE)
  )
  v <- venn_of(vvenn(df))
  expect_setequal(v$names, c("r", "py"))
  r <- v$regions
  expect_equal(r$count[r$r & !r$py], 1L)
  expect_equal(r$count[r$r & r$py], 2L)
  expect_equal(r$count[!r$r & r$py], 1L)
})

test_that("2- and 3-set diagrams render", {
  render_ok(vvenn(list(A = 1:5, B = 3:8)))
  render_ok(vvenn(list(A = 1:5, B = 3:8, C = c(2, 4, 6, 8))))
})

test_that("regions are drawn as solid geometry (paths), not translucent circles", {
  # each disjoint region is a filled <path> from vl_path_op; a 3-set diagram has
  # up to 7 of them, well above the 3 circle outlines.
  p <- vvenn(list(A = 1:6, B = 3:9, C = c(2, 5, 8, 11)))
  svg <- vellum::scene_svg(vellum::as_vellum_scene(p))
  expect_gt(lengths(gregexpr("<path", svg)), 3L)
})

test_that("only 2 or 3 sets are accepted", {
  expect_error(vvenn(list(A = 1)), "2 or 3 sets")
  expect_error(vvenn(list(A = 1, B = 2, C = 3, D = 4)), "2 or 3 sets")
})

test_that("a data frame with no logical columns errors clearly", {
  expect_error(
    vvenn(data.frame(a = 1:3, b = letters[1:3])),
    "logical membership"
  )
})

test_that("long set names stay inside the panel (VP-148)", {
  gp <- vellumplot:::.mark_gpar_text(11, face = "bold")
  box <- 116 # mm, a 5in plot's panel
  ext <- function(g) {
    x <- as.numeric(vctrs::field(g@x, "value"))
    y <- as.numeric(vctrs::field(g@y, "value"))
    w <- vellum::vl_convert(vellum::grobwidth(g), "mm") / box
    h <- vellum::vl_convert(vellum::grobheight(g), "mm") / box
    c(l = x - w / 2, r = x + w / 2, b = y - h / 2, t = y + h / 2)
  }
  # a short name is the plain label at its anchor, as before
  short <- vellumplot:::.venn_name_grob("Coffee", c(0.36, 0.76), 2L, gp, box)
  npc <- function(u) vellum::vl_unit(u, "npc")
  expect_identical(
    short,
    vellum::text_grob("Coffee", npc(0.36), npc(0.76), gp = gp)
  )
  # a long name wraps; its box stays inside the panel
  long <- "A rather long set name that overflows the box"
  for (lab in list(c(0.154, 0.27), c(0.846, 0.27), c(0.5, 0.92))) {
    g <- vellumplot:::.venn_name_grob(long, lab, 3L, gp, box)
    e <- ext(g)
    expect_gte(e[["l"]], 0)
    expect_lte(e[["r"]], 1)
    expect_gte(e[["b"]], 0)
    expect_lte(e[["t"]], 1)
  }
  # two sets: each name keeps to its own half, even one unbreakable word
  word <- "Supercalifragilisticexpialidocious"
  left <- ext(vellumplot:::.venn_name_grob(word, c(0.36, 0.76), 2L, gp, box))
  right <- ext(vellumplot:::.venn_name_grob(long, c(0.64, 0.76), 2L, gp, box))
  expect_gte(left[["l"]], 0)
  expect_lte(left[["r"]], 0.5)
  expect_gte(right[["l"]], 0.5)
  expect_lte(right[["r"]], 1)
  # and the diagram renders
  render_ok(vvenn(list(A = 1:5, B = 3:8) |> setNames(c(word, long))))
})

test_that("set names fit the laid-out panel: margin, titles, cells (VP-148)", {
  # the box the names are fitted to is the panel as laid out, not a guess
  # from the page
  orig <- vellumplot:::.venn_box_mm
  got <- NULL
  local_mocked_bindings(.venn_box_mm = function() {
    v <- orig()
    got <<- c(got, v)
    v
  })
  panel_side <- function(x) {
    sm <- vellum::scene_model(vellum::as_vellum_scene(x))
    pan <- sm$panels[grepl("panel-", sm$panels$name), , drop = FALSE]
    pmin(pan$px1 - pan$px0, pan$py1 - pan$py0) / x@dpi * 25.4
  }
  S3 <- list(
    `A rather long set name that overflows the box` = 1:5,
    `Another very long set name here` = 3:8,
    `Third long-winded name for set` = c(1, 8, 9)
  )
  for (p in list(
    vvenn(S3) |> theme(plot.margin = 20),
    vvenn(S3) |> labs(title = "A title", subtitle = "Sub", caption = "Cap")
  )) {
    got <- NULL
    side <- panel_side(p)
    expect_equal(got[1], side, tolerance = 0.02)
  }
  # in a composition of Venns (laid out cell by cell, since a Venn locks its
  # aspect, VP-154): each cell's own panel, within its 4in x 4in cell
  S2 <- S3[1:2]
  comp <- hconcat(vvenn(S3), vvenn(S2), width = 8, height = 4)
  got <- NULL
  side <- panel_side(comp)
  expect_length(got, 2L)
  expect_lte(max(got), max(side) + 0.5)
  expect_lt(max(got), 4 * 25.4) # not the cell-less 6in / page guess
  # and in a non-aligned one (a faceted cell beside it)
  f <- vplot(mtcars) |> mark_point(x = wt, y = mpg) |> facet_wrap(~cyl)
  got <- NULL
  invisible(vellum::as_vellum_scene(hconcat(
    f,
    vvenn(S3),
    width = 8,
    height = 4
  )))
  expect_lt(got[1], 4 * 25.4)
})

test_that("wrapped names above the circles grow upward (VP-148 review)", {
  gp <- vellumplot:::.mark_gpar_text(11, face = "bold")
  long <- "A rather long set name that overflows the box"
  g <- vellumplot:::.venn_name_grob(long, c(0.36, 0.76), 2L, gp, 116, up = 1)
  y <- as.numeric(vctrs::field(g@y, "value"))
  h <- vellum::vl_convert(vellum::grobheight(g), "mm") / 116
  h1 <- vellum::vl_convert(
    vellum::grobheight(vellum::text_grob(long, gp = gp)),
    "mm"
  ) /
    116
  # its bottom line sits where the single line would: bottom edge unchanged
  expect_gt(h, 1.5 * h1)
  expect_equal(y - h / 2, 0.76 - h1 / 2, tolerance = 1e-3)
  # on a tiny panel the font shrinks so the name fits above the circles
  tiny <- vellumplot:::.venn_name_grob(long, c(0.36, 0.76), 2L, gp, 30, up = 1)
  ht <- vellum::vl_convert(vellum::grobheight(tiny), "mm") / 30
  yt <- as.numeric(vctrs::field(tiny@y, "value"))
  expect_lt(tiny@gp@fontsize, 11)
  expect_lte(yt + ht / 2, 1)
})
