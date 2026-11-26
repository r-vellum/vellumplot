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
