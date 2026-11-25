# Layout measures text in the theme element's own font (VP-132).

test_that(".el_strwidth() measures in the element's family, face, weight and cex", {
  el <- element_text(size = 10, face = "bold", weight = 900, cex = 1.2)
  expect_equal(
    vellumplot:::.el_strwidth("Horsepower", el),
    vellum::vl_strwidth(
      "Horsepower", "", "bold", 10,
      cex = 1.2, unit = "mm", fontweight = 900
    )
  )
  # NULL / blank elements measure the default family, plain
  expect_equal(
    vellumplot:::.el_strwidth("Horsepower", NULL, 10),
    vellum::vl_strwidth("Horsepower", "", "plain", 10, unit = "mm")
  )
  expect_equal(
    vellumplot:::.el_strheight("Ag", element_blank(), 10),
    vellum::vl_strheight("Ag", "", "plain", 10, unit = "mm")
  )
})

bold_is_wider <- function(s, fs) {
  vellum::vl_strwidth(s, "", "bold", fs, unit = "mm") >
    vellum::vl_strwidth(s, "", "plain", fs, unit = "mm") + 0.5
}

test_that("the legend column fits a bold legend font (VP-132)", {
  lab <- "A long legend label WWWW"
  skip_if_not(bold_is_wider(lab, 9), "no wider bold face for the default family")
  d <- data.frame(x = 1:2, y = 1:2, g = c(lab, "b"))
  p <- vplot(d) |> mark_point(x = x, y = y, color = g)
  width_of <- function(p) {
    pl <- vellumplot:::.build_panels(p)
    g <- vellumplot:::.legend_guides(pl$scales)
    rt <- vellumplot:::.resolve_theme(p@theme)
    vellum::vl_convert(vellumplot:::.legend_width(g, rt), "mm")
  }
  plain <- width_of(p)
  bold <- width_of(theme(p, legend.text = element_text(face = "bold")))
  grow <- vellum::vl_strwidth(lab, "", "bold", 9, unit = "mm") -
    vellum::vl_strwidth(lab, "", "plain", 9, unit = "mm")
  expect_equal(bold - plain, grow, tolerance = 1e-6)
  # a numeric weight counts as well as a face
  heavy <- width_of(theme(p, legend.text = element_text(weight = 900)))
  expect_gt(heavy, plain)
  # the legend metrics carry the elements they measure in
  m <- vellumplot:::.legend_metrics(vellumplot:::.resolve_theme(
    theme(p, legend.title = element_text(face = "bold"))@theme
  ))
  expect_identical(m$title_el@face, "bold")
})

test_that("axis title and tick-label gutters fit a bold font (VP-132)", {
  lab <- "Toyota Corolla WWW"
  skip_if_not(bold_is_wider(lab, 9), "no wider bold face for the default family")
  plain <- element_text(size = 9)
  bold <- element_text(size = 9, face = "bold")
  mm <- function(u) vellum::vl_convert(u, "mm")
  expect_gt(
    mm(vellumplot:::.track_w_axis(bold, lab, 1.4)),
    mm(vellumplot:::.track_w_axis(plain, lab, 1.4)) + 0.5
  )
  # a rotated y title's gutter is its height: the heavier face's line
  expect_gte(
    mm(vellumplot:::.track_w(bold, lab, 1.4, rot = 90)),
    mm(vellumplot:::.track_w(plain, lab, 1.4, rot = 90))
  )
  expect_gt(
    mm(vellumplot:::.track_w(bold, lab, 1.4)),
    mm(vellumplot:::.track_w(plain, lab, 1.4)) + 0.5
  )
  # the default font measures exactly as before
  expect_equal(
    mm(vellumplot:::.track_w(plain, lab, 1.4)),
    mm(vellum::grobwidth(vellumplot:::.txt(lab, 9))) + 1.4
  )
})
