# Axis gutters and strips measure every candidate label in the element font
# (VP-136); rotated title bands and the tag reserve their rotated extent
# (VP-137).

mm <- function(u) vellum::vl_convert(u, "mm")

lay_of <- function(p) {
  pl <- vellumplot:::.build_panels(p)
  rt <- vellumplot:::.resolve_theme(vellumplot:::.theme_of(p))
  list(
    lay = vellumplot:::.build_layout(
      pl,
      vellumplot:::.legend_guides(pl$scales),
      p@labels,
      rt
    ),
    rt = rt
  )
}

test_that(".widest() picks the widest label, not the one with most characters", {
  el <- vellumplot:::.resolve_theme(vellumplot:::.theme_default())[[
    "axis.text.y"
  ]]
  w <- vellumplot:::.el_strwidth(c("1111111", "WWWW"), el)
  skip_if_not(w[2] > w[1], "WWWW is not wider than 1111111 in this font")
  expect_identical(vellumplot:::.widest(c("1111111", "WWWW"), el), "WWWW")
  expect_identical(vellumplot:::.widest(character(0), el), "0")
  expect_identical(vellumplot:::.widest(c("", "ab"), el), "ab")
  # an unrotated x row also measures the label with the most lines
  expect_setequal(
    vellumplot:::.x_probe(c("WWWW", "a\nb"), el),
    c("WWWW", "a\nb")
  )
})

test_that("the y tick gutter fits the widest label (VP-136)", {
  d <- data.frame(x = 1:2, y = c("1111111", "WWWW"))
  p <- vplot(d) |> mark_point(x = x, y = y)
  r <- lay_of(p)
  el <- r$rt[["axis.text.y"]]
  w <- vellumplot:::.el_strwidth(c("1111111", "WWWW"), el)
  skip_if_not(w[2] > w[1], "WWWW is not wider than 1111111 in this font")
  drawn <- mm(vellum::grobwidth(
    vellumplot:::.axis_text_grob("WWWW", el, c("right", "centre"))
  ))
  expect_gte(mm(r$lay$widths[r$lay$ylabels_col]), drawn)
})

test_that("rich strips pick the tallest label in the strip font (VP-136)", {
  el <- vellumplot:::.resolve_theme(vellumplot:::.theme_default())[[
    "strip.text"
  ]]
  tall <- vellum::md("a\n\nb\n\nc")
  probe <- vellumplot:::.strip_probe(list(vellum::md("**a**"), tall), el)
  expect_identical(probe, tall)
  # plain labels still probe by line count
  expect_identical(
    vellumplot:::.strip_probe(list(vellum::md("a"), "x\ny\nz"), el),
    "Ag\nAg\nAg"
  )
})

test_that("a rotated plot tag reserves its rotated height (VP-137)", {
  tag <- "A tag that is long"
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    labs(tag = tag)
  flat <- lay_of(p)
  up <- lay_of(theme(p, plot.tag = element_text(angle = 90)))
  el <- up$rt[["plot.tag"]]
  w <- vellumplot:::.el_strwidth(tag, el)
  h_flat <- mm(flat$lay$heights[flat$lay$tag_row])
  h_up <- mm(up$lay$heights[up$lay$tag_row])
  expect_gt(h_up, h_flat)
  expect_gte(h_up, w)
})

test_that("rotated composition title bands reserve their rotated height (VP-137)", {
  a <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
  b <- vplot(mtcars) |> mark_point(x = hp, y = mpg)
  title <- "A figure title"
  comp <- hconcat(a, b) |> compose_annotation(title = title)
  plans <- lapply(comp@plots, vellumplot:::.plan_plot)
  grid_h <- function(rt0) {
    glo <- vellumplot:::.composition_grid(plans, comp, TRUE, rt0)
    mm(glo$heights[glo$figtitle_row])
  }
  th <- vellumplot:::.theme_default()
  flat <- grid_h(vellumplot:::.resolve_theme(th))
  th$plot.title <- element_text(size = 14, angle = 90)
  rt_up <- vellumplot:::.resolve_theme(th)
  up <- grid_h(rt_up)
  expect_gt(up, flat)
  expect_gte(up, vellumplot:::.el_strwidth(title, rt_up[["plot.title"]]))
})
