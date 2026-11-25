# Mark-level labels (chord, sankey, hierarchy, scale bar, table) take the
# theme's base text font (VP-135).

heavy <- function(p) {
  theme(p, text = element_text(family = "Verdana", face = "bold"))
}

# The gpar font fields of every text grob in a scene, as a data frame.
text_fonts <- function(scene) {
  ss <- vellum::as_scene_spec(scene)
  out <- list()
  walk <- function(n) {
    if (!is.list(n)) {
      return()
    }
    if (identical(n$class, "grob_text")) {
      v <- function(x) if (is.list(x)) x$v else NA_character_
      out[[length(out) + 1L]] <<- data.frame(
        label = v(n$label)[1],
        family = v(n$gp$fontfamily),
        face = v(n$gp$fontface)
      )
    }
    for (k in n) {
      walk(k)
    }
  }
  walk(ss$root)
  do.call(rbind, out)
}

flows <- data.frame(
  from = c("A", "A", "B", "C"),
  to = c("B", "C", "C", "A"),
  value = c(3, 2, 4, 1)
)

test_that("chord labels draw in the theme font", {
  f <- text_fonts(vellum::as_vellum_scene(heavy(vchord(
    flows,
    from,
    to,
    value
  ))))
  lab <- f[f$label %in% c("A", "B", "C"), ]
  expect_equal(nrow(lab), 3L)
  expect_true(all(lab$family == "Verdana"))
  expect_true(all(lab$face == "bold"))
  # the default theme leaves the font fields unset
  f0 <- text_fonts(vellum::as_vellum_scene(vchord(flows, from, to, value)))
  expect_true(all(is.na(f0$family[f0$label %in% c("A", "B", "C")])))
})

test_that("chord label room is measured in the theme font", {
  wide <- data.frame(
    from = c("A rather long node name", "b"),
    to = c("b", "A rather long node name"),
    value = c(1, 2)
  )
  ext <- function(font) {
    assign("text_font", font, envir = vellumplot:::.mark_ctx)
    on.exit(assign("text_font", NULL, envir = vellumplot:::.mark_ctx))
    vellumplot:::.chord_extent(unique(wide$from), TRUE)
  }
  w_plain <- vellumplot:::.el_strwidth("A rather long node name", NULL, 8)
  w_bold <- vellumplot:::.el_strwidth(
    "A rather long node name",
    element_text(face = "bold"),
    8
  )
  skip_if_not(w_bold > w_plain + 0.5, "no wider bold face here")
  expect_gt(ext(list(face = "bold")), ext(NULL))
})

test_that("sankey and hierarchy labels draw in the theme font", {
  dag <- data.frame(from = c("A", "A", "B"), to = c("B", "C", "C"), value = 1:3)
  sk <- text_fonts(vellum::as_vellum_scene(heavy(vsankey(
    dag,
    from,
    to,
    value
  ))))
  expect_gt(nrow(sk), 0L)
  expect_true(all(sk$family == "Verdana"))
  h <- data.frame(
    id = c("root", "A", "B", "A1", "A2", "B1"),
    parent = c(NA, "root", "root", "A", "A", "B"),
    value = c(NA, NA, NA, 3, 2, 4)
  )
  for (type in c("treemap", "sunburst", "circlepack")) {
    p <- vhierarchy(h, id, parent, value, type = type)
    f <- text_fonts(vellum::as_vellum_scene(heavy(p)))
    expect_gt(nrow(f), 0L)
    expect_true(all(f$family == "Verdana"), label = type)
  }
})

test_that("the scale bar text draws in the theme font", {
  skip_if_not_installed("sf")
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  p <- vplot(nc) |>
    mark_sf() |>
    coord_sf(crs = 3857) |>
    mark_scalebar(unit = "km") |>
    heavy()
  f <- text_fonts(vellum::as_vellum_scene(p))
  bar <- f[f$label == "0" | grepl("km$", f$label), ]
  expect_gte(nrow(bar), 2L)
  expect_true(all(bar$family == "Verdana"))
})

test_that("vtable cells use the default theme's font, measured as drawn", {
  d <- data.frame(name = c("alpha", "beta"), n = c(1, 22))
  plain <- vtable(d)
  expect_null(plain@font)
  f0 <- text_fonts(vellum::as_vellum_scene(plain))
  expect_true(all(is.na(f0$family)))
  # a branded default theme's font is drawn and measured
  local_mocked_bindings(
    .theme_auto = function() {
      list(text = element_text(family = "Verdana", face = "bold"))
    }
  )
  vt <- vtable(d)
  expect_identical(vt@font, list(family = "Verdana", face = "bold"))
  f <- text_fonts(vellum::as_vellum_scene(vt))
  expect_true(all(f$family == "Verdana"))
  expect_true(all(f$face == "bold"))
  w <- vellum::vl_strwidth("alpha", "Verdana", "bold", 9, unit = "mm")
  expect_gte(vt@col_widths[1], w)
})
