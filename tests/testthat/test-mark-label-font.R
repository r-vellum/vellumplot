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

test_that("vtable cells use the default theme's font when drawn", {
  d <- data.frame(name = c("alpha", "beta"), n = c(1, 22))
  vt <- vtable(d)
  f0 <- text_fonts(vellum::as_vellum_scene(vt))
  expect_true(all(is.na(f0$family)))
  # the brand turned on after the table was built still reaches it (VP-133)
  local_mocked_bindings(
    .theme_auto = function() {
      list(text = element_text(family = "Verdana", face = "bold"))
    }
  )
  rv <- vellumplot:::.vtable_font(vt)
  expect_identical(rv$font, list(family = "Verdana", face = "bold"))
  f <- text_fonts(vellum::as_vellum_scene(vt))
  expect_true(all(f$family == "Verdana"))
  expect_true(all(f$face == "bold"))
  # text columns are re-measured in that font; the page follows
  w <- vellum::vl_strwidth("alpha", "Verdana", "bold", 9, unit = "mm")
  expect_gte(rv$vt@col_widths[1], w)
  expect_equal(
    rv$vt@width - vt@width,
    (sum(rv$vt@col_widths) - sum(vt@col_widths)) / 25.4
  )
})

test_that("a layer's own fontface replaces the theme's face and weight", {
  assign(
    "text_font",
    list(family = "Verdana", face = "bold", weight = 300L),
    envir = vellumplot:::.mark_ctx
  )
  on.exit(assign("text_font", NULL, envir = vellumplot:::.mark_ctx))
  themed <- vellumplot:::.mark_text_el(8)
  expect_identical(themed@face, "bold")
  expect_identical(vellumplot:::.el_weight(themed), 700L)
  own <- vellumplot:::.mark_text_el(8, list(params = list(fontface = "plain")))
  expect_identical(own@face, "plain")
  expect_null(own@weight)
  expect_identical(own@family, "Verdana")
})

test_that("animated chord label room is measured in the theme font", {
  # frozen-scale panels are built outside the draw; they still see the font
  seen <- NULL
  local_mocked_bindings(
    .chord_extent = function(labels, has_labels) {
      seen <<- c(seen, list(vellumplot:::.mark_ctx$text_font))
      1.2
    }
  )
  p <- heavy(vchord(flows, from, to, value))
  vellumplot:::.build_panels(p)
  expect_true(length(seen) > 0L)
  expect_true(all(vapply(
    seen,
    function(f) identical(f$family, "Verdana"),
    TRUE
  )))
  # and the caller's font is put back
  expect_null(vellumplot:::.mark_ctx$text_font)
})

# VP-139: the last labels that ignored the theme font.

serif <- function(p) {
  theme(p, text = element_text(family = "Georgia", face = "italic"))
}

test_that("venn counts and set names draw in the theme font", {
  v <- vvenn(list(Coffee = c("Ann", "Bo", "Cy"), Tea = c("Bo", "Di")))
  f0 <- text_fonts(vellum::as_vellum_scene(v))
  nm0 <- f0[f0$label %in% c("Coffee", "Tea"), ]
  expect_true(all(is.na(nm0$family)))
  expect_true(all(nm0$face == "bold"))
  f <- text_fonts(vellum::as_vellum_scene(serif(v)))
  nm <- f[f$label %in% c("Coffee", "Tea"), ]
  ct <- f[f$label %in% c("1", "2"), ]
  expect_identical(nrow(nm), 2L)
  expect_true(all(nm$family == "Georgia"))
  # set names stay bold: the fixed face replaces the theme's italic
  expect_true(all(nm$face == "bold"))
  expect_gte(nrow(ct), 2L)
  expect_true(all(ct$family == "Georgia"))
  expect_true(all(ct$face == "italic"))
})

test_that("a fixed face keeps the theme's family and lifts a light weight", {
  assign(
    "text_font",
    list(family = "Verdana", weight = 300L),
    envir = vellumplot:::.mark_ctx
  )
  on.exit(assign("text_font", NULL, envir = vellumplot:::.mark_ctx))
  el <- vellumplot:::.mark_text_el(8, face = "bold")
  expect_identical(el@family, "Verdana")
  expect_identical(el@face, "bold")
  expect_identical(vellumplot:::.el_weight(el), 700L)
  # with no theme font the gpar is the plain bold one it always was
  assign("text_font", NULL, envir = vellumplot:::.mark_ctx)
  expect_identical(
    vellumplot:::.mark_gpar_text(11, face = "bold"),
    vellum::vl_gpar(fontsize = 11, fontface = "bold")
  )
})

test_that("compass N and graticule labels draw in the theme font", {
  skip_if_not_installed("sf")
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  p <- vplot(nc) |>
    mark_sf() |>
    coord_sf(graticule = TRUE) |>
    mark_compass() |>
    serif()
  f <- text_fonts(vellum::as_vellum_scene(p))
  n <- f[f$label == "N", ]
  expect_identical(nrow(n), 1L)
  expect_identical(n$family, "Georgia")
  expect_identical(n$face, "bold")
  grat <- f[grepl("°", f$label), ]
  expect_gte(nrow(grat), 2L)
  expect_true(all(grat$family == "Georgia"))
})

test_that("significance bracket labels draw in the theme font", {
  set.seed(1)
  d <- data.frame(
    g = rep(c("a", "b"), each = 20),
    y = c(rnorm(20), rnorm(20, 1))
  )
  p <- vplot(d) |>
    mark_boxplot(x = g, y = y) |>
    mark_signif(x = g, y = y, label = "stars") |>
    serif()
  f <- text_fonts(vellum::as_vellum_scene(p))
  st <- f[grepl("^(\\*+|ns)$", f$label), ]
  expect_identical(nrow(st), 1L)
  expect_identical(st$family, "Georgia")
})

# VP-141: the bold header is measured bold.

test_that("vtable measures its header in the bold face it draws", {
  d <- data.frame(Mileage = c(1, 2))
  vt <- vtable(d)
  bold <- vellum::vl_strwidth("Mileage", "", "bold", 9, unit = "mm")
  plain <- vellum::vl_strwidth("Mileage", "", "plain", 9, unit = "mm")
  expect_gt(bold, plain)
  expect_equal(vt@col_widths[1], bold + 2 * vt@cell_pad, tolerance = 1e-9)
  # no header row: the undrawn name keeps the cells' face
  nh <- vtable(d, header = FALSE)
  expect_equal(nh@col_widths[1], plain + 2 * nh@cell_pad, tolerance = 1e-9)
  # a theme font re-measures the header in its bold
  local_mocked_bindings(
    .theme_auto = function() {
      list(text = element_text(family = "Verdana", weight = 300L))
    }
  )
  rv <- vellumplot:::.vtable_font(vt)
  vb <- vellum::vl_strwidth(
    "Mileage",
    "Verdana",
    "bold",
    9,
    unit = "mm",
    fontweight = 700L
  )
  expect_equal(rv$vt@col_widths[1], vb + 2 * vt@cell_pad, tolerance = 1e-9)
})
