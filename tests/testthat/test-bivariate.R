# Bivariate choropleths: bi_class(), bi_pal(), scale_fill_bivariate(),
# guide_bivariate().

guide_kinds <- function(p) {
  b <- vellumplot:::.build_panels(p)
  vapply(vellumplot:::.legend_guides(b$scales), `[[`, "", "kind")
}

test_that("bi_pal() returns dim^2 colours named by class", {
  p3 <- bi_pal()
  expect_length(p3, 9L)
  expect_identical(names(p3)[c(1, 2, 4, 9)], c("1-1", "2-1", "1-2", "3-3"))
  # The 3x3 palette is the published scheme, verbatim.
  expect_identical(unname(p3[c("1-1", "3-3")]), c("#e8e8e8", "#2a5a5b"))
  # Other sizes interpolate from it and keep its corners.
  p2 <- bi_pal("DkBlue", dim = 2)
  expect_length(p2, 4L)
  expect_identical(toupper(p2[["1-1"]]), toupper(p3[["1-1"]]))
  expect_identical(toupper(p2[["2-2"]]), toupper(p3[["3-3"]]))
  expect_length(bi_pal("GrPink", 4), 16L)
  expect_error(bi_pal("nope"), "must be one of")
  expect_error(bi_pal(dim = 5), "must be one of")
})

test_that("bi_class() bins both variables over the whole table", {
  d <- bi_class(mtcars, hp, wt)
  expect_s3_class(d$bi_class, "factor")
  expect_identical(levels(d$bi_class), vellumplot:::.bi_levels(3))
  # x is the first index, y the second.
  expect_identical(
    substr(as.character(d$bi_class[which.min(mtcars$hp)]), 1, 1),
    "1"
  )
  expect_identical(
    substr(as.character(d$bi_class[which.max(mtcars$wt)]), 3, 3),
    "3"
  )
  info <- attr(d$bi_class, "vellum_bivariate")
  expect_identical(info[c("x", "y", "dim")], list(x = "hp", y = "wt", dim = 3L))
  expect_length(info$x_breaks, 4L)
  # Missing values give a missing class.
  m <- mtcars
  m$hp[1] <- NA
  expect_true(is.na(bi_class(m, hp, wt)$bi_class[1]))
  # Custom name and class count.
  d4 <- bi_class(mtcars, hp, wt, dim = 2, name = "cls")
  expect_identical(levels(d4$cls), c("1-1", "2-1", "1-2", "2-2"))
})

test_that("bi_class() validates its input", {
  expect_error(bi_class(1:3, a, b), "data frame")
  expect_error(bi_class(mtcars, hp, wt, dim = 7), "must be one of")
  d <- data.frame(a = letters[1:3], b = 1:3)
  expect_error(bi_class(d, a, b), "numeric column")
})

test_that("a bi_class() column trains a bivariate scale on its own", {
  d <- bi_class(mtcars, hp, wt)
  p <- vplot(d) |> mark_point(x = hp, y = wt, color = bi_class)
  sc <- vellumplot:::.build_panels(p)$scales$color
  expect_identical(sc$type, "discrete")
  expect_identical(sc$bivariate$dim, 3L)
  expect_identical(sc$bivariate$xlab, "hp")
  expect_identical(sc$bivariate$ylab, "wt")
  expect_identical(sc$map(c("1-1", "3-3")), unname(bi_pal()[c("1-1", "3-3")]))
  # No title by default: the axis titles carry the meaning.
  expect_null(sc$name)
  expect_identical(guide_kinds(p), "color_bivariate")
})

test_that("scale_fill_bivariate() sets the palette and the axis titles", {
  d <- bi_class(mtcars, hp, wt)
  p <- vplot(d) |>
    mark_point(x = hp, y = wt, fill = bi_class) |>
    scale_fill_bivariate(palette = "GrPink", xlab = "Power", ylab = "Weight")
  sc <- vellumplot:::.build_panels(p)$scales$color
  expect_identical(sc$colors, unname(bi_pal("GrPink")))
  expect_identical(
    sc$bivariate[c("xlab", "ylab")],
    list(xlab = "Power", ylab = "Weight")
  )
  # guides() retitles without respelling the scale.
  p2 <- p |>
    guides(fill = guide_bivariate(xlab = "hp", key.size = 8, title = "T"))
  sc2 <- vellumplot:::.build_panels(p2)$scales$color
  expect_identical(sc2$bivariate$xlab, "hp")
  expect_identical(sc2$bivariate$key_size, 8)
  expect_identical(sc2$name, "T")
})

test_that("a plain 'x-y' class column works with an explicit dim or guide", {
  d <- data.frame(x = 1:4, y = 1:4, cls = c("1-1", "2-1", "1-2", "2-2"))
  p <- vplot(d) |>
    mark_point(x = x, y = y, color = cls) |>
    scale_color_bivariate(dim = 2, palette = c("red", "blue", "green", "black"))
  sc <- vellumplot:::.build_panels(p)$scales$color
  expect_identical(sc$map("2-1"), "blue")
  expect_identical(sc$bivariate$xlab, "x")
  # guide_bivariate() alone on such a column asks for the bivariate scale (dim 3).
  p3 <- vplot(d) |>
    mark_point(x = x, y = y, color = cls) |>
    guides(color = guide_bivariate())
  expect_identical(guide_kinds(p3), "color_bivariate")
  # Labels that are not classes of the scale's dim error clearly.
  bad <- vplot(d) |>
    mark_point(x = x, y = y, color = cls) |>
    scale_color_bivariate(dim = 2)
  d$cls[1] <- "3-1"
  bad@data <- d
  expect_error(vellumplot:::.build_panels(bad), "class labels")
})

test_that("scale and guide arguments are validated", {
  p <- vplot(mtcars)
  expect_error(scale_fill_bivariate(p, dim = 9), "must be one of")
  expect_error(scale_fill_bivariate(p, palette = c("red", "blue")), "explicit")
  expect_error(
    scale_fill_bivariate(p, palette = c("red", "blue"), dim = 2),
    "needs 4"
  )
  expect_error(guide_bivariate(xlab = 1), "single string")
  expect_error(guide_bivariate(key.size = -1), "positive")
  # guide_bivariate() on a continuous colour scale is a mistake.
  q <- vplot(mtcars) |>
    mark_point(x = hp, y = wt, color = mpg) |>
    scale_color_continuous() |>
    guides(color = guide_bivariate())
  expect_error(vellumplot:::.build_panels(q), "bivariate colour scale")
})

test_that("guide_legend() falls back to a keyed list of classes", {
  d <- bi_class(mtcars, hp, wt)
  p <- vplot(d) |>
    mark_point(x = hp, y = wt, color = bi_class) |>
    guides(color = guide_legend())
  expect_identical(guide_kinds(p), "color_discrete")
})

test_that("facet panels share the classes computed over the whole table", {
  d <- bi_class(mtcars, hp, wt)
  p <- vplot(d) |>
    mark_point(x = hp, y = wt, color = bi_class) |>
    facet_wrap(~am)
  sc <- vellumplot:::.build_panels(p)$scales$color
  expect_false(is.null(sc$bivariate))
  expect_identical(sc$bivariate$xlab, "hp")
  expect_length(sc$levels, 9L)
})

test_that("the bivariate legend measures and draws in every legend position", {
  d <- bi_class(mtcars, hp, wt)
  p <- vplot(d) |> mark_point(x = hp, y = wt, color = bi_class)
  g <- list(
    kind = "color_bivariate",
    sc = vellumplot:::.build_panels(p)$scales$color
  )
  rt <- vellumplot:::.resolve_theme(vellumplot:::.theme_of(p))
  m <- vellumplot:::.legend_metrics(rt)
  geo <- vellumplot:::.bi_geom(g, m)
  expect_equal(geo$side, 3 * 1.5 * m$key)
  expect_gt(vellumplot:::.guide_height_mm(g, m), geo$side)
  expect_gt(vellumplot:::.guide_col_width(g, m), geo$side)
  # A long axis title widens the guide rather than overflowing it.
  g$sc$bivariate$xlab <- strrep("long title ", 6)
  expect_gt(vellumplot:::.bi_geom(g, m)$width, geo$width)
  for (pos in c("right", "bottom", "top", "left")) {
    q <- p |> theme(legend.position = pos)
    expect_no_error(vellum::scene_raster(q))
  }
})

test_that("legend swatches and sf features are keyed for hover", {
  skip_if_not_installed("sf")
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  nc <- bi_class(nc, BIR74, SID74)
  p <- vplot(nc) |> mark_sf(fill = bi_class, tooltip = NAME)
  el <- vellum::scene_model(vellum::as_vellum_scene(p))$elements
  sw <- el[grepl("^legend:", el$key), ]
  expect_identical(sw$key, paste0("legend:color:", vellumplot:::.bi_levels(3)))
  expect_identical(sw$meta[[1]]$legend_for, "color:1-1")
  expect_match(sw$meta[[1]]$tooltip, "^BIR74: \\[")
  # Each county carries its class as legend membership, so a swatch highlights
  # exactly its counties.
  feat <- el[el$mark == "path" & !grepl("^legend:", el$key), ]
  memb <- vapply(feat$meta, function(x) x$legend %||% NA_character_, "")
  expect_setequal(
    unique(memb[!is.na(memb)]),
    paste0("color:", unique(as.character(nc$bi_class)))
  )
})

test_that("the bivariate scale round-trips through the portable spec", {
  d <- bi_class(mtcars, hp, wt)
  p <- vplot(d) |>
    mark_point(x = hp, y = wt, color = bi_class) |>
    scale_color_bivariate(palette = "DkViolet", xlab = "Power", ylab = "Weight")
  skip_if_not_installed("jsonlite")
  q <- spec_from_json(spec_to_json(p))
  a <- vellumplot:::.build_panels(p)$scales$color
  b <- vellumplot:::.build_panels(q)$scales$color
  expect_identical(b$colors, a$colors)
  expect_identical(b$bivariate$xlab, "Power")
})

test_that("bivariate choropleth structure snapshot", {
  skip_if_not_installed("sf")
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  nc <- bi_class(nc, BIR74, SID74)
  p <- vplot(nc) |>
    mark_sf(fill = bi_class, color = "white") |>
    scale_fill_bivariate(xlab = "Births", ylab = "SIDS")
  expect_snapshot(scene_digest(p))
})
