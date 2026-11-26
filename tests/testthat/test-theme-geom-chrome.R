# VP-61: drawn defaults outside the data marks (legend NA keys, sparklines,
# hierarchy / chord chrome, graph pies and label leaders, the outline effect)
# follow the theme's ink / paper / accent; light themes draw them as before.

chrome_paints <- function(p) {
  s <- unlist(vellum::as_scene_spec(vellum::as_vellum_scene(p)))
  unique(tolower(s[grepl("gp\\.(col|fill)(\\.v)?$", names(s))]))
}
ck_ink <- "#c7d0f0"
ck_paper <- "#0d0f18"
ck_accent <- "#08f7fe"
ck_mix <- function(amount) {
  tolower(vellumplot:::.col_mix(ck_ink, ck_paper, amount))
}

test_that(".geom_paint() neutral and accent roles", {
  local_geom <- function(g, code) {
    old <- vellumplot:::.mark_ctx$geom
    on.exit(assign("geom", old, envir = vellumplot:::.mark_ctx))
    assign("geom", g, envir = vellumplot:::.mark_ctx)
    code
  }
  g <- list(ink = "white", paper = "black", accent = "cyan", colour = "red")
  expect_identical(local_geom(g, vellumplot:::.geom_paint("grey30")), "red")
  expect_identical(
    local_geom(g, vellumplot:::.geom_paint("grey30", "neutral")),
    vellumplot:::.col_mix("white", "black", 0.3)
  )
  expect_identical(
    local_geom(g, vellumplot:::.geom_paint("firebrick", "accent")),
    "cyan"
  )
  expect_identical(
    local_geom(list(ink = "white"), vellumplot:::.geom_paint("firebrick", "accent")),
    "firebrick"
  )
  expect_identical(
    local_geom(NULL, vellumplot:::.geom_paint_as("#7f7f7f", "grey50")),
    "#7f7f7f"
  )
})

test_that("legend NA keys follow the theme", {
  d <- mtcars
  d$hp[c(2, 7)] <- NA
  p <- vplot(d) |> mark_point(x = wt, y = mpg, size = hp)
  # (the NA rows draw no point, with a warning; VR-38)
  quiet <- function(x) suppressWarnings(x, classes = "vellumplot_missing_rows")
  expect_true(all(c("grey70", "grey50") %in% quiet(chrome_paints(p))))
  cp <- quiet(chrome_paints(theme_cyberpunk(p)))
  expect_true(all(c(ck_mix(0.7), ck_mix(0.5)) %in% cp))
  expect_false("grey70" %in% cp)
})

test_that("sparklines take the theme's ink mix and accent", {
  sp <- vsparkline(c(3, 5, 2, 8, 6, 9, 4))
  expect_true(all(c("grey30", "firebrick") %in% chrome_paints(sp)))
  cp <- chrome_paints(theme_cyberpunk(sp))
  expect_true(all(c(ck_mix(0.3), ck_accent) %in% cp))
  expect_false(any(c("grey30", "firebrick") %in% cp))
  wl <- vsparkline(c(1, -1, 2, -3), type = "winloss")
  expect_true(all(c("#2c7fb8", "#d7301f") %in% chrome_paints(wl)))
  cp <- chrome_paints(theme_cyberpunk(wl))
  expect_true(all(c(ck_accent, "#d7301f") %in% cp))
  # explicit colours still win
  red <- vsparkline(c(1, 3, 2), color = "red", point_color = "blue")
  expect_true(all(c("red", "blue") %in% chrome_paints(theme_cyberpunk(red))))
  # an annotated sparkline follows its host plot's theme
  host <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    annotate("sparkline", x = 4, y = 30, values = c(1, 3, 2, 5, 4))
  expect_true("grey30" %in% chrome_paints(host))
  expect_true(ck_mix(0.3) %in% chrome_paints(theme_cyberpunk(host)))
})

test_that("hierarchy and chord chrome follow the theme", {
  h <- data.frame(
    id = c("root", "A", "B", "A1", "A2", "B1"),
    parent = c(NA, "root", "root", "A", "A", "B"),
    value = c(NA, NA, NA, 3, 2, 4)
  )
  sun <- vhierarchy(h, id, parent, value, root_label = TRUE)
  expect_true(all(c("white", "black") %in% chrome_paints(sun)))
  cp <- chrome_paints(theme_cyberpunk(sun))
  # (labels inside the cells keep their black / white contrast ink)
  expect_true(all(c(ck_paper, ck_ink) %in% cp))
  tree <- vhierarchy(h, id, parent, value, type = "treemap")
  expect_true(ck_paper %in% chrome_paints(theme_cyberpunk(tree)))
  # branch shades fade toward the paper, not white, on a dark theme
  fills <- function(p) {
    s <- unlist(vellum::as_scene_spec(vellum::as_vellum_scene(p)))
    tolower(s[grepl("fill(\\.v)?[0-9]*$", names(s))])
  }
  lum <- function(cols) colSums(grDevices::col2rgb(cols))
  light <- setdiff(fills(tree), c("white", "grey92", "chr"))
  dark <- setdiff(fills(theme_cyberpunk(tree)), c(ck_paper, "#161b2e", "chr"))
  expect_gt(mean(lum(light)), mean(lum(dark)))
  m <- matrix(
    c(0, 3, 2, 3, 0, 4, 2, 4, 0),
    3,
    dimnames = list(c("a", "b", "c"), c("a", "b", "c"))
  )
  ch <- vchord(m)
  expect_true(all(c("white", "black") %in% chrome_paints(ch)))
  expect_true(all(c(ck_paper, ck_ink) %in% chrome_paints(theme_cyberpunk(ch))))
})

test_that("outline() defaults to the theme's paper", {
  expect_null(outline()@color)
  expect_identical(outline(color = "black")@color, "black")
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, size = 3, effects = list(outline()))
  expect_true("white" %in% chrome_paints(p))
  expect_true(ck_paper %in% chrome_paints(theme_cyberpunk(p)))
  # and round-trips through the spec without a colour
  rt <- spec_from_json(spec_to_json(p))
  expect_null(rt@layers[[1]]@effects[[1]]@color)
})

test_that("graph pie borders and label leaders follow the theme", {
  g <- igraph::make_ring(6)
  igraph::V(g)$a <- 1:6
  igraph::V(g)$b <- 6:1
  pie <- vgraph(g) |> mark_edges() |> mark_node_pie(cols = c("a", "b"))
  expect_true("white" %in% chrome_paints(pie))
  expect_true(ck_paper %in% chrome_paints(theme_cyberpunk(pie)))
  rp <- vplot(head(mtcars, 12)) |>
    mark_point(x = wt, y = mpg) |>
    mark_text(x = wt, y = mpg, label = cyl, repel = TRUE, seed = 1)
  expect_true("grey50" %in% chrome_paints(rp))
  cp <- chrome_paints(theme_cyberpunk(rp))
  expect_true(ck_mix(0.5) %in% cp)
  expect_false("grey50" %in% cp)
})
