# Independent non-position scales across facet panels (F-8):
# `resolve_scale(color = "independent")` trains the colour scale per panel and
# gives each panel its own legend, drawn beside the panel (VP-53) unless its
# guide asks for another position.

df <- data.frame(
  x = 1:9,
  y = c(3, 1, 2, 5, 4, 6, 9, 7, 8),
  g = rep(c("a", "b", "c"), each = 3),
  k = c("p", "q", "r", "q", "s", "t", "u", "u", "v"),
  v = c(1, 2, 3, 10, 20, 30, 100, 200, 300)
)
base <- vplot(df) |>
  mark_point(x = x, y = y, color = k) |>
  facet_wrap(~g)
svg_of <- function(p) vellum::scene_svg(as_vellum_scene(p))

test_that("resolve_scale() accepts non-position aesthetics and aliases", {
  p <- resolve_scale(base, colour = "independent", size = "independent")
  expect_identical(vellumplot:::.resolve_for(p, "color"), "independent")
  expect_identical(vellumplot:::.resolve_for(p, "size"), "independent")
  expect_identical(
    vellumplot:::.resolve_for(resolve_scale(base, fill = "independent"), "fill"),
    "independent"
  )
  expect_identical(vellumplot:::.resolve_for(base, "color"), "shared")
  expect_error(resolve_scale(base, color = "free"))
})

test_that("the default (shared) colour scale is unchanged", {
  built <- vellumplot:::.build_panels(base)
  expect_identical(built$independent, character(0))
  expect_null(built$panels[[1]]$aes_sc)
  expect_identical(
    vellumplot:::.facet_legend_guides(built),
    vellumplot:::.legend_guides(built$scales)
  )
  # an explicit "shared" draws the same SVG as the default
  expect_identical(svg_of(resolve_scale(base, color = "shared")), svg_of(base))
})

test_that("an independent colour scale trains per panel", {
  p <- resolve_scale(base, color = "independent")
  built <- vellumplot:::.build_panels(p)
  expect_identical(built$independent, "color")
  lv <- lapply(built$panels, function(q) q$aes_sc$color$levels)
  expect_identical(lv, list(c("p", "q", "r"), c("q", "s", "t"), c("u", "v")))
  # the shared scale is still trained on the pooled data
  expect_length(built$scales$color$levels, 7)
  # each panel's first level takes the palette's first colour
  first <- vapply(
    built$panels,
    function(q) q$aes_sc$color$colors[[1]],
    character(1)
  )
  expect_length(unique(first), 1)
  expect_identical(
    built$panels[[3]]$aes_sc$color$map("u"),
    built$panels[[1]]$aes_sc$color$map("p")
  )
})

test_that("each panel gets its own legend, beside its panel", {
  p <- resolve_scale(base, color = "independent")
  guides <- vellumplot:::.facet_legend_guides(vellumplot:::.build_panels(p))
  titles <- vapply(guides, function(g) g$sc$name, character(1))
  expect_identical(titles, c("k", "k", "k"))
  expect_identical(vapply(guides, function(g) g$panel, 1L), 1:3)
  svg <- svg_of(p)
  expect_no_match(svg, "k (a)", fixed = TRUE)
})

test_that("a collected per-panel legend is titled with its strip", {
  p <- resolve_scale(base, color = "independent") |>
    guides(color = guide_legend(position = "right"))
  guides <- vellumplot:::.facet_legend_guides(vellumplot:::.build_panels(p))
  titles <- vapply(guides, function(g) g$sc$name, character(1))
  expect_identical(titles, c("k (a)", "k (b)", "k (c)"))
  expect_true(all(vapply(guides, function(g) is.null(g$panel), TRUE)))
  svg <- svg_of(p)
  expect_match(svg, "k (a)", fixed = TRUE)
  expect_match(svg, "k (c)", fixed = TRUE)
})

test_that("shared and independent legends coexist; labellers title them", {
  p <- vplot(df) |>
    mark_point(x = x, y = y, color = v, size = v) |>
    facet_wrap(~g, labeller = label_both) |>
    resolve_scale(color = "independent") |>
    guides(color = guide_colourbar(position = "right"))
  guides <- vellumplot:::.facet_legend_guides(vellumplot:::.build_panels(p))
  titles <- vapply(guides, function(g) g$sc$name, character(1))
  expect_identical(titles, c("v", "v (g: a)", "v (g: b)", "v (g: c)"))
  # colour + size independent and of one variable merge within each panel
  p2 <- vplot(df) |>
    mark_point(x = x, y = y, color = k, shape = k) |>
    facet_wrap(~g) |>
    resolve_scale(color = "independent", shape = "independent")
  g2 <- vellumplot:::.facet_legend_guides(vellumplot:::.build_panels(p2))
  expect_length(g2, 3)
})

test_that("grid panels are titled by their column and row strips", {
  d2 <- rbind(df, transform(df, y = y + 1, k = paste0(k, "2")))
  d2$h <- rep(c("top", "bottom"), each = nrow(df))
  p <- vplot(d2) |>
    mark_point(x = x, y = y, color = k) |>
    facet_grid(h ~ g) |>
    resolve_scale(color = "independent") |>
    guides(color = guide_legend(position = "bottom"))
  guides <- vellumplot:::.facet_legend_guides(vellumplot:::.build_panels(p))
  titles <- vapply(guides, function(g) g$sc$name, character(1))
  expect_length(titles, 6)
  expect_true("k (a, top)" %in% titles)
  expect_no_error(svg_of(p))
})

test_that("guides(color = 'none') hides the per-panel legends too", {
  p <- resolve_scale(base, color = "independent") |> guides(color = "none")
  guides <- vellumplot:::.facet_legend_guides(vellumplot:::.build_panels(p))
  expect_length(guides, 0)
})

test_that("a single panel or an unfaceted plot ignores the resolution", {
  p <- vplot(df) |>
    mark_point(x = x, y = y, color = k) |>
    resolve_scale(color = "independent")
  expect_identical(vellumplot:::.build_panels(p)$independent, character(0))
  expect_identical(
    svg_of(p),
    svg_of(vplot(df) |> mark_point(x = x, y = y, color = k))
  )
})

test_that("mark keys are unchanged by an independent colour scale", {
  keyed <- vplot(df) |>
    mark_point(x = x, y = y, color = k, data_id = paste0("r", x)) |>
    facet_wrap(~g)
  marks_of <- function(p) {
    el <- vellum::scene_model(as_vellum_scene(p))$elements
    k <- el$key[!is.na(el$key)]
    sort(k[!grepl("^legend:", k)])
  }
  expect_identical(
    marks_of(resolve_scale(keyed, color = "independent")),
    marks_of(keyed)
  )
})

test_that("an independent resolution round-trips through the spec", {
  skip_if_not_installed("jsonlite")
  p <- resolve_scale(base, color = "independent")
  p2 <- spec_from_json(spec_to_json(p))
  expect_identical(vellumplot:::.resolve_for(p2, "color"), "independent")
  norm <- function(s) gsub("vl[0-9]+", "vl", s)
  expect_identical(norm(svg_of(p2)), norm(svg_of(p)))
})
