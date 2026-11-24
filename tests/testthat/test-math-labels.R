# Math labels (tex(), plotmath(), md() with $...$) through every place a plot
# draws text (F-31). The typesetting is vellum's; these tests check that
# vellumplot passes the labels through un-coerced and that the plain-text seams
# (alt text, tooltips) carry the TeX source.

text_labels <- function(p) {
  s <- vellum::as_vellum_scene(p)
  nodes <- as.data.frame(vellum:::.scene_to_backend(s)$lint_table())
  nodes$label[nodes$kind == "text"]
}

d <- data.frame(
  x = 1:4,
  y = 1:4,
  g = rep(c("a", "b"), 2),
  f = rep(c("alpha[1]", "sqrt(beta)"), each = 2),
  lab = c("x^2", "hat(y)", "frac(1, n)", "bar(x)")
)

test_that("tex() and plotmath() are re-exported from vellum", {
  expect_identical(tex, vellum::tex)
  expect_identical(plotmath, vellum::plotmath)
})

test_that("math titles, axis and legend names draw as math", {
  p <- vplot(d) |>
    mark_point(x = x, y = y, colour = g) |>
    labs(
      title = md("mass $m_e$ in **kg**"),
      subtitle = tex("\\frac{a}{b}"),
      caption = plotmath("sum(x[i], i==1, n)"),
      x = tex("x_i"),
      y = tex("\\sqrt{y}"),
      colour = tex("\\beta_g")
    )
  labs <- text_labels(p)
  for (src in c(
    "mass $m_e$ in kg",
    "\\frac{a}{b}",
    "x_i",
    "\\sqrt{y}",
    "\\beta_g"
  )) {
    expect_true(src %in% labs, label = src)
  }
  expect_no_error(render_px(p))
})

test_that("a discrete legend takes a list of rich labels", {
  p <- vplot(d) |>
    mark_point(x = x, y = y, colour = g) |>
    scale_colour_discrete(labels = list(tex("\\alpha"), "plain"))
  labs <- text_labels(p)
  expect_true("\\alpha" %in% labs)
  expect_true("plain" %in% labs)
  expect_false("<object>" %in% labs)
  # Named by level, too: unnamed levels keep their name.
  p2 <- vplot(d) |>
    mark_point(x = x, y = y, colour = g) |>
    scale_colour_discrete(labels = list(b = tex("\\frac{1}{2}")))
  expect_true(all(c("a", "\\frac{1}{2}") %in% text_labels(p2)))
  # The legend measures the formula, not a placeholder string.
  expect_gt(vellumplot:::.mm_tw(list(tex("\\frac{1}{2}xxxxxxxx")), 10), 0)
})

test_that("manual scales take a list of rich labels", {
  p <- vplot(d) |>
    mark_point(x = x, y = y, colour = g, shape = g) |>
    scale_colour_manual(
      values = c("red", "blue"),
      labels = list(tex("\\sigma = 1"), "plain")
    ) |>
    scale_shape_manual(
      values = c(16, 17),
      labels = list(tex("\\sigma = 1"), "plain")
    )
  labs <- text_labels(p)
  expect_true("\\sigma = 1" %in% labs)
  expect_true("plain" %in% labs)
  expect_false("<object>" %in% labs)
  expect_no_error(render_px(p))
  expect_error(
    vplot(d) |> scale_colour_manual(values = "red", labels = list(1, 2)),
    "list of labels"
  )
})

test_that("legend swatch tooltips carry the label's text", {
  d$id <- seq_len(nrow(d))
  p <- vplot(d) |>
    mark_point(x = x, y = y, colour = g, data_id = id) |>
    scale_colour_discrete(labels = list(tex("\\alpha"), "b*c*"))
  el <- vellum::scene_model(vellum::as_vellum_scene(p))$elements
  sw <- el[!is.na(el$key) & grepl("^legend:", el$key), ]
  tt <- vapply(sw$meta, function(m) m[["tooltip"]] %||% NA_character_, "")
  expect_setequal(tt, c("\\alpha", "b*c*"))
})

test_that("plain strings beside rich labels are drawn literally", {
  lits <- c("b*c*", "under_score_x_", "$x$ cost", "**no**")
  p <- vplot(d) |>
    mark_point(x = x, y = y, colour = g) |>
    scale_colour_discrete(labels = list(tex("\\alpha"), lits[1]))
  expect_true("b*c*" %in% text_labels(p))
  for (x in lits) {
    labs <- list(tex("\\alpha"), x)
    expect_equal(
      vellumplot:::.mm_tw(labs, 10),
      max(
        vellumplot:::.mm_tw(tex("\\alpha"), 10),
        vellumplot:::.mm_tw(x, 10)
      ),
      tolerance = 1e-6
    )
  }
  # NA and "" entries draw nothing and do not fail.
  p <- vplot(d) |>
    mark_point(x = x, y = y, colour = g) |>
    scale_colour_discrete(labels = list(tex("\\alpha"), NA))
  expect_no_error(render_px(p))
  d3 <- data.frame(x = 1:3, y = 1:3, g = c("a", "b", "c"))
  p <- vplot(d3) |>
    mark_point(x = x, y = y, colour = g) |>
    scale_colour_discrete(labels = list(tex("\\alpha"), NA, ""))
  expect_no_error(render_px(p))
})

test_that("mixed rich / plain label lists draw every label in text marks", {
  d3 <- data.frame(x = 1:3, y = 1:3)
  p <- vplot(d3) |>
    mark_text(x = x, y = y, label = list(tex("\\beta"), "b", md("*c*")))
  labs <- text_labels(p)
  expect_true(all(c("\\beta", "b", "c") %in% labs))
  expect_false(any(grepl("object", labs)))
  p <- vplot(d3) |>
    mark_point(x = x, y = y) |>
    annotate("text", x = 1:2, y = 2, label = list(tex("\\beta"), "plain"))
  labs <- text_labels(p)
  expect_true(all(c("\\beta", "plain") %in% labs))
  expect_false(any(grepl("object", labs)))
})

test_that("mark_label(parse = TRUE) draws nothing for an NA label", {
  d3 <- data.frame(x = 1:3, y = 1:3, f = c("alpha", NA, "beta"))
  p <- vplot(d3) |> mark_label(x = x, y = y, label = f, parse = TRUE)
  kinds <- function(p) {
    s <- vellum:::.scene_to_backend(vellum::as_vellum_scene(p))
    as.data.frame(s$lint_table())$kind
  }
  # Two background boxes for the two labels, none for the NA row.
  expect_equal(sum(kinds(p) == "roundrect"), 2L)
  labs <- text_labels(p)
  expect_true(all(c("\\alpha", "\\beta") %in% labs))
  expect_false("" %in% labs)
})

test_that("mark_text() and mark_label() parse plotmath with parse = TRUE", {
  p <- vplot(d) |> mark_text(x = x, y = y, label = lab, parse = TRUE)
  labs <- text_labels(p)
  expect_true(all(c("x^{2}", "\\hat{y}", "\\frac{1}{n}", "\\bar{x}") %in% labs))
  p <- vplot(d) |> mark_label(x = x, y = y, label = lab, parse = TRUE)
  expect_true("\\frac{1}{n}" %in% text_labels(p))
  expect_no_error(render_px(p))
  # Without parse the strings are drawn as they are.
  p <- vplot(d) |> mark_text(x = x, y = y, label = lab)
  expect_true("frac(1, n)" %in% text_labels(p))
  expect_error(
    vplot(d) |> mark_text(x = x, y = y, label = lab, parse = "yes"),
    "parse"
  )
})

test_that("mark_label() takes rich labels, one or one per row", {
  p <- vplot(d) |>
    mark_label(x = x, y = y, label = tex(c("a_1", "b_2", "c_3", "d_4")))
  expect_true(all(c("a_1", "d_4") %in% text_labels(p)))
  p <- vplot(d) |> mark_label(x = x, y = y, label = tex("\\pi"))
  expect_equal(sum(text_labels(p) == "\\pi"), 4L)
})

test_that("annotate() takes a rich label", {
  p <- vplot(d) |>
    mark_point(x = x, y = y) |>
    annotate("text", x = 2, y = 3, label = tex("\\int_0^1 f")) |>
    annotate("label", x = 3, y = 2, label = md("$\\Delta t$ small"))
  labs <- text_labels(p)
  expect_true("\\int_0^1 f" %in% labs)
  expect_true("$\\Delta t$ small" %in% labs)
})

test_that("label_parsed() typesets facet strips as plotmath", {
  p <- vplot(d) |>
    mark_point(x = x, y = y) |>
    facet_wrap(~f, labeller = label_parsed)
  labs <- text_labels(p)
  expect_true(all(c("\\alpha_{1}", "\\sqrt{\\beta}") %in% labs))
  expect_no_error(render_px(p))
  # A grid with two variables on one side stacks them, or joins them as
  # plotmath's list() with multi_line = FALSE.
  d2 <- transform(d, h = rep(c("x[1]", "x[2]"), 2))
  p <- vplot(d2) |>
    mark_point(x = x, y = y) |>
    facet_grid(f + h ~ ., labeller = label_parsed)
  expect_no_error(render_px(p))
  # Stacked lines never overlap: the strip's box is at least the two line
  # boxes plus TeX's lineskip (0.1 em).
  h <- function(l) vellum::vl_strheight(l, fontsize = 10, unit = "pt")
  st <- vellumplot:::.stack_strip_lines(
    list(plotmath("frac(1, n)"), plotmath("beta"))
  )
  expect_gte(
    h(st),
    h(md("$\\frac{1}{n}$")) + h(md("$\\beta$")) + 1 - 1e-6
  )
  d4 <- data.frame(x = 1:2, y = 1:2, g = "frac(1, n)", h = "beta")
  p <- vplot(d4) |>
    mark_point(x = x, y = y) |>
    facet_wrap(~ g + h, labeller = label_parsed)
  expect_no_error(render_px(p))
  # A `$` in a value survives the stacking.
  d5 <- data.frame(x = 1:2, y = 1:2, g = c("'a$'", "'$'"), h = "beta")
  p <- vplot(d5) |>
    mark_point(x = x, y = y) |>
    facet_wrap(~ g + h, labeller = label_parsed)
  labs <- text_labels(p)
  expect_true(any(grepl("a\\$", labs, fixed = TRUE)))
  expect_no_error(render_px(p))
  out <- label_parsed(data.frame(a = "alpha", b = "beta"), multi_line = FALSE)
  expect_equal(out[[1]][[1]]@tex, "\\alpha, \\beta")
  # It survives the spec.
  p <- vplot(d) |>
    mark_point(x = x, y = y) |>
    facet_wrap(~f, labeller = label_parsed)
  back <- from_spec(as_spec(p))
  expect_true("\\alpha_{1}" %in% text_labels(back))
})

test_that("alt text and the accessible name use the labels' source", {
  p <- vplot(d) |>
    mark_point(x = x, y = y, colour = g) |>
    labs(title = tex("E = mc^2"), x = tex("x_i"), colour = md("*group*"))
  expect_equal(vellumplot:::.alt_name(p@labels), "E = mc^2")
  alt <- vellumplot:::.plot_alt(p)
  expect_match(alt, "x_i", fixed = TRUE)
  expect_match(alt, "colour shows group", fixed = TRUE)
})
