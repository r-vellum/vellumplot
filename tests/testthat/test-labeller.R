# Facet strip labellers (F-10).

p <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
fa <- function(plot) .facet_assign(plot)

test_that("the labeller functions follow ggplot2's interface", {
  df <- data.frame(cyl = c("4", "6"), am = c("0", "1"))
  expect_identical(label_value(df), list(cyl = c("4", "6"), am = c("0", "1")))
  expect_identical(label_value(df, multi_line = FALSE), list(c("4, 0", "6, 1")))
  expect_identical(
    label_both(df),
    list(cyl = c("cyl: 4", "cyl: 6"), am = c("am: 0", "am: 1"))
  )
  expect_identical(
    label_both(df, multi_line = FALSE),
    list(c("cyl, am: 4, 0", "cyl, am: 6, 1"))
  )
  expect_identical(
    label_both(df["cyl"], sep = " = ")[[1]],
    c("cyl = 4", "cyl = 6")
  )
  long <- data.frame(g = "a rather long group name")
  expect_identical(
    label_wrap_gen(10)(long),
    list(g = "a rather\nlong\ngroup\nname")
  )
  expect_error(label_wrap_gen(0), "positive number")
})

test_that("as_labeller() builds lookups and value mappers", {
  df <- data.frame(am = c("0", "1"))
  lk <- as_labeller(c(`0` = "automatic", `1` = "manual"))
  expect_true(inherits(lk, "labeller"))
  expect_identical(lk(df), list(am = c("automatic", "manual")))
  # values the lookup does not name fall back to the default labeller
  part <- as_labeller(c(`0` = "automatic"), default = label_both)
  expect_identical(part(df), list(am = c("automatic", "am: 1")))
  expect_identical(as_labeller(toupper)(data.frame(g = "a")), list(g = "A"))
  # a labeller passes through unchanged
  expect_identical(as_labeller(label_both), label_both)
  expect_error(as_labeller(1:3), "named character vector")
})

test_that("facet_wrap() strips use the labeller", {
  expect_identical(
    fa(p |> facet_wrap(~cyl, labeller = label_both))$wrap_labels,
    c("cyl: 4", "cyl: 6", "cyl: 8")
  )
  # several variables: one line each, or joined with multi_line = FALSE
  two <- fa(p |> facet_wrap(~ cyl + am, labeller = label_both))$wrap_labels
  expect_identical(two[1:2], c("cyl: 4\nam: 0", "cyl: 4\nam: 1"))
  expect_identical(
    fa(
      p |>
        facet_wrap(
          ~ cyl + am,
          labeller = .new_labeller(function(l) {
            label_value(l, multi_line = FALSE)
          })
        )
    )$wrap_labels[1:2],
    c("4, 0", "4, 1")
  )
  # the default is unchanged
  expect_identical(fa(p |> facet_wrap(~ cyl + am))$wrap_labels[1], "4, 0")
  # a named vector is a lookup table
  expect_identical(
    fa(
      p |> facet_wrap(~am, labeller = c(`0` = "automatic", `1` = "manual"))
    )$wrap_labels,
    c("automatic", "manual")
  )
})

test_that("facet_grid() labels both the row and the column strips", {
  g <- fa(p |> facet_grid(am ~ cyl, labeller = label_both))
  expect_identical(g$col_labels, c("cyl: 4", "cyl: 6", "cyl: 8"))
  expect_identical(g$row_labels, c("am: 0", "am: 1"))
  # one side only
  g1 <- fa(p |> facet_grid(. ~ cyl, labeller = label_both))
  expect_identical(g1$col_labels, c("cyl: 4", "cyl: 6", "cyl: 8"))
  expect_null(g1$row_labels)
})

test_that("labelled strips reach the rendered scene", {
  svg <- paste(
    vellum::scene_svg(vellum::as_vellum_scene(
      p |> facet_grid(am ~ cyl, labeller = label_both)
    )),
    collapse = ""
  )
  expect_match(svg, "cyl: 4", fixed = TRUE)
  expect_match(svg, "am: 1", fixed = TRUE)
})

test_that("a multi-line strip gets a taller strip track", {
  df <- data.frame(x = 1:2, y = 1:2, g = c("a rather long group name", "b"))
  q <- vplot(df) |> mark_point(x = x, y = y)
  expect_identical(.strip_probe(c("a", "b\nc\nd")), "Ag\nAg\nAg")
  expect_identical(.strip_probe(NULL), "Ag")
  expect_no_error(vellum::as_vellum_scene(
    q |> facet_wrap(~g, labeller = label_wrap_gen(10))
  ))
})

test_that("a bad labeller is refused; a custom function is not serialized", {
  expect_error(facet_wrap(p, ~cyl, labeller = 1), "labeller")
  expect_error(facet_grid(p, am ~ cyl, labeller = "x"), "labeller")
  expect_error(
    as_spec(p |> facet_wrap(~cyl, labeller = toupper)),
    class = "vellumplot_unserializable"
  )
  expect_error(
    as_spec(p |> facet_wrap(~cyl, labeller = function(labels) labels)),
    class = "vellumplot_unserializable"
  )
  # a lookup over a custom default cannot be described either
  expect_error(
    as_spec(
      p |> facet_wrap(~am, labeller = as_labeller(c(`0` = "a"), toupper))
    ),
    class = "vellumplot_unserializable"
  )
})

test_that("declarative labellers round-trip through the spec (VP-44)", {
  skip_if_not_installed("jsonlite")
  strip_svg <- function(q) {
    gsub("vl[0-9]+", "vl", vellum::scene_svg(vellum::as_vellum_scene(q)))
  }
  df <- data.frame(
    x = 1:4,
    y = 1:4,
    g = rep(c("a rather long group name", "short"), 2)
  )
  q <- vplot(df) |> mark_point(x = x, y = y)
  cases <- list(
    value = p |> facet_wrap(~cyl, labeller = label_value),
    both = p |> facet_grid(am ~ cyl, labeller = label_both),
    wrap = q |> facet_wrap(~g, labeller = label_wrap_gen(10)),
    wrap_single = q |>
      facet_wrap(~g, labeller = label_wrap_gen(8, multi_line = FALSE)),
    named_vector = p |>
      facet_wrap(~am, labeller = c(`0` = "automatic", `1` = "manual")),
    lookup_both = p |>
      facet_wrap(~am, labeller = as_labeller(c(`0` = "automatic"), label_both)),
    lookup_wrap = q |>
      facet_wrap(
        ~g,
        labeller = as_labeller(c(short = "S"), label_wrap_gen(10))
      )
  )
  for (nm in names(cases)) {
    p1 <- cases[[nm]]
    s <- as_spec(p1)
    expect_false(is.null(s$facet$labeller), label = nm)
    p2 <- from_spec(s)
    expect_identical(strip_svg(p2), strip_svg(p1), label = nm)
    p3 <- spec_from_json(spec_to_json(p1))
    expect_identical(strip_svg(p3), strip_svg(p1), label = nm)
    expect_identical(as_spec(p3)$facet, s$facet, label = nm)
  }
  s <- as_spec(cases$lookup_both)
  expect_identical(s$facet$labeller$type, "lookup")
  expect_identical(s$facet$labeller$values, list(`0` = "automatic"))
  expect_identical(s$facet$labeller$default, list(type = "both"))
  expect_identical(
    as_spec(cases$wrap)$facet$labeller,
    list(type = "wrap", width = 10, multi_line = TRUE)
  )
})
