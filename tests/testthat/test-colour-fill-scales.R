# VP-48: `color` and `fill` are separate scales with separate legends, merged
# into one legend when they map the same variable under the same title and
# keys -- as ggplot2.

train <- function(p) .train_scales(p, .resolve_layers(p))
kinds <- function(p) vapply(.legend_guides(train(p)), function(g) g$kind, "")

test_that("scale_fill_*() and scale_color_*() set their scales independently", {
  p <- vplot(mtcars) |>
    mark_bar(x = factor(cyl), fill = factor(gear), color = factor(gear)) |>
    scale_fill_manual(values = c("white", "grey70", "grey30")) |>
    scale_color_manual(values = c("red", "darkgreen", "blue"))
  sc <- train(p)
  expect_identical(sc$fill$colors, c("white", "grey70", "grey30"))
  expect_identical(sc$color$colors, c("red", "darkgreen", "blue"))
  # one variable, one title, the same keys: one legend whose keys carry both
  g <- .legend_guides(sc)
  expect_length(g, 1L)
  expect_identical(g[[1]]$kind, "merged")
  expect_identical(g[[1]]$sc$fills, c("white", "grey70", "grey30"))
  expect_identical(g[[1]]$sc$cols, c("red", "darkgreen", "blue"))
  expect_true(g[[1]]$sc$outlined)
  # the bars are filled by the fill scale and outlined by the colour scale
  svg <- plot_svg(p)
  expect_match(svg, 'fill="#4d4d4d"/><path d="[^"]*" fill="none" stroke="#0000ff"')
})

test_that("a fill scale does not reach a colour mapping (as ggplot2)", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, color = factor(cyl)) |>
    scale_fill_manual(values = c("red", "blue", "green"))
  sc <- train(p)
  expect_null(sc$fill)
  expect_false(identical(sc$color$colors, c("red", "blue", "green")))
})

test_that("colour and fill legends merge only on the same title and keys", {
  base <- vplot(mtcars) |>
    mark_bar(x = factor(cyl), fill = factor(gear), color = factor(gear))
  expect_identical(kinds(base), "merged")
  # another title keeps them apart
  expect_identical(
    kinds(base |> labs(fill = "Gears")),
    c("color_discrete", "color_discrete")
  )
  # so do other breaks
  expect_length(kinds(base |> scale_fill_discrete(breaks = c("3", "4"))), 2L)
  # and another variable
  other <- vplot(mtcars) |>
    mark_boxplot(x = factor(cyl), y = mpg, fill = factor(cyl), color = factor(am))
  expect_length(kinds(other), 2L)
})

test_that("a merged colour + fill + shape key keeps every encoding", {
  p <- vplot(mtcars) |>
    mark_point(
      x = wt,
      y = mpg,
      fill = factor(cyl),
      color = factor(cyl),
      shape = factor(cyl)
    ) |>
    scale_color_manual(values = c("black", "grey40", "grey70"))
  g <- .legend_guides(train(p))
  expect_length(g, 1L)
  expect_identical(g[[1]]$sc$cols, c("black", "grey40", "grey70"))
  expect_length(unique(g[[1]]$sc$fills), 3L)
  expect_length(g[[1]]$sc$shapes, 3L)
})

test_that("a merged legend keeps a filled mark's constant outline (#243)", {
  p <- vplot(mtcars) |>
    mark_bar(
      x = factor(gear),
      fill = factor(gear),
      alpha = factor(gear),
      color = "black"
    )
  g <- suppressWarnings(.legend_guides(train(p)))
  expect_length(g, 1L)
  expect_identical(g[[1]]$sc$cols, rep("black", 3))
  expect_true(g[[1]]$sc$outlined)
  grob <- .merged_key_grob(g[[1]]$sc, 1L, list(key = 5), NULL, list())
  expect_identical(grob@gp@col, "black")
})

test_that("interactive legend keys stay unique with two discrete legends", {
  d <- data.frame(x = c("a", "b", "a", "b"), y = 1:4, g = c("p", "q", "q", "p"))
  # a lone fill keys "color:<level>", as before VP-48
  lone <- vplot(d) |> mark_bar(x = x, y = y, fill = x, stat = "identity", data_id = x)
  expect_identical(train(lone)$fill$series_aes, NULL)
  expect_match(plot_svg(lone), "legend:color:a", fixed = TRUE)
  # beside a separate colour legend, the fill keys are "fill:<level>"
  both <- vplot(d) |>
    mark_bar(x = x, y = y, fill = x, color = g, stat = "identity", data_id = x)
  svg <- plot_svg(both)
  expect_match(svg, "legend:fill:a", fixed = TRUE)
  expect_match(svg, "legend:color:p", fixed = TRUE)
  m <- vellum::scene_model(vellum::as_vellum_scene(both))$elements
  legend <- unlist(lapply(m$meta, function(x) x$legend))
  expect_true(all(c("fill:a", "color:p") %in% legend))
  # merged (one variable): one set of "color:<level>" keys
  same <- vplot(d) |>
    mark_bar(x = x, y = y, fill = x, color = x, stat = "identity", data_id = x)
  svg <- plot_svg(same)
  expect_match(svg, "legend:color:a", fixed = TRUE)
  expect_false(grepl("legend:fill:", svg, fixed = TRUE))
})

test_that("guides(fill =) and lims(fill =) reach the fill scale", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, fill = factor(gear), color = factor(am)) |>
    guides(fill = "none")
  expect_identical(kinds(p), "color_discrete")
  expect_identical(.legend_guides(train(p))[[1]]$sc$name, "factor(am)")
  l <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, fill = hp, color = hp) |>
    lims(fill = c(0, 400))
  sc <- train(l)
  expect_identical(sc$fill$range, c(0, 400))
  expect_identical(sc$color$range, range(mtcars$hp))
})

test_that("a plot with both scales round-trips through the spec", {
  p <- vplot(mtcars) |>
    mark_bar(x = factor(cyl), fill = factor(gear), color = factor(gear)) |>
    scale_fill_brewer(palette = "Set2") |>
    scale_color_manual(values = c("red", "darkgreen", "blue")) |>
    labs(fill = "Gears", color = "Gears")
  s1 <- as_spec(p)
  p2 <- from_spec(s1)
  s2 <- as_spec(p2)
  expect_identical(s1[setdiff(names(s1), "data")], s2[setdiff(names(s2), "data")])
  expect_setequal(
    vapply(s1$scales, function(s) s$aesthetic, ""),
    c("fill", "color")
  )
  expect_identical(p2@labels$fill, "Gears")
  expect_identical(train(p2)$fill$colors, train(p)$fill$colors)
  expect_identical(train(p2)$color$colors, c("red", "darkgreen", "blue"))
})
