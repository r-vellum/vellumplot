# scale_*_brewer() / scale_*_distiller() draw real ColorBrewer palettes (via
# scales::pal_brewer()), not only the hcl.pals() names.

train <- function(p) {
  .train_scales(p, .resolve_layers(p))
}

brewer_names <- c(
  "Blues",
  "BuGn",
  "BuPu",
  "GnBu",
  "Greens",
  "Greys",
  "Oranges",
  "OrRd",
  "PuBu",
  "PuBuGn",
  "PuRd",
  "Purples",
  "RdPu",
  "Reds",
  "YlGn",
  "YlGnBu",
  "YlOrBr",
  "YlOrRd",
  "BrBG",
  "PiYG",
  "PRGn",
  "PuOr",
  "RdBu",
  "RdGy",
  "RdYlBu",
  "RdYlGn",
  "Spectral",
  "Accent",
  "Dark2",
  "Paired",
  "Pastel1",
  "Pastel2",
  "Set1",
  "Set2",
  "Set3"
)

test_that("scale_color_brewer() accepts Set1 (the reported repro)", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, color = factor(cyl)) |>
    scale_color_brewer(palette = "Set1")
  expect_no_error(vellum::as_vellum_scene(p))
  expect_identical(train(p)$color$colors, c("#E41A1C", "#377EB8", "#4DAF4A"))
})

test_that("every ColorBrewer palette compiles with k distinct colours", {
  d <- data.frame(x = 1:5, y = 1:5, g = letters[1:5])
  for (pal in brewer_names) {
    for (k in c(3L, 5L)) {
      dk <- d[seq_len(k), ]
      pc <- vplot(dk) |>
        mark_point(x = x, y = y, color = g) |>
        scale_color_brewer(palette = pal)
      pf <- vplot(dk) |>
        mark_bar(x = g, y = y, fill = g) |>
        scale_fill_brewer(palette = pal)
      expect_no_error(vellum::as_vellum_scene(pc))
      expect_no_error(vellum::as_vellum_scene(pf))
      cols <- train(pc)$color$colors
      expect_identical(cols, scales::pal_brewer(palette = pal)(k), info = pal)
      expect_length(unique(cols), k)
      expect_identical(train(pf)$color$colors, cols, info = pal)
    }
  }
})

test_that("ColorBrewer names match loosely; type/palette index and direction", {
  base <- vplot(mtcars) |> mark_point(x = wt, y = mpg, color = factor(cyl))
  set1 <- train(base |> scale_color_brewer(palette = "Set1"))$color$colors
  expect_identical(
    train(base |> scale_color_brewer(palette = "set 1"))$color$colors,
    set1
  )
  expect_identical(
    train(base |> scale_color_brewer(type = "qual", palette = 6))$color$colors,
    set1
  )
  expect_identical(
    train(
      base |> scale_color_brewer(palette = "Set1", direction = -1)
    )$color$colors,
    rev(set1)
  )
  expect_snapshot(error = TRUE, {
    scale_color_brewer(base, type = "qual", palette = 99)
    scale_color_brewer(base, palette = "Set1", direction = 2)
  })
})

test_that("more levels than a ColorBrewer palette has recycle with a warning", {
  d <- data.frame(x = 1:10, y = 1:10, g = letters[1:10])
  p <- vplot(d) |>
    mark_point(x = x, y = y, color = g) |>
    scale_color_brewer(palette = "Set2")
  expect_warning(cols <- train(p)$color$colors, "at most 8")
  expect_length(cols, 10)
})

test_that("scale_*_distiller() interpolates the ColorBrewer ramp", {
  base <- vplot(mtcars) |> mark_point(x = wt, y = mpg, color = hp)
  for (pal in c("Spectral", "RdBu", "YlOrRd", "Set1")) {
    p <- base |> scale_color_distiller(palette = pal)
    expect_no_error(vellum::as_vellum_scene(p))
    pal256 <- train(p)$color$pal256
    stops <- rev(scales::pal_brewer(palette = pal)(7))
    expect_identical(toupper(pal256[c(1, 256)]), stops[c(1, 7)], info = pal)
  }
  up <- train(base |> scale_color_distiller(palette = "Blues", direction = 1))
  expect_identical(toupper(up$color$pal256[1]), "#EFF3FF")
  pf <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, fill = hp, shape = "circle") |>
    scale_fill_distiller(palette = "PuOr")
  expect_no_error(vellum::as_vellum_scene(pf))
})

test_that("hcl.pals() names still work in the brewer/distiller scales", {
  base <- vplot(mtcars) |> mark_point(x = wt, y = mpg, color = factor(cyl))
  p <- base |> scale_color_brewer(palette = "Dark 3")
  expect_no_error(vellum::as_vellum_scene(p))
  expect_identical(train(p)$color$colors, grDevices::hcl.colors(3, "Dark 3"))
  pc <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, color = hp) |>
    scale_color_distiller(palette = "Batlow")
  expect_identical(
    train(pc)$color$pal256,
    train(
      vplot(mtcars) |>
        mark_point(x = wt, y = mpg, color = hp) |>
        scale_color_continuous(palette = "Batlow")
    )$color$pal256
  )
})

test_that("scale_color_discrete() also accepts a ColorBrewer-only name", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, color = factor(cyl)) |>
    scale_color_discrete(palette = "Paired")
  expect_identical(
    train(p)$color$colors,
    scales::pal_brewer(palette = "Paired")(3)
  )
})

test_that("a brewer scale survives spec serialization", {
  skip_if_not_installed("jsonlite")
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, color = factor(cyl)) |>
    scale_color_brewer(palette = "Set1")
  rt <- spec_from_json(spec_to_json(p), data = mtcars)
  expect_identical(train(rt)$color$colors, train(p)$color$colors)
})
