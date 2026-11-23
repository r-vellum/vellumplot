# VP-58: a theme's default mark colours (`theme(geom = element_geom())`), so a
# dark theme draws unmapped marks in a light ink.

# The distinct stroke / fill colours drawn inside the plot's layer groups
# (`layer-*`), read off the compiled scene's values.
geom_paints <- function(p) {
  s <- vellum::as_scene_spec(vellum::as_vellum_scene(p))
  out <- list(col = character(), fill = character())
  grab <- function(node, inside) {
    id <- node$id
    if (is.list(id)) {
      id <- id$v
    }
    if (is.character(id) && length(id) == 1L && grepl("^layer-", id)) {
      inside <- TRUE
    }
    if (inside && is.list(node$gp)) {
      for (k in c("col", "fill")) {
        v <- node$gp[[k]]
        if (is.list(v)) {
          v <- v$v
        }
        if (is.character(v)) {
          out[[k]] <<- c(out[[k]], v)
        }
      }
    }
    for (ch in node) {
      if (is.list(ch)) grab(ch, inside)
    }
  }
  grab(s$root, FALSE)
  lapply(out, function(v) unique(tolower(v)))
}

# Every stroke / fill colour anywhere in the scene (legend keys included).
scene_paints <- function(p) {
  s <- unlist(vellum::as_scene_spec(vellum::as_vellum_scene(p)))
  unique(tolower(s[grepl("gp\\.(col|fill)(\\.v)?$", names(s))]))
}

geom_pt <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
geom_bar <- vplot(mtcars) |> mark_bar(x = factor(cyl))
cp_ink <- "#c7d0f0"
cp_paper <- "#0d0f18"

test_that("element_geom() builds and validates a geom element", {
  g <- element_geom(ink = "white", paper = "black", color = "red")
  expect_true(S7::S7_inherits(g, vellumplot:::.element_geom))
  expect_identical(g@ink, "white")
  expect_identical(g@colour, "red")
  expect_null(g@accent)
  expect_error(element_geom(ink = "not-a-colour"), "single colour")
  expect_error(element_geom(paper = c("red", "blue")), "single colour")
  expect_error(theme(geom_pt, geom = element_line()), "element_geom")
})

test_that("theme(geom =) merges onto the theme's geom element", {
  p <- geom_pt |>
    theme(geom = element_geom(ink = "red")) |>
    theme(geom = element_geom(paper = "navy"))
  expect_identical(p@theme$geom@ink, "red")
  expect_identical(p@theme$geom@paper, "navy")
})

test_that("light themes keep the built-in mark colours", {
  expect_identical(geom_paints(geom_pt)$col, "black")
  expect_identical(geom_paints(geom_bar)$fill, "grey35")
  box <- vplot(mtcars) |> mark_boxplot(x = factor(cyl), y = mpg)
  expect_true("white" %in% geom_paints(box)$fill)
  sm <- geom_pt |> mark_smooth(x = wt, y = mpg)
  expect_true("#3366cc" %in% geom_paints(sm)$col)
  for (th in list(theme_minimal, theme_bw, theme_classic, theme_dark)) {
    expect_identical(geom_paints(th(geom_pt))$col, "black")
    expect_identical(geom_paints(th(geom_bar))$fill, "grey35")
  }
  # the default sketch paper / ink do not recolour the marks
  expect_identical(geom_paints(theme_sketch(geom_pt))$col, "black")
})

test_that("theme_cyberpunk() draws unmapped marks in its light ink", {
  expect_identical(geom_paints(theme_cyberpunk(geom_pt))$col, cp_ink)
  # bars: ggplot2's col_mix(ink, paper, 0.35)
  expect_identical(
    geom_paints(theme_cyberpunk(geom_bar))$fill,
    tolower(vellumplot:::.col_mix(cp_ink, cp_paper, 0.35))
  )
  sm <- geom_pt |> mark_smooth(x = wt, y = mpg) |> theme_cyberpunk()
  expect_true("#08f7fe" %in% geom_paints(sm)$col)
  # text in ink; a label's background in paper
  lab <- vplot(head(mtcars)) |>
    mark_label(x = wt, y = mpg, label = cyl) |>
    theme_cyberpunk()
  pt <- geom_paints(lab)
  expect_true(cp_ink %in% pt$col)
  expect_true(cp_paper %in% pt$fill)
  box <- vplot(mtcars) |>
    mark_boxplot(x = factor(cyl), y = mpg) |>
    theme_cyberpunk()
  expect_false("white" %in% geom_paints(box)$fill)
})

test_that("constants and mapped scales win over the theme's defaults", {
  red <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, color = "red") |>
    theme_cyberpunk()
  expect_identical(geom_paints(red)$col, "red")
  filled <- vplot(mtcars) |>
    mark_bar(x = factor(cyl), fill = "steelblue") |>
    theme_cyberpunk()
  expect_identical(geom_paints(filled)$fill, "steelblue")
  mapped <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, color = factor(cyl)) |>
    theme_cyberpunk()
  cols <- geom_paints(mapped)$col
  expect_false(cp_ink %in% cols)
  expect_true(all(cols %in% tolower(vellumplot:::.NEON_QUAL)))
})

test_that("element_geom(colour =, fill =) override every unmapped default", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    mark_bar(x = factor(cyl)) |>
    theme(geom = element_geom(colour = "orange", fill = "purple"))
  pt <- geom_paints(p)
  expect_true("orange" %in% pt$col)
  expect_true("purple" %in% pt$fill)
  expect_false("grey35" %in% pt$fill)
  # accent alone recolours smooths, leaving the rest as is
  sm <- geom_pt |>
    mark_smooth(x = wt, y = mpg) |>
    theme(geom = element_geom(accent = "darkgreen"))
  sp <- geom_paints(sm)
  expect_true(all(c("black", "darkgreen") %in% sp$col))
  expect_false("#3366cc" %in% sp$col)
})

test_that("a custom theme_sketch() ink and paper colour the marks", {
  p <- theme_sketch(geom_pt, ink = "#eeeeee", paper = "#222222")
  expect_identical(geom_paints(p)$col, "#eeeeee")
})

test_that("legend keys of unmapped colour follow the theme", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, size = hp) |>
    theme_cyberpunk()
  # point keys take the points' default, the theme's ink (VP-109)
  expect_true(tolower(cp_ink) %in% scene_paints(p))
  expect_false("grey35" %in% scene_paints(p))
})

test_that("an aligned composition colours each cell by its own theme", {
  comp <- wrap_plots(list(geom_pt, theme_cyberpunk(geom_pt)))
  paints <- scene_paints(comp)
  expect_true(all(c("black", cp_ink) %in% paints))
})

test_that(".col_mix() matches ggplot2's col_mix()", {
  expect_identical(vellumplot:::.col_mix("black", "white", 0.35), "#595959")
  expect_identical(vellumplot:::.col_mix("black", "white", 0), "black")
  expect_identical(vellumplot:::.col_mix("black", "white", 1), "white")
})

# VP-60: areas, ribbons, densities, tiles and rasters fill with ggplot2's
# col_mix(ink, paper, 0.2), not grey50 (theme_dark's panel colour).
vp60_df <- data.frame(x = 1:5, y = c(1, 3, 2, 4, 3))
vp60_grid <- expand.grid(x = 1:3, y = 1:3)
vp60_plots <- list(
  area = vplot(vp60_df) |> mark_area(x = x, y = y),
  ribbon = vplot(vp60_df) |> mark_ribbon(x = x, ymin = y - 1, ymax = y + 1),
  density = vplot(mtcars) |> mark_density(x = mpg),
  tile = vplot(vp60_grid) |> mark_tile(x = x, y = y)
)

test_that("unmapped bands and tiles fill with col_mix(ink, paper, 0.2)", {
  for (nm in names(vp60_plots)) {
    p <- vp60_plots[[nm]]
    expect_identical(geom_paints(p)$fill, "grey20", info = nm)
    # theme_dark keeps black ink, so the fill stays clear of its grey50 panel
    expect_identical(geom_paints(theme_dark(p))$fill, "grey20", info = nm)
    expect_identical(
      geom_paints(theme_cyberpunk(p))$fill,
      tolower(vellumplot:::.col_mix(cp_ink, cp_paper, 0.2)),
      info = nm
    )
  }
  ras <- vplot(vp60_grid) |> mark_raster(x = x, y = y)
  ras_vals <- unlist(vellum::as_scene_spec(vellum::as_vellum_scene(ras)))
  expect_true("grey20" %in% ras_vals)
  expect_false("grey50" %in% ras_vals)
})

test_that("default band and tile fills match ggplot2's layer_data()", {
  skip_if_not_installed("ggplot2", "4.0.0")
  hex <- function(x) {
    m <- grDevices::col2rgb(x)
    sprintf("#%02X%02X%02X", m[1], m[2], m[3])
  }
  gg <- list(
    area = ggplot2::geom_area(ggplot2::aes(x, y), data = vp60_df),
    ribbon = ggplot2::geom_ribbon(
      ggplot2::aes(x, ymin = y - 1, ymax = y + 1),
      data = vp60_df
    ),
    tile = ggplot2::geom_tile(ggplot2::aes(x, y), data = vp60_grid)
  )
  for (nm in names(gg)) {
    for (th in list(ggplot2::theme_grey(), ggplot2::theme_dark())) {
      ld <- ggplot2::layer_data(ggplot2::ggplot() + gg[[nm]] + th)
      expect_identical(
        hex(unique(ld$fill)),
        hex(geom_paints(vp60_plots[[nm]])$fill),
        info = nm
      )
    }
  }
})
