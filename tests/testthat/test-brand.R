# theme_brand() and brand.yml auto-apply (F-26).

train_brand <- function(p) {
  vellumplot:::.train_scales(p, vellumplot:::.resolve_layers(p))
}

# Write a brand.yml file into a fresh temp dir (removed after the test).
local_brand <- function(lines, name = "_brand.yml", env = parent.frame()) {
  dir <- local_tempfile(.local_envir = env)
  dir.create(dir)
  path <- file.path(dir, name)
  writeLines(lines, path)
  path
}

palette_brand <- c(
  "color:",
  "  palette:",
  "    navy: '#1f3b5a'",
  "    coral: '#ef6f6c'",
  "    sand: '#e0c48c'",
  "  foreground: navy",
  "  background: '#fffdf7'",
  "  primary: coral",
  "  secondary: sand"
)

keys_brand <- c(
  "color:",
  "  foreground: '#111111'",
  "  background: '#ffffff'",
  "  primary: '#0055aa'",
  "  secondary: '#aa5500'",
  "  success: '#0055aa'",
  "  danger: '#cc0000'"
)

dark_brand <- c(
  "color:",
  "  foreground: '#eeeeee'",
  "  background: '#111111'",
  "  primary: '#ff8800'"
)

# A fake Quarto execute-info file, shaped like Quarto 1.9's: the resolved
# brand sits in format$render$brand$light|dark$processedData.
local_execute_info <- function(brand = NULL, metadata = list(), env = parent.frame()) {
  skip_if_not_installed("jsonlite")
  path <- local_tempfile(fileext = ".json", .local_envir = env)
  render <- list(`keep-md` = FALSE)
  if (!is.null(brand)) {
    render$brand <- brand
  }
  info <- list(
    `document-path` = "doc.qmd",
    format = list(render = render, metadata = metadata)
  )
  jsonlite::write_json(info, path, auto_unbox = TRUE, null = "null")
  path
}

quarto_entry <- function(color, palette = NULL, typography = NULL) {
  data_col <- color
  if (!is.null(palette)) {
    data_col <- c(list(palette = palette), color)
  }
  list(
    data = list(color = data_col, typography = typography),
    brandDir = tempdir(),
    projectDir = tempdir(),
    processedData = list(
      color = c(palette, color),
      typography = typography
    )
  )
}

local_brand_state <- function(env = parent.frame()) {
  skip_if_not_installed("withr")
  vellumplot:::.brand_cache_reset()
  withr::defer(vellumplot:::.brand_cache_reset(), envir = env)
  withr::local_options(vellumplot.brand = NULL, .local_envir = env)
  withr::local_envvar(
    QUARTO_EXECUTE_INFO = NA,
    BRAND_YML_PATH = NA,
    .local_envir = env
  )
}

pts <- function() {
  vplot(mtcars) |> mark_point(x = wt, y = mpg, color = factor(cyl))
}

# --- the mapping --------------------------------------------------------------

test_that("theme_brand maps background, foreground and primary", {
  skip_if_not_installed("brand.yml")
  local_brand_state()
  p <- theme_brand(pts(), local_brand(palette_brand))
  th <- p@theme
  expect_identical(th$plot.background@fill, "#fffdf7")
  expect_identical(th$plot.title@colour, "#1f3b5a")
  expect_identical(th$geom@ink, "#1f3b5a")
  expect_identical(th$geom@paper, "#fffdf7")
  expect_identical(th$geom@accent, "#ef6f6c")
  # greys are foreground/background blends
  expect_identical(
    th$axis.text@colour,
    vellumplot:::.col_mix("#1f3b5a", "#fffdf7", 0.4)
  )
  expect_identical(attr(th, "vp_preset"), "minimal")
  expect_match(attr(th, "vp_brand"), "_brand.yml$")
  # theme_minimal's blank ticks and panel stay blank
  expect_true(vellumplot:::.is_blank(th$axis.ticks))
  expect_true(vellumplot:::.is_blank(th$panel.background))
  expect_no_error(render_px(p))
})

test_that("a dark brand gives a dark plot", {
  skip_if_not_installed("brand.yml")
  local_brand_state()
  p <- theme_brand(pts(), local_brand(dark_brand), base = theme_bw)
  th <- p@theme
  expect_identical(th$plot.background@fill, "#111111")
  expect_identical(th$panel.background@fill, "#111111")
  expect_identical(attr(th, "vp_preset"), "bw")
  px <- render_px(p)
  expect_lt(mean(px[5, 5, 1:3]), 0.1)
})

test_that("the palette follows color.palette order, unnamed and padded", {
  skip_if_not_installed("brand.yml")
  local_brand_state()
  p <- theme_brand(pts(), local_brand(palette_brand))
  pal <- p@theme[["palette"]]
  expect_null(names(pal))
  expect_identical(pal[1:3], c("#1f3b5a", "#ef6f6c", "#e0c48c"))
  expect_gte(length(pal), 24L)
  expect_false(anyDuplicated(toupper(pal)) > 0)
  # the levels take the brand colours in order (a named palette would have
  # turned every level grey50)
  sc <- train_brand(p)
  expect_identical(sc$color$map("4"), "#1f3b5a")
  expect_identical(sc$color$map("8"), "#e0c48c")
  # more levels than brand colours: no recycling
  many <- data.frame(x = 1:8, y = 1:8, g = letters[1:8])
  q <- vplot(many) |>
    mark_point(x = x, y = y, color = g) |>
    theme_brand(local_brand(palette_brand))
  cols <- vapply(letters[1:8], train_brand(q)$color$map, character(1))
  expect_false(anyDuplicated(cols) > 0)
  # a scale palette still wins
  r <- p |> scale_color_discrete(palette = "Blues")
  expect_false(identical(train_brand(r)$color$map("4"), "#1f3b5a"))
  # palette = FALSE keeps the default
  expect_null(theme_brand(pts(), local_brand(palette_brand), palette = FALSE)@theme[["palette"]])
})

test_that("without color.palette the palette is the distinct theme colours", {
  skip_if_not_installed("brand.yml")
  local_brand_state()
  pal <- theme_brand(pts(), local_brand(keys_brand))@theme[["palette"]]
  # primary, secondary, then danger (success repeats primary)
  expect_identical(pal[1:3], c("#0055aa", "#aa5500", "#cc0000"))
})

test_that("brand fonts map to text and headings; missing ones fall back", {
  skip_if_not_installed("brand.yml")
  local_brand_state()
  fam <- systemfonts::system_fonts()$family[1]
  path <- local_brand(c(
    keys_brand,
    "typography:",
    paste0("  base: '", fam, "'"),
    "  headings:",
    "    family: 'No Such Font F26'"
  ))
  expect_warning(
    p <- theme_brand(pts(), path),
    "No Such Font F26"
  )
  expect_identical(p@theme$text@family, fam)
  expect_null(p@theme$plot.title@family)
  q <- suppressWarnings(theme_brand(pts(), path, fonts = FALSE))
  expect_null(q@theme$text@family)
})

test_that("a file font is registered with fetch_fonts = TRUE", {
  skip_if_not_installed("brand.yml")
  skip_if_not_installed("systemfonts", "1.2.0")
  local_brand_state()
  src <- system.file("fonts", package = "vellum")
  ttf <- list.files(src, "\\.(ttf|otf)$", full.names = TRUE, recursive = TRUE)
  skip_if(length(ttf) == 0L, "no bundled font file to register")
  path <- local_brand(c(
    keys_brand,
    "typography:",
    "  fonts:",
    "    - family: F26 Brand Face",
    "      source: file",
    "      files:",
    "        - path: brandface.otf",
    "  base: F26 Brand Face"
  ))
  file.copy(ttf[1], file.path(dirname(path), "brandface.otf"))
  # systemfonts registers the file under its own internal family name, so the
  # check is that fetching ran without error and fell back with a warning if
  # the name differs.
  expect_no_error(suppressWarnings(theme_brand(pts(), path, fetch_fonts = TRUE)))
})

test_that("a google font is fetched with fetch_fonts = TRUE", {
  skip_on_cran()
  skip_if_offline()
  skip_if_not_installed("brand.yml")
  skip_if_not_installed("systemfonts", "1.2.0")
  local_brand_state()
  path <- local_brand(c(
    keys_brand,
    "typography:",
    "  fonts:",
    "    - family: Lobster",
    "      source: google",
    "  base: Lobster"
  ))
  p <- theme_brand(pts(), path, fetch_fonts = TRUE)
  expect_identical(p@theme$text@family, "Lobster")
  # auto-apply fetches with its own option
  withr::local_options(
    vellumplot.brand = path,
    vellumplot.brand.fetch_fonts = TRUE
  )
  th <- suppressMessages(vellumplot:::.theme_of(pts()))
  expect_identical(th$text@family, "Lobster")
})

test_that("theme_brand validates its arguments", {
  skip_if_not_installed("brand.yml")
  local_brand_state()
  path <- local_brand(palette_brand)
  expect_error(theme_brand(pts(), 1), "brand")
  expect_error(theme_brand(pts(), "nope/_brand.yml"), "does not exist")
  expect_error(theme_brand(pts(), path, base = "minimal"), "preset")
  expect_error(theme_brand(pts(), path, fonts = NA), "TRUE")
  withr::local_dir(tempdir())
  expect_error(theme_brand(pts()), "No brand found")
})

test_that("light and dark brands: mode picks one", {
  skip_if_not_installed("brand.yml")
  local_brand_state()
  both <- list(light = local_brand(palette_brand), dark = local_brand(dark_brand))
  expect_identical(theme_brand(pts(), both)@theme$plot.background@fill, "#fffdf7")
  expect_identical(
    theme_brand(pts(), both, mode = "dark")@theme$plot.background@fill,
    "#111111"
  )
  expect_warning(
    p <- theme_brand(pts(), both["light"], mode = "dark"),
    "no \"dark\" variant"
  )
  expect_identical(p@theme$plot.background@fill, "#fffdf7")
})

# --- finding the brand ----------------------------------------------------------

test_that("brand = NULL reads Quarto's resolved brand", {
  local_brand_state()
  ei <- local_execute_info(list(
    light = quarto_entry(
      list(foreground = "#1f3b5a", background = "#fffdf7", primary = "#ef6f6c"),
      palette = list(navy = "#1f3b5a", coral = "#ef6f6c")
    ),
    dark = quarto_entry(list(
      foreground = "#eeeeee",
      background = "#111111",
      primary = "#ff8800"
    ))
  ))
  withr::local_envvar(QUARTO_EXECUTE_INFO = ei)
  p <- theme_brand(pts())
  expect_identical(p@theme$plot.background@fill, "#fffdf7")
  expect_identical(p@theme[["palette"]][1:2], c("#1f3b5a", "#ef6f6c"))
  d <- theme_brand(pts(), mode = "dark")
  expect_identical(d@theme$plot.background@fill, "#111111")
})

test_that("Quarto's brand: false means no brand, even with a _brand.yml", {
  skip_if_not_installed("brand.yml")
  local_brand_state()
  path <- local_brand(palette_brand)
  withr::local_dir(dirname(path))
  ei <- local_execute_info(metadata = list(brand = FALSE))
  withr::local_envvar(QUARTO_EXECUTE_INFO = ei)
  expect_error(theme_brand(pts()), "No brand found")
  withr::local_options(vellumplot.brand = TRUE)
  expect_null(pts()@theme)
  expect_null(attr(vellumplot:::.theme_of(pts()), "vp_brand"))
})

test_that("outside Quarto, brand = NULL finds _brand.yml upwards", {
  skip_if_not_installed("brand.yml")
  local_brand_state()
  path <- local_brand(palette_brand)
  sub <- file.path(dirname(path), "a", "b")
  dir.create(sub, recursive = TRUE)
  withr::local_dir(sub)
  p <- theme_brand(pts())
  expect_identical(p@theme$plot.background@fill, "#fffdf7")
  # BRAND_YML_PATH wins
  other <- local_brand(dark_brand, name = "other.yml")
  withr::local_envvar(BRAND_YML_PATH = other)
  expect_identical(theme_brand(pts())@theme$plot.background@fill, "#111111")
})

# --- auto-apply -----------------------------------------------------------------

test_that("auto-apply is off by default", {
  skip_if_not_installed("brand.yml")
  local_brand_state()
  withr::local_dir(dirname(local_brand(palette_brand)))
  expect_identical(vellumplot:::.theme_of(pts()), vellumplot:::.theme_default())
})

test_that("options(vellumplot.brand) themes unthemed plots, once-informed", {
  skip_if_not_installed("brand.yml")
  local_brand_state()
  path <- local_brand(palette_brand)
  withr::local_options(vellumplot.brand = path, rlib_message_verbosity = "verbose")
  expect_message(th <- vellumplot:::.theme_of(pts()), "_brand.yml")
  expect_identical(th$plot.background@fill, "#fffdf7")
  # the palette and the sketch default go through the same resolver
  expect_identical(
    vellumplot:::.theme_palette(pts(), "discrete")[1],
    "#1f3b5a"
  )
  expect_identical(train_brand(pts())$color$map("4"), "#1f3b5a")
  expect_no_error(render_px(pts()))
  # composition chrome takes it too: the figure background is the brand's
  px <- render_px(hconcat(pts(), pts()))
  expect_equal(px[2, 2, 1:3], c(255, 253, 247) / 255, tolerance = 0.01)
})

test_that("auto-apply never overrides an explicit theme", {
  skip_if_not_installed("brand.yml")
  local_brand_state()
  withr::local_options(vellumplot.brand = local_brand(palette_brand))
  bw <- pts() |> theme_bw()
  expect_identical(vellumplot:::.theme_of(bw), bw@theme)
  expect_null(vellumplot:::.theme_palette(bw, "discrete"))
  # an explicit theme_brand() is not stacked on
  dark <- suppressMessages(pts() |> theme_brand(local_brand(dark_brand)))
  expect_identical(vellumplot:::.theme_of(dark), dark@theme)
  # theme() tweaks on an unthemed plot sit on top of the brand
  tw <- suppressMessages(pts() |> theme(legend.position = "none"))
  expect_identical(tw@theme$plot.background@fill, "#fffdf7")
  expect_identical(tw@theme$legend.position, "none")
})

test_that("option TRUE finds the brand: Quarto first, then a file", {
  skip_if_not_installed("brand.yml")
  local_brand_state()
  withr::local_options(vellumplot.brand = TRUE)
  withr::local_dir(dirname(local_brand(palette_brand)))
  th <- suppressMessages(vellumplot:::.theme_of(pts()))
  expect_identical(th$plot.background@fill, "#fffdf7")
  ei <- local_execute_info(list(light = quarto_entry(list(
    foreground = "#eeeeee",
    background = "#111111"
  ))))
  withr::local_envvar(QUARTO_EXECUTE_INFO = ei)
  th <- suppressMessages(vellumplot:::.theme_of(pts()))
  expect_identical(th$plot.background@fill, "#111111")
})

test_that("auto-apply caches per file and modification time", {
  skip_if_not_installed("brand.yml")
  local_brand_state()
  path <- local_brand(palette_brand)
  withr::local_options(vellumplot.brand = path)
  a <- suppressMessages(vellumplot:::.theme_of(pts()))
  expect_identical(length(ls(vellumplot:::.brand_cache)), 1L)
  b <- vellumplot:::.theme_of(pts())
  expect_identical(a, b)
  # a changed file (new mtime) is read again
  writeLines(dark_brand, path)
  Sys.setFileTime(path, Sys.time() + 5)
  c2 <- suppressMessages(vellumplot:::.theme_of(pts()))
  expect_identical(c2$plot.background@fill, "#111111")
})

test_that("a bad auto brand warns and falls back instead of failing", {
  skip_if_not_installed("brand.yml")
  local_brand_state()
  withr::local_options(vellumplot.brand = "no/such/_brand.yml")
  expect_warning(th <- vellumplot:::.theme_of(pts()), "does not exist")
  expect_identical(th, vellumplot:::.theme_default())
  bad <- local_brand(c("color:", "  foreground: [1, 2"))
  withr::local_options(vellumplot.brand = bad)
  expect_warning(th <- vellumplot:::.theme_of(pts()), "Could not apply")
  expect_identical(th, vellumplot:::.theme_default())
})
