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
    vellumplot:::.col_mix("#1f3b5a", "#fffdf7", 0.3)
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

test_that("a file font is registered from a path relative to the brand", {
  skip_if_not_installed("brand.yml")
  skip_if_not_installed("systemfonts", "1.2.0")
  local_brand_state()
  otf <- system.file("fonts", "STIXTwoMath-Regular.otf", package = "vellum")
  skip_if(!nzchar(otf), "vellum's bundled STIX Two Math not found")
  fam <- "STIX Two Math" # the family name inside the file
  path <- local_brand(c(
    keys_brand,
    "typography:",
    "  fonts:",
    paste0("    - family: ", fam),
    "      source: file",
    "      files:",
    "        - path: fonts/stix.otf",
    paste0("  base: ", fam)
  ))
  dir.create(file.path(dirname(path), "fonts"))
  file.copy(otf, file.path(dirname(path), "fonts", "stix.otf"))
  # from another working directory: the path resolves against the brand dir
  withr::local_dir(tempdir())
  b <- vellumplot:::.brand_resolve(path)
  added <- NULL
  with_mocked_bindings(
    vellumplot:::.brand_fetch_font(fam, b),
    add_fonts = function(files) added <<- files,
    .package = "systemfonts"
  )
  expect_identical(
    normalizePath(added),
    normalizePath(file.path(dirname(path), "fonts", "stix.otf"))
  )
  # end to end, when the family is not installed already (vellum's math
  # engine may have registered it)
  skip_if(
    fam %in% systemfonts::system_fonts()$family,
    "STIX Two Math is already installed"
  )
  expect_warning(
    p <- theme_brand(pts(), path, fetch_fonts = FALSE),
    "not installed"
  )
  expect_null(p@theme$text@family)
  expect_no_warning(p <- theme_brand(pts(), path, fetch_fonts = TRUE))
  expect_identical(p@theme$text@family, fam)
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
  th <- suppressMessages(vellumplot:::.theme_of(tw))
  expect_identical(th$plot.background@fill, "#fffdf7")
  expect_identical(th$legend.position, "none")
})

test_that("theme() on an unthemed plot resolves the brand at compile time (VP-133)", {
  skip_if_not_installed("brand.yml")
  local_brand_state()
  path <- local_brand(palette_brand)
  bg <- function(p) {
    suppressMessages(vellumplot:::.resolve_theme(p@theme))[["plot.background"]]@fill
  }
  # built with the option off, compiled with it on: the brand applies
  withr::with_options(list(vellumplot.brand = NULL), {
    off <- pts() |> theme(legend.position = "none")
    expect_null(attr(off@theme, "vp_brand"))
  })
  withr::with_options(list(vellumplot.brand = path), {
    expect_identical(bg(off), "#fffdf7")
    px <- suppressMessages(render_px(off))
    expect_equal(px[2, 2, 1:3], c(255, 253, 247) / 255, tolerance = 0.01)
    expect_identical(
      suppressMessages(vellumplot:::.theme_palette(off, "discrete"))[1:2],
      c("#1f3b5a", "#ef6f6c")
    )
    expect_identical(
      suppressMessages(train_brand(off))$color$map("4"),
      "#1f3b5a"
    )
    expect_identical(suppressMessages(vellumplot:::.theme_of(off))$legend.position, "none")
    # built with the option on ...
    on <- pts() |> theme(legend.position = "bottom")
  })
  # ... compiled with it off: the gray default, tweak kept
  withr::with_options(list(vellumplot.brand = NULL), {
    expect_identical(bg(on), "white")
    expect_null(vellumplot:::.theme_palette(on, "discrete"))
    expect_identical(vellumplot:::.theme_of(on)$legend.position, "bottom")
  })
  # successive tweaks replay in order, as when they merged at call time
  two <- pts() |>
    theme(text = element_text(size = rel(2))) |>
    theme(text = element_text(size = rel(1.5)), legend.position = "top")
  rt <- vellumplot:::.resolve_theme(two@theme)
  expect_equal(rt[["axis.text.x"]]@size, 9 * 3)
  expect_identical(rt[["legend.position"]], "top")
  # set_theme() defers the same way
  st <- withr::with_options(
    list(vellumplot.brand = NULL),
    pts() |> set_theme(panel_bg = "red")
  )
  withr::with_options(list(vellumplot.brand = path), {
    expect_identical(bg(st), "#fffdf7")
    expect_identical(
      suppressMessages(vellumplot:::.resolve_theme(st@theme))[["panel.background"]]@fill,
      "red"
    )
  })
  # a hand-drawn theme keeps its sketch default under later tweaks
  sk <- pts() |> theme_sketch() |> theme(legend.position = "none")
  expect_s3_class(vellumplot:::.theme_sketch_default(sk), "vellum_sketch")
  expect_null(vellumplot:::.theme_sketch_default(two))
  # a spec round trip keeps the tweak deferred: still branded under the option
  rt_off <- withr::with_options(
    list(vellumplot.brand = NULL),
    spec_from_json(spec_to_json(off), data = mtcars)
  )
  withr::with_options(list(vellumplot.brand = path), {
    rt_on <- spec_from_json(
      suppressMessages(spec_to_json(off)),
      data = mtcars
    )
    expect_true(vellumplot:::.is_theme_tweak(rt_on@theme))
    expect_identical(bg(rt_off), "#fffdf7")
    expect_identical(bg(rt_on), "#fffdf7")
    expect_identical(
      suppressMessages(vellumplot:::.theme_of(rt_on))$legend.position,
      "none"
    )
  })
  withr::with_options(list(vellumplot.brand = NULL), {
    expect_identical(bg(rt_on), "white")
  })
  # an explicit theme keeps merging at call time
  withr::with_options(list(vellumplot.brand = path), {
    bw <- pts() |> theme_bw() |> theme(legend.position = "none")
    expect_identical(attr(bw@theme, "vp_preset"), "bw")
    expect_identical(bg(bw), "white")
  })
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

# --- review fixes ---------------------------------------------------------------

test_that("brands without a primary colour work (no accent lookup error)", {
  skip_if_not_installed("brand.yml")
  local_brand_state()
  cases <- list(
    c("color:", "  foreground: '#111111'"),
    c("color: {}"),
    c("meta:", "  name: x"),
    c("typography:", "  base: Helvetica"),
    c("color:", "  palette:", "    a: '#ff0000'", "  foreground: '#111111'")
  )
  for (lines in cases) {
    p <- suppressWarnings(theme_brand(pts(), local_brand(lines)))
    expect_identical(p@theme$geom@accent, "#3366CC")
  }
  acc <- c("color:", "  palette:", "    accent: '#17bebb'")
  expect_identical(
    theme_brand(pts(), local_brand(acc))@theme$geom@accent,
    "#17bebb"
  )
})

test_that("palette entries in the background colour are left out", {
  skip_if_not_installed("brand.yml")
  local_brand_state()
  path <- local_brand(c(
    "color:",
    "  palette:",
    "    paper: '#fffdf7'",
    "    coral: '#ef6f6c'",
    "  background: paper"
  ))
  pal <- theme_brand(pts(), path)@theme[["palette"]]
  expect_identical(pal[1], "#ef6f6c")
  expect_false("#fffdf7" %in% tolower(pal))
})

test_that("unexpected execute-info JSON never breaks a plot", {
  local_brand_state()
  skip_if_not_installed("jsonlite")
  bad <- local_tempfile(fileext = ".json")
  withr::local_envvar(QUARTO_EXECUTE_INFO = bad)
  withr::local_options(vellumplot.brand = TRUE)
  for (json in c(
    '{"format": {"render": {"brand": {"light": "x"}}}}',
    '{"format": {"render": {"brand": true}}}',
    '{"format": {"render": {"brand": {"light": {"processedData": 3}}}}}',
    "{not json"
  )) {
    writeLines(json, bad)
    vellumplot:::.brand_cache_reset()
    expect_null(vellumplot:::.brand_quarto())
    expect_no_error(render_px(pts()))
  }
})

test_that("mode = 'dark' under Quarto with only a light brand warns", {
  local_brand_state()
  ei <- local_execute_info(list(light = quarto_entry(list(
    foreground = "#1f3b5a",
    background = "#fffdf7"
  ))))
  withr::local_envvar(QUARTO_EXECUTE_INFO = ei)
  expect_warning(p <- theme_brand(pts(), mode = "dark"), "no \"dark\" brand")
  expect_identical(p@theme$plot.background@fill, "#fffdf7")
})

test_that("a composition's page follows its themed plots, not the brand", {
  skip_if_not_installed("brand.yml")
  local_brand_state()
  corner <- function(x) round(render_px(x)[2, 2, 1:3] * 255)
  dark <- local_brand(dark_brand)
  a <- vplot(mtcars, width = 3, height = 3) |> mark_point(x = wt, y = mpg)
  bw <- theme_bw(a)
  # option off: two dark theme_brand() plots get a dark page
  d <- theme_brand(a, dark)
  expect_identical(
    corner(hconcat(d, d) |> compose_annotation(title = "T")),
    c(17, 17, 17)
  )
  # option on: explicit theme_bw() plots keep a white page and title band
  withr::local_options(vellumplot.brand = dark)
  comp <- suppressMessages(hconcat(bw, bw) |> compose_annotation(title = "T"))
  expect_identical(corner(comp), c(255, 255, 255))
  # all leaves unthemed: the brand page
  expect_identical(corner(suppressMessages(hconcat(a, a))), c(17, 17, 17))
  # an own composition theme still wins
  own <- suppressMessages(
    hconcat(d, d) |> theme(plot.background = element_rect(fill = "white"))
  )
  expect_identical(corner(own), c(255, 255, 255))
})

test_that("as_spec() warns that a brand theme is not kept", {
  skip_if_not_installed("brand.yml")
  local_brand_state()
  p <- theme_brand(pts(), local_brand(keys_brand))
  expect_warning(as_spec(p), "palette")
  expect_warning(as_spec(p), "theme_brand")
  # a preset's own palette still round-trips quietly
  expect_no_warning(as_spec(pts() |> theme_cyberpunk()))
})

# --- font weights ---------------------------------------------------------------

test_that("element_text(weight =) validates and reaches the text gpar", {
  expect_identical(element_text(weight = "semi-bold")@weight, 600L)
  expect_identical(element_text(weight = 300)@weight, 300L)
  expect_null(element_text()@weight)
  expect_error(element_text(weight = 250), "weight")
  gp <- vellumplot:::.el_gpar_text(element_text(size = 11, weight = 300))
  expect_identical(gp@fontweight, 300L)
  # inherited down the theme tree like any text property
  p <- pts() |> theme(text = element_text(weight = "light"))
  rt <- vellumplot:::.resolve_theme(p@theme)
  expect_identical(rt$axis.text.x@weight, 300L)
})

test_that("brand weights map to text and headings", {
  skip_if_not_installed("brand.yml")
  local_brand_state()
  path <- local_brand(c(
    keys_brand,
    "typography:",
    "  base:",
    "    weight: 300",
    "  headings:",
    "    weight: semi-bold"
  ))
  th <- theme_brand(pts(), path)@theme
  expect_identical(th$text@weight, 300L)
  expect_identical(th$plot.title@weight, 600L)
  expect_identical(th$strip.text@weight, 600L)
  expect_null(theme_brand(pts(), path, fonts = FALSE)@theme$text@weight)
})

test_that("a weighted title draws in that face", {
  sf <- systemfonts::system_fonts()
  fams <- c("Helvetica Neue", "Inter", "Lato", "Roboto", "Open Sans", "Noto Sans", "DejaVu Sans")
  fam <- Find(
    function(f) {
      w <- as.character(sf$weight[sf$family == f & !sf$italic])
      all(c("light", "normal") %in% w)
    },
    fams
  )
  skip_if(is.null(fam), "no family with light and regular faces")
  base <- pts() |>
    labs(title = "Fuel use by weight") |>
    theme_minimal(base_family = fam)
  light <- base |> theme(plot.title = element_text(weight = 300))
  svg_of <- function(p) vellum::scene_svg(p)
  expect_match(svg_of(light), 'font-weight="300"', fixed = TRUE)
  expect_false(identical(
    vellum::scene_svg(light, text = "outline"),
    vellum::scene_svg(base, text = "outline")
  ))
})

test_that("an explicit bold face is at least 700 over an inherited weight", {
  skip_if_not_installed("brand.yml")
  local_brand_state()
  path <- local_brand(c(
    keys_brand,
    "typography:",
    "  base:",
    "    weight: 300"
  ))
  q <- theme_brand(pts(), path) |>
    theme(plot.title = element_text(face = "bold"))
  rt <- vellumplot:::.resolve_theme(q@theme)
  expect_identical(rt$plot.title@weight, 300L) # inherited
  gp <- vellumplot:::.el_gpar_text(rt$plot.title)
  expect_identical(gp@fontweight, 700L)
  # a heavier inherited weight is kept
  gp <- vellumplot:::.el_gpar_text(element_text(face = "bold", weight = 900))
  expect_identical(gp@fontweight, 900L)
  gp <- vellumplot:::.el_gpar_text(element_text(face = "italic", weight = 300))
  expect_identical(gp@fontweight, 300L)
})

test_that("small text keeps at least a regular weight under a light brand", {
  skip_if_not_installed("brand.yml")
  local_brand_state()
  path <- local_brand(c(keys_brand, "typography:", "  base:", "    weight: 300"))
  rt <- vellumplot:::.resolve_theme(theme_brand(pts(), path)@theme)
  expect_identical(rt$axis.title.x@weight, 400L)
  expect_identical(rt$axis.title.y@weight, 400L)
  expect_identical(rt$legend.title@weight, 400L)
  expect_identical(rt$axis.text.x@weight, 400L)
  expect_identical(rt$legend.text@weight, 400L)
  expect_identical(rt$plot.caption@weight, 400L)
  heavy <- local_brand(c(keys_brand, "typography:", "  base:", "    weight: 500"))
  rt <- vellumplot:::.resolve_theme(theme_brand(pts(), heavy)@theme)
  expect_identical(rt$axis.text.x@weight, 500L)
})

test_that("odd brand weights round or warn, never vanish silently", {
  expect_identical(vellumplot:::.brand_weight(350), 400L)
  expect_identical(vellumplot:::.brand_weight(1000), 900L)
  expect_identical(vellumplot:::.brand_weight("semi-bold"), 600L)
  expect_warning(w <- vellumplot:::.brand_weight("chunky"), "chunky")
  expect_null(w)
  expect_null(vellumplot:::.brand_weight(NULL))
})

test_that("element_text() reports only weight errors as weight errors", {
  expect_error(element_text(weight = "chunky"), class = "rlang_error")
  expect_error(element_text(weight = "chunky"), "font weight")
  # an unrelated error inside the weight check is not relabelled
  local_mocked_bindings(
    vl_gpar = function(...) stop("boom"),
    .package = "vellum"
  )
  expect_error(element_text(weight = 300), "boom")
})
