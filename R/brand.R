#' @include theme.R
NULL

# brand.yml themes (F-26) ------------------------------------------------------
#
# A brand is read once into a small normalised list (`.brand_normalise()`):
#   source   the file it came from (for messages and the cache key)
#   dir      the directory relative font files resolve against
#   color    named list of the resolved theme colours (foreground, background,
#            primary, ...), hex strings
#   palette  named character vector of `color.palette`, resolved, in file order
#   base, headings   list(family, weight) from `typography`
#   fonts    the `typography.fonts` entries (family, source, files)
# Two routes produce it: inside a Quarto render, the brand Quarto resolved
# (`QUARTO_EXECUTE_INFO`'s `format$render$brand`), read with jsonlite; anywhere
# else, a `_brand.yml` read with the brand.yml package.

#' A plot theme from a brand.yml file
#'
#' `theme_brand()` themes a plot from a [brand.yml](https://posit-dev.github.io/brand-yml/)
#' file, the format Quarto uses for `_brand.yml`. The brand's colours, palette
#' and fonts go onto a base preset ([theme_minimal()] by default):
#'
#' * `background` becomes the page (plot and panel background, and the
#'   `paper` of [element_geom()]); `foreground` becomes the ink of titles and
#'   unmapped marks; `primary` is the `accent` of smooths and contours. The
#'   greys in between (text, axis text, gridlines, caption) are blends of
#'   foreground and background, as in brand.yml's own
#'   `theme_brand_ggplot2()` (axis text and caption a little darker, so they
#'   keep WCAG AA contrast), and a dark brand gives a dark plot.
#' * The discrete colour palette is `color.palette` in file order, or else the
#'   brand's `primary`, `secondary`, `tertiary`, `success`, `info`, `warning`
#'   and `danger` colours (those that are set and distinct). Entries in the
#'   background colour are left out. When a scale has
#'   more levels than the brand has colours, vellumplot's own qualitative
#'   colours follow. A `scale_*()` palette still wins.
#' * `typography.base` sets the family and weight of all text, and
#'   `typography.headings` the family of the title, subtitle and facet strips
#'   and the weight of the title and strips. Weights such as 300 (light) or
#'   `semi-bold` pick that face of a family that has it; axis and legend
#'   titles and labels and the caption stay at least regular (400) so they
#'   stay legible.
#'
#' Elements the base preset leaves out (the axis ticks of [theme_minimal()],
#' say) stay out.
#'
#' @section Where the brand comes from:
#' `brand = NULL` finds the brand the way Quarto does. Inside a Quarto render
#' it is the brand Quarto resolved for the document (a project `_brand.yml`,
#' or `brand:` in `_quarto.yml` or the front matter, including light and dark
#' brands; `brand: false` means no brand). Outside Quarto it is the file named
#' by the `BRAND_YML_PATH` environment variable, or the first `_brand.yml`
#' found in the working directory or a parent of it (also in a `brand/` or
#' `_brand/` subdirectory). Reading a file needs the brand.yml package.
#'
#' @section Using the brand for every plot:
#' Set `options(vellumplot.brand = TRUE)` (in a document's setup chunk, say) to
#' theme every plot that has no theme of its own with `theme_brand()`, found
#' as for `brand = NULL`. `options(vellumplot.brand = "path/to/_brand.yml")`
#' names the file. A plot with an explicit theme (`theme_bw()`, a
#' `theme_brand()` of its own) keeps it; [theme()] tweaks on an unthemed plot
#' apply on top of the brand. The first branded plot of a session says which
#' file it used. The option is off by default, so existing documents do not
#' change their look. Add `options(vellumplot.brand.fetch_fonts = TRUE)` to
#' fetch missing brand fonts there, as `fetch_fonts = TRUE` does.
#'
#' @section Fonts:
#' A brand font that is not installed falls back to the default family, with
#' a warning. `fetch_fonts = TRUE` installs it for the session first: a
#' `google` or `bunny` font is downloaded with [systemfonts::require_font()]
#' (this needs the network), and a `file` font is registered from the brand's
#' directory with [systemfonts::add_fonts()]. Fonts registered this way are
#' used for drawing and measuring text in every output format.
#'
#' @param plot A [PlotSpec].
#' @param brand The brand: `NULL` (default) to find it (see below), a path to a
#'   brand.yml file, a `brand_yml` object from [brand.yml::read_brand_yml()],
#'   or a list with `light` and `dark` entries (each a path or a `brand_yml`
#'   object), like Quarto's `brand: {light:, dark:}`.
#' @param mode `"light"` (default) or `"dark"`: which brand to use when there
#'   are both (in a Quarto document with light and dark brands, or a
#'   `list(light =, dark =)`). With a single brand it is used as given.
#' @param base A theme preset to start from, such as [theme_minimal()]
#'   (default), [theme_bw()] or [theme_classic()].
#' @param base_size Base text size in points, as for the presets.
#' @param fonts Use the brand's font families? `FALSE` keeps the base
#'   preset's.
#' @param palette Use the brand's colours as the default discrete palette?
#' @param fetch_fonts Download or register brand fonts that are not installed
#'   (see the Fonts section)? Default `FALSE`.
#' @return The modified [PlotSpec].
#' @seealso [theme_minimal()], [element_geom()]
#' @examples
#' brand <- tempfile(fileext = ".yml")
#' writeLines(c(
#'   "color:",
#'   "  palette:",
#'   "    ink: '#1d2b3a'",
#'   "    coral: '#e4572e'",
#'   "    teal: '#17bebb'",
#'   "    gold: '#ffc914'",
#'   "  foreground: ink",
#'   "  background: '#fbf8f1'",
#'   "  primary: coral"
#' ), brand)
#'
#' if (requireNamespace("brand.yml", quietly = TRUE)) {
#'   vplot(mtcars) |>
#'     mark_point(x = wt, y = mpg, color = factor(cyl)) |>
#'     labs(title = "Fuel use by weight") |>
#'     theme_brand(brand)
#' }
#' @export
theme_brand <- function(
  plot,
  brand = NULL,
  mode = c("light", "dark"),
  base = theme_minimal,
  base_size = 11,
  fonts = TRUE,
  palette = TRUE,
  fetch_fonts = FALSE
) {
  .check_plot(plot)
  mode <- rlang::arg_match(mode)
  if (!is.function(base)) {
    cli::cli_abort(
      "{.arg base} must be a theme preset function such as {.fn theme_minimal}."
    )
  }
  for (a in c("fonts", "palette", "fetch_fonts")) {
    if (!rlang::is_bool(get(a))) {
      cli::cli_abort("{.arg {a}} must be {.code TRUE} or {.code FALSE}.")
    }
  }
  b <- .brand_resolve(brand, mode)
  if (is.null(b)) {
    cli::cli_abort(c(
      "No brand found.",
      i = "Pass {.arg brand} a path to a brand.yml file, or set {.envvar BRAND_YML_PATH}."
    ))
  }
  plot@theme <- .brand_theme(
    b,
    base = base,
    base_size = base_size,
    fonts = fonts,
    palette = palette,
    fetch_fonts = fetch_fonts
  )
  plot
}

# Reading ----------------------------------------------------------------------

# A brand argument -> a normalised brand, or NULL when `brand = NULL` finds none.
.brand_resolve <- function(brand, mode = "light", call = rlang::caller_env()) {
  if (is.null(brand)) {
    return(.brand_find(mode, call = call))
  }
  if (inherits(brand, "brand_yml")) {
    return(.brand_from_yml(brand))
  }
  if (is.character(brand) && length(brand) == 1L && !is.na(brand)) {
    return(.brand_read_file(brand, call = call))
  }
  if (is.list(brand) && any(c("light", "dark") %in% names(brand))) {
    one <- brand[[mode]]
    if (is.null(one)) {
      other <- setdiff(c("light", "dark"), mode)
      cli::cli_warn(
        "The brand has no {.val {mode}} variant; using the {.val {other}} one.",
        call = call
      )
      one <- brand[[other]]
    }
    return(.brand_resolve(one, mode, call = call))
  }
  cli::cli_abort(
    c(
      "{.arg brand} must be a path to a brand.yml file, a {.cls brand_yml} object, or a list with {.field light}/{.field dark} entries.",
      x = "Got {.obj_type_friendly {brand}}."
    ),
    call = call
  )
}

# Find the brand with no path given: Quarto's resolved brand first, then a
# brand.yml file. NULL when there is none (or Quarto says `brand: false`).
.brand_find <- function(mode = "light", call = rlang::caller_env()) {
  q <- .brand_quarto(mode)
  if (isFALSE(q)) {
    return(NULL)
  }
  if (!is.null(q)) {
    return(q)
  }
  path <- .brand_find_file()
  if (is.null(path)) {
    return(NULL)
  }
  .brand_read_file(path, call = call)
}

# The brand Quarto resolved for the document being rendered, from the JSON
# file `QUARTO_EXECUTE_INFO` names: a normalised brand, FALSE when the
# document has no brand (`brand: false`, or no brand file), or NULL when this
# is not a Quarto render or the file does not carry a brand field (older
# Quarto; the caller then looks for a file). `processedData` is undocumented,
# so anything unexpected is NULL rather than an error.
.brand_quarto <- function(mode = "light") {
  tryCatch(.brand_quarto_read(mode), error = function(e) NULL)
}

.brand_quarto_read <- function(mode) {
  info <- .brand_quarto_info()
  if (is.null(info)) {
    return(NULL)
  }
  render <- info$format$render
  if (is.null(render)) {
    return(NULL)
  }
  if (!"brand" %in% names(render)) {
    # `brand: false` and "no brand at all" both drop the field; a disabled
    # brand also shows as `metadata$brand == FALSE`.
    if (isFALSE(info$format$metadata$brand)) {
      return(FALSE)
    }
    return(NULL)
  }
  br <- render$brand
  if (is.null(br) || !length(br)) {
    return(FALSE)
  }
  if (!is.list(br)) {
    return(NULL)
  }
  one <- br[[mode]]
  if (is.null(one)) {
    other <- setdiff(c("light", "dark"), mode)
    one <- br[[other]]
    if (identical(mode, "dark") && !is.null(one)) {
      cli::cli_warn(
        "The Quarto document has no {.val dark} brand; using the {.val light} one."
      )
    }
  }
  if (!is.list(one) || !is.list(one$processedData)) {
    return(NULL)
  }
  .brand_from_quarto(one, path = info$path)
}

# The parsed execute-info JSON (plus its `path`), or NULL outside Quarto.
.brand_quarto_info <- function() {
  path <- Sys.getenv("QUARTO_EXECUTE_INFO")
  if (!nzchar(path) || !file.exists(path)) {
    return(NULL)
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    return(NULL)
  }
  info <- tryCatch(
    jsonlite::read_json(path, simplifyVector = FALSE),
    error = function(e) NULL
  )
  if (is.null(info)) {
    return(NULL)
  }
  info$path <- path
  info
}

# One Quarto brand entry (`light` or `dark`) -> a normalised brand. Colours
# come from `processedData` (palette names already resolved to hex), the
# palette order and the font sources from the raw `data`.
.brand_from_quarto <- function(entry, path) {
  pd <- entry$processedData
  raw <- entry$data %||% entry$brand %||% list()
  cols <- pd$color %||% list()
  pal_names <- names(raw$color$palette %||% list())
  pal <- unlist(cols[intersect(pal_names, names(cols))])
  theme_cols <- cols[intersect(.BRAND_COLOR_KEYS, names(cols))]
  .brand_normalise(
    source = entry$brandDir %||% path,
    dir = entry$brandDir %||% dirname(path),
    color = theme_cols,
    palette = pal,
    typography = pd$typography,
    fonts = raw$typography$fonts
  )
}

# A brand.yml file -> a normalised brand (through the brand.yml package, which
# validates the file and resolves palette names and shorthands).
.brand_read_file <- function(path, call = rlang::caller_env()) {
  if (!file.exists(path)) {
    cli::cli_abort("Brand file {.file {path}} does not exist.", call = call)
  }
  rlang::check_installed("brand.yml", reason = "to read a brand.yml file.")
  b <- brand.yml::read_brand_yml(path)
  .brand_from_yml(b, path = normalizePath(path, mustWork = FALSE))
}

.brand_from_yml <- function(b, path = NULL) {
  rlang::check_installed("brand.yml", reason = "to use a brand.yml object.")
  path <- path %||% b$path
  cols <- b$color %||% list()
  pal <- cols$palette %||% list()
  pal <- vapply(
    names(pal),
    function(nm) {
      v <- brand.yml::brand_color_pluck(b, nm)
      if (is.null(v)) NA_character_ else as.character(v)
    },
    character(1)
  )
  keys <- intersect(.BRAND_COLOR_KEYS, names(cols))
  theme_cols <- lapply(
    stats::setNames(keys, keys),
    function(k) brand.yml::brand_color_pluck(b, k)
  )
  .brand_normalise(
    source = path %||% "<brand_yml>",
    dir = if (is.null(path)) getwd() else dirname(path),
    color = theme_cols,
    palette = pal[!is.na(pal)],
    typography = b$typography,
    fonts = b$typography$fonts
  )
}

# The brand colour keys that are single colours (`palette` aside).
.BRAND_COLOR_KEYS <- c(
  "foreground",
  "background",
  "primary",
  "secondary",
  "tertiary",
  "success",
  "info",
  "warning",
  "danger",
  "light",
  "dark"
)

.brand_normalise <- function(source, dir, color, palette, typography, fonts) {
  color <- Filter(function(v) .is_colour1(v), color)
  palette <- palette[vapply(palette, .is_colour1, logical(1))]
  face <- function(x) {
    if (is.null(x)) {
      return(list())
    }
    # `base: Lato` is shorthand for `base: {family: Lato}`
    if (is.character(x)) {
      return(list(family = x[1]))
    }
    list(family = x$family, weight = x$weight)
  }
  list(
    source = source,
    dir = dir,
    color = lapply(color, as.character),
    palette = palette,
    base = face(typography$base),
    headings = face(typography$headings),
    fonts = fonts %||% list()
  )
}

# The first brand.yml file found from the working directory upwards, as the
# brand.yml package searches: `BRAND_YML_PATH`, then `_brand.yml` /
# `_brand.yaml` in each directory or its `brand/` / `_brand/` subdirectory.
.brand_find_file <- function(dir = getwd(), max_up = 20L) {
  env <- Sys.getenv("BRAND_YML_PATH")
  if (nzchar(env)) {
    return(env)
  }
  candidates <- c(
    "_brand.yml",
    "_brand.yaml",
    file.path("brand", c("_brand.yml", "_brand.yaml")),
    file.path("_brand", c("_brand.yml", "_brand.yaml"))
  )
  dir <- normalizePath(dir, mustWork = FALSE)
  for (i in seq_len(max_up)) {
    hit <- file.path(dir, candidates)
    hit <- hit[file.exists(hit)]
    if (length(hit)) {
      return(hit[1])
    }
    up <- dirname(dir)
    if (identical(up, dir)) {
      break
    }
    dir <- up
  }
  NULL
}

# Mapping ----------------------------------------------------------------------

# A complete theme from a normalised brand, on top of the preset `base`.
.brand_theme <- function(
  b,
  base = theme_minimal,
  base_size = 11,
  fonts = TRUE,
  palette = TRUE,
  fetch_fonts = FALSE
) {
  col <- b$color
  bg <- col$background %||% "white"
  fg <- col$foreground %||% "black"
  accent <- col$primary %||%
    (if ("accent" %in% names(b$palette)) b$palette[["accent"]]) %||%
    "#3366CC"
  blend <- function(amount) .col_mix(fg, bg, amount)

  base_family <- ""
  head_family <- NULL
  if (fonts) {
    fam <- b$base$family
    if (!is.null(fam) && .brand_font_ready(fam, b, fetch_fonts)) {
      base_family <- fam
    }
    fam <- b$headings$family
    if (!is.null(fam) && .brand_font_ready(fam, b, fetch_fonts)) {
      head_family <- fam
    }
  }

  th <- base(vplot(data.frame()), base_size = base_size)@theme
  preset <- attr(th, "vp_preset")
  if (nzchar(base_family)) {
    th <- .merge_theme(th, list(text = element_text(family = base_family)))
  }
  base_weight <- if (fonts) .brand_weight(b$base$weight)
  head_weight <- if (fonts) .brand_weight(b$headings$weight)
  # Small text (axis and legend titles and labels, caption) stays at least
  # regular: a light body weight at 9-10 pt is too faint to read.
  small_weight <- if (!is.null(base_weight) && base_weight < 400L) 400L
  over <- list(
    text = element_text(colour = blend(0.1), weight = base_weight),
    title = element_text(colour = fg),
    plot.title = element_text(
      colour = fg,
      family = head_family,
      weight = head_weight
    ),
    plot.subtitle = element_text(colour = fg, family = head_family),
    plot.caption = element_text(colour = blend(0.25), weight = small_weight),
    plot.tag = element_text(colour = fg),
    axis.title = element_text(weight = small_weight),
    legend.title = element_text(weight = small_weight),
    axis.text = element_text(colour = blend(0.3), weight = small_weight),
    legend.text = element_text(colour = blend(0.1), weight = small_weight),
    strip.text = element_text(
      colour = blend(0.1),
      family = head_family,
      weight = head_weight
    ),
    panel.grid = element_line(colour = blend(0.85)),
    panel.grid.major = element_line(colour = blend(0.85)),
    panel.grid.minor = element_line(colour = blend(0.9)),
    axis.ticks = element_line(colour = blend(0.85)),
    axis.line = element_line(colour = blend(0.2)),
    plot.background = element_rect(fill = bg, colour = NA),
    panel.background = element_rect(fill = bg, colour = NA),
    strip.background = element_rect(fill = blend(0.9), colour = NA)
  )
  # Recolour only what the preset draws: a blank slot (theme_minimal's ticks,
  # theme_classic's grid) stays blank.
  cache <- new.env(parent = emptyenv())
  keep <- vapply(
    names(over),
    function(nm) {
      nm %in% c("text", "title", "plot.background") ||
        !.is_blank(.resolve_slot(nm, th, cache))
    },
    logical(1)
  )
  th <- .merge_theme(th, over[keep])
  th[["geom"]] <- element_geom(ink = fg, paper = bg, accent = accent)
  if (palette) {
    pal <- .brand_palette(b)
    if (length(pal)) th[["palette"]] <- pal
  }
  attr(th, "vp_preset") <- preset
  attr(th, "vp_brand") <- b$source
  th
}

# The brand's default discrete palette (unnamed: a named vector would be read
# as a manual mapping by level name): `color.palette` in file order, else the
# set and distinct theme colours; padded with vellumplot's qualitative colours
# so a scale with more levels than the brand has colours does not recycle.
.brand_palette <- function(b, n = 24L) {
  pal <- unname(b$palette)
  if (!length(pal)) {
    keys <- c(
      "primary",
      "secondary",
      "tertiary",
      "success",
      "info",
      "warning",
      "danger"
    )
    pal <- unname(unlist(b$color[intersect(keys, names(b$color))]))
  }
  # a palette entry in the page colour would draw invisible marks
  bg <- b$color$background
  if (length(pal) && !is.null(bg)) {
    pal <- pal[toupper(.brand_hex(pal)) != toupper(.brand_hex(bg))]
  }
  if (!length(pal)) {
    return(NULL)
  }
  pal <- pal[!duplicated(toupper(.brand_hex(pal)))]
  pad <- unique(c(.VELLUM_QUAL, .qual_palette(18L)))
  pad <- pad[!toupper(pad) %in% toupper(.brand_hex(pal))]
  c(pal, pad)[seq_len(max(n, length(pal)))]
}

.brand_hex <- function(x) {
  m <- grDevices::col2rgb(x)
  grDevices::rgb(m[1, ], m[2, ], m[3, ], maxColorValue = 255)
}

# Fonts ------------------------------------------------------------------------

# A brand.yml weight (a number, or a name such as "semi-bold") as a font
# weight for element_text(), or NULL when unset. A number between the
# hundreds (CSS allows any 1-1000) rounds to the nearest of 100..900; one
# vellum cannot use warns and is ignored, rather than rejecting the brand.
.brand_weight <- function(w) {
  if (is.null(w)) {
    return(NULL)
  }
  if (length(w) == 1L && is.numeric(w) && !is.na(w) && w >= 1 && w <= 1000) {
    w <- min(max(round(w / 100) * 100, 100), 900)
  }
  tryCatch(
    vellum::vl_gpar(fontweight = w)@fontweight,
    vellum_error_fontweight = function(e) {
      cli::cli_warn(
        "Brand font weight {.val {w}} is not a weight vellumplot can draw; ignoring it.",
        .frequency = "once",
        .frequency_id = paste0("vellumplot-brand-weight-", paste(w, collapse = ","))
      )
      NULL
    }
  )
}

# Is `family` installed (or registered)? With `fetch`, try the brand's source
# for it first. A family that stays missing warns once per session and FALSE
# (the caller falls back to the default family).
.brand_font_ready <- function(family, b, fetch = FALSE) {
  if (.font_installed(family)) {
    return(TRUE)
  }
  if (fetch) {
    .brand_fetch_font(family, b)
    if (.font_installed(family, refresh = TRUE)) {
      return(TRUE)
    }
  }
  cli::cli_warn(
    c(
      "Brand font {.val {family}} is not installed; using the default family.",
      i = if (!fetch) {
        "Set {.code fetch_fonts = TRUE} to download or register it for this session."
      }
    ),
    .frequency = "once",
    .frequency_id = paste0("vellumplot-brand-font-", family)
  )
  FALSE
}

.font_cache <- new.env(parent = emptyenv())

.font_installed <- function(family, refresh = FALSE) {
  if (!requireNamespace("systemfonts", quietly = TRUE)) {
    return(TRUE)
  }
  fams <- .font_cache$families
  if (is.null(fams) || refresh) {
    fams <- unique(c(
      systemfonts::system_fonts()$family,
      systemfonts::registry_fonts()$family
    ))
    .font_cache$families <- fams
  }
  tolower(family) %in% tolower(fams)
}

# Install a brand font for the session from its `typography.fonts` source.
.brand_fetch_font <- function(family, b) {
  entry <- Filter(
    function(f) identical(tolower(f$family %||% ""), tolower(family)),
    b$fonts
  )
  src <- if (length(entry)) entry[[1]]$source %||% "system" else "system"
  if (src %in% c("google", "bunny")) {
    rlang::check_installed(
      "systemfonts",
      version = "1.2.0",
      reason = "to download brand fonts."
    )
    tryCatch(
      systemfonts::require_font(family, error = FALSE, verbose = FALSE),
      error = function(e) NULL
    )
  } else if (identical(src, "file")) {
    rlang::check_installed(
      "systemfonts",
      version = "1.2.0",
      reason = "to register brand fonts."
    )
    files <- vapply(
      entry[[1]]$files %||% list(),
      function(f) if (is.character(f)) f[1] else f$path %||% NA_character_,
      character(1)
    )
    files <- files[!is.na(files)]
    paths <- ifelse(
      grepl("^(/|[A-Za-z]:)", files),
      files,
      file.path(b$dir, files)
    )
    paths <- paths[file.exists(paths)]
    if (length(paths)) {
      tryCatch(systemfonts::add_fonts(paths), error = function(e) NULL)
    }
  }
  invisible()
}

# Auto-apply -------------------------------------------------------------------

.brand_cache <- new.env(parent = emptyenv())

# The brand theme `options(vellumplot.brand)` asks for, or NULL: off (the
# default), no brand found, or a brand that fails to read (a warning, once;
# auto-apply never stops a plot from drawing). Parsed brands are cached per
# file and modification time.
.brand_auto_theme <- function() {
  opt <- getOption("vellumplot.brand")
  if (is.null(opt) || isFALSE(opt)) {
    return(NULL)
  }
  path <- .brand_auto_path(opt)
  if (is.null(path)) {
    return(NULL)
  }
  fetch <- isTRUE(getOption("vellumplot.brand.fetch_fonts"))
  key <- paste(path, unclass(file.mtime(path)), fetch)
  hit <- .brand_cache[[key]]
  if (!is.null(hit)) {
    return(hit)
  }
  th <- tryCatch(
    {
      b <- if (identical(path, Sys.getenv("QUARTO_EXECUTE_INFO"))) {
        .brand_quarto("light")
      } else {
        .brand_read_file(path)
      }
      if (is.list(b)) .brand_theme(b, fetch_fonts = fetch) else NULL
    },
    error = function(e) {
      cli::cli_warn(
        c(
          "Could not apply the brand from {.file {path}}.",
          x = conditionMessage(e)
        ),
        .frequency = "once",
        .frequency_id = paste0("vellumplot-brand-error-", key)
      )
      NULL
    }
  )
  if (is.null(th)) {
    return(NULL)
  }
  cli::cli_inform(
    "Theming plots with the brand from {.file {attr(th, 'vp_brand')}} ({.code options(vellumplot.brand)}).",
    .frequency = "once",
    .frequency_id = paste0("vellumplot-brand-", key)
  )
  .brand_cache[[key]] <- th
  th
}

# The file auto-apply reads: the path the option names; else, in a Quarto
# render that carries a brand, the execute-info file; else a brand.yml file
# found from the working directory. NULL for none (including `brand: false`).
.brand_auto_path <- function(opt) {
  if (is.character(opt)) {
    if (length(opt) != 1L || is.na(opt) || !file.exists(opt)) {
      cli::cli_warn(
        "{.code options(vellumplot.brand)} names a file that does not exist: {.file {opt}}.",
        .frequency = "once",
        .frequency_id = paste0("vellumplot-brand-missing-", opt)
      )
      return(NULL)
    }
    return(normalizePath(opt))
  }
  if (!isTRUE(opt)) {
    return(NULL)
  }
  qi <- Sys.getenv("QUARTO_EXECUTE_INFO")
  if (nzchar(qi) && file.exists(qi)) {
    q <- .brand_quarto_state(qi)
    if (identical(q, "brand")) {
      return(qi)
    }
    if (identical(q, "none")) {
      return(NULL)
    }
  }
  .brand_find_file()
}

# Does the execute-info file carry a brand ("brand"), say there is none
# ("none"), or not know ("unknown", the caller searches for a file)? Cached
# per file and modification time so a render reads the JSON once.
.brand_quarto_state <- function(path) {
  key <- paste("state", path, unclass(file.mtime(path)))
  hit <- .brand_cache[[key]]
  if (!is.null(hit)) {
    return(hit)
  }
  q <- .brand_quarto("light")
  state <- if (isFALSE(q)) "none" else if (is.null(q)) "unknown" else "brand"
  .brand_cache[[key]] <- state
  state
}

# Forget cached brands (tests).
.brand_cache_reset <- function() {
  rm(list = ls(.brand_cache, all.names = TRUE), envir = .brand_cache)
  rm(list = ls(.font_cache, all.names = TRUE), envir = .font_cache)
  invisible()
}
