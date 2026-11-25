#' @include classes.R elements.R theme-tree.R
NULL

# A theme is a *complete* element theme: a named list keyed by element slots
# (element_*() objects) plus scalar settings (see theme-tree.R). Presets build a
# complete theme; theme() / set_theme() element-merge a partial onto it. The
# compile pipeline resolves it once (.resolve_theme) before drawing.

# The default (gray) theme. Text sizes below the `text` root are `rel()`
# multiples of `base_size`, so `theme(text = element_text(size = ...))` and a
# preset's `base_size` cascade to every text element; at the default
# `base_size = 11` they resolve to exactly 14/11/9/12/10/9/9/9 pt. Axis/legend
# titles inherit black from the text root; the tick/line/minor elements carry
# their intended values for the drawers.
.theme_gray_complete <- function(
  base_size = .BASE_SIZE,
  base_family = "",
  call = rlang::caller_env()
) {
  .check_base_text(base_size, base_family, call = call)
  b <- .BASE_SIZE
  list(
    text = element_text(
      colour = "black",
      size = base_size,
      family = if (nzchar(base_family)) base_family
    ),
    plot.title = element_text(size = rel(14 / b), colour = "grey10"),
    plot.subtitle = element_text(size = rel(11 / b), colour = "grey30"),
    plot.caption = element_text(size = rel(9 / b), colour = "grey30"),
    plot.tag = element_text(size = rel(12 / b), colour = "grey10"),
    legend.title = element_text(size = rel(10 / b)),
    axis.text = element_text(size = rel(9 / b), colour = "grey20"),
    legend.text = element_text(size = rel(9 / b), colour = "grey20"),
    strip.text = element_text(size = rel(9 / b), colour = "grey10"),
    line = element_line(colour = "white", linewidth = 1),
    panel.grid.minor = element_line(linewidth = 0.5),
    axis.ticks = element_line(colour = "grey20"),
    axis.line = element_blank(),
    rect = element_rect(fill = NA, colour = NA),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "grey92", colour = NA),
    strip.background = element_rect(fill = "grey85", colour = NA),
    legend.background = element_blank(),
    legend.key = element_blank()
  )
}

.theme_default <- function() .theme_gray_complete()

.check_base_text <- function(
  base_size,
  base_family,
  call = rlang::caller_env()
) {
  if (
    !is.numeric(base_size) ||
      length(base_size) != 1L ||
      !is.finite(base_size) ||
      base_size <= 0
  ) {
    cli::cli_abort(
      "{.arg base_size} must be a single positive number (points).",
      call = call
    )
  }
  if (
    !is.character(base_family) ||
      length(base_family) != 1L ||
      is.na(base_family)
  ) {
    cli::cli_abort("{.arg base_family} must be a single string.", call = call)
  }
  invisible()
}

# The resolved-or-default complete theme for a spec. The single theme
# resolver: a spec with no theme of its own gets the automatic default, and one
# holding only deferred `theme()` / `set_theme()` tweaks gets them replayed onto
# that default here, at compile time (VP-133).
.theme_of <- function(spec) {
  th <- spec@theme
  if (is.null(th)) {
    return(.theme_auto())
  }
  if (.is_theme_tweak(th)) {
    return(.theme_replay(.theme_auto(), th))
  }
  th
}

# Deferred theme tweaks (VP-133). `theme()` on a plot with no theme of its own
# does not merge onto `.theme_auto()` at call time -- that would freeze the
# brand state (`options(vellumplot.brand)`) of the moment `theme()` ran. It
# stores the tweak instead: an empty theme list whose `vp_tweaks` attribute
# holds each call's arguments in order, which `.theme_of()` replays onto the
# automatic default when the plot compiles. Replaying the calls one by one
# (rather than merging them into one partial theme) keeps the semantics of
# successive calls exactly, e.g. two `rel()` text sizes multiply.
.is_theme_tweak <- function(theme) !is.null(attr(theme, "vp_tweaks"))

# Add one call's arguments to a plot's deferred tweaks (starting them on an
# unthemed plot). `unblank` records the call's drawn elements as `theme()` does
# (`.mark_unblank()`); `set_theme()` does not.
.theme_add_tweak <- function(theme, args, unblank = TRUE) {
  tweaks <- attr(theme, "vp_tweaks")
  structure(
    list(),
    vp_tweaks = c(tweaks, list(list(args = args, unblank = unblank)))
  )
}

# Replay deferred tweaks onto a base theme, call by call.
.theme_replay <- function(base, theme) {
  for (tw in attr(theme, "vp_tweaks")) {
    base <- .merge_theme(base, tw$args)
    if (isTRUE(tw$unblank)) {
      base <- .mark_unblank(base, tw$args)
    }
  }
  base
}

# The default theme for a plot that sets none: the brand theme when
# `options(vellumplot.brand)` asks for one and a brand is found (see
# `theme_brand()`), else the gray default.
.theme_auto <- function() .brand_auto_theme() %||% .theme_default()

# The theme a composition's figure (page background, title bands, collected
# legend chrome) draws with: the composition's own theme when it has one;
# else, when any leaf plot sets a theme, the first leaf's (so composed dark or
# branded plots get a matching page) with the figure-level settings
# (legend placement, spacing, margins) left at their defaults; else the
# automatic default (`.theme_auto()`), the brand when auto-apply is on.
.comp_figure_theme <- function(comp) {
  if (!is.null(comp@theme)) {
    return(comp@theme)
  }
  leaves <- .composition_leaf_plots(comp)
  themed <- vapply(leaves, function(p) !is.null(p@theme), logical(1))
  if (!any(themed)) {
    return(.theme_auto())
  }
  th <- .theme_of(leaves[[1]])
  th[intersect(names(th), .theme_setting_names)] <- NULL
  th
}

# Neon qualitative palette for theme_cyberpunk (bright saturated hues on a dark
# canvas). Ordered so the first few series stay maximally distinct.
.NEON_QUAL <- c(
  "#08F7FE", # cyan
  "#FE53BB", # magenta
  "#F5D300", # yellow
  "#00FF9F", # green
  "#9D72FF", # violet
  "#FF6E27" # orange
)

# Neon sequential ramp (dark base -> cyan -> magenta) for continuous fills.
.NEON_RAMP <- c("#0d0f18", "#08F7FE", "#FE53BB")

# The plot-wide sketch default (a `vellum_sketch` from theme_sketch()), or NULL.
# Rides the theme as a plain key, like `palette`; mark emitters read it as the
# fallback when a layer sets no `sketch =` of its own (see `.mark_sketch`).
.theme_sketch_default <- function(spec) {
  # `.theme_of()` without building the gray default, which has no sketch.
  th <- spec@theme
  s <- (if (is.null(th) || .is_theme_tweak(th)) {
    .theme_replay(.brand_auto_theme() %||% list(), th)
  } else {
    th
  })[["sketch"]]
  if (inherits(s, "vellum_sketch")) s else NULL
}

# A theme-supplied default palette for a colour scale, or NULL: `palette` for a
# discrete scale, `palette.continuous` for continuous/binned. Read from the raw
# theme list (these keys ride the theme but are not drawn elements).
.theme_palette <- function(spec, kind) {
  # `.theme_of()` without building the gray default, which has no palette.
  th <- spec@theme
  if (is.null(th) || .is_theme_tweak(th)) {
    th <- .theme_replay(.brand_auto_theme() %||% list(), th)
  }
  if (identical(kind, "discrete")) {
    th[["palette"]]
  } else {
    th[["palette.continuous"]]
  }
}

# Legacy NA-as-"draw nothing" colour -> element (for set_theme back-compat).
.legacy_rect <- function(col) {
  if (is.na(col)) element_blank() else element_rect(fill = col, colour = NA)
}
.legacy_line <- function(col) {
  if (is.na(col)) element_blank() else element_line(colour = col)
}

#' Plot themes
#'
#' Control the non-data look of a plot. `theme_gray()` is the default (grey panel,
#' white gridlines); `theme_minimal()` drops the panel fill for light gridlines on
#' the page; `theme_bw()` is a white panel with light grey gridlines;
#' `theme_classic()` has axis lines and no gridlines; `theme_void()` strips
#' everything but the marks, legend, and titles, with no plot margin (as in
#' ggplot2); `theme_cyberpunk()` is a dark
#' neon theme (see Details).
#'
#' [theme()] overrides individual elements on top of the current theme using
#' [element_text()] / [element_line()] / [element_rect()] / [element_blank()].
#' `set_theme()` is a small back-compatible shortcut for the most common colours.
#' On a plot with no theme of its own, both apply on top of the default theme
#' chosen when the plot is drawn (the brand theme under
#' `options(vellumplot.brand)`, see [theme_brand()], else `theme_gray()`).
#'
#' @param plot A [PlotSpec]. [theme()] also accepts a `PlotComposition`, setting
#'   the figure-level chrome (title bands, collected legend, panel spacing, tags).
#' @param ... Named theme elements, e.g. `plot.title = element_text(size = 16)`,
#'   `panel.grid.minor = element_blank()`, or settings like `legend.position`,
#'   one of `"right"` (default), `"left"`, `"top"`, `"bottom"`, `"none"`, or
#'   `"inside"`. An inside legend sits over the panels at
#'   `legend.position.inside = c(x, y)` (fractions of the panel area; the older
#'   `legend.position = c(x, y)` works too), its `legend.justification` point
#'   (`c(x, y)` or words such as `c("right", "top")`, defaulting to the
#'   position) on that spot. Legends drawn beside each facet panel
#'   (`guide_legend(position = "panel")`) sit on the side `legend.position`
#'   names, justified along the panel edge by `legend.justification` (e.g.
#'   `"top"` aligns a legend right of a panel with the panel's top).
#'   `geom = element_geom(ink, paper, accent)` sets the default colours of
#'   unmapped marks (see [element_geom()]).
#'   Legend geometry is tunable via `legend.key.size` (key/swatch side, mm),
#'   `legend.key.point.min` (the smallest radius, mm, a point key of a layer
#'   with a constant `size` is drawn at, default 1 -- an unsized point's
#'   radius -- so tiny points keep readable keys; a size legend keeps its
#'   mapped sizes), `legend.spacing` (gap between stacked guides, mm), and `legend.margin`
#'   (inset around the legend block, one or four millimetres, `t, r, b, l`).
#' @param base_size Base text size in points (default 11). Every text element
#'   is sized relative to it, so `theme_gray(base_size = 14)` scales titles,
#'   axis text, legend text and strips together.
#' @param base_family Base font family for all text; `""` (default) keeps the
#'   system default.
#' @param panel_bg,grid_col,label_col,strip_bg Colours (or `NA` to draw nothing)
#'   for the panel background, gridlines, axis-label/legend text, and facet strip
#'   background.
#' @return The modified [PlotSpec].
#' @examples
#' vplot(mtcars) |> mark_point(x = wt, y = mpg) |> theme_minimal()
#' vplot(mtcars) |>
#'   mark_point(x = wt, y = mpg) |>
#'   theme(panel.grid.minor = element_blank(), legend.position = "none")
#'
#' # a legend tucked into the top-right corner of the panel
#' vplot(mtcars) |>
#'   mark_point(x = wt, y = mpg, color = factor(cyl)) |>
#'   theme(legend.position = "inside", legend.justification = c(1, 1))
#' @export
theme_gray <- function(plot, base_size = 11, base_family = "") {
  .check_plot(plot)
  plot@theme <- .theme_gray_complete(base_size, base_family)
  attr(plot@theme, "vp_preset") <- "gray"
  plot
}

#' @rdname theme_gray
#' @export
theme_minimal <- function(plot, base_size = 11, base_family = "") {
  .check_plot(plot)
  plot@theme <- .merge_theme(
    .theme_gray_complete(base_size, base_family),
    list(
      panel.background = element_blank(),
      panel.grid = element_line(colour = "grey92"),
      strip.background = element_blank(),
      axis.text = element_text(colour = "grey30"),
      legend.text = element_text(colour = "grey30"),
      axis.ticks = element_blank()
    )
  )
  attr(plot@theme, "vp_preset") <- "minimal"
  plot
}

#' @rdname theme_gray
#' @export
theme_bw <- function(plot, base_size = 11, base_family = "") {
  .check_plot(plot)
  plot@theme <- .merge_theme(
    .theme_gray_complete(base_size, base_family),
    list(
      panel.background = element_rect(fill = "white", colour = NA),
      panel.grid = element_line(colour = "grey90")
    )
  )
  attr(plot@theme, "vp_preset") <- "bw"
  plot
}

#' @rdname theme_gray
#' @export
theme_classic <- function(plot, base_size = 11, base_family = "") {
  .check_plot(plot)
  plot@theme <- .merge_theme(
    .theme_gray_complete(base_size, base_family),
    list(
      panel.background = element_rect(fill = "white", colour = NA),
      panel.grid = element_blank(),
      axis.line = element_line(colour = "black"),
      axis.ticks = element_line(colour = "black"),
      strip.background = element_blank()
    )
  )
  attr(plot@theme, "vp_preset") <- "classic"
  plot
}

#' @rdname theme_gray
#' @export
theme_void <- function(plot, base_size = 11, base_family = "") {
  .check_plot(plot)
  plot@theme <- .merge_theme(
    .theme_gray_complete(base_size, base_family),
    list(
      panel.background = element_blank(),
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      axis.line = element_blank(),
      strip.background = element_blank(),
      # no margin, as in ggplot2: a void plot fills its box (VP-128)
      plot.margin = c(0, 0, 0, 0)
    )
  )
  attr(plot@theme, "vp_preset") <- "void"
  plot
}

#' @rdname theme_gray
#' @export
theme_light <- function(plot, base_size = 11, base_family = "") {
  .check_plot(plot)
  plot@theme <- .merge_theme(
    .theme_gray_complete(base_size, base_family),
    list(
      panel.background = element_rect(fill = "white", colour = NA),
      panel.grid = element_line(colour = "grey87"),
      axis.ticks = element_line(colour = "grey70"),
      axis.text = element_text(colour = "grey30"),
      strip.background = element_rect(fill = "grey70", colour = NA)
    )
  )
  attr(plot@theme, "vp_preset") <- "light"
  plot
}

#' @rdname theme_gray
#' @export
theme_dark <- function(plot, base_size = 11, base_family = "") {
  .check_plot(plot)
  plot@theme <- .merge_theme(
    .theme_gray_complete(base_size, base_family),
    list(
      panel.background = element_rect(fill = "grey50", colour = NA),
      panel.grid = element_line(colour = "grey42"),
      axis.ticks = element_line(colour = "grey20"),
      strip.background = element_rect(fill = "grey15", colour = NA)
    )
  )
  attr(plot@theme, "vp_preset") <- "dark"
  plot
}

#' @rdname theme_gray
#' @export
theme_linedraw <- function(plot, base_size = 11, base_family = "") {
  .check_plot(plot)
  plot@theme <- .merge_theme(
    .theme_gray_complete(base_size, base_family),
    list(
      panel.background = element_rect(fill = "white", colour = NA),
      panel.grid = element_line(colour = "black", linewidth = 0.1),
      axis.ticks = element_line(colour = "black"),
      axis.text = element_text(colour = "black"),
      strip.background = element_rect(fill = "black", colour = NA)
    )
  )
  attr(plot@theme, "vp_preset") <- "linedraw"
  plot
}

#' @rdname theme_gray
#' @details
#' `theme_cyberpunk()` sets a dark canvas with dim neon gridlines and a bright
#' neon default palette (both discrete and continuous), in the spirit of
#' [mplcyberpunk](https://github.com/dhaitz/mplcyberpunk). It pairs with the
#' [glow()] layer effect and [linear_gradient()] fills for the full neon look;
#' the palette is only a *default*, so `scale_*` overrides still win. Unmapped
#' marks draw in the light foreground colour (`element_geom(ink = "#c7d0f0")`),
#' with smooths and contours in neon cyan.
#' @export
theme_cyberpunk <- function(plot, base_size = 11, base_family = "") {
  .check_plot(plot)
  ink <- "#0d0f18" # near-black canvas
  grid <- "#2b3350"
  fg <- "#c7d0f0"
  th <- .merge_theme(
    .theme_gray_complete(base_size, base_family),
    list(
      text = element_text(colour = fg),
      plot.title = element_text(colour = "#08F7FE"),
      plot.subtitle = element_text(colour = "#8fa0c8"),
      plot.caption = element_text(colour = "#8fa0c8"),
      plot.background = element_rect(fill = ink, colour = NA),
      panel.background = element_rect(fill = ink, colour = NA),
      panel.grid.major = element_line(colour = grid, linewidth = 0.6),
      panel.grid.minor = element_line(colour = "#1c2136", linewidth = 0.4),
      axis.text = element_text(colour = "#8fa0c8"),
      legend.text = element_text(colour = "#8fa0c8"),
      axis.ticks = element_line(colour = grid),
      strip.background = element_rect(fill = "#161b2e", colour = NA),
      strip.text = element_text(colour = fg)
    )
  )
  # Unmapped marks draw in the light foreground (points, lines, text), bars and
  # areas in a mix of it with the canvas, smooths and contours in neon cyan.
  th[["geom"]] <- element_geom(ink = fg, paper = ink, accent = "#08F7FE")
  # Neon palette defaults ride the theme (read by .train_colour when no scale
  # palette is set); they are plain keys, not drawn elements.
  th[["palette"]] <- .NEON_QUAL
  th[["palette.continuous"]] <- .NEON_RAMP
  attr(th, "vp_preset") <- "cyberpunk"
  plot@theme <- th
  plot
}

#' Hand-drawn plot theme
#'
#' `theme_sketch()` turns a whole plot hand-drawn in one line: it sets a
#' plot-wide [sketch()] default that every geometry mark and theme element
#' inherits (wobbly gridlines, axis lines, ticks, and marks), on a warm
#' paper-toned background. Text is never sketched — pass a handwriting `font` to
#' complete the look.
#'
#' It is a complete theme (like [theme_cyberpunk()]): the sketch it sets is only
#' a *default*, so any mark's `sketch =` argument, any [element_line()] /
#' [element_rect()] `sketch =` slot, or a `scale_*` override still wins. Force an
#' individual element crisp with `sketch = NA`.
#'
#' @param plot A [PlotSpec].
#' @param roughness,bowing,fill_style,seed Passed to [sketch()] to build the
#'   plot-wide default (see [sketch()] for the full set of knobs).
#' @param font Font family for all text (text is not sketched; a handwriting font
#'   such as `"Comic Sans MS"` or `"Chilanka"` sells the hand-drawn look).
#'   `NULL` (default) keeps the system default family.
#' @param paper,ink Background and foreground (text/line) colours. Setting
#'   either also makes them the default mark colours ([element_geom()]), so
#'   unmapped marks draw in `ink` and fills mix `ink` with `paper`.
#' @return The modified [PlotSpec].
#' @seealso [sketch()], [theme_gray()], [mark_point()]
#' @examples
#' vplot(mtcars) |>
#'   mark_point(x = wt, y = mpg, color = factor(cyl)) |>
#'   theme_sketch()
#'
#' # tune the wobble; a crosshatch-filled bar layer that stays crisp
#' df <- data.frame(g = c("a", "b", "c"), n = c(3, 5, 2))
#' vplot(df) |>
#'   mark_bar(x = g, y = n, fill = g) |>
#'   theme_sketch(roughness = 1.4, fill_style = "crosshatch", seed = 7)
#' @export
theme_sketch <- function(
  plot,
  roughness = 1,
  bowing = 1,
  fill_style = "hachure",
  seed = 1L,
  font = NULL,
  paper = "#f4ecd8",
  ink = "#2b2b2b"
) {
  .check_plot(plot)
  sk <- sketch(
    roughness = roughness,
    bowing = bowing,
    fill_style = fill_style,
    seed = seed
  )
  grid <- "#c7b98f" # muted pencil grid on paper
  # Sketch-ify the plot's CURRENT theme rather than resetting to grey. The ruled
  # grid, axis lines and ticks are part of the sketch look, but a self-contained
  # chart (vwaffle/vvenn/vsankey) is deliberately axis-free -- hand-drawing it must
  # not draw a coordinate frame it never had. Gate all of that on whether the base
  # theme actually shows an axis at all (its resolved `axis.text`, which those
  # charts blank): an axis chart gets the full frame; an axis-free one keeps its
  # blanks. The paper/ink palette applies to both.
  base <- .theme_of(plot)
  cache <- new.env(parent = emptyenv())
  has_axes <- !.is_blank(.resolve_slot("axis.text", base, cache))
  frame <- function(el) if (has_axes) el else element_blank()
  th <- .merge_theme(
    base,
    list(
      text = element_text(colour = ink, family = font),
      plot.title = element_text(colour = ink),
      plot.background = element_rect(fill = paper, colour = NA),
      panel.background = element_rect(fill = paper, colour = NA),
      # the ruled grid, axis lines and ticks go hand-drawn -- only on an axis chart.
      panel.grid.major = frame(
        element_line(colour = grid, linewidth = 0.6, sketch = sk)
      ),
      panel.grid.minor = frame(
        element_line(colour = grid, linewidth = 0.4, sketch = sk)
      ),
      axis.line = frame(
        element_line(colour = ink, linewidth = 0.8, sketch = sk)
      ),
      axis.ticks = frame(element_line(colour = ink, sketch = sk)),
      axis.text = frame(element_text(colour = ink)),
      strip.background = element_rect(fill = "#e7dcc0", colour = NA),
      strip.text = element_text(colour = ink)
    )
  )
  # Marks follow a custom paper / ink (`element_geom()`); the default pair keeps
  # them as on any light theme.
  if (!missing(ink) || !missing(paper)) {
    th <- .merge_theme(th, list(geom = element_geom(ink = ink, paper = paper)))
  }
  # The plot-wide mark default rides the theme as a plain key (read by mark
  # emitters + legend keys via `.theme_sketch_default`), like `palette`.
  th[["sketch"]] <- sk
  plot@theme <- th
  plot
}

#' @rdname theme_gray
#' @export
theme <- function(plot, ...) {
  args <- rlang::list2(...)
  .validate_theme_args(args)
  # A composition carries its own figure-level theme (title bands, collected
  # legend, panel spacing, tags); merge onto its base like a plot's.
  if (S7::S7_inherits(plot, PlotComposition)) {
    plot@theme <- .mark_unblank(
      .merge_theme(plot@theme %||% .theme_auto(), args),
      args
    )
    return(plot)
  }
  .check_plot(plot)
  # An unthemed plot stores the tweak, resolved onto the automatic (brand or
  # gray) default at compile time (VP-133); a themed one merges now.
  plot@theme <- if (is.null(plot@theme) || .is_theme_tweak(plot@theme)) {
    .theme_add_tweak(plot@theme, args)
  } else {
    .mark_unblank(.merge_theme(plot@theme, args), args)
  }
  plot
}

#' @rdname theme_gray
#' @export
set_theme <- function(
  plot,
  panel_bg = NULL,
  grid_col = NULL,
  label_col = NULL,
  strip_bg = NULL
) {
  .check_plot(plot)
  over <- list()
  if (!is.null(panel_bg)) {
    over$panel.background <- .legacy_rect(panel_bg)
  }
  if (!is.null(grid_col)) {
    over$panel.grid <- .legacy_line(grid_col)
  }
  if (!is.null(label_col)) {
    over$axis.text <- element_text(colour = label_col)
    over$legend.text <- element_text(colour = label_col)
  }
  if (!is.null(strip_bg)) {
    over$strip.background <- .legacy_rect(strip_bg)
  }
  plot@theme <- if (is.null(plot@theme) || .is_theme_tweak(plot@theme)) {
    .theme_add_tweak(plot@theme, over, unblank = FALSE)
  } else {
    .merge_theme(plot@theme, over)
  }
  plot
}
