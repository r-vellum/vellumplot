#' @include classes.R elements.R theme-tree.R
NULL

# A theme is a *complete* element theme: a named list keyed by element slots
# (element_*() objects) plus scalar settings (see theme-tree.R). Presets build a
# complete theme; theme() / set_theme() element-merge a partial onto it. The
# compile pipeline resolves it once (.resolve_theme) before drawing.

# The default (gray) theme. Axis/legend titles inherit black from the text root;
# the tick/line/minor elements carry their intended values for the drawers.
.theme_gray_complete <- function() {
  list(
    text = element_text(colour = "black", size = 11),
    plot.title = element_text(size = 14, colour = "grey10"),
    plot.subtitle = element_text(size = 11, colour = "grey30"),
    plot.caption = element_text(size = 9, colour = "grey30"),
    plot.tag = element_text(size = 12, colour = "grey10"),
    legend.title = element_text(size = 10),
    axis.text = element_text(size = 9, colour = "grey20"),
    legend.text = element_text(size = 9, colour = "grey20"),
    strip.text = element_text(size = 9, colour = "grey10"),
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

# The resolved-or-default complete theme for a spec.
.theme_of <- function(spec) spec@theme %||% .theme_default()

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
  th <- spec@theme
  if (is.null(th)) {
    return(NULL)
  }
  s <- th[["sketch"]]
  if (inherits(s, "vellum_sketch")) s else NULL
}

# A theme-supplied default palette for a colour scale, or NULL: `palette` for a
# discrete scale, `palette.continuous` for continuous/binned. Read from the raw
# theme list (these keys ride the theme but are not drawn elements).
.theme_palette <- function(spec, kind) {
  th <- spec@theme
  if (is.null(th)) {
    return(NULL)
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
#' everything but the marks, legend, and titles; `theme_cyberpunk()` is a dark
#' neon theme (see Details).
#'
#' [theme()] overrides individual elements on top of the current theme using
#' [element_text()] / [element_line()] / [element_rect()] / [element_blank()].
#' `set_theme()` is a small back-compatible shortcut for the most common colours.
#'
#' @param plot A [PlotSpec].
#' @param ... Named theme elements, e.g. `plot.title = element_text(size = 16)`,
#'   `panel.grid.minor = element_blank()`, or settings like `legend.position`,
#'   one of `"right"` (default), `"left"`, `"top"`, `"bottom"`, or `"none"`.
#'   Legend geometry is tunable via `legend.key.size` (key/swatch side, mm),
#'   `legend.spacing` (gap between stacked guides, mm), and `legend.margin`
#'   (inset around the legend block, one or four millimetres, `t, r, b, l`).
#' @param panel_bg,grid_col,label_col,strip_bg Colours (or `NA` to draw nothing)
#'   for the panel background, gridlines, axis-label/legend text, and facet strip
#'   background.
#' @return The modified [PlotSpec].
#' @examples
#' vplot(mtcars) |> mark_point(x = wt, y = mpg) |> theme_minimal()
#' vplot(mtcars) |>
#'   mark_point(x = wt, y = mpg) |>
#'   theme(panel.grid.minor = element_blank(), legend.position = "none")
#' @export
theme_gray <- function(plot) {
  .check_plot(plot)
  plot@theme <- .theme_gray_complete()
  plot
}

#' @rdname theme_gray
#' @export
theme_minimal <- function(plot) {
  .check_plot(plot)
  plot@theme <- .merge_theme(
    .theme_gray_complete(),
    list(
      panel.background = element_blank(),
      panel.grid = element_line(colour = "grey92"),
      strip.background = element_blank(),
      axis.text = element_text(colour = "grey30"),
      legend.text = element_text(colour = "grey30"),
      axis.ticks = element_blank()
    )
  )
  plot
}

#' @rdname theme_gray
#' @export
theme_bw <- function(plot) {
  .check_plot(plot)
  plot@theme <- .merge_theme(
    .theme_gray_complete(),
    list(
      panel.background = element_rect(fill = "white", colour = NA),
      panel.grid = element_line(colour = "grey90")
    )
  )
  plot
}

#' @rdname theme_gray
#' @export
theme_classic <- function(plot) {
  .check_plot(plot)
  plot@theme <- .merge_theme(
    .theme_gray_complete(),
    list(
      panel.background = element_rect(fill = "white", colour = NA),
      panel.grid = element_blank(),
      axis.line = element_line(colour = "black"),
      axis.ticks = element_line(colour = "black"),
      strip.background = element_blank()
    )
  )
  plot
}

#' @rdname theme_gray
#' @export
theme_void <- function(plot) {
  .check_plot(plot)
  plot@theme <- .merge_theme(
    .theme_gray_complete(),
    list(
      panel.background = element_blank(),
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      axis.line = element_blank(),
      strip.background = element_blank()
    )
  )
  plot
}

#' @rdname theme_gray
#' @details
#' `theme_cyberpunk()` sets a dark canvas with dim neon gridlines and a bright
#' neon default palette (both discrete and continuous), in the spirit of
#' [mplcyberpunk](https://github.com/dhaitz/mplcyberpunk). It pairs with the
#' [glow()] layer effect and [linear_gradient()] fills for the full neon look;
#' the palette is only a *default*, so `scale_*` overrides still win.
#' @export
theme_cyberpunk <- function(plot) {
  .check_plot(plot)
  ink <- "#0d0f18" # near-black canvas
  grid <- "#2b3350"
  fg <- "#c7d0f0"
  th <- .merge_theme(
    .theme_gray_complete(),
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
  # Neon palette defaults ride the theme (read by .train_colour when no scale
  # palette is set); they are plain keys, not drawn elements.
  th[["palette"]] <- .NEON_QUAL
  th[["palette.continuous"]] <- .NEON_RAMP
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
#' @param paper,ink Background and foreground (text/line) colours.
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
  th <- .merge_theme(
    .theme_gray_complete(),
    list(
      text = element_text(colour = ink, family = font),
      plot.title = element_text(colour = ink),
      plot.background = element_rect(fill = paper, colour = NA),
      panel.background = element_rect(fill = paper, colour = NA),
      # the ruled grid, axis lines and ticks go hand-drawn
      panel.grid.major = element_line(
        colour = grid,
        linewidth = 0.6,
        sketch = sk
      ),
      panel.grid.minor = element_line(
        colour = grid,
        linewidth = 0.4,
        sketch = sk
      ),
      axis.line = element_line(colour = ink, linewidth = 0.8, sketch = sk),
      axis.ticks = element_line(colour = ink, sketch = sk),
      axis.text = element_text(colour = ink),
      strip.background = element_rect(fill = "#e7dcc0", colour = NA),
      strip.text = element_text(colour = ink)
    )
  )
  # The plot-wide mark default rides the theme as a plain key (read by mark
  # emitters + legend keys via `.theme_sketch_default`), like `palette`.
  th[["sketch"]] <- sk
  plot@theme <- th
  plot
}

#' @rdname theme_gray
#' @export
theme <- function(plot, ...) {
  .check_plot(plot)
  args <- rlang::list2(...)
  .validate_theme_args(args)
  plot@theme <- .merge_theme(.theme_of(plot), args)
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
  plot@theme <- .merge_theme(.theme_of(plot), over)
  plot
}
