#' @include seam.R compile-resolve.R
NULL

# A discrete legend beyond this many levels is not readable as a legend.
.LINT_MAX_LEGEND <- 24L

# Legend-bearing aesthetics whose *discrete* trained scales get grammar-level
# lint (position scales are not legends; continuous ones get a colour bar, not a
# per-level key). `fill` folds into `color` upstream, so it is covered here.
.LINT_LEGEND_AES <- c(
  "color",
  "shape",
  "pattern",
  "linetype",
  "edge_color",
  "edge_linetype"
)

# Grammar-aware lint from the trained scales: findings vellum cannot see because
# they are about the *encoding*, not the resolved geometry. Returns the same
# four columns `vl_lint()` does (`rule`, `severity`, `node`, `message`).
.plot_lint_grammar <- function(x) {
  empty <- data.frame(
    rule = character(),
    severity = character(),
    node = character(),
    message = character(),
    stringsAsFactors = FALSE
  )
  built <- tryCatch(.build_panels(x), error = function(e) NULL)
  if (is.null(built)) {
    return(empty)
  }
  scales <- built$scales
  rows <- list()
  add <- function(rule, severity, node, message) {
    rows[[length(rows) + 1L]] <<- data.frame(
      rule = rule,
      severity = severity,
      node = node,
      message = message,
      stringsAsFactors = FALSE
    )
  }
  for (aes in .LINT_LEGEND_AES) {
    sc <- scales[[aes]]
    if (is.null(sc) || !identical(sc$kind, "discrete")) {
      next
    }
    k <- length(sc$levels)
    if (k == 1L) {
      add(
        "single_level_scale",
        "note",
        paste0("scale:", aes),
        sprintf(
          "The %s scale has a single level (%s): the encoding conveys nothing and its legend is redundant.",
          aes,
          sc$levels[1]
        )
      )
    } else if (k > .LINT_MAX_LEGEND) {
      add(
        "legend_overflow",
        "warning",
        paste0("scale:", aes),
        sprintf(
          "The %s scale has %d levels: a legend that long is unreadable -- consider binning, faceting, or direct labels.",
          aes,
          k
        )
      )
    }
  }
  if (!length(rows)) {
    return(empty)
  }
  do.call(rbind, rows)
}

#' Lint a plot for legibility and accessibility problems
#'
#' `plot_lint()` compiles a plot and reports the design problems a static render
#' hides from a green test suite: text too small to read, colour contrast below
#' the WCAG threshold, labels that overlap or fall off the panel, and
#' grammar-level mistakes such as an encoding with a single level or a legend too
#' long to read. It is the "flag it" step of the accessibility workflow — pair it
#' with `render_plot(cvd = )` to *see* a failing palette and [scale_pattern()] /
#' [pattern_hatch()] to *fix* it with a redundant non-colour encoding.
#'
#' The geometric rules (contrast, text size, overlap, off-canvas) come from the
#' engine's [vellum::vl_lint()], which judges them in resolved device pixels; the
#' grammar rules are added here from the trained scales. Findings are returned
#' most-severe first.
#'
#' @param x A [PlotSpec] (or anything [vellum::as_vellum_scene()] accepts).
#' @param min_text_px Minimum legible text height in pixels (default `7`); text
#'   below it is flagged.
#' @param min_contrast Minimum acceptable contrast ratio between a mark and its
#'   background (default `3`, the WCAG AA threshold for graphical objects).
#' @return A data frame (class `vellum_lint`) with one row per finding: `rule`,
#'   `severity` (`"warning"`/`"note"`), `node`, and a human `message`. Zero rows
#'   when the plot is clean.
#' @seealso [render_plot()] (`cvd =`), [scale_pattern()], [pattern_hatch()],
#'   [vellum::vl_lint()]
#' @examples
#' # a tiny-text, single-level-scale plot trips the linter
#' p <- vplot(mtcars) |>
#'   mark_point(x = wt, y = mpg, color = "one group") |>
#'   theme(axis.text = element_text(size = 2))
#' plot_lint(p)
#' @export
plot_lint <- function(x, min_text_px = 7, min_contrast = 3) {
  scene <- vellum::as_vellum_scene(x)
  geo <- as.data.frame(vellum::vl_lint(
    scene,
    min_text_px = min_text_px,
    min_contrast = min_contrast
  ))
  cols <- c("rule", "severity", "node", "message")
  out <- rbind(geo[cols], .plot_lint_grammar(x))
  # warnings before notes, otherwise stable; reuse the engine's grouped printer.
  out <- out[order(match(out$severity, c("warning", "note"))), , drop = FALSE]
  rownames(out) <- NULL
  class(out) <- c("vellum_lint", "data.frame")
  out
}
