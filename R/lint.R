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

# The trained scales behind a compiled scene, or NULL when there are none to be
# had. Grammar rules live in vellum's registry, which hands a rule the resolved
# scene; `.compile_plot()` leaves the spec on the scene so the encoding is still
# reachable from there. A scene that did not come from vellumplot has no
# attribute, which is how these rules stay silent on other people's scenes.
.lint_scales <- function(scene) {
  spec <- attr(scene, "vellumplot_spec", exact = TRUE)
  if (is.null(spec)) {
    return(NULL)
  }
  built <- tryCatch(.build_panels(spec), error = function(e) NULL)
  built$scales
}

# The discrete, legend-bearing scales, as (aes, scale) pairs.
.lint_discrete_scales <- function(scene) {
  scales <- .lint_scales(scene)
  keep <- vapply(
    .LINT_LEGEND_AES,
    function(a) {
      sc <- scales[[a]]
      !is.null(sc) && identical(sc$kind, "discrete")
    },
    logical(1)
  )
  .LINT_LEGEND_AES[keep]
}

# One grammar finding. Not built with `vellum::vl_lint_finding()`: that is
# vectorised over rows of the node table and these findings are about a scale,
# which has no node and no box. vellum fills the geometry columns with NA.
.lint_scale_finding <- function(rule, severity, aes, message) {
  data.frame(
    rule = rule,
    severity = severity,
    node = paste0("scale:", aes),
    message = message,
    stringsAsFactors = FALSE
  )
}

# Grammar-aware rules: findings vellum cannot see because they are about the
# *encoding* rather than the resolved geometry. Registered from `.onLoad()`, so
# `vellum::vl_lint()` on a compiled plot reports them alongside the geometric
# ones without going through `plot_lint()` at all.
.register_lint_rules <- function() {
  vellum::vl_lint_rule(
    "single_level_scale",
    function(scene, nodes, ctx) {
      out <- lapply(.lint_discrete_scales(scene), function(aes) {
        sc <- .lint_scales(scene)[[aes]]
        if (length(sc$levels) != 1L) {
          return(NULL)
        }
        .lint_scale_finding(
          "single_level_scale",
          "note",
          aes,
          sprintf(
            "The %s scale has a single level (%s): the encoding conveys nothing and its legend is redundant.",
            aes,
            sc$levels[1]
          )
        )
      })
      out <- do.call(rbind, out[!vapply(out, is.null, logical(1))])
      out
    },
    "A discrete encoding has one level, so it encodes nothing.",
    tags = "grammar"
  )

  vellum::vl_lint_rule(
    "legend_overflow",
    function(scene, nodes, ctx) {
      out <- lapply(.lint_discrete_scales(scene), function(aes) {
        sc <- .lint_scales(scene)[[aes]]
        k <- length(sc$levels)
        if (k <= .LINT_MAX_LEGEND) {
          return(NULL)
        }
        .lint_scale_finding(
          "legend_overflow",
          "warning",
          aes,
          sprintf(
            "The %s scale has %d levels: a legend that long is unreadable -- consider binning, faceting, or direct labels.",
            aes,
            k
          )
        )
      })
      out <- do.call(rbind, out[!vapply(out, is.null, logical(1))])
      out
    },
    "A discrete encoding has more levels than a legend can show.",
    tags = "grammar"
  )
}

#' Lint a plot for legibility and accessibility problems
#'
#' `plot_lint()` compiles a plot and reports the design problems a static render
#' hides from a green test suite: text too small to read, colour contrast below
#' the WCAG threshold, two palette colours a colour-blind reader cannot tell
#' apart, labels that overlap or fall off the panel, and grammar-level mistakes
#' such as an encoding with a single level or a legend too long to read. It is
#' the "flag it" step of the accessibility workflow — pair it with
#' `render_plot(cvd = )` to *see* a failing palette and [scale_pattern()] /
#' [pattern_hatch()] to *fix* it with a redundant non-colour encoding.
#'
#' The geometric rules come from the engine's [vellum::vl_lint()], which judges
#' them in resolved device pixels. The grammar rules are registered into the same
#' registry by vellumplot, so they are not special: [vellum::vl_lint_rules()]
#' lists them, `rules =` selects them, and `vellum::vl_lint()` on a compiled plot
#' reports them too. Findings are returned most-severe first.
#'
#' Because every finding carries the box it refers to,
#' [vellum::vl_lint_overlay()] can draw the report onto the plot, and
#' [vellum::vl_lint_assert()] can fail a test on it.
#'
#' @param x A [PlotSpec] (or anything [vellum::as_vellum_scene()] accepts).
#' @param ... Passed to [vellum::vl_lint()]: `rules`, `exclude`, `severity`,
#'   `cvd`, `min_text_pt`, `max_overplot` and the rest of the thresholds.
#' @param min_text_px Minimum legible text height in pixels (default `7`); text
#'   below it is flagged.
#' @param min_contrast Minimum acceptable contrast ratio between a mark and its
#'   background (default `3`, the WCAG AA threshold for graphical objects).
#' @return A data frame (class `vellum_lint`) with one row per finding: `rule`,
#'   `severity` (`"warning"`/`"note"`), `node`, a human `message`, and the
#'   device-px box `x0`/`y0`/`x1`/`y1` — `NA` for a grammar finding, which is
#'   about a scale and so has no box. Zero rows when the plot is clean.
#' @seealso [render_plot()] (`cvd =`), [scale_pattern()], [pattern_hatch()],
#'   [vellum::vl_lint()], [vellum::vl_lint_overlay()], [vellum::vl_lint_assert()]
#' @examples
#' # a tiny-text, single-level-scale plot trips the linter
#' p <- vplot(transform(mtcars, grp = "one group")) |>
#'   mark_point(x = wt, y = mpg, color = grp) |>
#'   theme(axis.text = element_text(size = 2))
#' plot_lint(p)
#'
#' # the engine's arguments come through, so a project can accept a finding
#' plot_lint(p, exclude = "scale:color")
#' @export
plot_lint <- function(x, ..., min_text_px = 7, min_contrast = 3) {
  scene <- vellum::as_vellum_scene(x)
  vellum::vl_lint(
    scene,
    ...,
    min_text_px = min_text_px,
    min_contrast = min_contrast
  )
}
