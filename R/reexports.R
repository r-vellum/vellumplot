# Functions vellumplot re-exports from vellum unchanged. They used to be thin
# wrappers (`md <- function(text) vellum::md(text)`), which made them distinct
# objects, so attaching both packages (as `library(vellumverse)` does) reported
# each one as a conflict. Re-exporting the vellum object itself keeps the name
# available with vellumplot alone and makes the two identical. The topics below
# keep vellumplot's own documentation for them; they carry no \usage because the
# functions are vellum's (see the linked vellum reference for the signatures).

#' Rich-text labels
#'
#' `md(text)` builds a rich-text label from a markdown subset: `**bold**`,
#' `*italic*` / `_italic_`, `^superscript^`, `~subscript~`, a colour span
#' `[text]{#c00}`, and inline math `$...$` (see [tex()]). It is [vellum::md()],
#' re-exported. The result can be used
#' anywhere vellumplot draws a *title*: [labs()] (`title` / `subtitle` /
#' `caption` / `tag` / `x` / `y` / `color`) and `scale_*(name = )`.
#' Per-element rich labels go in `mark_text()` as a list, one per row.
#'
#' `text` is a length-one markdown string; the result is a `vellum_md_label`
#' object accepted by [vellum::text_grob()]. See [vellum::md()] for the full
#' reference.
#'
#' @examples
#' vplot(mtcars) |>
#'   mark_point(x = wt, y = mpg) |>
#'   labs(title = md("**Fuel** economy"), y = md("Efficiency (mi gal^-1^)"))
#' @name md
#' @importFrom vellum md
#' @export md
NULL

#' Math labels
#'
#' `tex()` builds a label from TeX math notation and `plotmath()` from R's
#' plotmath notation (as ggplot2's `parse = TRUE` reads it); `tex()` also
#' takes plotmath unevaluated, `tex(quote(x[i]^2))`. They are [vellum::tex()]
#' and [vellum::plotmath()], re-exported. A math label goes wherever an [md()]
#' label does: [labs()] titles, scale names, discrete scale `labels =`
#' (a list, one label per key), `mark_text()` and `mark_label()` labels (one
#' label, or a list with one per row; or `parse = TRUE` to parse a column),
#' facet strips through [label_parsed()], and [annotate()]. [md()] labels can
#' carry inline math between `$` signs. The formula is typeset by vellum, the
#' same in PNG, SVG and PDF. Alt text ([plot_alt()]) and the SVG's accessible
#' `<title>`/`<desc>` read a math title as plain text instead (Greek letters
#' and common symbols as Unicode: `tex("\\alpha^{2}")` reads "α^2"); the
#' widget's tooltips show its TeX source.
#'
#' @examples
#' vplot(mtcars) |>
#'   mark_point(x = wt, y = mpg) |>
#'   labs(
#'     title = md("Fuel use against weight, $\\hat{y} = \\beta_0 + \\beta_1 x$"),
#'     x = tex("w\\ (10^3\\,\\mathrm{lb})"),
#'     y = plotmath("mpg ~ (mi~gal^-1)")
#'   )
#' @name tex
#' @aliases plotmath
#' @importFrom vellum tex plotmath
#' @export tex plotmath
NULL

#' Gradient fill paints
#'
#' `linear_gradient()` and `radial_gradient()` are [vellum::linear_gradient()]
#' and [vellum::radial_gradient()], re-exported. A gradient is an unscaled
#' *value* for the `fill` aesthetic: pass it directly, e.g.
#' `mark_area(x = t, y = y, fill = linear_gradient(c("#00e5ff", "#00e5ff00")))`,
#' and the filled region (area / ribbon / bar) is painted with the paint as a
#' single grob. Use `"transparent"` (or an `"#RRGGBB00"` colour) as a stop to
#' fade out --- the "glow fade under a line" look. A gradient cannot be
#' *mapped* to a data column (it is one paint per region).
#'
#' Both take `colours` and optional `stops`. A linear gradient's
#' `x1`/`y1`/`x2`/`y2` (in `units`, `"npc"` by default) set its direction; a
#' radial gradient takes `cx`/`cy`/`r` plus the optional focal circle
#' `fx`/`fy`/`fr`. Both also take `extend` and `interpolation` (`"srgb"`
#' default, or `"oklab"` / `"oklch"` to blend the stops perceptually ---
#' `"oklch"` additionally preserves chroma by rotating hue). Each returns a
#' `vellum_gradient` object usable as a `fill` value. See
#' [vellum::linear_gradient()] / [vellum::radial_gradient()] for the full
#' reference.
#'
#' @seealso [glow()], [theme_cyberpunk()]
#' @examples
#' df <- data.frame(x = 1:20, y = cumsum(abs(rnorm(20))))
#' vplot(df) |>
#'   mark_area(x = x, y = y, fill = linear_gradient(c("#00e5ff", "#00e5ff00"),
#'     x1 = 0, y1 = 1, x2 = 0, y2 = 0))
#' @name gradients
#' @aliases linear_gradient radial_gradient
#' @importFrom vellum linear_gradient radial_gradient
#' @export linear_gradient radial_gradient
NULL

#' Custom tiling-pattern fill
#'
#' `vl_pattern()` is [vellum::vl_pattern()], re-exported, for building a
#' pattern fill from an arbitrary tile grob when the ready-made
#' [pattern_stripe()] family does not fit. Like a gradient, the result (a
#' `vellum_pattern` object) is an unscaled `fill` *value*. It takes the tile
#' `grob`, the tile `width` / `height` and anchor `x` / `y` in `units`, and
#' `extend`; see [vellum::vl_pattern()] for the full reference.
#'
#' @seealso [pattern_stripe()], [linear_gradient()]
#' @examples
#' dots <- vellum::circle_grob(r = 0.25, gp = vellum::vl_gpar(fill = "grey30"))
#' vl_pattern(dots, width = 0.08, height = 0.08)
#' @name vl_pattern
#' @importFrom vellum vl_pattern
#' @export vl_pattern
NULL

#' Hand-drawn ("sketch") rendering
#'
#' `sketch()` is [vellum::sketch()], re-exported --- the one vocabulary
#' vellumplot speaks for the hand-drawn look (wobbly outlines, hachure fills, à
#' la [Rough.js](https://roughjs.com)). Pass a `sketch()` value to any mark's
#' `sketch =` argument, to an [element_line()] / [element_rect()] `sketch =`
#' slot, or set it plot-wide with [theme_sketch()]:
#'
#' ```r
#' vplot(mpg) |> mark_point(x = displ, y = hwy, sketch = sketch(roughness = 1.2))
#' ```
#'
#' Sketch is a geometry property, not a layer [effect][glow]: it perturbs the
#' mark itself (its wobble is generated natively in the vellum engine, so it is
#' exact, cross-backend, and works in PDF), rather than compositing extra copies.
#' Text is never sketched --- pair a handwriting `family` with it for a fully
#' hand-drawn plot.
#'
#' Resolution is most-specific-wins: a mark's `sketch =` beats an element slot,
#' which beats the plot-wide [theme_sketch()] default. At any level `sketch = NA`
#' (or `FALSE`) forces that element crisp, overriding a broader default;
#' `sketch = NULL` inherits.
#'
#' Its arguments are the sketch parameters of [vellum::sketch()] ---
#' `roughness`, `bowing`, `fill_style`, `fill_weight`, `hachure_angle`,
#' `hachure_gap`, `curve_tightness`, `disable_multi_stroke`,
#' `preserve_vertices`, `seed`; see there for defaults. It returns a
#' `vellum_sketch` object.
#'
#' @seealso [theme_sketch()], [mark_point()], [element_line()]
#' @examples
#' vplot(mtcars) |>
#'   mark_point(x = wt, y = mpg, sketch = sketch(roughness = 1.5, seed = 7))
#' @name sketch
#' @importFrom vellum sketch
#' @export sketch
NULL
