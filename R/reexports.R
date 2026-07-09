#' Rich-text labels
#'
#' A thin wrapper around [vellum::md()] that builds a rich-text label from a
#' markdown subset: `**bold**`, `*italic*` / `_italic_`, `^superscript^`,
#' `~subscript~`, and a colour span `[text]{#c00}`. The result can be used
#' anywhere vellumplot draws a *title*: [labs()] (`title` / `subtitle` /
#' `caption` / `tag` / `x` / `y` / `color`) and `scale_*(name = )`. Per-element
#' rich labels (in `mark_text()`) are not yet supported.
#'
#' @param text A length-one markdown string.
#' @return A `vellum_md_label` object accepted by [vellum::text_grob()].
#' @examples
#' vplot(mtcars) |>
#'   mark_point(x = wt, y = mpg) |>
#'   labs(title = md("**Fuel** economy"), y = md("Efficiency (mi gal^-1^)"))
#' @export
md <- function(text) {
  vellum::md(text)
}

#' Gradient fill paints
#'
#' Thin re-exports of [vellum::linear_gradient()] and [vellum::radial_gradient()].
#' A gradient is an unscaled *value* for the `fill` aesthetic: pass it directly,
#' e.g. `mark_area(x = t, y = y, fill = linear_gradient(c("#00e5ff", "#00e5ff00")))`,
#' and the filled region (area / ribbon / bar) is painted with the paint as a
#' single grob. Use `"transparent"` (or an `"#RRGGBB00"` colour) as a stop to fade
#' out — the "glow fade under a line" look. The gradient's `x1`/`y1`/`x2`/`y2`
#' (in `units`, `"npc"` by default) set its direction. A gradient cannot be
#' *mapped* to a data column (it is one paint per region).
#'
#' @param colours,stops,x1,y1,x2,y2,r,units,extend See [vellum::linear_gradient()]
#'   / [vellum::radial_gradient()].
#' @param ... Passed through to the vellum constructor.
#' @return A `vellum_gradient` object usable as a `fill` value.
#' @seealso [glow()], [theme_cyberpunk()]
#' @examples
#' df <- data.frame(x = 1:20, y = cumsum(abs(rnorm(20))))
#' vplot(df) |>
#'   mark_area(x = x, y = y, fill = linear_gradient(c("#00e5ff", "#00e5ff00"),
#'     x1 = 0, y1 = 1, x2 = 0, y2 = 0))
#' @name gradients
#' @export
linear_gradient <- function(colours, stops = NULL, ...) {
  vellum::linear_gradient(colours, stops = stops, ...)
}

#' @rdname gradients
#' @export
radial_gradient <- function(colours, stops = NULL, ...) {
  vellum::radial_gradient(colours, stops = stops, ...)
}

#' Hand-drawn ("sketch") rendering
#'
#' A re-export of [vellum::sketch()] — the one vocabulary vellumplot speaks for the
#' hand-drawn look (wobbly outlines, hachure fills, à la
#' [Rough.js](https://roughjs.com)). Pass a `sketch()` value to any mark's
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
#' Text is never sketched — pair a handwriting `family` with it for a fully
#' hand-drawn plot.
#'
#' Resolution is most-specific-wins: a mark's `sketch =` beats an element slot,
#' which beats the plot-wide [theme_sketch()] default. At any level `sketch = NA`
#' (or `FALSE`) forces that element crisp, overriding a broader default;
#' `sketch = NULL` inherits.
#'
#' @param roughness Wobble amount (`>= 0`; `0` is nearly crisp, `1` the default,
#'   higher is wilder).
#' @param bowing How much straight edges bow.
#' @param fill_style One of `"hachure"`, `"solid"`, `"crosshatch"`, `"zigzag"`,
#'   `"dots"`.
#' @param fill_weight,hachure_angle,hachure_gap,curve_tightness,disable_multi_stroke,preserve_vertices,seed
#'   Further tuning; see [vellum::sketch()].
#' @return A `vellum_sketch` object.
#' @seealso [theme_sketch()], [mark_point()], [element_line()]
#' @examples
#' vplot(mtcars) |>
#'   mark_point(x = wt, y = mpg, sketch = sketch(roughness = 1.5, seed = 7))
#' @export
sketch <- function(
  roughness = 1,
  bowing = 1,
  fill_style = c("hachure", "solid", "crosshatch", "zigzag", "dots"),
  fill_weight = NULL,
  hachure_angle = -41,
  hachure_gap = NULL,
  curve_tightness = 0,
  disable_multi_stroke = FALSE,
  preserve_vertices = FALSE,
  seed = 1L
) {
  vellum::sketch(
    roughness = roughness,
    bowing = bowing,
    fill_style = fill_style,
    fill_weight = fill_weight,
    hachure_angle = hachure_angle,
    hachure_gap = hachure_gap,
    curve_tightness = curve_tightness,
    disable_multi_stroke = disable_multi_stroke,
    preserve_vertices = preserve_vertices,
    seed = seed
  )
}
