#' @include vellumplot-package.R
NULL

# Theme elements --------------------------------------------------------------
#
# A theme is built from typed `element_*()` objects. Every visual property
# defaults to NULL, meaning "inherit from this element's parent in the theme
# tree" (see theme-tree.R). `element_blank()` is a marker meaning "draw nothing".
# The element classes are S7 records (serializable, so the spec stays data); the
# exported `element_text()` / `element_line()` / `element_rect()` are thin
# constructors that add the `color`/`colour` alias and `margin` recycling.

.element_text <- S7::new_class(
  ".element_text",
  package = "vellumplot",
  properties = list(
    family = S7::class_any,
    face = S7::class_any,
    colour = S7::class_any,
    size = S7::class_any, # points
    cex = S7::class_any, # relative size multiplier on `size`
    hjust = S7::class_any,
    vjust = S7::class_any,
    angle = S7::class_any,
    lineheight = S7::class_any,
    margin = S7::class_any, # numeric length-4 (t, r, b, l) in mm
    weight = S7::class_any # numeric font weight 100..900 (F-26)
  )
)

.element_line <- S7::new_class(
  ".element_line",
  package = "vellumplot",
  properties = list(
    colour = S7::class_any,
    linewidth = S7::class_any,
    linetype = S7::class_any,
    lineend = S7::class_any,
    # A hand-drawn sketch spec, `NA` for forced-crisp, or NULL to inherit.
    sketch = S7::class_any
  )
)

.element_rect <- S7::new_class(
  ".element_rect",
  package = "vellumplot",
  properties = list(
    fill = S7::class_any,
    colour = S7::class_any,
    linewidth = S7::class_any,
    linetype = S7::class_any,
    sketch = S7::class_any
  )
)

#' Theme elements
#'
#' Typed building blocks for [theme()]. Each describes how a family of theme
#' slots is drawn; any property left `NULL` is inherited from the slot's parent
#' in the theme tree. `element_blank()` draws nothing.
#'
#' @param family,face,size,cex,colour,color,hjust,vjust,angle,lineheight,margin
#'   Text properties. `color` is an alias for `colour`; `size` is in points and
#'   `cex` is a relative multiplier on the inherited `size`; `margin` is a
#'   numeric vector of millimetres (recycled to length 4).
#' @param weight Font weight for text: a number from `100` to `900` in steps
#'   of 100 (300 light, 400 regular, 600 semi-bold, 700 bold) or a CSS weight
#'   name such as `"light"` or `"semi-bold"`, for families that ship more than
#'   regular and bold (see [vellum::vl_gpar()]'s `fontweight`). It keeps the
#'   italic of `face`; a bold `face` draws at least 700, so `face = "bold"` on
#'   a title stays bold under a light inherited weight. `NULL` (default)
#'   inherits.
#' @param linewidth,linetype,lineend Line properties.
#' @param fill Fill colour (rectangles).
#' @param sketch For [element_line()] / [element_rect()], a [sketch()] spec giving
#'   that theme element (gridlines, axis lines, ticks, panel/strip/legend
#'   backgrounds) a hand-drawn look, `NA` to force it crisp, or `NULL` (default)
#'   to inherit from its parent element / the plot-wide [theme_sketch()] default.
#'   Text elements are never sketched.
#' @return An element object for use in [theme()].
#' @examples
#' vplot(mtcars) |>
#'   mark_point(x = wt, y = mpg) |>
#'   theme(plot.title = element_text(size = 16), panel.grid.minor = element_blank())
#' @name element
#' @export
element_text <- function(
  family = NULL,
  face = NULL,
  colour = NULL,
  color = NULL,
  size = NULL,
  cex = NULL,
  hjust = NULL,
  vjust = NULL,
  angle = NULL,
  lineheight = NULL,
  margin = NULL,
  weight = NULL
) {
  colour <- colour %||% color
  if (!is.null(margin)) {
    margin <- rep_len(margin, 4L)
  }
  if (!is.null(weight)) {
    # vellum validates and normalises a weight (a name becomes its number)
    call <- environment()
    weight <- tryCatch(
      vellum::vl_gpar(fontweight = weight)@fontweight,
      vellum_error_fontweight = function(e) {
        cli::cli_abort(
          c(
            "{.arg weight} must be a font weight from 100 to 900 in steps of 100, or a weight name.",
            i = "For example {.code 300}, {.code \"light\"} or {.code \"semi-bold\"}."
          ),
          call = call
        )
      }
    )
  }
  .element_text(
    family = family,
    face = face,
    colour = colour,
    size = size,
    cex = cex,
    hjust = hjust,
    vjust = vjust,
    angle = angle,
    lineheight = lineheight,
    margin = margin,
    weight = weight
  )
}

#' @rdname element
#' @export
element_line <- function(
  colour = NULL,
  color = NULL,
  linewidth = NULL,
  linetype = NULL,
  lineend = NULL,
  sketch = NULL
) {
  .element_line(
    colour = colour %||% color,
    linewidth = linewidth,
    linetype = linetype,
    lineend = lineend,
    sketch = .check_sketch(sketch)
  )
}

#' @rdname element
#' @export
element_rect <- function(
  fill = NULL,
  colour = NULL,
  color = NULL,
  linewidth = NULL,
  linetype = NULL,
  sketch = NULL
) {
  .element_rect(
    fill = fill,
    colour = colour %||% color,
    linewidth = linewidth,
    linetype = linetype,
    sketch = .check_sketch(sketch)
  )
}

.element_geom <- S7::new_class(
  ".element_geom",
  package = "vellumplot",
  properties = list(
    ink = S7::class_any,
    paper = S7::class_any,
    accent = S7::class_any,
    colour = S7::class_any,
    fill = S7::class_any
  )
)

#' Default mark colours of a theme
#'
#' `element_geom()` sets the colours a theme gives marks whose colour or fill
#' is neither mapped nor set, as ggplot2's `element_geom()` does. Pass it as
#' `theme(geom = element_geom(...))`; dark themes such as [theme_cyberpunk()]
#' set a light `ink` so unmapped points, lines and text stay visible.
#'
#' Each mark's default is derived from these as in ggplot2. Points, lines,
#' text, rules and error bars use `ink`; filled marks use a mix of `ink` and
#' `paper` (bars, histograms and areas `col_mix(ink, paper, 0.35)`, the default
#' light-theme `"grey35"`); box plot, violin and label backgrounds use
#' `paper`; fitted smooths and contours use `accent`. A mapped scale and a
#' constant `color =` / `fill =` on a mark always win.
#'
#' @param ink Foreground colour of marks (default `"black"`).
#' @param paper Background colour marks are mixed against (default `"white"`).
#' @param accent Colour of fitted smooths and contour lines (default
#'   `"#3366CC"`).
#' @param colour,color A colour for every unmapped mark stroke, overriding the
#'   colour derived from `ink` / `accent`. `color` is an alias for `colour`.
#' @param fill A fill for every unmapped filled mark, overriding the fill
#'   derived from `ink` / `paper`.
#' @return An element object for the `geom` slot of [theme()].
#' @seealso [theme()], [theme_cyberpunk()]
#' @examples
#' vplot(mtcars) |>
#'   mark_point(x = wt, y = mpg) |>
#'   theme(
#'     plot.background = element_rect(fill = "grey10"),
#'     panel.background = element_rect(fill = "grey15"),
#'     geom = element_geom(ink = "grey90", paper = "grey15")
#'   )
#' @export
element_geom <- function(
  ink = NULL,
  paper = NULL,
  accent = NULL,
  colour = NULL,
  color = NULL,
  fill = NULL
) {
  colour <- colour %||% color
  vals <- list(
    ink = ink,
    paper = paper,
    accent = accent,
    colour = colour,
    fill = fill
  )
  for (nm in names(vals)) {
    v <- vals[[nm]]
    if (!is.null(v) && !.is_colour1(v)) {
      cli::cli_abort("{.arg {nm}} must be a single colour.")
    }
  }
  do.call(.element_geom, vals)
}

# A single valid colour string (a name, hex, or NA).
.is_colour1 <- function(x) {
  if (length(x) != 1L) {
    return(FALSE)
  }
  if (is.na(x)) {
    return(TRUE)
  }
  is.character(x) &&
    !inherits(try(grDevices::col2rgb(x), silent = TRUE), "try-error")
}

#' @rdname element
#' @export
element_blank <- S7::new_class(
  "element_blank",
  package = "vellumplot"
)

#' Relative sizing
#'
#' `rel()` gives a theme element size as a multiple of the size it inherits,
#' as in ggplot2. `theme(axis.text = element_text(size = rel(0.8)))` draws the
#' axis text at 80% of the base text size, so it follows a later
#' `theme(text = element_text(size = ...))` or a preset's `base_size`.
#'
#' A `rel()` size is resolved against the parent element in the theme tree
#' (`text` -> `axis.text` -> `axis.text.x` and so on). Set on a slot that
#' already holds an absolute size, such as the `text` root, it scales that
#' size instead: `theme(text = element_text(size = rel(2)))` doubles every
#' text size.
#'
#' @param x A single positive number, the multiplier.
#' @return A number of class `rel`, for the `size` of [element_text()].
#' @examples
#' vplot(mtcars) |>
#'   mark_point(x = wt, y = mpg) |>
#'   theme(axis.text = element_text(size = rel(1.2)))
#' @export
rel <- function(x) {
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x) || x <= 0) {
    cli::cli_abort("{.arg x} must be a single positive number.")
  }
  structure(as.double(x), class = "rel")
}

#' @export
print.rel <- function(x, ...) {
  cat("<rel> ", format(unclass(x)), "\n", sep = "")
  invisible(x)
}

.is_rel <- function(x) inherits(x, "rel")

# Combine a child size with the size it inherits: a `rel()` child multiplies
# an absolute parent (giving points) or another `rel()` (giving a `rel()`); an
# absolute child, or one with nothing to scale, is returned as is.
.combine_size <- function(parent, child) {
  if (!.is_rel(child) || is.null(parent)) {
    child
  } else if (.is_rel(parent)) {
    rel(unclass(parent) * unclass(child))
  } else {
    parent * unclass(child)
  }
}

# --- helpers ----------------------------------------------------------------

# Is `x` an element_blank (draw nothing)?
.is_blank <- function(x) S7::S7_inherits(x, element_blank)

# Merge a child element onto its (already-resolved) parent: each unset (NULL)
# child property falls back to the parent's. Both are the same element class.
.merge_element <- function(parent, child) {
  cls <- S7::S7_class(child)
  pp <- S7::props(parent)
  cp <- S7::props(child)
  merged <- Map(function(c, p) c %||% p, cp, pp[names(cp)])
  do.call(cls, merged)
}

# Inherit a child element from its resolved parent in the theme tree: the
# property fallback of `.merge_element()`, plus a `rel()` text size resolved
# against the parent's size.
.inherit_element <- function(parent, child) {
  merged <- .merge_element(parent, child)
  if (S7::S7_inherits(child, .element_text) && .is_rel(child@size)) {
    merged@size <- .combine_size(parent@size, child@size)
  }
  merged
}

# Build a vellum gpar from a resolved element (NULL fields stay NULL = inherit).
# `features` opts into OpenType features on the text (e.g. "tnum" for tabular
# figures on axis labels, so digits keep a constant width and stop jittering
# between ticks). `cex` is a relative size multiplier vellum applies to
# `fontsize` -- how a theme cascades a base size (`element_text(cex = 1.2)`).
.el_gpar_text <- function(el, features = NULL) {
  # A bold face means at least 700, as a `**bold**` run in a rich label does:
  # a weight inherited from a parent (a light brand body, say) must not turn
  # an element's own `face = "bold"` light.
  weight <- el@weight
  if (!is.null(weight) && grepl("bold", el@face %||% "")) {
    weight <- max(weight, 700L)
  }
  vellum::vl_gpar(
    fontsize = el@size,
    cex = el@cex %||% 1,
    col = el@colour,
    fontfamily = el@family,
    fontface = el@face,
    fontweight = weight,
    lineheight = el@lineheight,
    features = features
  )
}

.el_gpar_line <- function(el) {
  vellum::vl_gpar(
    col = el@colour,
    lwd = el@linewidth,
    lty = el@linetype,
    lineend = el@lineend
  )
}

.el_gpar_rect <- function(el) {
  vellum::vl_gpar(
    fill = el@fill,
    col = el@colour,
    lwd = el@linewidth,
    lty = el@linetype
  )
}

# Text rotation for a resolved text element (degrees). Blank elements draw
# nothing and carry no `angle` property, so they read as unrotated.
.el_rot <- function(el) if (.is_blank(el)) 0 else el@angle %||% 0

# The resolved sketch for a drawn line/rect element: the `vellum_sketch` it (or,
# via the theme tree, a parent element) carries, else NULL (crisp). `NA` (forced
# crisp) and an unset slot both resolve to NULL here. A per-element seed `offset`
# keeps distinct elements (gridlines vs axis line vs panel border) from sharing
# an identical wobble. Blank elements draw nothing, so never carry a sketch.
.el_sketch <- function(el, offset = 0L) {
  if (.is_blank(el)) {
    return(NULL)
  }
  # Only line/rect elements carry a `sketch` property; a text element has none.
  s <- if (
    S7::S7_inherits(el, .element_line) || S7::S7_inherits(el, .element_rect)
  ) {
    S7::prop(el, "sketch")
  } else {
    NULL
  }
  if (is.null(s) || !inherits(s, "vellum_sketch") || !offset) {
    if (inherits(s, "vellum_sketch")) s else NULL
  } else {
    s$seed <- s$seed + offset
    s
  }
}
