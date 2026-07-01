#' @include quill-package.R
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
  package = "quill",
  properties = list(
    family = S7::class_any,
    face = S7::class_any,
    colour = S7::class_any,
    size = S7::class_any, # points
    hjust = S7::class_any,
    vjust = S7::class_any,
    angle = S7::class_any,
    lineheight = S7::class_any,
    margin = S7::class_any # numeric length-4 (t, r, b, l) in mm
  )
)

.element_line <- S7::new_class(
  ".element_line",
  package = "quill",
  properties = list(
    colour = S7::class_any,
    linewidth = S7::class_any,
    linetype = S7::class_any,
    lineend = S7::class_any
  )
)

.element_rect <- S7::new_class(
  ".element_rect",
  package = "quill",
  properties = list(
    fill = S7::class_any,
    colour = S7::class_any,
    linewidth = S7::class_any,
    linetype = S7::class_any
  )
)

#' Theme elements
#'
#' Typed building blocks for [theme()]. Each describes how a family of theme
#' slots is drawn; any property left `NULL` is inherited from the slot's parent
#' in the theme tree. `element_blank()` draws nothing.
#'
#' @param family,face,size,colour,color,hjust,vjust,angle,lineheight,margin
#'   Text properties. `color` is an alias for `colour`; `size` is in points;
#'   `margin` is a numeric vector of millimetres (recycled to length 4).
#' @param linewidth,linetype,lineend Line properties.
#' @param fill Fill colour (rectangles).
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
  hjust = NULL,
  vjust = NULL,
  angle = NULL,
  lineheight = NULL,
  margin = NULL
) {
  colour <- colour %||% color
  if (!is.null(margin)) {
    margin <- rep_len(margin, 4L)
  }
  .element_text(
    family = family,
    face = face,
    colour = colour,
    size = size,
    hjust = hjust,
    vjust = vjust,
    angle = angle,
    lineheight = lineheight,
    margin = margin
  )
}

#' @rdname element
#' @export
element_line <- function(
  colour = NULL,
  color = NULL,
  linewidth = NULL,
  linetype = NULL,
  lineend = NULL
) {
  .element_line(
    colour = colour %||% color,
    linewidth = linewidth,
    linetype = linetype,
    lineend = lineend
  )
}

#' @rdname element
#' @export
element_rect <- function(
  fill = NULL,
  colour = NULL,
  color = NULL,
  linewidth = NULL,
  linetype = NULL
) {
  .element_rect(
    fill = fill,
    colour = colour %||% color,
    linewidth = linewidth,
    linetype = linetype
  )
}

#' @rdname element
#' @export
element_blank <- S7::new_class(
  "element_blank",
  package = "quill"
)

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

# Build a vellum gpar from a resolved element (NULL fields stay NULL = inherit).
.el_gpar_text <- function(el) {
  vellum::gpar(
    fontsize = el@size,
    col = el@colour,
    fontfamily = el@family,
    fontface = el@face,
    lineheight = el@lineheight
  )
}

.el_gpar_line <- function(el) {
  vellum::gpar(
    col = el@colour,
    lwd = el@linewidth,
    lty = el@linetype,
    lineend = el@lineend
  )
}

.el_gpar_rect <- function(el) {
  vellum::gpar(
    fill = el@fill,
    col = el@colour,
    lwd = el@linewidth,
    lty = el@linetype
  )
}

# Text rotation for a resolved text element (degrees).
.el_rot <- function(el) el@angle %||% 0
