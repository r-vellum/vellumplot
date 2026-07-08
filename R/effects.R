#' @include classes.R marks.R
NULL

# Which marks each effect kind can decorate.
.STROKE_POINT_MARKS <- c(
  "point",
  "line",
  "step",
  "rule",
  "segment",
  "edges",
  "nodes"
)
# The marks an effect applies to (used for validation + error messages). All
# current effects (glow / outline / shadow) decorate stroked and point marks;
# if an effect ever needs a different set this becomes a per-class dispatch.
.effect_marks <- function() {
  .STROKE_POINT_MARKS
}

#' Neon glow layer effect
#'
#' A render effect for stroked and point marks, in the spirit of
#' [mplcyberpunk](https://github.com/dhaitz/mplcyberpunk): the mark is drawn as
#' several widened, low-opacity copies composited additively (a `"screen"` blend)
#' beneath the crisp original, producing a soft neon halo. Pass it to a mark's
#' `effects` argument, e.g. `mark_line(..., effects = list(glow()))`. Pairs
#' naturally with [theme_cyberpunk()].
#'
#' The glow is applied per style group, so a colour-mapped multi-series line
#' glows each series in its own hue. It applies to `mark_point()`, `mark_line()`,
#' `mark_step()`, `mark_rule()`, `mark_segment()`, `mark_edges()`, and
#' `mark_nodes()`; other marks reject it with an error.
#'
#' @param size Extra visual spread, in millimetres, added to the stroke width (or
#'   point diameter) at the outermost copy.
#' @param layers Number of stacked halo copies.
#' @param alpha Opacity of each copy (they accumulate toward the centre).
#' @param blend Blend mode compositing the halo copies, typically `"screen"` or
#'   `"lighten"` (any CSS `mix-blend-mode` name).
#' @param color Halo colour, or `NULL` (default) to inherit the mark's own
#'   resolved colour — the usual neon look.
#' @return A `GlowSpec` object for a mark's `effects` list.
#' @seealso [outline()], [shadow()], [theme_cyberpunk()]
#' @examples
#' df <- data.frame(x = 1:20, y = cumsum(rnorm(20)))
#' vplot(df) |>
#'   mark_line(x = x, y = y, color = "#00e5ff", effects = list(glow())) |>
#'   theme_cyberpunk()
#' @export
glow <- function(
  size = 6,
  layers = 6L,
  alpha = 0.12,
  blend = "screen",
  color = NULL
) {
  .check_pos_num(size, "size", "mm")
  .check_pos_int(layers, "layers")
  .check_unit_alpha(alpha)
  .check_opt_colour(color, "color")
  GlowSpec(
    size = as.double(size),
    layers = as.integer(layers),
    alpha = as.double(alpha),
    blend = .check_blend(blend),
    color = color
  )
}

#' Outline (halo) layer effect
#'
#' Draws one opaque, wider copy of a stroked or point mark beneath the crisp
#' original in a contrasting colour, so the mark stays legible over a busy or
#' dark backdrop (the "sticker" look). Applies to the same marks as [glow()].
#'
#' @param size Halo width per side, in millimetres.
#' @param color Outline colour.
#' @param alpha Outline opacity.
#' @return An `OutlineSpec` object for a mark's `effects` list.
#' @seealso [glow()], [shadow()]
#' @examples
#' vplot(mtcars) |>
#'   mark_point(x = wt, y = mpg, color = factor(cyl), size = 3,
#'     effects = list(outline()))
#' @export
outline <- function(size = 1, color = "white", alpha = 1) {
  .check_pos_num(size, "size", "mm")
  .check_req_colour(color, "color")
  .check_unit_alpha(alpha)
  OutlineSpec(size = as.double(size), color = color, alpha = as.double(alpha))
}

#' Shadow layer effect
#'
#' Draws dark, low-opacity copies of a stroked or point mark beneath the
#' original, offset by (`x`, `y`) and softened by stacking a few widened copies —
#' a drop shadow (with an offset) or an ambient shadow (offset `0`). Applies to
#' the same marks as [glow()].
#'
#' The offset is an **absolute distance in millimetres** (`+x` right, `+y` up),
#' resolved device-side, so a drop shadow stays the same physical distance and is
#' isotropic regardless of the panel's size or aspect (via `vellum`'s compound
#' `npc + mm` unit).
#'
#' @param x,y Shadow offset in millimetres (`+x` right, `+y` up). Defaults to a
#'   small down-right drop.
#' @param color Shadow colour.
#' @param alpha Opacity of each copy.
#' @param spread Softening spread, in millimetres, over which the copies widen.
#' @param layers Number of stacked copies (more = softer).
#' @return A `ShadowSpec` object for a mark's `effects` list.
#' @seealso [glow()], [outline()]
#' @examples
#' df <- data.frame(x = 1:20, y = cumsum(rnorm(20)))
#' vplot(df) |> mark_line(x = x, y = y, effects = list(shadow()))
#' @export
shadow <- function(
  x = 0.5,
  y = -0.5,
  color = "black",
  alpha = 0.3,
  spread = 1.5,
  layers = 3L
) {
  .check_num(x, "x")
  .check_num(y, "y")
  .check_req_colour(color, "color")
  .check_unit_alpha(alpha)
  if (!is.numeric(spread) || length(spread) != 1L || !is.finite(spread) || spread < 0) {
    cli::cli_abort("{.arg spread} must be a single non-negative number (mm).")
  }
  .check_pos_int(layers, "layers")
  ShadowSpec(
    x = as.double(x),
    y = as.double(y),
    color = color,
    alpha = as.double(alpha),
    spread = as.double(spread),
    layers = as.integer(layers)
  )
}

# --- shared argument checks -------------------------------------------------

.check_pos_num <- function(v, arg, kind) {
  if (!is.numeric(v) || length(v) != 1L || !is.finite(v) || v <= 0) {
    cli::cli_abort("{.arg {arg}} must be a single positive number ({kind}).")
  }
}
.check_num <- function(v, arg) {
  if (!is.numeric(v) || length(v) != 1L || !is.finite(v)) {
    cli::cli_abort("{.arg {arg}} must be a single finite number.")
  }
}
.check_pos_int <- function(v, arg) {
  if (!is.numeric(v) || length(v) != 1L || !is.finite(v) || v < 1 || v %% 1 != 0) {
    cli::cli_abort("{.arg {arg}} must be a single positive integer.")
  }
}
.check_unit_alpha <- function(v) {
  if (!is.numeric(v) || length(v) != 1L || !is.finite(v) || v < 0 || v > 1) {
    cli::cli_abort("{.arg alpha} must be a single number in {.val [0, 1]}.")
  }
}
.check_req_colour <- function(v, arg) {
  if (!is.character(v) || length(v) != 1L) {
    cli::cli_abort("{.arg {arg}} must be a single colour.")
  }
}
.check_opt_colour <- function(v, arg) {
  if (!is.null(v) && (!is.character(v) || length(v) != 1L)) {
    cli::cli_abort("{.arg {arg}} must be a single colour, or {.code NULL}.")
  }
}

# --- validation + lookup ----------------------------------------------------

# Validate a layer's `effects` list against its mark; return it unchanged. Each
# entry must be an Effect, and its kind must apply to this mark.
.check_effects <- function(effects, mark, call = rlang::caller_env()) {
  if (!length(effects)) {
    return(list())
  }
  if (!is.list(effects)) {
    cli::cli_abort(
      "{.arg effects} must be a list of effects, e.g. {.code list(glow())}.",
      call = call
    )
  }
  for (e in effects) {
    if (!S7::S7_inherits(e, Effect)) {
      cli::cli_abort(
        "Each entry of {.arg effects} must come from an effect constructor like {.fn glow}.",
        call = call
      )
    }
    ok <- .effect_marks()
    if (!mark %in% ok) {
      cli::cli_abort(
        c(
          "This effect does not apply to a {.val {mark}} mark.",
          i = "It applies to: {.val {ok}}."
        ),
        call = call
      )
    }
  }
  effects
}

# A resolved layer's effects, in list order (first = furthest back). All current
# effects (glow/outline/shadow) draw beneath the core.
.underlay_effects <- function(L) {
  L$effects %||% list()
}
