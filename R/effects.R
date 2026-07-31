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
# The marks an effect applies to (used for validation + error messages). Glow /
# outline / shadow decorate stroked and point marks. Text marks are handled
# separately in `.check_effects()`: `shadow()` (a blurred offset copy) and
# `glow()` (a blurred halo) both read on text now that blur is a real engine
# effect (`vl_viewport(blur=)`); only a *sharp* `outline()` still needs a glyph
# outline the text primitive cannot stroke.
.effect_marks <- function() {
  .STROKE_POINT_MARKS
}

# Text marks that `shadow()` / `glow()` can decorate (via real blur).
.TEXT_EFFECT_MARKS <- c("text", "label", "node_text", "edge_text")

#' Neon glow layer effect
#'
#' A render effect for stroked, point, and text marks, in the spirit of
#' [mplcyberpunk](https://github.com/dhaitz/mplcyberpunk): a widened copy of the
#' mark is drawn beneath the crisp original and softened with a **real Gaussian
#' blur** (`vellum`'s `vl_viewport(blur=)`), composited additively (a `"screen"`
#' blend) into a soft neon halo. Pass it to a mark's `effects` argument, e.g.
#' `mark_line(..., effects = list(glow()))`. Pairs naturally with
#' [theme_cyberpunk()].
#'
#' The glow is applied per style group, so a colour-mapped multi-series line
#' glows each series in its own hue. It applies to `mark_point()`, `mark_line()`,
#' `mark_step()`, `mark_rule()`, `mark_segment()`, `mark_edges()`, `mark_nodes()`,
#' and text marks (`mark_text()` / `mark_label()`); other marks reject it.
#'
#' @param size Halo spread in millimetres: the blur radius (and, for stroked/point
#'   marks, how much the copy is widened before blurring).
#' @param layers,blend Retained for compatibility and the neon look: `layers`
#'   scales the halo opacity (it no longer stacks copies -- one blurred layer
#'   replaces them), and `blend` (typically `"screen"`/`"lighten"`) composites
#'   the halo.
#' @param alpha Base halo opacity (scaled by `layers`).
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
  .check_nonneg_num(spread, "spread", "mm")
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

#' Motion-trail layer effects
#'
#' Draw a fading trail of a stroked or point mark: `n` copies marching off along
#' a direction (`x`, `y`), each further out and fainter than the last, composited
#' beneath the crisp original — a speed-blur or animation-still look. `motion()`
#' defaults to many close, low-opacity copies (a smooth blur streak); `echo()`
#' defaults to a few wider-spaced, more-opaque copies (discrete ghost repeats).
#' Both build the same effect, differing only in defaults, and apply to the same
#' marks as [glow()].
#'
#' The direction is an **absolute distance in millimetres** (`+x` right, `+y`
#' up), resolved device-side via `vellum`'s compound `npc + mm` unit, so the
#' trail keeps the same physical length and stays isotropic regardless of the
#' panel's size or aspect (as [shadow()]'s offset does).
#'
#' @param x,y Trail direction and length in millimetres (`+x` right, `+y` up):
#'   the offset of the furthest copy. Defaults to a rightward streak.
#' @param n Number of trail copies.
#' @param alpha Opacity of the nearest (strongest) copy; copies fade toward the
#'   tail.
#' @param decay Fade exponent shaping the opacity ramp along the trail (higher =
#'   faster fade toward the tail; `0` = no fade).
#' @param spread Optional widening, in millimetres, applied progressively toward
#'   the tail (`0` keeps a constant width).
#' @param blend Blend mode compositing the trail copies (any CSS
#'   `mix-blend-mode` name); `"normal"` for opaque ghosts.
#' @param color Trail colour, or `NULL` (default) to inherit the mark's own
#'   resolved colour.
#' @return A `MotionSpec` object for a mark's `effects` list.
#' @seealso [glow()], [shadow()], [outline()]
#' @examples
#' df <- data.frame(x = 1:20, y = cumsum(rnorm(20)))
#' vplot(df) |> mark_line(x = x, y = y, effects = list(motion(x = 4)))
#'
#' vplot(mtcars) |>
#'   mark_point(x = wt, y = mpg, size = 4, effects = list(echo(x = 5)))
#' @export
motion <- function(
  x = 3,
  y = 0,
  n = 8L,
  alpha = 0.15,
  decay = 1,
  spread = 0,
  blend = "normal",
  color = NULL
) {
  .motion_spec(x, y, n, alpha, decay, spread, blend, color)
}

#' @rdname motion
#' @export
echo <- function(
  x = 4,
  y = 0,
  n = 3L,
  alpha = 0.45,
  decay = 1,
  spread = 0,
  blend = "normal",
  color = NULL
) {
  .motion_spec(x, y, n, alpha, decay, spread, blend, color)
}

# Shared builder for motion() / echo(): validate and construct a MotionSpec.
.motion_spec <- function(x, y, n, alpha, decay, spread, blend, color) {
  .check_num(x, "x")
  .check_num(y, "y")
  .check_pos_int(n, "n")
  .check_unit_alpha(alpha)
  .check_nonneg_num(decay, "decay")
  .check_nonneg_num(spread, "spread")
  .check_opt_colour(color, "color")
  MotionSpec(
    x = as.double(x),
    y = as.double(y),
    n = as.integer(n),
    alpha = as.double(alpha),
    decay = as.double(decay),
    spread = as.double(spread),
    blend = .check_blend(blend),
    color = color
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
.check_nonneg_num <- function(v, arg, unit = NULL) {
  if (!is.numeric(v) || length(v) != 1L || !is.finite(v) || v < 0) {
    suffix <- if (!is.null(unit)) paste0(" (", unit, ")") else ""
    cli::cli_abort("{.arg {arg}} must be a single non-negative number{suffix}.")
  }
}
.check_pos_int <- function(v, arg) {
  if (
    !is.numeric(v) || length(v) != 1L || !is.finite(v) || v < 1 || v %% 1 != 0
  ) {
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
  is_text <- mark %in% .TEXT_EFFECT_MARKS
  ok <- .effect_marks() # loop-invariant: the marks any non-text effect applies to
  for (e in effects) {
    if (!S7::S7_inherits(e, Effect)) {
      cli::cli_abort(
        "Each entry of {.arg effects} must come from an effect constructor like {.fn glow}.",
        call = call
      )
    }
    if (is_text) {
      # shadow() and glow() both read on text now: a real Gaussian blur (a drop
      # shadow, or a soft halo) needs no glyph outline. outline() is still
      # rejected -- a *sharp* halo would have to stroke the glyph outlines, which
      # the text primitive cannot do (a blurred "outline" is just a glow).
      if (!S7::S7_inherits(e, ShadowSpec) && !S7::S7_inherits(e, GlowSpec)) {
        cli::cli_abort(
          c(
            "Only {.fn shadow} and {.fn glow} apply to a text mark ({.val {mark}}).",
            i = "A sharp {.fn outline} needs a glyph outline the text primitive can't draw yet."
          ),
          call = call
        )
      }
    } else {
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
  }
  effects
}

# A resolved layer's effects, in list order (first = furthest back). All current
# effects (glow/outline/shadow) draw beneath the core.
.underlay_effects <- function(L) {
  L$effects %||% list()
}
