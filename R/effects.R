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
.SKETCH_MARKS <- c("line", "step", "segment", "area", "ribbon")
.INNER_MARKS <- c("area", "ribbon", "bar")

# The marks an effect applies to (used for validation + error messages).
.effect_marks <- function(e) {
  if (S7::S7_inherits(e, SketchSpec)) {
    .SKETCH_MARKS
  } else if (S7::S7_inherits(e, InnerSpec)) {
    .INNER_MARKS
  } else {
    # glow / outline / shadow
    .STROKE_POINT_MARKS
  }
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
#' @seealso [outline()], [shadow()], [sketch()], [inner_glow()], [theme_cyberpunk()]
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
#' The offset is a **fraction of the panel** (npc; `+x` right, `+y` up), not an
#' absolute distance, so it scales with the panel and is slightly anisotropic on
#' a non-square panel. (A device-exact offset would need backend support.)
#'
#' @param x,y Shadow offset as a fraction of the panel width / height
#'   (`+x` right, `+y` up). Defaults to a small down-right drop.
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
  x = 0.006,
  y = -0.006,
  color = "black",
  alpha = 0.3,
  spread = 1.5,
  layers = 3L
) {
  .check_num(x, "x")
  .check_num(y, "y")
  .check_req_colour(color, "color")
  .check_unit_alpha(alpha)
  if (!is.numeric(spread) || length(spread) != 1L || spread < 0) {
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

#' Hand-drawn (sketch) layer effect
#'
#' Perturbs a mark's path with smooth low-frequency noise so straight lines
#' wobble like hand-drawn ink (an XKCD-style look). Applies to path marks:
#' `mark_line()`, `mark_step()`, `mark_segment()`, `mark_area()`, and
#' `mark_ribbon()`. Pairs with [theme_sketch()].
#'
#' @param amount Wobble scale, as a fraction of the panel's data range.
#' @param detail Number of noise harmonics (higher = busier wobble).
#' @param seed Integer seed making the wobble reproducible across renders.
#' @return A `SketchSpec` object for a mark's `effects` list.
#' @seealso [theme_sketch()]
#' @examples
#' df <- data.frame(x = 1:20, y = cumsum(rnorm(20)))
#' vplot(df) |>
#'   mark_line(x = x, y = y, effects = list(sketch())) |>
#'   theme_sketch()
#' @export
sketch <- function(amount = 0.01, detail = 6L, seed = 1) {
  .check_pos_num(amount, "amount", "fraction")
  .check_pos_int(detail, "detail")
  if (!is.numeric(seed) || length(seed) != 1L) {
    cli::cli_abort("{.arg seed} must be a single number.")
  }
  SketchSpec(
    amount = as.double(amount),
    detail = as.integer(detail),
    seed = as.double(seed)
  )
}

#' Inner glow / inner shadow layer effect
#'
#' For filled marks (`mark_area()`, `mark_ribbon()`, `mark_bar()`), draws a soft
#' band of light (`inner_glow()`) or dark (`inner_shadow()`) just *inside* the
#' fill's edge, by masking a wide boundary stroke to the fill shape. Drawn over
#' the fill.
#'
#' @param size Band width, in millimetres.
#' @param color Band colour.
#' @param alpha Band opacity.
#' @return An `InnerSpec` object for a mark's `effects` list.
#' @seealso [glow()]
#' @examples
#' df <- data.frame(x = 1:30, y = cumsum(abs(rnorm(30))))
#' vplot(df) |>
#'   mark_area(x = x, y = y, fill = "#0a2a43", effects = list(inner_glow()))
#' @export
inner_glow <- function(size = 3, color = "white", alpha = 0.6) {
  .check_pos_num(size, "size", "mm")
  .check_req_colour(color, "color")
  .check_unit_alpha(alpha)
  InnerSpec(
    size = as.double(size),
    color = color,
    alpha = as.double(alpha),
    dark = FALSE
  )
}

#' @rdname inner_glow
#' @export
inner_shadow <- function(size = 3, color = "black", alpha = 0.5) {
  .check_pos_num(size, "size", "mm")
  .check_req_colour(color, "color")
  .check_unit_alpha(alpha)
  InnerSpec(
    size = as.double(size),
    color = color,
    alpha = as.double(alpha),
    dark = TRUE
  )
}

# --- shared argument checks -------------------------------------------------

.check_pos_num <- function(v, arg, kind) {
  if (!is.numeric(v) || length(v) != 1L || v <= 0) {
    cli::cli_abort("{.arg {arg}} must be a single positive number ({kind}).")
  }
}
.check_num <- function(v, arg) {
  if (!is.numeric(v) || length(v) != 1L) {
    cli::cli_abort("{.arg {arg}} must be a single number.")
  }
}
.check_pos_int <- function(v, arg) {
  if (!is.numeric(v) || length(v) != 1L || v < 1) {
    cli::cli_abort("{.arg {arg}} must be a single positive integer.")
  }
}
.check_unit_alpha <- function(v) {
  if (!is.numeric(v) || length(v) != 1L || v < 0 || v > 1) {
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
    ok <- .effect_marks(e)
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

# The first effect of a class on a resolved layer, or NULL.
.first_effect <- function(L, cls) {
  for (e in L$effects %||% list()) {
    if (S7::S7_inherits(e, cls)) {
      return(e)
    }
  }
  NULL
}

# Effects of a resolved layer that draw *beneath* the core (glow/outline/shadow),
# in list order (first = furthest back).
.underlay_effects <- function(L) {
  Filter(
    function(e) {
      S7::S7_inherits(e, GlowSpec) ||
        S7::S7_inherits(e, OutlineSpec) ||
        S7::S7_inherits(e, ShadowSpec)
    },
    L$effects %||% list()
  )
}

# Effects that draw *over* the core (inner glow/shadow), in list order.
.overlay_effects <- function(L) {
  Filter(function(e) S7::S7_inherits(e, InnerSpec), L$effects %||% list())
}
