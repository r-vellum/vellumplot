#' @include classes.R marks.R
NULL

# Marks a glow effect can decorate: stroked and point marks. Filled marks
# (bar/area/tile/sf polygons) and text are out of scope for now.
.GLOW_MARKS <- c("point", "line", "step", "rule", "segment", "edges", "nodes")

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
#' @seealso [theme_cyberpunk()], [linear_gradient()]
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
  if (!is.numeric(size) || length(size) != 1L || size <= 0) {
    cli::cli_abort("{.arg size} must be a single positive number (mm).")
  }
  if (!is.numeric(layers) || length(layers) != 1L || layers < 1) {
    cli::cli_abort("{.arg layers} must be a single positive integer.")
  }
  if (!is.numeric(alpha) || length(alpha) != 1L || alpha < 0 || alpha > 1) {
    cli::cli_abort("{.arg alpha} must be a single number in {.val [0, 1]}.")
  }
  if (!is.null(color) && (!is.character(color) || length(color) != 1L)) {
    cli::cli_abort("{.arg color} must be a single colour, or {.code NULL}.")
  }
  GlowSpec(
    size = as.double(size),
    layers = as.integer(layers),
    alpha = as.double(alpha),
    blend = .check_blend(blend),
    color = color
  )
}

# Validate a layer's `effects` list against its mark; return it unchanged. Each
# entry must be an Effect; a glow requires a stroked/point mark.
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
    if (S7::S7_inherits(e, GlowSpec) && !mark %in% .GLOW_MARKS) {
      cli::cli_abort(
        c(
          "{.fn glow} applies only to stroked and point marks.",
          i = "Supported: {.fn mark_point}, {.fn mark_line}, {.fn mark_step}, {.fn mark_rule}, {.fn mark_segment}, {.fn mark_edges}, {.fn mark_nodes}.",
          x = "This is a {.val {mark}} mark."
        ),
        call = call
      )
    }
  }
  effects
}

# The first glow effect on a resolved layer, or NULL.
.layer_glow <- function(L) {
  for (e in L$effects %||% list()) {
    if (S7::S7_inherits(e, GlowSpec)) {
      return(e)
    }
  }
  NULL
}
