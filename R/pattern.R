#' @include classes.R
NULL

#' Pattern (hatch) fills
#'
#' Build a tiling **pattern** to use as a constant `fill` value on a filled mark,
#' the texture counterpart of [linear_gradient()]. Distinguishing regions by
#' texture (not only hue) keeps a plot legible in greyscale print and under
#' colour-vision deficiency. Each constructor assembles a small tile and returns a
#' `vellum_pattern` (via [vellum::vl_pattern()]); pass it straight to `fill`:
#'
#' ```r
#' vplot(df) |> mark_bar(x = g, y = n, fill = pattern_stripe(angle = 45))
#' ```
#'
#' A pattern is an unscaled *value*, not a data-mapped channel (like a gradient).
#' It applies to any filled mark: bars, areas, ribbons, rects, tiles, boxplots,
#' violins, ridgelines, half-eyes, hulls/ellipses, and `sf` polygons. To vary the
#' texture *by a variable*, map the `pattern` aesthetic instead (a discrete scale).
#'
#' Patterns render on every backend (PNG, SVG, and — since the tile embeds as an
#' image — PDF). The tile is rasterised at the scene resolution, so at extreme
#' magnification it can soften; at ordinary sizes it is crisp.
#'
#' @param color Line/dot colour (any R colour). Default `"grey20"`.
#' @param bg Background fill painted behind the motif, or `NA` (default) for a
#'   transparent tile so whatever is under the shape shows through.
#' @param angle Stripe orientation in degrees, restricted to `0`, `45`, `90`, or
#'   `135` (the orientations that tile seamlessly). Other values error.
#' @param spacing Distance between repeats, in `units`. Default `4`.
#' @param linewidth Stripe / grid line width (as elsewhere in the package).
#'   Default `1`.
#' @param size For `pattern_dot()`, the dot diameter in `units`; for
#'   `pattern_checker()`, the square size in `units`.
#' @param units Length unit for `spacing`/`size`: `"mm"` (default), `"in"`,
#'   `"pt"`, `"npc"`, or `"native"`.
#' @return A `vellum_pattern` object, usable as a `fill` value.
#' @seealso [linear_gradient()], [vellum::vl_pattern()]
#' @examples
#' df <- data.frame(g = c("a", "b", "c"), n = c(3, 5, 2))
#' vplot(df) |> mark_bar(x = g, y = n, fill = pattern_crosshatch())
#' @name patterns
NULL

# Seamless-tiling orientations. Arbitrary angles need an oversized wrapping tile
# (deferred); restrict to the four that tile cleanly in a unit cell.
.PATTERN_ANGLES <- c(0, 45, 90, 135)

.check_pattern_angle <- function(angle) {
  a <- as.numeric(angle)[1] %% 180
  if (!isTRUE(a %in% .PATTERN_ANGLES)) {
    cli::cli_abort(c(
      "Pattern {.arg angle} must be one of {.val {(.PATTERN_ANGLES)}} degrees.",
      i = "Seamless tiling is only defined for those orientations."
    ))
  }
  a
}

# An optional background rect drawn first (so the motif sits on top), or nothing
# for a transparent tile.
.pattern_bg <- function(bg) {
  if (is.null(bg) || (length(bg) == 1L && is.na(bg))) {
    return(list())
  }
  list(vellum::rect_grob(gp = vellum::vl_gpar(fill = bg, col = NA)))
}

# One stripe line spanning the unit tile at the given seamless angle.
.stripe_grob <- function(color, angle, linewidth) {
  seg <- switch(
    as.character(angle),
    "0" = list(x = c(0, 1), y = c(0.5, 0.5)),
    "90" = list(x = c(0.5, 0.5), y = c(0, 1)),
    "45" = list(x = c(0, 1), y = c(0, 1)),
    "135" = list(x = c(0, 1), y = c(1, 0))
  )
  vellum::lines_grob(
    seg$x,
    seg$y,
    gp = vellum::vl_gpar(col = color, lwd = linewidth)
  )
}

#' @rdname patterns
#' @export
pattern_stripe <- function(
  color = "grey20",
  bg = NA,
  angle = 45,
  spacing = 4,
  linewidth = 1,
  units = "mm"
) {
  angle <- .check_pattern_angle(angle)
  tile <- c(.pattern_bg(bg), list(.stripe_grob(color, angle, linewidth)))
  vellum::vl_pattern(tile, width = spacing, height = spacing, units = units)
}

#' @rdname patterns
#' @export
pattern_crosshatch <- function(
  color = "grey20",
  bg = NA,
  angle = 45,
  spacing = 4,
  linewidth = 1,
  units = "mm"
) {
  angle <- .check_pattern_angle(angle)
  a2 <- (angle + 90) %% 180 # the perpendicular set (still a seamless angle)
  tile <- c(
    .pattern_bg(bg),
    list(
      .stripe_grob(color, angle, linewidth),
      .stripe_grob(color, a2, linewidth)
    )
  )
  vellum::vl_pattern(tile, width = spacing, height = spacing, units = units)
}

#' @rdname patterns
#' @export
pattern_grid <- function(
  color = "grey20",
  bg = NA,
  spacing = 4,
  linewidth = 1,
  units = "mm"
) {
  # A horizontal + vertical grid: crosshatch at 0/90.
  pattern_crosshatch(
    color = color,
    bg = bg,
    angle = 0,
    spacing = spacing,
    linewidth = linewidth,
    units = units
  )
}

#' @rdname patterns
#' @export
pattern_dot <- function(
  color = "grey20",
  bg = NA,
  spacing = 4,
  size = 1.5,
  units = "mm"
) {
  if (!is.numeric(size) || size <= 0) {
    cli::cli_abort("{.arg size} (dot diameter) must be a positive number.")
  }
  r <- min(0.5, (size / spacing) / 2) # dot radius as a fraction of the cell
  tile <- c(
    .pattern_bg(bg),
    list(vellum::circle_grob(
      x = 0.5,
      y = 0.5,
      r = r,
      gp = vellum::vl_gpar(fill = color, col = NA)
    ))
  )
  vellum::vl_pattern(tile, width = spacing, height = spacing, units = units)
}

#' @rdname patterns
#' @export
pattern_checker <- function(
  color = "grey20",
  bg = NA,
  size = 4,
  units = "mm"
) {
  # Two opposite quadrants filled -> a checkerboard of `size` squares once tiled.
  quad <- function(x, y) {
    vellum::rect_grob(
      x = x,
      y = y,
      width = 0.5,
      height = 0.5,
      gp = vellum::vl_gpar(fill = color, col = NA)
    )
  }
  tile <- c(.pattern_bg(bg), list(quad(0.25, 0.25), quad(0.75, 0.75)))
  vellum::vl_pattern(
    tile,
    width = 2 * size,
    height = 2 * size,
    units = units
  )
}
