#' @include classes.R
NULL

#' Control a scale's legend
#'
#' `guides()` overrides the legend (guide) for one or more aesthetics without
#' respelling the whole `scale_*()`. Pass `"none"` (or [guide_none()]) to hide a
#' legend, [guide_legend()] to tweak a keyed legend (reverse the key order,
#' override the title, restyle the keys with `override.aes`), `guide_colourbar()`
#' to size, tick and style a continuous colour bar, or `guide_coloursteps()` to draw a
#' **binned** colour scale as a segmented bar instead of swatches. Applies to the
#' non-position legends (`color`/`fill`, `size`, `shape`, `alpha`, `linetype`);
#' position axes are unaffected.
#'
#' @param plot A [PlotSpec].
#' @param ... Named by aesthetic, e.g. `guides(color = "none", shape = guide_legend(reverse = TRUE))`.
#' @param title An axis/legend title override, or `NULL` to keep the default.
#' @param reverse Reverse the order of the legend keys / the colour bar.
#' @param barwidth,barheight The colour bar's own width and height, in millimetres
#'   (`guide_colourbar()`). `barheight` sets the bar's **length** on the default
#'   vertical bar (and its thickness on a horizontal one); `barwidth` its
#'   thickness (and the bar length on a horizontal legend). `NULL` auto-sizes.
#' @param ticks Draw the break ticks on the colour bar? (Default `TRUE` for
#'   `guide_colourbar()`, `FALSE` for the segmented `guide_coloursteps()`.)
#' @param ticks.colour Colour of the break ticks (default `"white"`).
#' @param label.position Which side of a **vertical** colour bar the labels sit,
#'   `"right"` (default) or `"left"`.
#' @param n.breaks Roughly how many ticks (and labels) to put on a **continuous**
#'   colour bar --- a target, not a promise: the break algorithm prefers round
#'   numbers over the bar's range and returns whatever count reads best near the
#'   one asked for, so `n.breaks = 4` may draw 3 or 5 rather than tick a value
#'   like 23.33. `NULL` (default) keeps the automatic count. An explicit
#'   `breaks =` on the scale names the values that get a tick and outranks this;
#'   so does `labels =`, which is paired with those breaks.
#'
#'   There is deliberately no `nbin` argument (ggplot2's band count for the
#'   gradient). The bar is drawn as one real gradient fill, not a stack of
#'   rectangles approximating one, so it has no bands to count; the segmented
#'   look is [guide_coloursteps()] on a binned scale, where the segments are the
#'   scale's own bins.
#' @param nested For a **size** legend, draw the keys as concentric,
#'   bottom-aligned circles (a proportional-symbol / "bubble" legend) with a
#'   leader from each circle to its label, instead of stacked rows. Best with a
#'   wide size range (e.g. `scale_size(range = c(2, 12))`) so the circles are
#'   large enough to read; ignored by non-size legends. Drawn for a vertical
#'   legend (a horizontal one keeps the stacked keys).
#' @param override.aes A named list of aesthetics to force on the legend **keys**,
#'   independent of the plotted data --- the classic "make faint, small points
#'   legible in the key" fix. Recognised names: `size` (mm), `alpha`,
#'   `colour`/`color`, `fill`, `shape`, and `linewidth`. For example
#'   `override.aes = list(size = 5, alpha = 1)` draws big, opaque keys over a
#'   scatter of tiny translucent points. `NULL` (default) leaves the keys as
#'   drawn from the data.
#' @return `guides()`: the modified [PlotSpec]. `guide_none()` / `guide_legend()`:
#'   a guide specification for use inside `guides()`.
#' @examples
#' vplot(mtcars) |>
#'   mark_point(x = wt, y = mpg, color = factor(cyl)) |>
#'   guides(color = "none")
#'
#' vplot(mtcars) |>
#'   mark_point(x = wt, y = mpg, color = factor(cyl)) |>
#'   guides(color = guide_legend(reverse = TRUE))
#'
#' # legible keys over faint, tiny points
#' vplot(mtcars) |>
#'   mark_point(x = wt, y = mpg, color = factor(cyl), alpha = 0.15, size = 0.6) |>
#'   guides(color = guide_legend(override.aes = list(size = 5, alpha = 1)))
#'
#' # a taller, wider colour bar with left-side labels and no ticks
#' vplot(mtcars) |>
#'   mark_point(x = wt, y = mpg, color = hp) |>
#'   guides(color = guide_colourbar(
#'     barwidth = 8, barheight = 60, ticks = FALSE, label.position = "left"
#'   ))
#'
#' # about four ticks on the bar instead of the automatic count
#' vplot(mtcars) |>
#'   mark_point(x = wt, y = mpg, color = hp) |>
#'   guides(color = guide_colourbar(n.breaks = 4))
#'
#' # a binned colour scale as a segmented bar
#' vplot(mtcars) |>
#'   mark_point(x = wt, y = mpg, color = hp) |>
#'   scale_color_binned() |>
#'   guides(color = guide_coloursteps())
#'
#' # a proportional-symbol (nested-circle) size legend
#' vplot(mtcars) |>
#'   mark_point(x = wt, y = mpg, size = hp) |>
#'   scale_size(range = c(2, 12)) |>
#'   guides(size = guide_legend(nested = TRUE))
#' @export
guides <- function(plot, ...) {
  .check_plot(plot)
  args <- list(...)
  nm <- names(args)
  if (is.null(nm) || any(!nzchar(nm))) {
    cli::cli_abort("All arguments to {.fn guides} must be named by aesthetic.")
  }
  for (aes in nm) {
    plot <- .set_guide(plot, aes, args[[aes]])
  }
  plot
}

#' @rdname guides
#' @export
guide_none <- function() "none"

#' @rdname guides
#' @export
guide_legend <- function(
  title = NULL,
  reverse = FALSE,
  override.aes = NULL,
  nested = FALSE
) {
  list(
    kind = "legend",
    title = title,
    reverse = isTRUE(reverse),
    override.aes = .check_override_aes(override.aes),
    nested = isTRUE(nested)
  )
}

#' @rdname guides
#' @export
guide_colourbar <- function(
  title = NULL,
  barwidth = NULL,
  barheight = NULL,
  ticks = TRUE,
  ticks.colour = "white",
  label.position = NULL,
  n.breaks = NULL,
  reverse = FALSE
) {
  .guide_bar(
    "colourbar",
    title,
    barwidth,
    barheight,
    ticks,
    ticks.colour,
    label.position,
    reverse,
    n.breaks
  )
}

#' @rdname guides
#' @export
guide_colorbar <- guide_colourbar

#' @rdname guides
#' @export
guide_coloursteps <- function(
  title = NULL,
  barwidth = NULL,
  barheight = NULL,
  ticks = FALSE,
  ticks.colour = "white",
  label.position = NULL,
  reverse = FALSE
) {
  .guide_bar(
    "coloursteps",
    title,
    barwidth,
    barheight,
    ticks,
    ticks.colour,
    label.position,
    reverse
  )
}

#' @rdname guides
#' @export
guide_colorsteps <- guide_coloursteps

# Shared builder + validation for guide_colourbar() / guide_coloursteps().
.guide_bar <- function(
  kind,
  title,
  barwidth,
  barheight,
  ticks,
  ticks.colour,
  label.position,
  reverse,
  n.breaks = NULL
) {
  pos <- label.position %||% "right"
  if (!pos %in% c("right", "left")) {
    cli::cli_abort(
      "{.arg label.position} must be {.val right} or {.val left}."
    )
  }
  list(
    kind = kind,
    title = title,
    bar_width = .check_bar_dim(barwidth, "barwidth"),
    bar_height = .check_bar_dim(barheight, "barheight"),
    ticks = isTRUE(ticks),
    ticks_colour = ticks.colour,
    label_position = pos,
    n_breaks = .check_n_breaks(n.breaks),
    reverse = isTRUE(reverse)
  )
}

# A colourbar dimension (`barwidth` / `barheight`) in mm: NULL, or one positive
# number.
.check_bar_dim <- function(x, arg) {
  if (is.null(x)) {
    return(NULL)
  }
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x) || x <= 0) {
    cli::cli_abort(
      "{.arg {arg}} must be a single positive number (millimetres)."
    )
  }
  as.numeric(x)
}

# A requested tick count (`n.breaks`): NULL, or one whole number >= 2. Below two
# there is no bar left to read -- a single tick names one value and says nothing
# about the range -- so the floor is a real constraint, not a guard against
# arithmetic.
.check_n_breaks <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  if (
    !is.numeric(x) ||
      length(x) != 1L ||
      !is.finite(x) ||
      x < 2 ||
      x != round(x)
  ) {
    cli::cli_abort("{.arg n.breaks} must be a single whole number >= 2.")
  }
  as.integer(x)
}

# Validate `override.aes`: NULL, or a fully-named list of aesthetic overrides.
.check_override_aes <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  if (
    !is.list(x) ||
      length(x) == 0L ||
      is.null(names(x)) ||
      any(!nzchar(names(x)))
  ) {
    cli::cli_abort(
      "{.arg override.aes} must be a named list, e.g. {.code list(size = 5, alpha = 1)}."
    )
  }
  # Canonicalise the British spelling so the drawer only checks one key.
  names(x)[names(x) == "colour"] <- "color"
  x
}

# Attach a guide spec to the scale for `aesthetic`: update the existing scale if
# one is declared, else add a guide-only scale (its type/palette are derived from
# the data at train time, exactly as if no scale had been declared).
.set_guide <- function(plot, aesthetic, guide) {
  aesthetic <- .canonical_lim_aes(aesthetic)
  # Match color/fill as aliases (as `.scale_for()` does): a `scale_fill_*()`
  # stores aesthetic "fill", so an exact match on the canonical "color" would
  # miss it and append an empty guide-only scale that shadows the real palette.
  aliases <- .aes_aliases(aesthetic)
  idx <- which(vapply(
    plot@scales,
    function(s) s@aesthetic %in% aliases,
    logical(1)
  ))
  if (length(idx)) {
    plot@scales[[idx[length(idx)]]]@guide <- guide
  } else {
    s <- ScaleSpec(aesthetic = aesthetic)
    s@guide <- guide
    plot@scales <- c(plot@scales, list(s))
  }
  plot
}

# Apply a guide spec to a trained (non-position) scale: `"none"` drops the
# legend; `guide_legend()` can rename it or reverse the key order.
.apply_guide <- function(trained, guide) {
  if (is.null(guide) || is.null(trained)) {
    return(trained)
  }
  if (identical(guide, "none")) {
    trained$no_guide <- TRUE
    return(trained)
  }
  if (is.list(guide)) {
    if (!is.null(guide$title)) {
      trained$name <- guide$title
    }
    if (isTRUE(guide$reverse)) {
      trained <- .reverse_guide(trained)
    }
    if (!is.null(guide[["override.aes"]])) {
      # Carried to the key drawers (`.key_grob` / `.colour_key_grob`), which force
      # these aesthetics on the swatches only -- the marks are untouched.
      trained$override_aes <- guide[["override.aes"]]
    }
    # guide_legend(nested = TRUE) on a size scale draws its keys as concentric,
    # bottom-aligned circles (a proportional-symbol legend) instead of stacked
    # rows; flagged here, re-routed to the bubble drawer in .legend_guides().
    if (isTRUE(guide$nested)) {
      trained$nested <- TRUE
    }
    # guide_colourbar() / guide_coloursteps(): carry the bar geometry + tick/label
    # options onto the trained colour scale for the continuous-guide drawers to
    # read. `coloursteps` also flags a binned scale to render as a segmented bar
    # (re-routed from discrete swatches to the stepped bar in .legend_guides()).
    if (!is.null(guide$n_breaks)) {
      trained <- .rebreak_bar(trained, guide$n_breaks)
    }
    if (guide$kind %in% c("colourbar", "coloursteps")) {
      trained$bar_width <- guide$bar_width
      trained$bar_height <- guide$bar_height
      trained$bar_ticks <- guide$ticks
      trained$bar_ticks_colour <- guide$ticks_colour
      trained$bar_label_pos <- guide$label_position
      if (identical(guide$kind, "coloursteps")) {
        trained$stepped <- TRUE
        # `.reverse_guide()` above already flipped the bin colours (the `binned`
        # branch) but deliberately NOT `breaks` (they would desync a swatch
        # legend). The stepped bar pairs each break with a segment edge, so flip
        # the breaks too here to keep colours and boundary labels in step.
        if (isTRUE(guide$reverse)) {
          trained$breaks <- rev(trained$breaks)
        }
      }
    }
  }
  trained
}

# guide_colourbar(n.breaks = n): re-derive the bar's tick positions for a
# requested count. The ticks of a continuous bar are the trained legend breaks,
# so asking for a different number of them means asking `scales` for a different
# break set over the same range -- not thinning the existing one, which would
# leave ugly gaps and could drop the end labels that tell you the range.
#
# `n` is a *target*: `breaks_extended()` optimises for round numbers over the
# range and returns whatever count reads best nearby, so `n.breaks = 4` may draw
# 3 or 5. That is the same contract as `ggplot2::scale_*(n.breaks =)`, and it is
# the right one -- honouring the count exactly would put ticks on values like
# 23.33.
#
# Silently a no-op unless it applies: only a continuous bar has derived ticks,
# and an explicit `breaks =` / `labels =` on the scale names the values that get
# a tick, which outranks a count.
.rebreak_bar <- function(tr, n) {
  if (
    !identical(tr$kind, "continuous") ||
      isTRUE(tr$breaks_asked) ||
      isTRUE(tr$labels_asked) ||
      is.null(tr$range)
  ) {
    return(tr)
  }
  b <- scales::breaks_extended(n = n)(tr$range)
  b <- .filter_breaks_labels(b, NULL, tr$range)$breaks
  # A range `breaks_extended()` cannot place a tick in at this count keeps the
  # ticks it has: a bar with no labels at all is worse than one labelled at a
  # count you did not ask for.
  if (!length(b)) {
    return(tr)
  }
  tr$legend_breaks <- b
  tr$legend_labels <- .label_number_default(b)
  tr
}

# Reverse a legend's key order (display only; the data -> aesthetic mapping is
# unchanged). Reverses the paired display vectors for the guide's kind.
.reverse_guide <- function(tr) {
  rev_fields <- function(tr, fields) {
    for (f in fields) {
      if (!is.null(tr[[f]])) tr[[f]] <- rev(tr[[f]])
    }
    tr
  }
  switch(
    tr$kind,
    discrete = rev_fields(tr, c("levels", "labels", "colors")),
    # Not `breaks`: they are bin boundaries (length n+1) paired index-wise with
    # the length-n colours/labels; reversing them here would desync boundaries
    # from swatches. Reverse only the n-length display vectors.
    binned = rev_fields(tr, c("levels", "labels", "colors")),
    shape = rev_fields(tr, c("levels", "shapes")),
    linetype = rev_fields(tr, c("levels", "linetypes")),
    # A continuous colourbar positions each label by its value, so reversing the
    # value/label arrays is a visible no-op. Flag the drawer to flip the gradient
    # and mirror each position (1 - frac) instead, keeping value<->label pairing.
    continuous = {
      tr$reverse_bar <- TRUE
      tr
    },
    rev_fields(
      tr,
      c(
        "legend_breaks",
        "legend_labels",
        "legend_sizes",
        "legend_alphas",
        "legend_widths"
      )
    )
  )
}
