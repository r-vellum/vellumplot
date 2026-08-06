#' @include classes.R
NULL

#' Control a scale's legend
#'
#' `guides()` overrides the legend (guide) for one or more aesthetics without
#' respelling the whole `scale_*()`. Pass `"none"` (or [guide_none()]) to hide a
#' legend, or [guide_legend()] to tweak it (reverse the key order, override the
#' title). Applies to the non-position legends (`color`/`fill`, `size`, `shape`,
#' `alpha`, `linetype`); position axes are unaffected.
#'
#' @param plot A [PlotSpec].
#' @param ... Named by aesthetic, e.g. `guides(color = "none", shape = guide_legend(reverse = TRUE))`.
#' @param title An axis/legend title override, or `NULL` to keep the default.
#' @param reverse Reverse the order of the legend keys / the colour bar.
#' @param barwidth,barheight The colour bar's own width and height, in millimetres
#'   (`guide_colourbar()`). `barheight` sets the bar's **length** on the default
#'   vertical bar (and its thickness on a horizontal one); `barwidth` its
#'   thickness (and the bar length on a horizontal legend). `NULL` auto-sizes.
#' @param ticks Draw the break ticks on the colour bar? (`guide_colourbar()`,
#'   default `TRUE`.)
#' @param ticks.colour Colour of the break ticks (default `"white"`).
#' @param label.position Which side of a **vertical** colour bar the labels sit,
#'   `"right"` (default) or `"left"`.
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
guide_legend <- function(title = NULL, reverse = FALSE, override.aes = NULL) {
  list(
    kind = "legend",
    title = title,
    reverse = isTRUE(reverse),
    override.aes = .check_override_aes(override.aes)
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
  reverse = FALSE
) {
  pos <- label.position %||% "right"
  if (!pos %in% c("right", "left", "top", "bottom")) {
    cli::cli_abort(
      "{.arg label.position} must be one of {.val {c('right', 'left', 'top', 'bottom')}}."
    )
  }
  list(
    kind = "colourbar",
    title = title,
    bar_width = .check_bar_dim(barwidth, "barwidth"),
    bar_height = .check_bar_dim(barheight, "barheight"),
    ticks = isTRUE(ticks),
    ticks_colour = ticks.colour,
    label_position = pos,
    reverse = isTRUE(reverse)
  )
}

#' @rdname guides
#' @export
guide_colorbar <- guide_colourbar

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
    # guide_colourbar(): carry the bar geometry + tick/label options onto the
    # trained colour scale for the continuous-guide drawers to read.
    if (identical(guide$kind, "colourbar")) {
      trained$bar_width <- guide$bar_width
      trained$bar_height <- guide$bar_height
      trained$bar_ticks <- guide$ticks
      trained$bar_ticks_colour <- guide$ticks_colour
      trained$bar_label_pos <- guide$label_position
    }
  }
  trained
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
