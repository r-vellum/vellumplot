#' @include classes.R
NULL

#' Control a scale's legend
#'
#' `guides()` overrides the legend (guide) for one or more aesthetics without
#' respelling the whole `scale_*()`. Pass `"none"` (or [guide_none()]) to hide a
#' legend, [guide_legend()] to tweak a keyed legend (reverse the key order,
#' override the title, restyle the keys with `override.aes`), `guide_colourbar()`
#' to size, tick and style a continuous colour bar, or `guide_coloursteps()` to draw a
#' **binned** colour scale as a segmented bar instead of swatches; [guide_bins()]
#' draws any binned scale's keys with the bin boundaries on an axis. Applies to the
#' non-position legends (`color`/`fill`, `size`, `shape`, `alpha`, `linetype`);
#' for the position axes, pass [guide_axis()] (or `"none"`) as `x` / `y`.
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
#' @param position Where this legend goes: `"right"`, `"left"`, `"top"`,
#'   `"bottom"` or `"inside"` (over the panels, at `theme(legend.position.inside
#'   = )` / `legend.justification`), or `"panel"`: beside each facet panel, on
#'   the side `theme(legend.position = )` names (right by default; inside each
#'   panel when the theme puts legends inside) and justified along the panel
#'   edge by `legend.justification`. A scale resolved `"independent"` across
#'   panels ([resolve_scale()]) draws each panel's legend there by default; a
#'   shared scale placed `"panel"` repeats its legend beside every panel. `NULL`
#'   (default) follows `theme(legend.position = )` (and `"panel"` for an
#'   independent scale). A guide's own position overrides the theme for
#'   that legend only, as in ggplot2 3.5, so legends can sit on different sides
#'   of one plot; legends at one position stack as usual. A legend at the top or
#'   bottom is laid out horizontally. `theme(legend.position = "none")` still
#'   hides every legend. Two legends that would merge (one variable, one title)
#'   only merge when they share a position.
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
#'
#' # one legend at the bottom, the other on the (theme's default) right
#' vplot(mtcars) |>
#'   mark_point(x = wt, y = mpg, color = factor(cyl), size = hp) |>
#'   guides(color = guide_legend(position = "bottom"))
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
guide_none <- function() {
  if (.ggplot2_asks()) return(.ggplot2_own("guide_none"))
  "none"
}

#' @rdname guides
#' @export
guide_legend <- function(
  title = NULL,
  reverse = FALSE,
  override.aes = NULL,
  nested = FALSE,
  position = NULL
) {
  if (.ggplot2_asks()) return(.ggplot2_own("guide_legend"))
  list(
    kind = "legend",
    title = title,
    reverse = isTRUE(reverse),
    override.aes = .check_override_aes(override.aes),
    nested = isTRUE(nested),
    position = .check_guide_position(position)
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
  reverse = FALSE,
  position = NULL
) {
  if (.ggplot2_asks()) return(.ggplot2_own("guide_colourbar"))
  .guide_bar(
    "colourbar",
    title,
    barwidth,
    barheight,
    ticks,
    ticks.colour,
    label.position,
    reverse,
    n.breaks,
    position
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
  reverse = FALSE,
  position = NULL
) {
  if (.ggplot2_asks()) return(.ggplot2_own("guide_coloursteps"))
  .guide_bar(
    "coloursteps",
    title,
    barwidth,
    barheight,
    ticks,
    ticks.colour,
    label.position,
    reverse,
    position = position
  )
}

#' @rdname guides
#' @export
guide_colorsteps <- guide_coloursteps

#' Axis guide
#'
#' `guide_axis()` controls how a position axis draws its tick labels and line:
#' pass it as `guides(x = guide_axis(...))` (or `y`), or as the `guide` of a
#' position scale (`scale_x_continuous(guide = guide_axis(...))`). As in
#' ggplot2, it can stagger crowded labels over several rows (`n.dodge`), rotate
#' them (`angle`), drop the ones that would overlap (`check.overlap`), add minor
#' ticks, and cap the axis line at the outermost breaks. The gutter that holds
#' the labels is measured with the dodging and rotation applied, so nothing is
#' clipped. `guides(x = "none")` draws the axis without labels or ticks.
#'
#' @param title The axis title, or `NULL` to keep the scale's.
#' @param check.overlap If `TRUE`, leave out labels that would overlap one
#'   already drawn, keeping the first, the last and then those in the middle
#'   first (ggplot2's priority). Needs the panel's size, so it applies to a
#'   plot with a set page size; free facet scales are thinned panel by panel,
#'   and fixed-aspect and marginal plots use their laid-out panel size. Polar
#'   axes keep every label.
#' @param angle The rotation of the labels in degrees (overrides the theme's
#'   `axis.text.x`/`axis.text.y` angle), or `NULL` to keep the theme's. The
#'   labels are justified so their ends meet the ticks.
#' @param n.dodge The number of rows (x) or columns (y) to stagger the labels
#'   over, so neighbours alternate between them (default `1`).
#' @param minor.ticks If `TRUE`, also draw shorter ticks halfway between the
#'   breaks (on a continuous scale).
#' @param cap Cap the axis line (theme element `axis.line`) at the outermost
#'   breaks: `"none"` (default; the line spans the panel), `"both"`, `"upper"`
#'   or `"lower"`. `TRUE` / `FALSE` mean `"both"` / `"none"`. With
#'   `minor.ticks = TRUE` the line reaches the outermost tick, major or minor,
#'   as in ggplot2.
#' @return A guide specification for [guides()] or a scale's `guide`.
#' @examples
#' cars <- data.frame(model = rownames(mtcars), mpg = mtcars$mpg)
#' vplot(cars) |>
#'   mark_bar(x = model, y = mpg) |>
#'   guides(x = guide_axis(n.dodge = 2))
#'
#' vplot(cars) |>
#'   mark_bar(x = model, y = mpg) |>
#'   guides(x = guide_axis(angle = 45))
#'
#' vplot(mtcars) |>
#'   mark_point(x = wt, y = mpg) |>
#'   scale_y_continuous(guide = guide_axis(minor.ticks = TRUE)) |>
#'   theme(axis.line = element_line()) |>
#'   guides(x = guide_axis(cap = "both"))
#' @export
guide_axis <- function(
  title = NULL,
  check.overlap = FALSE,
  angle = NULL,
  n.dodge = 1,
  minor.ticks = FALSE,
  cap = "none"
) {
  if (.ggplot2_asks()) return(.ggplot2_own("guide_axis"))
  if (
    !is.null(angle) &&
      (!is.numeric(angle) || length(angle) != 1L || !is.finite(angle))
  ) {
    cli::cli_abort(
      "{.arg angle} must be a single number of degrees or {.code NULL}."
    )
  }
  if (
    !is.numeric(n.dodge) ||
      length(n.dodge) != 1L ||
      !is.finite(n.dodge) ||
      n.dodge < 1 ||
      n.dodge != round(n.dodge)
  ) {
    cli::cli_abort("{.arg n.dodge} must be a whole number of at least 1.")
  }
  if (isTRUE(cap)) {
    cap <- "both"
  } else if (isFALSE(cap)) {
    cap <- "none"
  }
  cap <- rlang::arg_match0(cap, c("none", "both", "upper", "lower"), "cap")
  list(
    kind = "axis",
    title = title,
    check.overlap = isTRUE(check.overlap),
    angle = angle,
    n.dodge = as.integer(n.dodge),
    minor.ticks = isTRUE(minor.ticks),
    cap = cap
  )
}

#' @rdname guide_axis
#' @description `guide_axis_logticks()` draws log10 ticks: long ones at the
#'   powers of ten, middle ones at 5 x 10^k and short ones at the other
#'   integer multiples (ggplot2's `guide_axis_logticks()`). Use it on a
#'   `trans = "log10"` axis, or with `prescale.base = 10` on an axis whose data
#'   are already log10 values.
#' @param long,mid,short Lengths of the ticks at 10^k, 5 x 10^k and the other
#'   multiples, relative to the theme's `axis.ticks.length`.
#' @param prescale.base The base the data were already log-transformed with
#'   (e.g. `10` for `log10(x)` plotted on a linear axis), or `NULL` (default)
#'   when the axis itself transforms (or the data are on their own scale).
#' @export
guide_axis_logticks <- function(
  long = 2.25,
  mid = 1.5,
  short = 0.75,
  prescale.base = NULL,
  cap = "none",
  title = NULL
) {
  if (.ggplot2_asks()) return(.ggplot2_own("guide_axis_logticks"))
  for (nm in c("long", "mid", "short")) {
    v <- get(nm)
    if (!is.numeric(v) || length(v) != 1L || !is.finite(v) || v < 0) {
      cli::cli_abort("{.arg {nm}} must be a single non-negative number.")
    }
  }
  if (
    !is.null(prescale.base) &&
      (!is.numeric(prescale.base) ||
        length(prescale.base) != 1L ||
        !is.finite(prescale.base) ||
        prescale.base <= 1)
  ) {
    cli::cli_abort(
      "{.arg prescale.base} must be a number above 1, or {.code NULL}."
    )
  }
  g <- guide_axis(title = title, cap = cap)
  g$logticks <- list(
    long = long,
    mid = mid,
    short = short,
    base = prescale.base
  )
  g
}

# The log ticks of a trained position scale under `guide_axis_logticks()`:
# native positions and relative lengths of every 1..9 x 10^k inside its domain.
.log_ticks <- function(sc, lt) {
  dom <- range(sc$domain)
  log_axis <- identical(sc$transform, "log10")
  base <- lt$base
  # the axis's data range
  rng <- if (log_axis) {
    10^dom
  } else if (!is.null(base)) {
    base^dom
  } else {
    dom
  }
  rng[1] <- max(rng[1], .Machine$double.xmin)
  if (!all(is.finite(rng)) || rng[2] <= 0) {
    return(list(pos = numeric(0), len = numeric(0)))
  }
  k <- seq(floor(log10(rng[1])), ceiling(log10(rng[2])))
  v <- as.vector(outer(1:9, 10^k))
  mult <- rep(1:9, length(k))
  inside <- v >= rng[1] & v <= rng[2]
  v <- v[inside]
  mult <- mult[inside]
  pos <- if (log_axis) {
    log10(v)
  } else if (!is.null(base)) {
    log(v, base)
  } else {
    v
  }
  len <- ifelse(mult == 1, lt$long, ifelse(mult == 5, lt$mid, lt$short))
  list(pos = pos, len = len)
}

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
  n.breaks = NULL,
  position = NULL
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
    reverse = isTRUE(reverse),
    position = .check_guide_position(position)
  )
}

# The positions a legend guide's `position =` accepts (`NULL` follows the
# theme's `legend.position`).
.GUIDE_POSITIONS <- c("right", "left", "top", "bottom", "inside", "panel")

# A guide's `position =`: NULL, or one of `.GUIDE_POSITIONS`.
.check_guide_position <- function(position) {
  if (is.null(position)) {
    return(NULL)
  }
  if (!.is_string(position) || !position %in% .GUIDE_POSITIONS) {
    cli::cli_abort(
      "{.arg position} must be one of {.or {.val {(.GUIDE_POSITIONS)}}}, or {.code NULL}."
    )
  }
  position
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
  guide <- .guide_math_title(guide, call = rlang::caller_env())
  idx <- which(vapply(
    plot@scales,
    function(s) identical(s@aesthetic, aesthetic),
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
# legend; `guide_legend()` can rename it or reverse the key order. Fields are set
# through `.scale_set()` and the result is re-checked (R/trained-scale.R), so a
# guide cannot leave the scale breaking its invariants.
.apply_guide <- function(trained, guide) {
  if (is.null(guide) || is.null(trained)) {
    return(trained)
  }
  if (identical(guide, "none")) {
    return(.scale_set(trained, no_guide = TRUE))
  }
  if (is.list(guide)) {
    if (identical(guide$kind, "bivariate")) {
      trained <- .apply_bivariate_guide(trained, guide)
    } else if (identical(guide$kind, "legend")) {
      # guide_legend() on a bivariate scale: the classes as a plain keyed list.
      trained <- .scale_set(trained, bivariate = NULL)
    }
    if (!is.null(guide$title)) {
      trained <- .scale_set(trained, name = guide$title)
    }
    # `position =`: where this legend goes, read by the layout
    # (`.legend_groups()`, R/compile-layout.R).
    if (!is.null(guide$position)) {
      trained <- .scale_set(trained, guide_position = guide$position)
    }
    if (isTRUE(guide$reverse)) {
      trained <- .reverse_guide(trained)
    }
    # guide_bins(): keys per bin with the bin boundaries on an axis (drawn by
    # the bins drawers, R/guide-bins.R)
    if (identical(guide$kind, "bins")) {
      trained <- .apply_bins_guide(trained, guide)
    }
    if (!is.null(guide[["override.aes"]])) {
      # Carried to the key drawers (`.key_grob` / `.colour_key_grob`), which force
      # these aesthetics on the swatches only -- the marks are untouched.
      trained <- .scale_set(trained, override_aes = guide[["override.aes"]])
    }
    # guide_legend(nested = TRUE) on a size scale draws its keys as concentric,
    # bottom-aligned circles (a proportional-symbol legend) instead of stacked
    # rows; flagged here, re-routed to the bubble drawer in .legend_guides().
    # Any other scale ignores it.
    if (isTRUE(guide$nested) && identical(trained$aesthetic, "size")) {
      trained <- .scale_set(trained, nested = TRUE)
    }
    # guide_colourbar() / guide_coloursteps(): carry the bar geometry + tick/label
    # options onto the trained colour scale for the continuous-guide drawers to
    # read. `coloursteps` also flags a binned scale to render as a segmented bar
    # (re-routed from discrete swatches to the stepped bar in .legend_guides()).
    if (!is.null(guide$n_breaks)) {
      trained <- .rebreak_bar(trained, guide$n_breaks)
    }
    if (guide$kind %in% c("colourbar", "coloursteps")) {
      trained <- .scale_set(
        trained,
        bar_width = guide$bar_width,
        bar_height = guide$bar_height,
        bar_ticks = guide$ticks,
        bar_ticks_colour = guide$ticks_colour,
        bar_label_pos = guide$label_position
      )
      if (
        identical(guide$kind, "coloursteps") &&
          identical(trained$type, "binned")
      ) {
        # A continuous scale under guide_coloursteps() keeps its bar.
        # `.reverse_guide()` above already flipped the bin colours (the `binned`
        # branch) but deliberately NOT `breaks` (they would desync a swatch
        # legend). The stepped bar pairs each break with a segment edge, so flip
        # the breaks too here to keep colours and boundary labels in step.
        trained <- .scale_set(
          trained,
          stepped = TRUE,
          breaks = if (isTRUE(guide$reverse)) {
            rev(trained$breaks)
          } else {
            trained$breaks
          }
        )
      }
    }
  }
  # `.reverse_guide()`, `.rebreak_bar()` and the bivariate guide edit fields
  # directly; re-check what they leave.
  .validate_trained_scale(trained)
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
    !identical(tr$type, "continuous") ||
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
# unchanged). Reverses the paired display vectors the scale's legend draws: by
# type for a colour scale, by the registry row's guide for the others.
.reverse_guide <- function(tr) {
  rev_fields <- function(tr, fields) {
    for (f in fields) {
      if (!is.null(tr[[f]])) tr[[f]] <- rev(tr[[f]])
    }
    tr
  }
  switch(
    .scale_guide_key(tr) %||% "",
    discrete = rev_fields(tr, c("levels", "labels", "colors")),
    # Not `breaks`: they are bin boundaries (length n+1) paired index-wise with
    # the length-n colours/labels; reversing them here would desync boundaries
    # from swatches. Reverse only the n-length display vectors.
    binned = rev_fields(tr, c("levels", "labels", "colors")),
    shape = rev_fields(tr, c("levels", "shapes")),
    linetype = rev_fields(tr, c("levels", "linetypes")),
    pattern = rev_fields(tr, c("levels", "patterns")),
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
        "levels",
        "legend_breaks",
        "legend_labels",
        "legend_sizes",
        "legend_alphas",
        "legend_widths"
      )
    )
  )
}

# Carry a position scale's axis guide (`guide_axis()`, or `"none"`) onto its
# trained scale as `axis_guide`, where the layout (gutter sizes, overlap) and the
# axis drawers read it; a `title` renames the axis.
.attach_axis_guide <- function(sc, scalespec) {
  g <- if (!is.null(scalespec)) scalespec@guide
  if (identical(g, "none")) {
    return(.scale_set(sc, axis_guide = list(kind = "none")))
  }
  if (!is.list(g) || !identical(g$kind, "axis")) {
    return(sc)
  }
  sc <- .scale_set(sc, axis_guide = g)
  if (!is.null(g$title)) {
    sc <- .scale_set(sc, name = g$title)
  }
  sc
}

# The axis guide of a trained position scale, or the default (plain) one.
.axis_guide_of <- function(sc) {
  g <- sc$axis_guide
  if (is.null(g)) {
    list(kind = "axis", n.dodge = 1L, cap = "none")
  } else {
    g
  }
}

# The rotation of an axis's tick labels: the guide's `angle`, else the theme
# element's.
.axis_rot <- function(sc, el) {
  .axis_guide_of(sc)$angle %||% .el_rot(el)
}

# The labels an axis draws: all of them, or none under `guide_none()`.
.axis_shows_labels <- function(sc) !identical(.axis_guide_of(sc)$kind, "none")

# Which of `n` labels to keep so none overlaps a kept one (ggplot2's
# `check.overlap`): first, last, then recursively the middle of each remaining
# run take priority. `centre`/`size` are each label's centre and extent along
# the axis (same units).
.overlap_keep <- function(centre, size) {
  n <- length(centre)
  keep <- rep(FALSE, n)
  if (!n) {
    return(keep)
  }
  prio <- .axis_label_priority(n)
  lo <- centre - size / 2
  hi <- centre + size / 2
  for (i in prio) {
    if (!is.finite(centre[i])) {
      next
    }
    k <- which(keep)
    if (!length(k) || !any(lo[i] < hi[k] & hi[i] > lo[k])) {
      keep[i] <- TRUE
    }
  }
  keep
}

# ggplot2's label priority order: the ends, then bisect.
.axis_label_priority <- function(n) {
  if (n <= 0L) {
    return(integer(0))
  }
  if (n == 1L) {
    return(1L)
  }
  out <- as.integer(c(1L, n))
  runs <- list(c(1L, n))
  while (length(runs)) {
    nxt <- list()
    for (r in runs) {
      if (r[2] - r[1] > 1L) {
        m <- (r[1] + r[2]) %/% 2L
        out <- c(out, m)
        nxt <- c(nxt, list(c(r[1], m), c(m, r[2])))
      }
    }
    runs <- nxt
  }
  out
}

# Set a position scale's `guide` from a scale constructor's `guide =` argument
# (validated as `guides()` would take it). NULL leaves it alone.
.with_axis_guide <- function(plot, aesthetic, guide) {
  if (is.null(guide)) {
    return(plot)
  }
  if (
    !identical(guide, "none") &&
      !(is.list(guide) && identical(guide$kind, "axis"))
  ) {
    cli::cli_abort(
      "{.arg guide} of a position scale must be {.fn guide_axis} or {.val none}.",
      call = rlang::caller_env()
    )
  }
  .set_guide(plot, aesthetic, guide)
}
