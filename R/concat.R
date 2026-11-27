#' @include classes.R seam.R
NULL

# A composition of plots arranged on a grid. A sub-plot keeps its own scales and
# axes; the composition aligns their panels (a shared track grid) and, by default,
# collects identical legends into one. `plots` may hold nested `PlotComposition`s
# (a recursive view tree), so every node can carry its own annotation.
PlotComposition <- S7::new_class(
  "PlotComposition",
  package = "vellumplot",
  properties = list(
    plots = S7::class_list, # list<PlotSpec | PlotComposition>
    nrow = S7::new_property(S7::class_double, default = 1),
    ncol = S7::new_property(S7::class_double, default = 1),
    byrow = S7::new_property(S7::class_logical, default = TRUE),
    widths = S7::new_property(S7::class_any, default = NULL), # per-col panel weights
    heights = S7::new_property(S7::class_any, default = NULL), # per-row panel weights
    guides = S7::new_property(S7::class_character, default = "collect"), # collect|keep
    design = S7::new_property(S7::class_any, default = NULL), # list<area> | NULL
    labels = S7::new_property(S7::class_list, default = list()), # figure title/...
    tag = S7::new_property(S7::class_any, default = NULL), # auto-tag spec | NULL
    theme = S7::new_property(S7::class_any, default = NULL), # figure-level theme | NULL
    insets = S7::new_property(S7::class_list, default = list()), # list<inset spec>
    width = S7::new_property(S7::class_double, default = 6),
    height = S7::new_property(S7::class_double, default = 4),
    dpi = S7::new_property(S7::class_double, default = 96)
  )
)

.collect_plots <- function(dots, call = rlang::caller_env()) {
  if (!length(dots)) {
    cli::cli_abort("Provide at least one plot to arrange.", call = call)
  }
  for (i in seq_along(dots)) {
    p <- dots[[i]]
    if (
      !S7::S7_inherits(p, PlotSpec) &&
        !S7::S7_inherits(p, PlotComposition) &&
        !S7::S7_inherits(p, Spacer)
    ) {
      cli::cli_abort(
        c(
          "Composition expects {.cls PlotSpec} or {.cls PlotComposition} objects.",
          "x" = "Argument {i} is {.obj_type_friendly {p}}."
        ),
        call = call
      )
    }
  }
  dots
}

#' Arrange plots side by side
#'
#' Compose several independent plots into one image. `hconcat()` lays them in a
#' row, `vconcat()` in a column, and `concat()` on a grid of `ncol`/`nrow` cells.
#' Each sub-plot keeps its own scales, axes, and legend (this is view
#' composition, not faceting).
#'
#' Sub-plot panels are **aligned** across the grid (a shared track grid lines up
#' panel edges even when axis labels differ), and identical legends are
#' **collected** into one shared legend by default (`guides = "collect"`). Pass
#' `PlotComposition`s in `...` to nest a sub-grid. Annotate the whole figure with
#' [compose_annotation()].
#'
#' @param ... [PlotSpec]s or `PlotComposition`s to arrange.
#' @param ncol,nrow Grid dimensions for `concat()` (defaults to roughly square).
#' @param byrow Fill the grid by row (default) or by column.
#' @param widths,heights Relative panel sizes per column / row (recycled numeric
#'   vector), or `NULL` for equal panels. `hconcat()` takes `widths` and
#'   `vconcat()` takes `heights`.
#' @param guides `"collect"` (default) to dedupe identical legends across
#'   sub-plots into one, or `"keep"` to leave each sub-plot's legend in place.
#'   The collected legend goes where the composition's theme puts it
#'   (`theme(comp, legend.position = )`: `"right"`, the default, `"left"`,
#'   `"top"`, `"bottom"` or `"none"`), as in patchwork; the sub-plots' own
#'   themes do not move it.
#' @param design An explicit layout: a list of [area()]s (one per plot, in the
#'   order plots are passed) or a textual layout string (rows split by `\n`, `#`
#'   an empty cell). In a string, distinct letters bind to the plots in `...` in
#'   **alphabetical** order (the first letter to the first plot), so there must
#'   be exactly one letter per plot and every row must be the same width.
#'   Enables spanning; `NULL` (default) uses the regular `ncol`/`nrow` grid.
#' @param width,height Output size in inches (defaults scale with the grid).
#' @param dpi Output resolution in dots per inch. `NULL` (default) inherits the
#'   first sub-plot's resolution; overridable at render time via [render_plot()].
#' @return A `PlotComposition` (renders via [render_plot()] / [vellum::render()]).
#' @examples
#' a <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
#' b <- vplot(mtcars) |> mark_histogram(x = mpg, bins = 10)
#' hconcat(a, b)
#' @export
concat <- function(
  ...,
  ncol = NULL,
  nrow = NULL,
  byrow = TRUE,
  widths = NULL,
  heights = NULL,
  guides = c("collect", "keep"),
  design = NULL,
  width = NULL,
  height = NULL,
  dpi = NULL
) {
  .concat_impl(
    .collect_plots(list(...)),
    ncol = ncol,
    nrow = nrow,
    byrow = byrow,
    widths = widths,
    heights = heights,
    guides = match.arg(guides),
    design = design,
    width = width,
    height = height,
    dpi = dpi
  )
}

# The body of `concat()`, shared with `hconcat()`, `vconcat()` and
# `wrap_plots()` so they pass their plots as a list rather than splice them
# into a `do.call()` (whose errors named no function and printed the plot
# objects, VP-151 review). Errors name `call`, the user's function.
.concat_impl <- function(
  plots,
  ncol = NULL,
  nrow = NULL,
  byrow = TRUE,
  widths = NULL,
  heights = NULL,
  guides = "collect",
  design = NULL,
  width = NULL,
  height = NULL,
  dpi = NULL,
  call = rlang::caller_env()
) {
  if (!is.null(dpi)) {
    .check_dpi(dpi, call = call)
  }
  if (!is.null(width)) {
    .check_dim(width, "width", call = call)
  }
  if (!is.null(height)) {
    .check_dim(height, "height", call = call)
  }
  n <- length(plots)
  design <- .parse_design(design, n, call = call)
  if (!is.null(design)) {
    nrow <- max(vapply(design, function(a) a$b, numeric(1)))
    ncol <- max(vapply(design, function(a) a$r, numeric(1)))
  }
  if (is.null(ncol) && is.null(nrow)) {
    ncol <- ceiling(sqrt(n))
  }
  if (is.null(ncol)) {
    ncol <- ceiling(n / nrow)
  }
  if (is.null(nrow)) {
    nrow <- ceiling(n / ncol)
  }
  ncol <- as.double(ncol)
  nrow <- as.double(nrow)
  .check_track_weights(widths, "widths", ncol, "column", call)
  .check_track_weights(heights, "heights", nrow, "row", call)
  # Inherit size / dpi from the first real sub-plot; a leading spacer must not
  # dictate the composition's cell size or resolution.
  ref <- .first_real_plot(plots)
  w0 <- .comp_unit_size(ref, "width")
  h0 <- .comp_unit_size(ref, "height")
  PlotComposition(
    plots = plots,
    nrow = nrow,
    ncol = ncol,
    byrow = byrow,
    widths = if (!is.null(widths)) as.double(widths),
    heights = if (!is.null(heights)) as.double(heights),
    guides = guides,
    design = design,
    width = width %||% (ncol * w0),
    height = height %||% (nrow * h0),
    dpi = as.double(dpi %||% .comp_dpi(ref))
  )
}

# `widths` / `heights`: NULL, or positive finite weights, one per column / row
# or a single one for all. Before, a string, NA, a negative or a surplus
# weight was accepted and drew nonsense (VP-151 review).
.check_track_weights <- function(x, arg, n, what, call) {
  if (is.null(x)) {
    return(invisible())
  }
  if (
    !is.numeric(x) ||
      !length(x) ||
      anyNA(x) ||
      !all(is.finite(x)) ||
      any(x <= 0)
  ) {
    cli::cli_abort(
      "{.arg {arg}} must be positive numbers, not {.obj_type_friendly {x}}{if (is.numeric(x)) paste0(' (', paste(x, collapse = ', '), ')')}.",
      call = call
    )
  }
  if (!length(x) %in% c(1L, n)) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must have one value or one per {what}.",
        i = "Got {length(x)} value{?s} for {n} {if (n == 1) what else paste0(what, 's')}."
      ),
      call = call
    )
  }
  invisible()
}

# The first non-Spacer sub-plot, used to inherit size / dpi. Falls back to the
# first element when every entry is a spacer.
.first_real_plot <- function(plots) {
  for (p in plots) {
    if (!S7::S7_inherits(p, Spacer)) {
      return(p)
    }
  }
  plots[[1]]
}

# A reserved-but-empty cell.
Spacer <- S7::new_class("Spacer", package = "vellumplot")

#' Reserve an empty cell in a composition
#'
#' Use inside [concat()] / [wrap_plots()] (or a `design`) to leave a gap.
#' @return A spacer placeholder.
#' @examples
#' a <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
#' concat(a, plot_spacer(), a, plot_spacer(), ncol = 2)
#' @export
plot_spacer <- function() Spacer()

#' Define a layout area for `design =`
#'
#' An `area()` is a rectangular block of grid cells (1-based, inclusive) that a
#' sub-plot occupies. Pass a list of `area()`s (one per plot, in order) as
#' `concat(..., design = )` to place plots on an explicit, possibly spanning,
#' grid.
#'
#' @param t,l,b,r Top, left, bottom, right cell indices (1-based, inclusive).
#' @return An `area` spec.
#' @examples
#' a <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
#' b <- vplot(mtcars) |> mark_point(x = hp, y = mpg)
#' concat(a, b, design = list(area(1, 1, 1, 2), area(2, 1, 2, 1)))
#' @export
area <- function(t, l, b = t, r = l) {
  # A cell index must be a single positive integer. Guard before coercion:
  # `as.numeric("a")` is a silent `NA`, so `NA > b` would abort with the opaque
  # "missing value where TRUE/FALSE needed" rather than a clear message.
  cell <- function(v, nm) {
    n <- suppressWarnings(as.numeric(v))
    if (length(n) != 1L || is.na(n) || n < 1 || n != round(n)) {
      cli::cli_abort(c(
        "{.arg {nm}} must be a single positive integer (a 1-based cell index).",
        i = "Got {.obj_type_friendly {v}}."
      ))
    }
    n
  }
  t <- cell(t, "t")
  l <- cell(l, "l")
  b <- cell(b, "b")
  r <- cell(r, "r")
  if (t > b || l > r) {
    cli::cli_abort(c(
      "An {.fn area} must have {.code t <= b} and {.code l <= r}.",
      i = "Got t={t}, l={l}, b={b}, r={r}."
    ))
  }
  list(t = t, l = l, b = b, r = r)
}

# Normalise `design` into a list of area()s (one per plot) or NULL.
# Accepts a list of area()s, or a textual layout (rows separated by newlines;
# each distinct letter is one plot in alphabetical order, "#" = empty).
.parse_design <- function(design, n, call = rlang::caller_env()) {
  if (is.null(design)) {
    return(NULL)
  }
  if (is.list(design)) {
    if (length(design) != n) {
      cli::cli_abort(
        c(
          "A {.arg design} list must have one {.fn area} per plot.",
          i = "Got {length(design)} area{?s} for {n} plot{?s}."
        ),
        call = call
      )
    }
    # Validate each element is a well-formed area (t/l/b/r), so a hand-built list
    # missing a field fails here rather than later as a cryptic `$b`/`$r` error.
    ok_area <- function(a) {
      is.list(a) &&
        all(c("t", "l", "b", "r") %in% names(a)) &&
        all(vapply(
          a[c("t", "l", "b", "r")],
          function(v) is.numeric(v) && length(v) == 1L && !is.na(v),
          logical(1)
        )) &&
        a$t <= a$b &&
        a$l <= a$r
    }
    if (!all(vapply(design, ok_area, logical(1)))) {
      cli::cli_abort(
        c(
          "Each element of a {.arg design} list must be an {.fn area}.",
          i = "Build them with {.fn area}, e.g. {.code area(1, 1, 2, 2)}."
        ),
        call = call
      )
    }
    return(design)
  }
  if (is.character(design)) {
    rows <- trimws(strsplit(trimws(design), "\n")[[1]])
    widths <- nchar(rows)
    if (length(unique(widths)) != 1L) {
      cli::cli_abort(
        c(
          "Each row of a {.arg design} layout string must be the same width.",
          i = "Got rows of differing widths: {.val {widths}}."
        ),
        call = call
      )
    }
    m <- do.call(rbind, lapply(rows, function(s) strsplit(s, "")[[1]]))
    # Distinct letters bind to the plots in `...` in alphabetical order; "#" is
    # an empty cell.
    letters_used <- sort(setdiff(unique(as.vector(m)), "#"))
    if (length(letters_used) != n) {
      cli::cli_abort(
        c(
          "A {.arg design} layout must name exactly one area per plot.",
          i = "Got {length(letters_used)} area{?s} ({.val {letters_used}}) for {n} plot{?s}.",
          i = "Plots fill the lettered areas in alphabetical order."
        ),
        call = call
      )
    }
    out <- lapply(letters_used, function(ch) {
      rc <- which(m == ch, arr.ind = TRUE)
      tt <- min(rc[, 1])
      bb <- max(rc[, 1])
      ll <- min(rc[, 2])
      rr <- max(rc[, 2])
      # A letter must tile a solid rectangle; an L-shaped or scattered region
      # would silently expand to a filled bounding box and overlap a neighbour.
      if (nrow(rc) != (bb - tt + 1L) * (rr - ll + 1L)) {
        cli::cli_abort(
          c(
            "Area {.val {ch}} in the {.arg design} layout must be a solid rectangle.",
            i = "Its cells don't fill their {bb - tt + 1L} x {rr - ll + 1L} bounding box."
          ),
          call = call
        )
      }
      area(tt, ll, bb, rr)
    })
    return(out)
  }
  cli::cli_abort(
    "{.arg design} must be a list of {.fn area}s or a layout string.",
    call = call
  )
}

#' Overlay a plot as an inset
#'
#' Place `plot` as an inset over `base`, positioned by fractional coordinates
#' (0–1) of the chosen reference box. Returns a 1-cell `PlotComposition` carrying
#' the inset (compose it further or render directly).
#'
#' @param base A [PlotSpec] or `PlotComposition` to draw underneath.
#' @param plot The [PlotSpec] to overlay.
#' @param left,bottom,right,top Inset position as fractions (0–1).
#' @param align_to Reference box the fractions are measured in, as in
#'   patchwork's `inset_element()`:
#'   * `"panel"` (default): the base plot's panel area. For a faceted plot this
#'     is the area spanned by all panels (from the first panel to the last,
#'     the strips and gaps between them included).
#'   * `"plot"`: the base plot without its title, subtitle, caption and tag,
#'     axes and legends included (inside the plot margin).
#'   * `"full"`: the whole `base` cell, plot margin included.
#'
#'   When `base` is a composition of several plots there is no single plot to
#'   align to, and every inset is placed in the whole figure (`"full"`).
#' @param on_top Draw the inset above (`TRUE`) or below the base.
#' @param shadow Optional drop shadow cast by the inset, from
#'   [vellum::vl_shadow()] (e.g. `vl_shadow(dx = 2, dy = 3, blur = 6)`).
#' @details Up to vellumplot 0.21.1 every inset was placed in the whole base
#'   cell, whatever `align_to` said; pass `align_to = "full"` for that placement.
#'
#'   A plot with a locked aspect ratio (`coord_fixed()`, `coord_sf()`,
#'   `coord_polar()`, or `theme(aspect.ratio =)`) is fitted inside the inset
#'   box: the box shrinks in one direction to the plot's own shape, keeping
#'   the side nearest the edge it sits on (a box in the right half keeps its
#'   right edge, one in the top half its top edge), so its background and
#'   border hug the panel rather than framing empty bands.
#'
#'   Each inset's viewport is named `"inset-<k>"` (the k-th inset of the
#'   composition), so it can be found with [vellum::node_names()].
#' @return A `PlotComposition`.
#' @examples
#' a <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
#' b <- vplot(mtcars) |> mark_histogram(x = mpg, bins = 8)
#' inset(a, b, left = 0.5, bottom = 0.45, right = 1, top = 1)
#' @export
inset <- function(
  base,
  plot,
  left = 0.6,
  bottom = 0.6,
  right = 0.98,
  top = 0.98,
  align_to = c("panel", "plot", "full"),
  on_top = TRUE,
  shadow = NULL
) {
  align_to <- match.arg(align_to)
  if (!S7::S7_inherits(plot, PlotSpec)) {
    cli::cli_abort("{.arg plot} (the inset) must be a {.cls PlotSpec}.")
  }
  # left/bottom/right/top are fractions of the anchor region; a value outside
  # [0, 1] or an inverted pair (left >= right, bottom >= top) yields a
  # negative-size or overflowing viewport that renders silently wrong.
  bounds <- c(left = left, bottom = bottom, right = right, top = top)
  if (
    !all(vapply(
      bounds,
      function(v) is.numeric(v) && length(v) == 1L && is.finite(v),
      logical(1)
    ))
  ) {
    cli::cli_abort(
      "Inset {.arg left}/{.arg bottom}/{.arg right}/{.arg top} must each be a single finite number."
    )
  }
  if (any(bounds < 0) || any(bounds > 1)) {
    cli::cli_abort(
      "Inset {.arg left}/{.arg bottom}/{.arg right}/{.arg top} must be fractions in {.val {c(0, 1)}}."
    )
  }
  if (left >= right || bottom >= top) {
    cli::cli_abort(c(
      "Inset bounds must satisfy {.code left < right} and {.code bottom < top}.",
      "x" = "Got left={left}, right={right}, bottom={bottom}, top={top}."
    ))
  }
  comp <- if (S7::S7_inherits(base, PlotComposition)) {
    base
  } else {
    concat(base, ncol = 1, nrow = 1)
  }
  comp@insets <- c(
    comp@insets,
    list(list(
      plot = plot,
      left = left,
      bottom = bottom,
      right = right,
      top = top,
      align_to = align_to,
      on_top = on_top,
      shadow = shadow
    ))
  )
  comp
}

# Page size of a sub-plot (PlotSpec or nested PlotComposition) for auto-sizing.
.comp_unit_size <- function(x, which) {
  if (S7::S7_inherits(x, PlotComposition)) {
    if (which == "width") x@width / x@ncol else x@height / x@nrow
  } else if (S7::S7_inherits(x, Spacer)) {
    if (which == "width") 6 else 4
  } else {
    S7::prop(x, which)
  }
}

# Resolution to inherit for a composition: the first sub-plot's dpi (a Spacer
# carries none, so fall back to the scene default).
.comp_dpi <- function(x) {
  if (S7::S7_inherits(x, PlotSpec) || S7::S7_inherits(x, PlotComposition)) {
    x@dpi
  } else {
    96
  }
}

#' @rdname concat
#' @param plots A list of [PlotSpec]s / `PlotComposition`s (for `wrap_plots()`).
#' @export
wrap_plots <- function(
  plots,
  ncol = NULL,
  nrow = NULL,
  byrow = TRUE,
  widths = NULL,
  heights = NULL,
  guides = c("collect", "keep"),
  design = NULL,
  width = NULL,
  height = NULL,
  dpi = NULL
) {
  # Strip names so a list element named like a `concat` parameter (e.g. `ncol`)
  # is a plot, not that argument.
  .concat_impl(
    .collect_plots(unname(plots)),
    ncol = ncol,
    nrow = nrow,
    byrow = byrow,
    widths = widths,
    heights = heights,
    guides = match.arg(guides),
    design = design,
    width = width,
    height = height,
    dpi = dpi
  )
}

#' @rdname concat
#' @export
hconcat <- function(
  ...,
  widths = NULL,
  guides = c("collect", "keep"),
  width = NULL,
  height = NULL,
  dpi = NULL
) {
  plots <- .concat_dots(list(...), "hconcat")
  .concat_impl(
    plots,
    ncol = length(plots),
    nrow = 1,
    widths = widths,
    guides = match.arg(guides),
    width = width,
    height = height,
    dpi = dpi
  )
}

#' @rdname concat
#' @export
vconcat <- function(
  ...,
  heights = NULL,
  guides = c("collect", "keep"),
  width = NULL,
  height = NULL,
  dpi = NULL
) {
  plots <- .concat_dots(list(...), "vconcat")
  .concat_impl(
    plots,
    ncol = 1,
    nrow = length(plots),
    heights = heights,
    guides = match.arg(guides),
    width = width,
    height = height,
    dpi = dpi
  )
}

# The plots passed in `...` to `hconcat()` / `vconcat()`. A named argument
# that is not one of the function's own (e.g. `heights =` to `hconcat()`)
# used to slip through to `concat()` and still count towards the grid size, so
# `vconcat(a, b, heights = c(3, 1))` made a three-row grid, and
# `hconcat(a, b, width = 8)` a three-column one (VP-151). Reject those, and
# any non-plot, with a clear error.
.concat_dots <- function(dots, fn, call = rlang::caller_env()) {
  nm <- names(dots) %||% rep("", length(dots))
  bad <- nzchar(nm) & nm %in% names(formals(concat))
  if (any(bad)) {
    own <- setdiff(names(formals(fn)), "...")
    cli::cli_abort(
      c(
        "{.fn {fn}} does not take {.arg {nm[bad]}}.",
        i = "Its arguments are {.arg {own}}; use {.fn concat} for full control."
      ),
      call = call
    )
  }
  .collect_plots(unname(dots), call = call)
}

#' Annotate a composition
#'
#' Add figure-level text (a `title`, `subtitle`, `caption`) spanning the whole
#' composition, and/or auto-tag the sub-plots (`A`, `B`, `C`, …). Because every
#' `PlotComposition` carries its own annotation, this works at any nesting level
#' (unlike patchwork, where annotation is top-level only).
#'
#' @param plot A `PlotComposition` (from [concat()] / [hconcat()] / [vconcat()]).
#' @param title,subtitle,caption Figure-level text (or `NULL`).
#' @param tag_levels Auto-tag style: `"A"`, `"a"`, `"1"`, `"i"`, or `"I"`
#'   (`NULL` = no tags).
#' @param tag_prefix,tag_suffix Strings wrapped around each tag.
#' @return The modified `PlotComposition`.
#' @seealso [theme()], which also accepts a composition to set the figure-level
#'   chrome (title bands, collected legend, panel spacing, tags).
#' @examples
#' a <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
#' b <- vplot(mtcars) |> mark_point(x = hp, y = mpg)
#' hconcat(a, b) |> compose_annotation(title = "Fuel economy", tag_levels = "A")
#' @export
compose_annotation <- function(
  plot,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  tag_levels = NULL,
  tag_prefix = "",
  tag_suffix = ""
) {
  if (!S7::S7_inherits(plot, PlotComposition)) {
    cli::cli_abort("{.arg plot} must be a {.cls PlotComposition}.")
  }
  over <- list(title = title, subtitle = subtitle, caption = caption)
  over <- over[!vapply(over, is.null, logical(1))]
  plot@labels <- utils::modifyList(plot@labels, over)
  if (!is.null(tag_levels)) {
    plot@tag <- list(
      levels = tag_levels,
      prefix = tag_prefix,
      suffix = tag_suffix
    )
  }
  plot
}

# Compile a composition to a vellum scene. The aligned path (.compile_aligned,
# in compile-composition.R) shares one track grid across sub-plots so panel edges
# line up and legends collect; it handles the common case (single-panel sub-plots
# on a regular grid). Anything it can't align yet (faceted/polar sub-plots) falls
# back to the independent per-cell layout.
# --- composition cell ids ----------------------------------------------------
# Every leaf plot of a composition is a *cell*, numbered depth-first in the order
# `.composition_leaf_plots()` walks them ("cell-1", "cell-2", ...; spacers and
# insets are not cells). The compiler prefixes each cell's panel viewport names
# with its id and records it in the panel `meta` (`cell`), so the panels stay
# unique across the figure and a host (vellumwidget) can tell which cell a panel
# belongs to -- e.g. to zoom the detail cell a `bind_scale()` names. The
# interaction model tags binds/selections with the same ids.
.comp_cell_ctx <- new.env(parent = emptyenv())

.comp_cell_id <- function(k) sprintf("cell-%d", k)

# Advance to the next cell; the next panel draw claims it (`.comp_cell_take()`).
.comp_cell_next <- function() {
  n <- (.comp_cell_ctx$n %||% 0L) + 1L
  .comp_cell_ctx$n <- n
  .comp_cell_ctx$current <- .comp_cell_id(n)
  invisible(.comp_cell_ctx$current)
}

# Claim the pending cell id (NULL outside a composition). Claiming clears it, so
# a plot drawn *inside* a cell's marks (an inset grob) is not mistaken for it.
.comp_cell_take <- function() {
  cur <- .comp_cell_ctx$current
  .comp_cell_ctx$current <- NULL
  cur
}

.comp_cell_reset <- function() {
  .comp_cell_ctx$n <- 0L
  .comp_cell_ctx$current <- NULL
  .comp_cell_ctx$shared_cond_tags <- NULL
}

# A panel viewport's name inside cell `cell` ("cell-2-panel-1-1"); unchanged
# outside a composition.
.cell_panel_name <- function(pname, cell) {
  if (is.null(cell)) pname else paste0(cell, "-", pname)
}

.compile_composition <- function(comp) {
  .provenance_reset()
  .comp_cell_reset()
  on.exit(.comp_cell_reset(), add = TRUE)
  # condition tags two cells share, prefixed per cell (VP-75)
  .comp_cell_ctx$shared_cond_tags <- .comp_shared_cond_tags(
    .composition_leaf_plots(comp)
  )
  scene <- vellum::vl_scene(
    width = comp@width,
    height = comp@height,
    dpi = comp@dpi,
    # The page takes the figure theme's plot.background (white by default;
    # the first themed sub-plot's or a brand's page colour, F-26).
    bg = .scene_bg(.resolve_theme(.comp_figure_theme(comp))),
    # Accessibility: the figure title (if any) names the composition and the
    # generated summary describes it. Additive; see R/alt.R.
    title = .alt_name(comp@labels),
    desc = .alt_desc_safe(comp)
  )
  if (.comp_alignable(comp)) {
    scene <- vellum::push(scene, vellum::vl_viewport())
    scene <- .draw_composition(scene, comp, page = c(comp@width, comp@height))
    scene <- vellum::pop(scene)
  } else {
    # The top-level grid fills the page, so an inset's box has a known size
    # there (a fixed-aspect inset fits to it, VP-120).
    scene <- .compile_composition_independent(
      scene,
      comp,
      box_mm = c(comp@width, comp@height) * 25.4
    )
  }
  # Provenance accumulates across every sub-plot's marks; carry it out (DESIGN §4).
  attr(scene, "vellumplot_provenance") <- .provenance_snapshot()
  scene
}

# Legacy layout: an outer grid of equal null cells, each holding one sub-plot
# rendered with .draw_plot() (its own grid_layout inside the cell). No panel
# alignment. Used as a fallback for sub-plots the aligned path can't place
# (facets, polar / sf / fixed-aspect coords, designs, insets, nested
# compositions).
#
# The figure title / subtitle / caption and the collected legend
# (`guides = "collect"`) take bands around the cell grid, drawn by the aligned
# path's own helpers; before, this layout dropped them all (VP-152). A
# composition with none of them draws exactly as before.
.compile_composition_independent <- function(scene, comp, box_mm = NULL) {
  rt0 <- .resolve_theme(.comp_figure_theme(comp))
  col <- .comp_collect_independent(comp, rt0)
  comp <- col$comp
  figguides <- col$figguides
  # the collected legend keys its swatches when a collected cell is
  # interactive, as the cells' own legends did (`.tag_legend_swatch()`)
  rt0[[".interactive"]] <- col$interactive
  side <- .fig_legend_side(rt0)
  has <- function(x) !is.null(x) && !identical(x, "")
  lab <- comp@labels
  # A nested composition next to its parent's figure band drops its own
  # margin on that side (VP-168): its cells' plot margins there, and its own
  # margin row above / below its bands.
  trim <- attr(comp, "vp_band_trim")
  trim_top <- isTRUE(trim[["top"]])
  trim_bottom <- isTRUE(trim[["bottom"]])
  if (
    !has(lab$title) &&
      !has(lab$subtitle) &&
      !has(lab$caption) &&
      !length(figguides)
  ) {
    comp <- .comp_trim_band_margins(comp, trim_top, trim_bottom)
    return(.comp_independent_cells(scene, comp, box_mm, rt0))
  }
  m <- .comp_apply_band_trim(rep_len(rt0[["plot.margin"]] %||% 0, 4L), comp)
  # columns: margin, [left legend], cells, [right legend], margin. The cells
  # also cover the margin columns (each cell keeps its own plot margin), the
  # title bands sit inside them.
  W <- .tracks()
  H <- .tracks()
  .tk_add(W, vellum::vl_unit(m[4], "mm"))
  leg_col <- leg_row <- NA_integer_
  if (length(figguides) && side == "left") {
    leg_col <- .tk_add(W, .legend_width(figguides, rt0))
  }
  cells_col <- .tk_add(W, vellum::vl_unit(1, "null"))
  if (length(figguides) && side == "right") {
    leg_col <- .tk_add(W, .legend_width(figguides, rt0))
  }
  .tk_add(W, vellum::vl_unit(m[2], "mm"))
  # rows: [margin, title, subtitle, top legend], cells, [bottom legend,
  # caption, margin]; a margin row only on a side with a band.
  top <- has(lab$title) ||
    has(lab$subtitle) ||
    (length(figguides) && side == "top")
  bottom <- has(lab$caption) || (length(figguides) && side == "bottom")
  comp <- .comp_trim_band_margins(comp, top || trim_top, bottom || trim_bottom)
  if (top) {
    .tk_add(H, vellum::vl_unit(m[1], "mm"))
  }
  band_row <- function(label, el, pad) {
    if (has(label)) {
      .tk_add(H, .track_h_band(rt0[[el]], label, pad, NA_real_))
    } else {
      NA_integer_
    }
  }
  glo <- list(
    figtitle_row = band_row(lab$title, "plot.title", 2 * .PAD_MM),
    figsubtitle_row = band_row(lab$subtitle, "plot.subtitle", .PAD_MM)
  )
  if (length(figguides) && side == "top") {
    leg_row <- .tk_add(H, .legend_height(figguides, rt0))
  }
  cells_row <- .tk_add(H, vellum::vl_unit(1, "null"))
  if (length(figguides) && side == "bottom") {
    leg_row <- .tk_add(H, .legend_height(figguides, rt0))
  }
  glo$figcaption_row <- band_row(lab$caption, "plot.caption", .PAD_MM)
  if (bottom) {
    .tk_add(H, vellum::vl_unit(m[3], "mm"))
  }
  widths <- .tk_units(W)
  heights <- .tk_units(H)
  nw <- length(W$u)
  # the cells span every column but the legend's, every row but the bands'
  c0 <- if (!is.na(leg_col) && side == "left") cells_col else 1L
  c1 <- if (!is.na(leg_col) && side == "right") cells_col else nw
  # Insets placed in the whole figure keep the whole figure, bands and
  # legend included, as their reference box.
  fig_ins <- which(!.comp_insets_in_plot(comp))
  scene <- .draw_insets(
    scene,
    comp,
    on_top = FALSE,
    box_mm = box_mm,
    which = fig_ins
  )
  fig_box <- box_mm
  scene <- vellum::push(
    scene,
    vellum::vl_viewport(layout = vellum::grid_layout(widths, heights))
  )
  # the bands span the columns inside the margins (`.draw_band()` takes the
  # first column and a count)
  scene <- vellum::push(
    scene,
    vellum::vl_viewport(
      row = 1,
      col = 2,
      rowspan = length(H$u),
      colspan = nw - 2L
    )
  )
  scene <- vellum::push(
    scene,
    vellum::vl_viewport(
      layout = vellum::grid_layout(widths[seq(2, nw - 1L)], heights)
    )
  )
  glo$ncol_total <- nw - 2L
  scene <- .draw_figure_bands(scene, comp, glo, rt0)
  scene <- vellum::pop(vellum::pop(scene))
  # The collected legend's keys take the composition theme's default mark
  # colours, else the first plot's, as in the aligned path; the caller's are
  # put back afterwards.
  old_geom <- .mark_ctx$geom
  old_ground <- .mark_ctx$geom_ground
  on.exit(assign("geom", old_geom, envir = .mark_ctx), add = TRUE)
  on.exit(assign("geom_ground", old_ground, envir = .mark_ctx), add = TRUE)
  first <- .first_real_plot(comp@plots)
  .set_geom_ctx(
    if (
      !is.null(.theme_geom(comp@theme)) || !S7::S7_inherits(first, PlotSpec)
    ) {
      comp@theme
    } else {
      .theme_of(first)
    }
  )
  if (!is.na(leg_col)) {
    scene <- .draw_legends(
      scene,
      list(row = cells_row, col = leg_col),
      figguides,
      rt0,
      orient = "vertical"
    )
  }
  if (!is.na(leg_row)) {
    scene <- .draw_legends(
      scene,
      list(row = leg_row, col = cells_col),
      figguides,
      rt0,
      orient = "horizontal"
    )
  }
  # The cells' box: the figure's less the bands and the legend.
  if (!is.null(box_mm)) {
    mm <- function(u) {
      sum(vapply(seq_along(u), function(i) vellum::vl_convert(u[i], "mm"), 0))
    }
    keep_w <- setdiff(seq_len(nw), seq(c0, c1))
    keep_h <- setdiff(seq_along(H$u), cells_row)
    box_mm <- box_mm - c(mm(widths[keep_w]), mm(heights[keep_h]))
  }
  scene <- vellum::push(
    scene,
    vellum::vl_viewport(row = cells_row, col = c0, colspan = c1 - c0 + 1L)
  )
  scene <- .comp_independent_cells(
    scene,
    comp,
    box_mm,
    rt0,
    figure_insets = FALSE
  )
  scene <- vellum::pop(vellum::pop(scene))
  .draw_insets(scene, comp, on_top = TRUE, box_mm = fig_box, which = fig_ins)
}

# Which of a composition's insets are aligned to its base plot's panel or
# plot area and drawn by `.draw_plot()` inside the plot's own layout grid
# (VP-127)? That needs a single base plot; the rest are placed in the whole
# figure.
.comp_insets_in_plot <- function(comp) {
  single <- is.null(comp@design) &&
    length(comp@plots) == 1L &&
    S7::S7_inherits(comp@plots[[1]], PlotSpec)
  vapply(
    comp@insets,
    function(ins) single && !identical(ins$align_to %||% "full", "full"),
    logical(1)
  )
}

# The collected legend of an independently laid-out composition
# (`guides = "collect"`): every outside legend of its plot cells, pooled and
# deduplicated by `.guide_key()` as the aligned path does. Per guide, as
# there: a cell's legends outside its panel join the figure's legend, and its
# legends inside its panel or beside each facet panel (`position = "panel"`,
# independent facet scales) stay with it. A collected cell carries the keys
# of the guides it gave up (attribute `vp_collected_keys`), which
# `.draw_plot()` leaves out. Nested compositions collect their own; a
# single-plot composition (an `inset()` base) collects nothing, since it has
# nothing to share a legend with and its insets are placed against its own
# box, legend included. Returns the composition with the collected cells
# marked, the pooled guides (none under `legend.position = "none"` on the
# composition, which drops them, as in the aligned path), and whether any
# collected cell is interactive (its legend swatches are then keyed).
.comp_collect_independent <- function(comp, rt0) {
  out <- list(comp = comp, figguides = list(), interactive = FALSE)
  if (!identical(comp@guides, "collect") || length(comp@plots) < 2L) {
    return(out)
  }
  pooled <- list()
  seen <- character(0)
  plots <- comp@plots
  for (i in seq_along(plots)) {
    p <- plots[[i]]
    if (!S7::S7_inherits(p, PlotSpec) || !length(p@layers)) {
      next
    }
    plan <- .plan_plot(p)
    outside <- unlist(
      plan$lay$legend_groups[.LEGEND_SIDES],
      recursive = FALSE
    )
    if (!length(outside)) {
      next
    }
    keys <- vapply(outside, .guide_key, "")
    for (k in seq_along(outside)) {
      if (!keys[k] %in% seen) {
        seen <- c(seen, keys[k])
        pooled <- c(pooled, outside[k])
      }
    }
    attr(plots[[i]], "vp_collected_keys") <- keys
    out$interactive <- out$interactive || .spec_interactive(p)
  }
  if (!length(pooled)) {
    return(out)
  }
  out$comp@plots <- plots
  if (!identical(.fig_legend_side(rt0), "none")) {
    out$figguides <- pooled
  }
  out
}

# A cell's own plot margin on the side of a figure band (`side`: "top" or
# "bottom") is dropped: the band's own margin row already separates the cell
# from the page edge, so keeping both left twice the gap the aligned layout
# has between a title and the panels (VP-152 review). Only cells in the first
# (top) / last (bottom) row of the grid. A nested composition there is marked
# (attribute `vp_band_trim`) to drop its own outer margin on that side when
# drawn (`.comp_apply_band_trim()`, VP-168).
.comp_trim_band_margins <- function(comp, top, bottom) {
  if (!top && !bottom) {
    return(comp)
  }
  plots <- comp@plots
  for (i in seq_along(plots)) {
    p <- plots[[i]]
    nested <- S7::S7_inherits(p, PlotComposition)
    if (!S7::S7_inherits(p, PlotSpec) && !nested) {
      next
    }
    if (!is.null(comp@design)) {
      r0 <- comp@design[[i]]$t
      r1 <- comp@design[[i]]$b
    } else {
      r0 <- r1 <- .comp_cell(i, comp)$r
    }
    if (nested) {
      edge <- c(top = top && r0 == 1, bottom = bottom && r1 == comp@nrow)
      if (any(edge)) {
        old <- attr(p, "vp_band_trim") %||% c(top = FALSE, bottom = FALSE)
        attr(p, "vp_band_trim") <- edge | old
        plots[[i]] <- p
      }
      next
    }
    m <- rep_len(
      .resolve_theme(.theme_of(p))[["plot.margin"]] %||% 0,
      4L
    )
    m0 <- m
    if (top && r0 == 1) {
      m[1] <- 0
    }
    if (bottom && r1 == comp@nrow) {
      m[3] <- 0
    }
    if (!identical(m, m0)) {
      plots[[i]] <- theme(p, plot.margin = m)
    }
  }
  comp@plots <- plots
  comp
}

# A nested composition's outer margin (t, r, b, l mm) less the sides its
# parent marked as next to a figure band (`.comp_trim_band_margins()`).
.comp_apply_band_trim <- function(m, comp) {
  trim <- attr(comp, "vp_band_trim")
  if (isTRUE(trim[["top"]])) {
    m[1] <- 0
  }
  if (isTRUE(trim[["bottom"]])) {
    m[3] <- 0
  }
  m
}

# The cell grid of an independently laid-out composition, drawn into the
# current viewport (`box_mm` its size in mm, when known). `rt0` is the figure
# theme, resolved before `.comp_collect_independent()` themed any cell.
# `figure_insets = FALSE` leaves the insets placed in the whole figure to the
# caller.
.comp_independent_cells <- function(
  scene,
  comp,
  box_mm = NULL,
  rt0,
  figure_insets = TRUE
) {
  ncol <- comp@ncol
  nrow <- comp@nrow
  wfun <- function(v, k) {
    if (is.null(v)) {
      vellum::vl_unit(rep(1, k), "null")
    } else {
      vellum::vl_unit(rep_len(as.numeric(v), k), "null")
    }
  }
  # Each cell's grid area: its top row, left column and spans.
  area <- function(i) {
    if (!is.null(comp@design)) {
      a <- comp@design[[i]]
      list(r = a$t, c = a$l, rs = a$b - a$t + 1L, cs = a$r - a$l + 1L)
    } else {
      pos <- .comp_cell(i, comp)
      list(r = pos$r, c = pos$c, rs = 1L, cs = 1L)
    }
  }
  # The composition's auto-tags (`tag_levels`), one per drawn cell (spacers get
  # none), take a band above each row of cells as tall as the row's tallest
  # tag, as in the aligned path (VP-142, VP-145). Before, this layout drew no
  # tags at all.
  drawn <- which(!vapply(comp@plots, S7::S7_inherits, NA, Spacer))
  ctags <- .comp_tags(comp, length(drawn), rt0)
  heights <- wfun(comp@heights, nrow)
  # grid row of each composition row's cells (doubles, as `.comp_cell()` gives)
  row_of <- as.double(seq_len(nrow))
  if (!is.null(ctags)) {
    band <- lapply(seq_len(nrow), function(r) {
      hit <- vapply(drawn, function(i) area(i)$r == r, NA)
      .umax(lapply(ctags[hit], function(tg) {
        .track_h_band(rt0[["plot.tag"]], tg, .PAD_MM, NA_real_)
      }))
    })
    hw <- rep_len(as.numeric(comp@heights %||% 1), nrow)
    heights <- do.call(
      c,
      unlist(
        lapply(seq_len(nrow), function(r) {
          list(band[[r]], vellum::vl_unit(hw[r], "null"))
        }),
        recursive = FALSE
      )
    )
    row_of <- 2 * seq_len(nrow)
  }
  scene <- vellum::push(
    scene,
    vellum::vl_viewport(
      layout = vellum::grid_layout(wfun(comp@widths, ncol), heights)
    )
  )
  # A cell's size (inches), when the figure's is known (`box_mm`): its share of
  # the null tracks once the tag bands are taken. NULL otherwise.
  ww <- rep_len(as.numeric(comp@widths %||% 1), ncol)
  hh <- rep_len(as.numeric(comp@heights %||% 1), nrow)
  band_mm <- if (!is.null(ctags)) {
    sum(vapply(band, function(u) vellum::vl_convert(u, "mm"), numeric(1)))
  } else {
    0
  }
  cell_in <- function(a) {
    if (is.null(box_mm) || !all(is.finite(box_mm))) {
      return(NULL)
    }
    cs <- seq(a$c, length.out = a$cs)
    rs <- seq(a$r, length.out = a$rs)
    c(
      box_mm[1] * sum(ww[cs]) / sum(ww),
      (box_mm[2] - band_mm) * sum(hh[rs]) / sum(hh)
    ) /
      25.4
  }
  # Insets aligned to the base plot's panel area or plot area are drawn by
  # `.draw_plot()` inside the plot's own layout grid (VP-127); that needs a
  # single base plot. The rest are placed in the whole figure.
  aligned <- .comp_insets_in_plot(comp)
  # under figure bands, the figure-wide insets are drawn by the caller
  fig_ins <- if (figure_insets) which(!aligned) else integer(0)
  # Insets with `on_top = FALSE` sit behind the base plots (visible only through
  # gaps); the rest overlay after the cells are drawn.
  scene <- .draw_insets(
    scene,
    comp,
    on_top = FALSE,
    box_mm = box_mm,
    which = fig_ins
  )
  for (i in seq_along(comp@plots)) {
    p <- comp@plots[[i]]
    if (S7::S7_inherits(p, Spacer)) {
      next
    }
    a <- area(i)
    # with a tag band, rows interleave (band, cells): a cell spanning `rs`
    # composition rows covers the bands between them too
    top <- row_of[a$r]
    span <- row_of[a$r + a$rs - 1L] - top + 1L
    if (!is.null(ctags)) {
      scene <- .draw_tag(
        scene,
        top - 1L,
        a$cs,
        ctags[match(i, drawn)],
        rt0,
        col = a$c
      )
    }
    scene <- vellum::push(
      scene,
      vellum::vl_viewport(row = top, col = a$c, rowspan = span, colspan = a$cs)
    )
    if (S7::S7_inherits(p, PlotComposition)) {
      # A nested composition is only drawn by the aligned path when it is itself
      # alignable; otherwise it needs the independent layout too (the aligned
      # path assumes single-panel, non-faceted sub-plots and would silently drop
      # extra panels/strips).
      if (.comp_alignable(p)) {
        scene <- .draw_composition(scene, p)
      } else {
        scene <- .compile_composition_independent(scene, p)
      }
    } else {
      if (S7::S7_inherits(p, PlotSpec)) {
        .comp_cell_next()
      }
      plot_insets <- if (any(aligned)) {
        list(
          insets = comp@insets,
          which = which(aligned),
          box_mm = box_mm
        )
      }
      scene <- .draw_plot(scene, p, insets = plot_insets, page = cell_in(a))
    }
    scene <- vellum::pop(scene)
  }
  scene <- .draw_insets(
    scene,
    comp,
    on_top = TRUE,
    box_mm = box_mm,
    which = fig_ins
  )
  vellum::pop(scene)
}

# Draw inset plots positioned by fractional coordinates of the current viewport.
# `on_top` selects which insets to draw: those layered above the base plots
# (`TRUE`, the default) or behind them (`FALSE`); `which` the indices of
# `comp@insets` to consider (all by default). `box_mm` is the viewport's
# physical size (width, height) in mm, when known.
.draw_insets <- function(
  scene,
  comp,
  on_top = TRUE,
  box_mm = NULL,
  which = seq_along(comp@insets)
) {
  for (k in which) {
    ins <- comp@insets[[k]]
    if (!identical(isTRUE(ins$on_top), on_top)) {
      next
    }
    scene <- .draw_inset(scene, ins, k, box_mm)
  }
  scene
}

# Draw one inset (the k-th of its composition) at its fractional position in
# the current viewport, whose physical size is `box_mm` (NULL when unknown).
.draw_inset <- function(scene, ins, k, box_mm = NULL) {
  left <- ins$left
  right <- ins$right
  bottom <- ins$bottom
  top <- ins$top
  # A fixed-aspect inset shrinks to its own shape inside the box, keeping the
  # side nearest the edge it sits on (VP-120). Needs the box's physical size.
  if (!is.null(box_mm) && all(is.finite(box_mm))) {
    cf <- .plot_content_fit(ins$plot)
    fit <- if (!is.null(cf)) {
      .fit_box(cf, (right - left) * box_mm[1], (top - bottom) * box_mm[2])
    }
    if (!is.null(fit)) {
      fw <- fit[["w"]] / box_mm[1]
      fh <- fit[["h"]] / box_mm[2]
      if ((left + right) / 2 >= 0.5) {
        left <- right - fw
      } else {
        right <- left + fw
      }
      if ((bottom + top) / 2 >= 0.5) bottom <- top - fh else top <- bottom + fh
    }
  }
  w <- right - left
  h <- top - bottom
  scene <- vellum::push(
    scene,
    vellum::vl_viewport(
      x = vellum::vl_unit(left + w / 2, "npc"),
      y = vellum::vl_unit(bottom + h / 2, "npc"),
      width = vellum::vl_unit(w, "npc"),
      height = vellum::vl_unit(h, "npc"),
      shadow = ins$shadow,
      # named, so the inset can be found (node_names()) and edited by name
      name = paste0("inset-", k)
    )
  )
  scene <- .draw_plot(scene, ins$plot)
  vellum::pop(scene)
}

# Grid cell (1-based row/col) of the i-th sub-plot, honouring byrow.
.comp_cell <- function(i, comp) {
  if (comp@byrow) {
    list(r = (i - 1L) %/% comp@ncol + 1L, c = (i - 1L) %% comp@ncol + 1L)
  } else {
    list(r = (i - 1L) %% comp@nrow + 1L, c = (i - 1L) %/% comp@nrow + 1L)
  }
}

S7::method(.as_vellum_scene, PlotComposition) <- function(x, ...) {
  scene <- .compile_composition(x)
  # Cells are drawn through `.draw_plot()`, which leaves its trained-scale
  # summary on the scene it returns -- so without this a composition would carry
  # the *last* cell's scales and the grammar lint rules would report on that one
  # cell as though it were the whole figure. There is no single set of scales for
  # a composition, so it carries none. See R/lint.R and vellumplot#147.
  attr(scene, "vellumplot_lint_scales") <- NULL
  scene
}

# Printing a composition draws it (like a plot); summary() reports its shape.
S7::method(print, PlotComposition) <- function(x, ...) {
  vellum::display(vellum::as_vellum_scene(x))
  invisible(x)
}

S7::method(plot, PlotComposition) <- function(x, y, ...) {
  vellum::display(vellum::as_vellum_scene(x))
  invisible(x)
}

S7::method(summary, PlotComposition) <- function(object, ...) {
  nested <- sum(vapply(
    object@plots,
    function(p) S7::S7_inherits(p, PlotComposition),
    logical(1)
  ))
  cli::cli_text(
    "{.cls PlotComposition} {length(object@plots)} plot{?s} in a {object@nrow}x{object@ncol} grid"
  )
  cli::cli_text(
    "guides: {object@guides}{if (nested) paste0(', ', nested, ' nested')}"
  )
  if (length(object@labels) || !is.null(object@tag)) {
    bits <- c(
      if (!is.null(object@labels$title)) "title",
      if (!is.null(object@tag)) paste0("tags (", object@tag$levels[1], ")")
    )
    cli::cli_text("annotation: {paste(bits, collapse = ', ')}")
  }
  invisible(object)
}
