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

.collect_plots <- function(dots) {
  if (!length(dots)) {
    cli::cli_abort("Provide at least one plot to arrange.")
  }
  for (i in seq_along(dots)) {
    p <- dots[[i]]
    if (
      !S7::S7_inherits(p, PlotSpec) &&
        !S7::S7_inherits(p, PlotComposition) &&
        !S7::S7_inherits(p, Spacer)
    ) {
      cli::cli_abort(c(
        "Composition expects {.cls PlotSpec} or {.cls PlotComposition} objects.",
        "x" = "Argument {i} is {.obj_type_friendly {p}}."
      ))
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
#'   vector), or `NULL` for equal panels.
#' @param guides `"collect"` (default) to dedupe identical legends across
#'   sub-plots into one, or `"keep"` to leave each sub-plot's legend in place.
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
  plots <- .collect_plots(list(...))
  guides <- match.arg(guides)
  if (!is.null(dpi)) {
    .check_dpi(dpi)
  }
  n <- length(plots)
  design <- .parse_design(design, n)
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
    widths = widths,
    heights = heights,
    guides = guides,
    design = design,
    width = width %||% (ncol * w0),
    height = height %||% (nrow * h0),
    dpi = as.double(dpi %||% .comp_dpi(ref))
  )
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
.parse_design <- function(design, n) {
  if (is.null(design)) {
    return(NULL)
  }
  if (is.list(design)) {
    if (length(design) != n) {
      cli::cli_abort(c(
        "A {.arg design} list must have one {.fn area} per plot.",
        i = "Got {length(design)} area{?s} for {n} plot{?s}."
      ))
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
      cli::cli_abort(c(
        "Each element of a {.arg design} list must be an {.fn area}.",
        i = "Build them with {.fn area}, e.g. {.code area(1, 1, 2, 2)}."
      ))
    }
    return(design)
  }
  if (is.character(design)) {
    rows <- trimws(strsplit(trimws(design), "\n")[[1]])
    widths <- nchar(rows)
    if (length(unique(widths)) != 1L) {
      cli::cli_abort(c(
        "Each row of a {.arg design} layout string must be the same width.",
        i = "Got rows of differing widths: {.val {widths}}."
      ))
    }
    m <- do.call(rbind, lapply(rows, function(s) strsplit(s, "")[[1]]))
    # Distinct letters bind to the plots in `...` in alphabetical order; "#" is
    # an empty cell.
    letters_used <- sort(setdiff(unique(as.vector(m)), "#"))
    if (length(letters_used) != n) {
      cli::cli_abort(c(
        "A {.arg design} layout must name exactly one area per plot.",
        i = "Got {length(letters_used)} area{?s} ({.val {letters_used}}) for {n} plot{?s}.",
        i = "Plots fill the lettered areas in alphabetical order."
      ))
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
        cli::cli_abort(c(
          "Area {.val {ch}} in the {.arg design} layout must be a solid rectangle.",
          i = "Its cells don't fill their {bb - tt + 1L} x {rr - ll + 1L} bounding box."
        ))
      }
      area(tt, ll, bb, rr)
    })
    return(out)
  }
  cli::cli_abort(
    "{.arg design} must be a list of {.fn area}s or a layout string."
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
#' @param align_to Reference box: `"panel"`, `"plot"`, or `"full"` (currently the
#'   whole `base` cell).
#' @param on_top Draw the inset above (`TRUE`) or below the base.
#' @return A `PlotComposition`.
#' @examples
#' a <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
#' b <- vplot(mtcars) |> mark_histogram(x = mpg, bins = 8)
#' inset(a, b, left = 0.55, bottom = 0.55, right = 0.98, top = 0.98)
#' @export
inset <- function(
  base,
  plot,
  left = 0.6,
  bottom = 0.6,
  right = 0.98,
  top = 0.98,
  align_to = c("panel", "plot", "full"),
  on_top = TRUE
) {
  align_to <- match.arg(align_to)
  if (!S7::S7_inherits(plot, PlotSpec)) {
    cli::cli_abort("{.arg plot} (the inset) must be a {.cls PlotSpec}.")
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
      on_top = on_top
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
  # is spliced as a positional plot, not bound to that argument.
  do.call(
    concat,
    c(
      unname(plots),
      list(
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
    )
  )
}

#' @rdname concat
#' @export
hconcat <- function(..., guides = c("collect", "keep"), height = NULL) {
  concat(
    ...,
    ncol = ...length(),
    nrow = 1,
    guides = match.arg(guides),
    height = height
  )
}

#' @rdname concat
#' @export
vconcat <- function(..., guides = c("collect", "keep"), width = NULL) {
  concat(
    ...,
    ncol = 1,
    nrow = ...length(),
    guides = match.arg(guides),
    width = width
  )
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
.compile_composition <- function(comp) {
  .provenance_reset()
  scene <- vellum::vl_scene(
    width = comp@width,
    height = comp@height,
    dpi = comp@dpi,
    bg = "white",
    # Accessibility: the figure title (if any) names the composition and the
    # generated summary describes it. Additive; see R/alt.R.
    title = .alt_name(comp@labels),
    desc = .alt_desc_safe(comp)
  )
  if (.comp_alignable(comp)) {
    scene <- vellum::push(scene, vellum::vl_viewport())
    scene <- .draw_composition(scene, comp)
    scene <- vellum::pop(scene)
  } else {
    scene <- .compile_composition_independent(scene, comp)
  }
  # Provenance accumulates across every sub-plot's marks; carry it out (DESIGN §4).
  attr(scene, "vellumplot_provenance") <- .provenance_snapshot()
  scene
}

# Legacy layout: an outer grid of equal null cells, each holding one sub-plot
# rendered with .draw_plot() (its own grid_layout inside the cell). No alignment
# or guide collection. Used as a fallback for sub-plots the aligned path can't
# yet place (facets, polar coords).
.compile_composition_independent <- function(scene, comp) {
  ncol <- comp@ncol
  nrow <- comp@nrow
  wfun <- function(v, k) {
    if (is.null(v)) {
      vellum::vl_unit(rep(1, k), "null")
    } else {
      vellum::vl_unit(rep_len(as.numeric(v), k), "null")
    }
  }
  scene <- vellum::push(
    scene,
    vellum::vl_viewport(
      layout = vellum::grid_layout(
        wfun(comp@widths, ncol),
        wfun(comp@heights, nrow)
      )
    )
  )
  # Insets with `on_top = FALSE` sit behind the base plots (visible only through
  # gaps); the rest overlay after the cells are drawn.
  scene <- .draw_insets(scene, comp, on_top = FALSE)
  for (i in seq_along(comp@plots)) {
    p <- comp@plots[[i]]
    if (S7::S7_inherits(p, Spacer)) {
      next
    }
    vp <- if (!is.null(comp@design)) {
      a <- comp@design[[i]]
      vellum::vl_viewport(
        row = a$t,
        col = a$l,
        rowspan = a$b - a$t + 1,
        colspan = a$r - a$l + 1
      )
    } else {
      pos <- .comp_cell(i, comp)
      vellum::vl_viewport(row = pos$r, col = pos$c)
    }
    scene <- vellum::push(scene, vp)
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
      scene <- .draw_plot(scene, p)
    }
    scene <- vellum::pop(scene)
  }
  scene <- .draw_insets(scene, comp, on_top = TRUE)
  vellum::pop(scene)
}

# Draw inset plots positioned by fractional coordinates of the current viewport.
# `on_top` selects which insets to draw: those layered above the base plots
# (`TRUE`, the default) or behind them (`FALSE`).
.draw_insets <- function(scene, comp, on_top = TRUE) {
  for (ins in comp@insets) {
    if (!identical(isTRUE(ins$on_top), on_top)) {
      next
    }
    w <- ins$right - ins$left
    h <- ins$top - ins$bottom
    scene <- vellum::push(
      scene,
      vellum::vl_viewport(
        x = vellum::vl_unit(ins$left + w / 2, "npc"),
        y = vellum::vl_unit(ins$bottom + h / 2, "npc"),
        width = vellum::vl_unit(w, "npc"),
        height = vellum::vl_unit(h, "npc")
      )
    )
    scene <- .draw_plot(scene, ins$plot)
    scene <- vellum::pop(scene)
  }
  scene
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
