#' @include classes.R print.R
NULL

# ---------------------------------------------------------------------------
# Alt text: a screen-reader-usable prose description of a plot (WCAG 1.1.1).
#
# vellumplot is the *author* of the ecosystem's accessibility story: it turns the
# inspectable PlotSpec into an accessible name (the plot title) and a text
# alternative (this alt text), then hands both to vellum's scene title/desc at
# the compile seam (see .compile_plot / .compile_composition). vellum emits them
# as an accessible SVG (role="img" + <title>/<desc>) and a tagged PDF Figure.
#
# The alt text is either authored by the user (labs(alt=)) or generated here
# from the spec. Generation is spec-only (no compile / no trained scales) so it
# is cheap and cannot recurse through the compile seam that calls it.
# ---------------------------------------------------------------------------

# Human-readable chart-type phrase per mark. Unknown marks fall back to
# "<mark> plot" so a new mark never produces a broken sentence.
.MARK_PHRASE <- c(
  point = "scatter plot",
  jitter = "jittered scatter plot",
  line = "line chart",
  step = "step chart",
  area = "area chart",
  ribbon = "ribbon chart",
  bar = "bar chart",
  col = "bar chart",
  histogram = "histogram",
  freqpoly = "frequency polygon",
  boxplot = "box plot",
  violin = "violin plot",
  tile = "heatmap",
  raster = "heatmap",
  rect = "rectangle plot",
  hex = "hex-bin plot",
  bin2d = "binned heatmap",
  smooth = "smoothed-trend plot",
  density = "density plot",
  contour = "contour plot",
  rug = "rug plot",
  rule = "reference-line plot",
  segment = "segment plot",
  spoke = "spoke plot",
  errorbar = "error-bar plot",
  linerange = "line-range plot",
  pointrange = "point-range plot",
  crossbar = "crossbar plot",
  text = "text-label plot",
  label = "text-label plot",
  sf = "map",
  edges = "network graph",
  nodes = "network graph",
  node_text = "node labels"
)

.mark_phrase <- function(mark) {
  if (mark %in% names(.MARK_PHRASE)) {
    .MARK_PHRASE[[mark]]
  } else {
    paste0(mark, " plot")
  }
}

# "a", "a and b", "a, b, and c".
.oxford <- function(x) {
  n <- length(x)
  if (n <= 1L) {
    return(paste0(x, collapse = ""))
  }
  if (n == 2L) {
    return(paste(x, collapse = " and "))
  }
  paste0(paste(x[-n], collapse = ", "), ", and ", x[[n]])
}

# The channel-expression label for the first layer that maps `aes`, or NULL.
# color/colour/fill are aliases for the colour channel.
.alt_channel <- function(spec, aes) {
  keys <- if (aes == "color") c("color", "colour", "fill") else aes
  for (L in spec@layers) {
    for (k in keys) {
      ch <- L@encoding[[k]]
      if (!is.null(ch)) {
        return(.channel_label(ch))
      }
    }
  }
  NULL
}

# The human title for an aesthetic: a labs() override wins, else the mapped
# expression. Only plain-string overrides are used (a rich md() title has no
# reliable plain-text form for a description).
.alt_axis_title <- function(spec, aes) {
  # Mirror the compiler's title resolution (scale name -> labs -> mapped
  # expression) so the description matches the rendered axis/legend title. A
  # `scale_*(name = )` lives on the spec, so no compile is needed. Preserve the
  # NULL-when-unmapped contract the sentence logic below relies on.
  sc <- .scale_for(spec, aes)
  if (
    !is.null(sc) &&
      !is.null(sc@name) &&
      is.character(sc@name) &&
      nzchar(sc@name)
  ) {
    return(sc@name)
  }
  lab <- spec@labels[[aes]]
  if (!is.null(lab) && is.character(lab) && nzchar(lab)) {
    return(lab)
  }
  .alt_channel(spec, aes)
}

# The faceting variables, as a readable "a and b" string, or NULL.
.alt_facet_vars <- function(f) {
  lab <- function(qs) {
    if (is.null(qs) || !length(qs)) {
      return(NULL)
    }
    paste(
      vapply(
        qs,
        function(q) rlang::as_label(rlang::quo_get_expr(q)),
        character(1)
      ),
      collapse = " and "
    )
  }
  if (identical(f@type, "wrap")) {
    lab(f@cols)
  } else {
    paste(c(lab(f@rows), lab(f@cols)), collapse = " and ")
  }
}

# The "Faceted by ..." sentence, or NULL when the plot is not faceted.
.alt_facet_sentence <- function(spec) {
  if (is.null(spec@facet)) {
    return(NULL)
  }
  vars <- tryCatch(.alt_facet_vars(spec@facet), error = function(e) NULL)
  if (!is.null(vars) && nzchar(vars)) paste0("Faceted by ", vars, ".") else NULL
}

# Build the automatic alt text from a PlotSpec: chart type, what is mapped to
# each axis / colour / size, the observation count, and any faceting.
.plot_alt_auto <- function(spec) {
  marks <- vapply(spec@layers, function(L) L@mark, character(1))
  if (!length(marks)) {
    return("An empty plot with no layers.")
  }
  phrases <- unique(vapply(marks, .mark_phrase, character(1)))
  s1 <- if (length(phrases) == 1L) {
    paste0("A ", phrases, ".")
  } else {
    paste0("A plot combining ", .oxford(phrases), ".")
  }

  col <- .alt_axis_title(spec, "color")
  sz <- .alt_axis_title(spec, "size")
  extras <- c(
    if (!is.null(col)) paste0("colour shows ", col),
    if (!is.null(sz)) paste0("size shows ", sz)
  )

  # A graph (from vgraph()) maps layout coordinates to x/y, so the "plots y
  # against x" phrasing is noise; report node/edge counts instead. Colour/size
  # encodings on a graph are still meaningful, so keep them.
  is_graph <- !is.null(spec@edge_data)
  if (is_graph) {
    nn <- tryCatch(nrow(spec@data), error = function(e) NULL)
    ne <- tryCatch(nrow(spec@edge_data), error = function(e) NULL)
    counts <- c(
      if (!is.null(nn)) paste0(nn, " node", if (nn != 1L) "s"),
      if (!is.null(ne)) paste0(ne, " edge", if (ne != 1L) "s")
    )
    s2 <- if (length(counts)) {
      body <- paste0("It has ", paste(counts, collapse = " and "))
      if (length(extras)) {
        body <- paste0(body, ", where ", paste(extras, collapse = " and "))
      }
      paste0(body, ".")
    }
    # Node count already stated ("N nodes"); skip the generic observation count.
    return(paste(
      c(s1, s2, if (!is.null(spec@facet)) .alt_facet_sentence(spec)),
      collapse = " "
    ))
  }

  x <- .alt_axis_title(spec, "x")
  y <- .alt_axis_title(spec, "y")
  # An unmapped y on a bar/histogram plot renders as a "count" axis (mirrors the
  # compiler's .y_axis_title); report it so the description names both axes.
  if (is.null(y)) {
    y_mapped <- any(vapply(
      spec@layers,
      function(L) "y" %in% names(L@encoding),
      logical(1)
    ))
    if (!y_mapped && any(marks %in% c("bar", "histogram"))) {
      y <- "count"
    }
  }
  s2 <- NULL
  axis <- if (!is.null(x) && !is.null(y)) {
    paste0("It plots ", y, " (vertical axis) against ", x, " (horizontal axis)")
  } else if (!is.null(x)) {
    paste0("It shows ", x, " on the horizontal axis")
  } else if (!is.null(y)) {
    paste0("It shows ", y, " on the vertical axis")
  }
  if (!is.null(axis)) {
    if (length(extras)) {
      axis <- paste0(axis, ", where ", paste(extras, collapse = " and "))
    }
    s2 <- paste0(axis, ".")
  }

  n <- tryCatch(nrow(spec@data), error = function(e) NULL)
  s3 <- if (!is.null(n) && !is.na(n) && n > 0) {
    paste0("Based on ", n, " observation", if (n != 1L) "s", ".")
  }

  s4 <- .alt_facet_sentence(spec)

  paste(c(s1, s2, s3, s4), collapse = " ")
}

#' Text alternative (alt text) for a plot
#'
#' Returns the description used as the plot's text alternative for assistive
#' technology. If [labs()] set an explicit `alt`, that string is returned
#' verbatim; otherwise vellumplot generates a prose summary from the specification —
#' the chart type, what each axis / colour / size encodes, the number of
#' observations, and any faceting.
#'
#' [render_plot()] (and any compile through [vellum::as_vellum_scene()]) passes
#' this text to the scene's description, so the exported **SVG** carries it in
#' `<desc>` (with `role="img"`) and the exported **PDF** tags the chart as a
#' `Figure` whose `Alt` is this text. The plot **title** (from `labs(title=)`)
#' becomes the accessible name. See the *Accessibility* article.
#'
#' @param x A [PlotSpec] or a plot composition.
#' @return A single string.
#' @seealso [labs()], [vellum::describe()]
#' @examples
#' p <- vplot(mtcars) |> mark_point(x = wt, y = mpg, color = hp)
#' plot_alt(p)
#' plot_alt(labs(p, alt = "Heavier cars get fewer miles per gallon."))
#' @export
plot_alt <- function(x) {
  if (S7::S7_inherits(x, PlotSpec)) {
    manual <- x@labels$alt
    if (
      !is.null(manual) &&
        is.character(manual) &&
        length(manual) == 1L &&
        nzchar(manual)
    ) {
      return(manual)
    }
    return(.plot_alt_auto(x))
  }
  if (S7::S7_inherits(x, PlotComposition)) {
    manual <- x@labels$alt
    if (
      !is.null(manual) &&
        is.character(manual) &&
        length(manual) == 1L &&
        nzchar(manual)
    ) {
      return(manual)
    }
    n <- length(x@plots)
    return(paste0(
      "A composition of ",
      n,
      " plot",
      if (n != 1L) "s",
      " arranged in a ",
      x@nrow,
      " by ",
      x@ncol,
      " grid."
    ))
  }
  cli::cli_abort("{.arg x} must be a {.cls PlotSpec} or plot composition.")
}

# The accessible name (scene title): a plain-string labs() title, else NULL.
# A rich md() title has no reliable plain-text form and is skipped.
.alt_name <- function(labels) {
  t <- labels$title
  if (!is.null(t) && is.character(t) && nzchar(t)) t else NULL
}

# Best-effort alt text for the compile seam: never lets a description-generation
# problem break a render (returns NULL, i.e. no accessibility markup, on error).
.alt_desc_safe <- function(x) {
  tryCatch(plot_alt(x), error = function(e) NULL)
}
