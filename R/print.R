#' @include classes.R
NULL

# A short text label for a channel's expression, e.g. `wt` or `factor(cyl)`.
.channel_label <- function(ch) .pronoun_label(rlang::quo_get_expr(ch@expr))

# A mapping's label, without a leading `.data$` / `.env$` pronoun: `.env$ink`
# (an R value mapped on purpose, VP-126) titles its legend "ink".
.pronoun_label <- function(e) {
  if (
    is.call(e) &&
      length(e) == 3L &&
      (identical(e[[1]], as.name("$")) || identical(e[[1]], as.name("[["))) &&
      is.symbol(e[[2]]) &&
      rlang::as_string(e[[2]]) %in% c(".data", ".env")
  ) {
    nm <- e[[3]]
    if (is.symbol(nm)) {
      return(rlang::as_string(nm))
    }
    if (is.character(nm) && length(nm) == 1L) {
      return(nm)
    }
  }
  rlang::as_label(e)
}

# A printable form of a title/label value. Plain strings print verbatim; a rich
# md() / tex() label prints its text (see `.label_text()`), so `paste0()` does
# not mangle it in summary output.
.lab_str <- function(x) {
  if (is.character(x) || .is_rich(x)) .label_text(x) else "<rich text>"
}

# The facet variable expressions (a list of quosures) as one string joined by
# `sep`, or NULL for an empty/absent set. Shared by the summary tree (" + ") and
# the alt-text builder (" and ").
.facet_var_labels <- function(qs, sep) {
  if (is.null(qs) || !length(qs)) {
    return(NULL)
  }
  paste(
    vapply(
      qs,
      function(q) rlang::as_label(rlang::quo_get_expr(q)),
      character(1)
    ),
    collapse = sep
  )
}

# One line per layer: `mark_point(x = wt, y = mpg, color = hp)  [size = 3]`.
.format_layer <- function(layer) {
  enc <- vapply(
    names(layer@encoding),
    function(nm) paste0(nm, " = ", .channel_label(layer@encoding[[nm]])),
    character(1)
  )
  par <- vapply(
    names(layer@params),
    function(nm) {
      paste0(nm, " = ", paste(format(layer@params[[nm]]), collapse = ", "))
    },
    character(1)
  )
  out <- paste0("mark_", layer@mark, "(", paste(enc, collapse = ", "), ")")
  if (length(par)) {
    out <- paste0(out, "  [", paste(par, collapse = ", "), "]")
  }
  if (!is.null(layer@data)) {
    out <- paste0(out, "  {own data}")
  }
  out
}

.format_scale <- function(s) {
  bits <- paste0(s@aesthetic, ": ", s@type)
  if (!is.null(s@trans) && !identical(s@trans, "identity")) {
    tr <- if (is.character(s@trans)) s@trans else "custom"
    bits <- paste0(bits, "/", tr)
  }
  if (!is.null(s@domain)) {
    bits <- paste0(
      bits,
      " domain[",
      paste(format(s@domain), collapse = ", "),
      "]"
    )
  }
  if (!is.null(s@range)) {
    bits <- paste0(
      bits,
      " range[",
      paste(format(s@range), collapse = ", "),
      "]"
    )
  }
  if (!is.null(s@name)) {
    bits <- paste0(bits, " name=", .lab_str(s@name))
  }
  bits
}

# Print a readable tree: data dims, page size, layers, declared scales.
.print_plotspec <- function(x, ...) {
  d <- x@data
  # A spec can carry no data frame (e.g. a graph spec built differently);
  # report 0x0 rather than let nrow(NULL)/ncol(NULL) mangle the summary line.
  nr <- nrow(d) %||% 0L
  nc <- ncol(d) %||% 0L
  cli::cli_text(
    "{.cls PlotSpec} {.field {nr}}x{.field {nc}} ({nc} column{?s}), page {x@width}x{x@height} in"
  )
  if (length(x@layers)) {
    cli::cli_h3("layers")
    cli::cli_ul(vapply(x@layers, .format_layer, character(1)))
  } else {
    cli::cli_text("{.emph no layers}")
  }
  if (length(x@scales)) {
    cli::cli_h3("scales")
    cli::cli_ul(vapply(x@scales, .format_scale, character(1)))
  }
  if (length(x@labels)) {
    cli::cli_h3("labels")
    cli::cli_ul(vapply(
      names(x@labels),
      function(nm) paste0(nm, ": ", .lab_str(x@labels[[nm]])),
      character(1)
    ))
  }
  if (!is.null(x@facet)) {
    f <- x@facet
    lab <- function(qs) .facet_var_labels(qs, " + ")
    spec <- if (f@type == "wrap") {
      paste0("wrap(", lab(f@cols), ")")
    } else {
      paste0("grid(", lab(f@rows), " ~ ", lab(f@cols), ")")
    }
    cli::cli_text("facet: {spec}")
  }
  if (!is.null(x@coord)) {
    co <- x@coord
    lims <- c(
      if (!is.null(co@xlim)) {
        paste0("xlim[", paste(co@xlim, collapse = ", "), "]")
      },
      if (!is.null(co@ylim)) {
        paste0("ylim[", paste(co@ylim, collapse = ", "), "]")
      }
    )
    extra <- if (identical(co@kind, "polar")) {
      c(
        paste0("theta=", co@theta),
        paste0("start=", co@start),
        paste0("dir=", co@direction)
      )
    }
    cli::cli_text("coord: {paste(c(co@kind, extra, lims), collapse = ' ')}")
  }
  invisible(x)
}

# Printing a plot draws it into the active graphics device (the RStudio /
# Positron Plots pane, or a knitr/Quarto chunk), like ggplot2 — via vellum's
# display(), which compiles through the as_vellum_scene() seam and no-ops when
# there is no device (so scripts / R CMD check draw nothing). `summary()` shows
# the inspectable spec tree instead. (Methods registered at load by
# S7::methods_register().)
S7::method(print, PlotSpec) <- function(x, ...) {
  vellum::display(vellum::as_vellum_scene(x))
  invisible(x)
}

S7::method(plot, PlotSpec) <- function(x, y, ...) {
  vellum::display(vellum::as_vellum_scene(x))
  invisible(x)
}

S7::method(summary, PlotSpec) <- function(object, ...) {
  .print_plotspec(object, ...)
}

# Adaptive knit output. In an HTML target emit a crisp inline SVG (vector,
# selectable text, resolution-independent) instead of a rasterised device
# figure; other targets (LaTeX / Word) fall through to the default device render
# via `print()`, which knitr captures as a figure. Registered at load (see
# `.register_knit_print()`) so it activates only when knitr is present and a
# document is knitting -- interactive `print()` is untouched.
.knit_print_plot <- function(x, ...) {
  if (
    requireNamespace("knitr", quietly = TRUE) && isTRUE(knitr::is_html_output())
  ) {
    return(knitr::asis_output(.knit_svg(x)))
  }
  print(x)
  invisible(x)
}

# The inline SVG a plot knits to in HTML. By default it fills the column
# (`scaling = "fit"`), at the plot's own aspect. A chunk that sets
# `fig-width` / `fig-height` (a value other than the document's default) gives
# a plot without an explicit size (`.has_default_size()`) that page size, and
# caps the figure at that width (CSS `max-width`): it still shrinks to a
# narrower column, but a small figure stays small instead of being stretched
# to the column. A plot with an explicit size keeps it and fills the column as
# before (VP-165).
.knit_svg <- function(x) {
  fs <- .knit_fig_size()
  if ((is.null(fs$width) && is.null(fs$height)) || !.has_default_size(x)) {
    return(plot_svg(x, scaling = "fit", inline = TRUE))
  }
  svg <- plot_svg(x, width = fs$width, height = fs$height, inline = TRUE)
  sub(
    '(<svg[^>]*?)width="([0-9.]+)" height="[0-9.]+"',
    '\\1width="100%" height="100%" style="max-width:\\2px"',
    svg
  )
}

# The figure size the current chunk asks for: `fig.width` / `fig.height`
# (inches) where the chunk's value differs from the document default
# (`knitr::opts_chunk`), NULL otherwise. Outside a chunk both are NULL.
.knit_fig_size <- function() {
  pick <- function(o) {
    cur <- knitr::opts_current$get(o)
    ok <- is.numeric(cur) && length(cur) == 1L && is.finite(cur) && cur > 0
    if (ok && !identical(cur, knitr::opts_chunk$get(o))) as.double(cur)
  }
  list(width = pick("fig.width"), height = pick("fig.height"))
}

# Was the plot left at its default size? A plot from `vplot()` without
# `width` / `height` is 6 x 4 inches, and a composition of such plots without
# its own size is 6 x 4 per grid cell. A plot set to exactly that size counts
# as default-sized too; anything else (a sparkline, a table, a plot or
# composition given a size) has an explicit size.
.has_default_size <- function(x) {
  if (S7::S7_inherits(x, PlotSpec)) {
    return(x@width == 6 && x@height == 4)
  }
  if (S7::S7_inherits(x, PlotComposition)) {
    return(
      isTRUE(all.equal(c(x@width, x@height), c(x@ncol * 6, x@nrow * 4)))
    )
  }
  FALSE
}

.register_knit_print <- function() {
  if (!requireNamespace("knitr", quietly = TRUE)) {
    return(invisible())
  }
  ns <- asNamespace("knitr")
  for (cls in c(
    "vellumplot::PlotSpec",
    "vellumplot::PlotComposition",
    "vellumplot::VTable"
  )) {
    registerS3method("knit_print", cls, .knit_print_plot, envir = ns)
  }
  invisible()
}
