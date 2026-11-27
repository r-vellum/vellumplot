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
# (`scaling = "fit"`), at the plot's own aspect. The chunk's figure size
# (`.knit_fig_size()`) sizes the dimensions a plot was given no value for
# (`.size_defaults()`): it is laid out at that size, and a chunk-set
# `fig-width` also caps the figure at that width (CSS `max-width`, 96 px per
# inch), so it still shrinks to a narrower column but a small figure is not
# stretched to fill it. A chunk's `out-width` becomes the figure's CSS width.
# A plot given a size keeps it and fills the column as before (VP-165).
.knit_svg <- function(x) {
  fs <- .knit_fig_size()
  free <- .size_defaults(x)
  w <- if (free[["width"]]) fs$width
  h <- NULL
  if (free[["height"]]) {
    h <- if (!is.null(fs$asp)) (w %||% x@width) * fs$asp else fs$height
  }
  svg <- plot_svg(x, width = w, height = h, scaling = "fit", inline = TRUE)
  style <- c(
    if (!is.null(fs$out_width)) paste0("width:", fs$out_width),
    if (!is.null(w)) sprintf("max-width:%spx", format(w * 96, digits = 7))
  )
  if (!length(style)) {
    return(svg)
  }
  sub(
    '(<svg[^>]*?width="100%" height="100%")',
    paste0('\\1 style="', paste(style, collapse = ";"), '"'),
    svg
  )
}

# The figure size the current chunk asks for, in inches: `width` and `height`
# where the chunk's value differs from the document's default
# (`knitr::opts_chunk`), else NULL; `asp`, the chunk's own `fig.asp` (else
# NULL), which then sets the height from the width; and `out_width`, the
# chunk's `out.width` (as a CSS length; NULL when unset or knitr's own). A value equal to the document default
# counts as not set. knitr folds a `fig.asp` into the chunk's `fig.height`,
# so under a document-wide `fig.asp` the default height is
# `fig.width * fig.asp`. Outside a chunk everything is NULL.
.knit_fig_size <- function() {
  num <- function(v) is.numeric(v) && length(v) == 1L && is.finite(v) && v > 0
  same <- function(a, b) num(b) && isTRUE(all.equal(a, b))
  cur <- knitr::opts_current$get
  doc <- knitr::opts_chunk$get
  w <- cur("fig.width")
  w0 <- doc("fig.width")
  width <- if (num(w) && !same(w, w0)) as.double(w)
  a <- cur("fig.asp")
  a0 <- doc("fig.asp")
  asp <- if (num(a) && !same(a, a0)) as.double(a)
  height <- NULL
  if (is.null(asp)) {
    h <- cur("fig.height")
    h0 <- if (num(a0) && num(w0)) w0 * a0 else doc("fig.height")
    height <- if (num(h) && !same(h, h0)) as.double(h)
  }
  ow <- cur("out.width")
  # under `fig.retina`, knitr fills an unset `out.width` with the figure's
  # pixel width (`fig.width * dpi`, before it scales `dpi` by the retina
  # factor); that one is not the author's
  r <- cur("fig.retina")
  auto <- num(ow) &&
    num(w) &&
    num(cur("dpi")) &&
    num(r) &&
    r != 1 &&
    (same(ow, w * cur("dpi") / r) || same(ow, w * cur("dpi")))
  out_width <- if (auto) {
    NULL
  } else if (is.numeric(ow) && length(ow) == 1L && is.finite(ow)) {
    paste0(ow, "px")
  } else if (is.character(ow) && length(ow) == 1L && nzchar(ow)) {
    ow
  }
  list(width = width, height = height, asp = asp, out_width = out_width)
}

# Which of the plot's dimensions were left at their default, so a knitr chunk
# may size them: `vplot()` records the `width` / `height` it was not given
# (attribute `vp_default_size`), and a composition the ones it was not given
# whose first plot left them at the default too. Anything else (a table, a
# sparkline, a spec read back from JSON) counts as sized.
.size_defaults <- function(x) {
  d <- attr(x, "vp_default_size")
  if (
    !is.logical(d) ||
      !all(c("width", "height") %in% names(d)) ||
      !(S7::S7_inherits(x, PlotSpec) || S7::S7_inherits(x, PlotComposition))
  ) {
    return(c(width = FALSE, height = FALSE))
  }
  c(width = isTRUE(d[["width"]]), height = isTRUE(d[["height"]]))
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
