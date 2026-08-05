#' @include classes.R
NULL

# A short text label for a channel's expression, e.g. `wt` or `factor(cyl)`.
.channel_label <- function(ch) rlang::as_label(rlang::quo_get_expr(ch@expr))

# A printable form of a title/label value. Plain strings print verbatim; a rich
# `md()` object (a `vellum_md_label`) is shown as a placeholder so `paste0()`
# does not mangle it in summary output.
.lab_str <- function(x) if (is.character(x)) x else "<rich text>"

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
    return(knitr::asis_output(plot_svg(x, scaling = "fit", inline = TRUE)))
  }
  print(x)
  invisible(x)
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
