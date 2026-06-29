#' @include classes.R
NULL

# A short text label for a channel's expression, e.g. `wt` or `factor(cyl)`.
.channel_label <- function(ch) rlang::as_label(rlang::quo_get_expr(ch@expr))

# One line per layer: `mark_point(x = wt, y = mpg, color = hp)  [size = 3]`.
.format_layer <- function(layer) {
  enc <- vapply(
    names(layer@encoding),
    function(nm) paste0(nm, " = ", .channel_label(layer@encoding[[nm]])),
    character(1)
  )
  par <- vapply(
    names(layer@params),
    function(nm) paste0(nm, " = ", format(layer@params[[nm]])),
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
    bits <- paste0(bits, " name=", s@name)
  }
  bits
}

# Print a readable tree: data dims, page size, layers, declared scales.
.print_plotspec <- function(x, ...) {
  d <- x@data
  cli::cli_text(
    "{.cls PlotSpec} {.field {nrow(d)}}x{.field {ncol(d)}} ({ncol(d)} column{?s}), page {x@width}x{x@height} in"
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
      function(nm) paste0(nm, ": ", x@labels[[nm]]),
      character(1)
    ))
  }
  if (!is.null(x@facet)) {
    f <- x@facet
    lab <- function(qs) {
      paste(
        vapply(
          qs,
          function(q) rlang::as_label(rlang::quo_get_expr(q)),
          character(1)
        ),
        collapse = " + "
      )
    }
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
    cli::cli_text("coord: {paste(c(co@kind, lims), collapse = ' ')}")
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
