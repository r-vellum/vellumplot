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
  if (length(par)) out <- paste0(out, "  [", paste(par, collapse = ", "), "]")
  out
}

.format_scale <- function(s) {
  bits <- paste0(s@aesthetic, ": ", s@type)
  if (!is.null(s@domain)) {
    bits <- paste0(bits, " domain[", paste(format(s@domain), collapse = ", "), "]")
  }
  if (!is.null(s@name)) bits <- paste0(bits, " name=", s@name)
  bits
}

# Print a readable tree: data dims, page size, layers, declared scales.
.print_plotspec <- function(x, ...) {
  d <- x@data
  cli::cli_text("{.cls PlotSpec} {.field {nrow(d)}}x{.field {ncol(d)}} ({ncol(d)} column{?s}), page {x@width}x{x@height} in")
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
  invisible(x)
}

# Registered as an S3 print method for the S7 class via S7::methods_register()
# in .onLoad().
S7::method(print, PlotSpec) <- .print_plotspec
