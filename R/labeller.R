#' Facet strip labellers
#'
#' Labellers turn facet values into the text of the facet strips. Pass one as
#' the `labeller` argument of [facet_wrap()] or [facet_grid()].
#'
#' * `label_value()` shows the value itself (`"4"`).
#' * `label_both()` shows the variable name and the value (`"cyl: 4"`).
#' * `label_wrap_gen()` returns a labeller that wraps long values onto several
#'   lines of at most `width` characters.
#' * `as_labeller()` builds a labeller from a lookup table (a named character
#'   vector, `c("4" = "Four cylinders")`) or from a function that takes the
#'   values and returns their labels (`toupper`). Values the lookup table
#'   does not name keep the `default` labeller's text.
#'
#' A named character vector or a plain function can also be passed straight to
#' `labeller =`; it goes through `as_labeller()`.
#'
#' A labeller is a function of one argument, `labels`: a data frame with one
#' character column per facet variable (named after it) and one row per strip.
#' It returns a list of character vectors, one per line of the strip. With
#' several facet variables on one side, `multi_line = TRUE` puts each variable
#' on its own line of the strip, and `multi_line = FALSE` joins them into one
#' line, separated by `", "`.
#'
#' @param labels A data frame of facet values, one column per facet variable.
#' @param multi_line Whether each facet variable gets its own line in the
#'   strip (`TRUE`), or all of them share one line (`FALSE`).
#' @param sep For `label_both()`, the text between the variable name and the
#'   value.
#' @param width For `label_wrap_gen()`, the maximum number of characters per
#'   line.
#' @param x For `as_labeller()`, a named character vector (a lookup table from
#'   value to label), a function from values to labels, or a labeller (returned
#'   as is).
#' @param default For `as_labeller()`, the labeller for values a lookup table
#'   does not name.
#' @return `label_value()` and `label_both()` return a list of character
#'   vectors. `label_wrap_gen()` and `as_labeller()` return a labeller
#'   function.
#' @seealso [facet_wrap()], [facet_grid()]
#' @examples
#' p <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
#' p |> facet_wrap(~cyl, labeller = label_both)
#' p |> facet_grid(am ~ cyl, labeller = label_both)
#'
#' # a lookup table, for one side or both
#' p |> facet_wrap(~am, labeller = c(`0` = "automatic", `1` = "manual"))
#' p |> facet_wrap(~am, labeller = as_labeller(c(`0` = "automatic"), label_both))
#'
#' # long values wrapped onto several lines
#' df <- data.frame(
#'   x = 1:4,
#'   y = 1:4,
#'   g = rep(c("a rather long group name", "short"), 2)
#' )
#' vplot(df) |>
#'   mark_point(x = x, y = y) |>
#'   facet_wrap(~g, labeller = label_wrap_gen(10))
#' @name labellers
NULL

#' @rdname labellers
#' @export
label_value <- function(labels, multi_line = TRUE) {
  labels <- .labeller_values(labels)
  if (isTRUE(multi_line)) {
    labels
  } else {
    list(.join_labels(labels))
  }
}
class(label_value) <- c("function", "labeller")

#' @rdname labellers
#' @export
label_both <- function(labels, multi_line = TRUE, sep = ": ") {
  vars <- names(labels)
  labels <- .labeller_values(labels)
  if (isTRUE(multi_line)) {
    Map(function(v, nm) paste0(nm, sep, v), labels, vars)
  } else {
    list(paste0(paste(vars, collapse = ", "), sep, .join_labels(labels)))
  }
}
class(label_both) <- c("function", "labeller")

#' @rdname labellers
#' @export
label_wrap_gen <- function(width = 25, multi_line = TRUE) {
  if (!is.numeric(width) || length(width) != 1L || is.na(width) || width < 1) {
    cli::cli_abort("{.arg width} must be a positive number.")
  }
  force(multi_line)
  .new_labeller(function(labels) {
    lapply(label_value(labels, multi_line = multi_line), function(v) {
      vapply(
        strwrap(v, width = width, simplify = FALSE),
        paste,
        character(1),
        collapse = "\n"
      )
    })
  })
}

#' @rdname labellers
#' @export
as_labeller <- function(x, default = label_value, multi_line = TRUE) {
  if (.is_labeller(x)) {
    return(x)
  }
  default <- as_labeller(default)
  force(multi_line)
  per_value <- if (is.function(x)) {
    function(v, fallback) as.character(x(v))
  } else if (is.character(x) && !is.null(names(x))) {
    function(v, fallback) {
      hit <- unname(x[v])
      ifelse(is.na(hit) | !v %in% names(x), fallback, hit)
    }
  } else {
    cli::cli_abort(
      "{.arg x} must be a labeller, a function, or a named character vector."
    )
  }
  .new_labeller(function(labels) {
    vals <- .labeller_values(labels)
    fallback <- default(labels)
    # A default that collapses the variables (multi_line = FALSE) leaves no
    # per-variable fallback; use the raw values then.
    if (length(fallback) != length(vals)) {
      fallback <- vals
    }
    out <- Map(per_value, vals, fallback)
    if (isTRUE(multi_line)) out else list(.join_labels(out))
  })
}

# Mark a function as a labeller (takes the facet-values data frame), so
# `as_labeller()` passes it through instead of wrapping it as a value mapper.
.new_labeller <- function(f) {
  class(f) <- c("function", "labeller")
  f
}

.is_labeller <- function(x) is.function(x) && inherits(x, "labeller")

# A labeller's input as a plain list of character vectors, one per variable.
.labeller_values <- function(labels) {
  lapply(as.list(labels), as.character)
}

# Join per-variable labels into one line per strip.
.join_labels <- function(labels) {
  do.call(paste, c(unname(labels), sep = ", "))
}

# Validate and normalise a facet `labeller =` argument: NULL (the default
# strip text), a labeller, or anything `as_labeller()` accepts.
.as_facet_labeller <- function(labeller, call = rlang::caller_env()) {
  if (is.null(labeller) || .is_labeller(labeller)) {
    return(labeller)
  }
  if (
    is.function(labeller) ||
      (is.character(labeller) && !is.null(names(labeller)))
  ) {
    return(as_labeller(labeller))
  }
  cli::cli_abort(
    c(
      "{.arg labeller} must be a labeller, a function, or a named character vector.",
      i = "For example {.code labeller = label_both}."
    ),
    call = call
  )
}

# The strip text for one facet dimension. `quos` are the dimension's facet
# quosures, `parts` the per-strip values (one character vector per strip, one
# entry per quosure, as `.facet_dim()` records them) and `levels` the default
# text. A NULL labeller keeps the default; otherwise the labeller's lines are
# stacked with newlines.
.strip_labels <- function(labeller, quos, parts, levels) {
  if (is.null(labeller) || !length(quos)) {
    return(levels)
  }
  vars <- vapply(
    quos,
    function(q) rlang::as_label(rlang::quo_get_expr(q)),
    character(1)
  )
  cols <- lapply(seq_along(quos), function(k) {
    vapply(parts, function(p) p[[k]], character(1))
  })
  df <- as.data.frame(
    rlang::set_names(cols, make.unique(vars)),
    stringsAsFactors = FALSE,
    optional = TRUE
  )
  out <- labeller(df)
  if (!is.list(out) || !length(out)) {
    cli::cli_abort(
      "A facet labeller must return a list of character vectors."
    )
  }
  out <- lapply(out, function(v) rep_len(as.character(v), length(parts)))
  do.call(paste, c(unname(out), sep = "\n"))
}

# A probe string with as many lines as the tallest of `labs`, for sizing a
# strip track.
.strip_probe <- function(labs) {
  n <- max(1L, lengths(strsplit(as.character(labs), "\n", fixed = TRUE)))
  paste(rep("Ag", n), collapse = "\n")
}
