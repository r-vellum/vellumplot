#' Facet strip labellers
#'
#' Labellers turn facet values into the text of the facet strips. Pass one as
#' the `labeller` argument of [facet_wrap()] or [facet_grid()].
#'
#' * `label_value()` shows the value itself (`"4"`).
#' * `label_both()` shows the variable name and the value (`"cyl: 4"`).
#' * `label_parsed()` reads each value as R's plotmath notation and typesets
#'   it as math (`"alpha[1]"` shows as a subscripted alpha), as ggplot2's
#'   `label_parsed()` does; see [plotmath()]. With several facet variables,
#'   `multi_line = FALSE` joins them as plotmath's `list(a, b)`.
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
#' The declarative labellers (`label_value`, `label_both`, `label_parsed`,
#' `label_wrap_gen()` and lookup tables over one of the first three) survive
#' [as_spec()]; an arbitrary function does not.
#'
#' A labeller is a function of one argument, `labels`: a data frame with one
#' character column per facet variable (named after it) and one row per strip.
#' It returns a list of character vectors, one per line of the strip (or of
#' lists of rich labels, [md()] or [tex()], as `label_parsed()` does). With
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
#'   vectors, `label_parsed()` a list of lists of [tex()] labels. `label_wrap_gen()` and `as_labeller()` return a labeller
#'   function.
#' @seealso [facet_wrap()], [facet_grid()]
#' @examples
#' p <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
#' p |> facet_wrap(~cyl, labeller = label_both)
#' p |> facet_grid(am ~ cyl, labeller = label_both)
#'
#' # values in plotmath notation, typeset as math
#' d <- data.frame(x = 1:4, y = 1:4, g = rep(c("alpha[1]", "sqrt(beta)"), 2))
#' vplot(d) |>
#'   mark_point(x = x, y = y) |>
#'   facet_wrap(~g, labeller = label_parsed)
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
attr(label_value, "vp_spec") <- list(type = "value")

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
attr(label_both, "vp_spec") <- list(type = "both")

#' @rdname labellers
#' @export
label_parsed <- function(labels, multi_line = TRUE) {
  labels <- .labeller_values(labels)
  if (!isTRUE(multi_line) && length(labels) > 1L) {
    labels <- list(paste0("list(", .join_labels(labels), ")"))
  } else if (!isTRUE(multi_line)) {
    labels <- unname(labels)
  }
  lapply(labels, function(v) {
    out <- vellum::plotmath(v)
    if (length(v) == 1L) list(out) else out
  })
}
class(label_parsed) <- c("function", "labeller")
attr(label_parsed, "vp_spec") <- list(type = "parsed")

#' @rdname labellers
#' @export
label_wrap_gen <- function(width = 25, multi_line = TRUE) {
  if (!is.numeric(width) || length(width) != 1L || is.na(width) || width < 1) {
    cli::cli_abort("{.arg width} must be a positive number.")
  }
  force(multi_line)
  .new_labeller(
    function(labels) {
      lapply(label_value(labels, multi_line = multi_line), function(v) {
        vapply(
          strwrap(v, width = width, simplify = FALSE),
          paste,
          character(1),
          collapse = "\n"
        )
      })
    },
    spec = list(
      type = "wrap",
      width = as.double(width),
      multi_line = isTRUE(multi_line)
    )
  )
}

#' @rdname labellers
#' @export
as_labeller <- function(x, default = label_value, multi_line = TRUE) {
  if (.is_labeller(x)) {
    return(x)
  }
  default <- as_labeller(default)
  force(multi_line)
  spec <- NULL
  per_value <- if (is.function(x)) {
    function(v, fallback) as.character(x(v))
  } else if (is.character(x) && !is.null(names(x))) {
    # A lookup table over a declarative default is itself declarative, so the
    # spec serializer can carry it (VP-44).
    default_spec <- .labeller_spec(default)
    if (!is.null(default_spec)) {
      spec <- list(
        type = "lookup",
        values = as.list(x),
        default = default_spec,
        multi_line = isTRUE(multi_line)
      )
    }
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
  }, spec = spec)
}

# Mark a function as a labeller (takes the facet-values data frame), so
# `as_labeller()` passes it through instead of wrapping it as a value mapper.
# `spec`, when given, is the labeller's declarative description (a plain list
# with a `type`), which lets `as_spec()` serialize it; arbitrary functions
# have none.
.new_labeller <- function(f, spec = NULL) {
  class(f) <- c("function", "labeller")
  attr(f, "vp_spec") <- spec
  f
}

# The declarative description of a labeller (see `.new_labeller()`), or NULL
# for an arbitrary function.
.labeller_spec <- function(labeller) {
  if (!.is_labeller(labeller)) {
    return(NULL)
  }
  attr(labeller, "vp_spec", exact = TRUE)
}

# Rebuild a labeller from its declarative description (the inverse of
# `.labeller_spec()`), as read back from a spec.
.labeller_from_spec <- function(spec) {
  type <- spec$type %||% ""
  multi_line <- !isFALSE(spec$multi_line)
  switch(
    type,
    value = label_value,
    both = label_both,
    parsed = label_parsed,
    wrap = label_wrap_gen(as.double(spec$width), multi_line = multi_line),
    lookup = {
      vals <- spec$values
      lookup <- vapply(vals, function(v) as.character(v)[[1]], character(1))
      names(lookup) <- names(vals)
      as_labeller(
        lookup,
        default = .labeller_from_spec(spec$default %||% list(type = "value")),
        multi_line = multi_line
      )
    },
    cli::cli_abort("Unknown facet labeller type {.val {type}} in spec.")
  )
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
  if (any(vapply(out, is.list, logical(1)))) {
    # Rich lines (label_parsed()): one rich label per strip.
    out <- lapply(out, function(v) {
      v <- if (is.list(v)) v else as.list(as.character(v))
      v[rep_len(seq_along(v), length(parts))]
    })
    return(lapply(seq_along(parts), function(i) {
      .stack_strip_lines(lapply(out, `[[`, i))
    }))
  }
  out <- lapply(out, function(v) rep_len(as.character(v), length(parts)))
  do.call(paste, c(unname(out), sep = "\n"))
}

# One strip's lines as one rich label: a single line as is, several stacked as
# an md() label of inline formulas (and plain text), one per line.
.stack_strip_lines <- function(lines) {
  if (length(lines) == 1L) {
    return(.as_rich_list(lines)[[1]])
  }
  src <- vapply(
    lines,
    function(l) {
      if (inherits(l, "vellum::vellum_tex_label")) {
        if (nzchar(l@tex)) paste0("$", l@tex, "$") else ""
      } else if (is.character(l)) {
        l
      } else {
        cli::cli_abort(
          "A facet labeller can stack {.fn tex} labels and text, not {.cls {class(l)[1]}} labels."
        )
      }
    },
    character(1)
  )
  vellum::md(paste(src, collapse = "\n"))
}

# A probe string with as many lines as the tallest of `labs`, for sizing a
# strip track.
.strip_probe <- function(labs, fontsize = 10) {
  if (is.list(labs)) {
    # Rich strips: the tallest label itself (a plain one by its line count).
    cand <- lapply(labs, function(l) if (.is_rich(l)) l else .strip_probe(l))
    h <- vapply(
      cand,
      function(l) vellum::vl_strheight(l, fontsize = fontsize, unit = "pt"),
      numeric(1)
    )
    return(cand[[which.max(h)]])
  }
  n <- max(1L, lengths(strsplit(as.character(labs), "\n", fixed = TRUE)))
  paste(rep("Ag", n), collapse = "\n")
}
