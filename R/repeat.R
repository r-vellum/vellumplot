#' @include classes.R concat.R
NULL

#' Repeat a view across fields
#'
#' Replicate a plot, re-pointing one or more encodings at a different data field
#' each time, and arrange the copies as a composition (like [concat()], so each
#' sub-plot keeps its own scales and axes). Supply each aesthetic as a character
#' vector of column names; all vectors must be the same length `N`, and are
#' zipped to produce `N` sub-plots.
#'
#' @param plot A [PlotSpec]; the repeated aesthetic(s) are set on every layer
#'   (added if not already mapped).
#' @param ... Named aesthetics, each a character vector of field names, e.g.
#'   `x = c("wt", "hp", "disp")`.
#' @param ncol,nrow Grid dimensions (passed to [concat()]).
#' @return A `PlotComposition`.
#' @examples
#' repeat_(vplot(mtcars) |> mark_point(y = mpg), x = c("wt", "hp", "disp"))
#' @export
repeat_ <- function(plot, ..., ncol = NULL, nrow = NULL) {
  .check_plot(plot)
  fields <- rlang::list2(...)
  nms <- names(fields)
  if (!length(fields) || is.null(nms) || any(!nzchar(nms))) {
    cli::cli_abort(
      "{.fn repeat_} needs named aesthetics, e.g. {.code x = c(\"wt\", \"hp\")}."
    )
  }
  fields <- lapply(fields, as.character)
  if (length(unique(lengths(fields))) != 1L) {
    cli::cli_abort(
      "All field vectors in {.fn repeat_} must be the same length."
    )
  }
  n <- length(fields[[1]])

  # Set each repeated aesthetic on every layer (adding it when absent, e.g. the
  # canonical `mark_point(y = mpg)` then repeat `x`, or re-pointing it when
  # present). The new field's symbol resolves against the data via data-masking.
  specs <- lapply(seq_len(n), function(i) {
    s <- plot
    s@layers <- lapply(s@layers, function(L) {
      for (a in nms) {
        L@encoding[[a]] <- channel(
          expr = rlang::new_quosure(
            rlang::sym(fields[[a]][i]),
            rlang::empty_env()
          )
        )
      }
      L
    })
    s
  })
  do.call(concat, c(specs, list(ncol = ncol, nrow = nrow)))
}
