#' @include classes.R
NULL

#' Control a scale's legend
#'
#' `guides()` overrides the legend (guide) for one or more aesthetics without
#' respelling the whole `scale_*()`. Pass `"none"` (or [guide_none()]) to hide a
#' legend, or [guide_legend()] to tweak it (reverse the key order, override the
#' title). Applies to the non-position legends (`color`/`fill`, `size`, `shape`,
#' `alpha`, `linetype`); position axes are unaffected.
#'
#' @param plot A [PlotSpec].
#' @param ... Named by aesthetic, e.g. `guides(color = "none", shape = guide_legend(reverse = TRUE))`.
#' @param title An axis/legend title override, or `NULL` to keep the default.
#' @param reverse Reverse the order of the legend keys (discrete legends).
#' @return `guides()`: the modified [PlotSpec]. `guide_none()` / `guide_legend()`:
#'   a guide specification for use inside `guides()`.
#' @examples
#' vplot(mtcars) |>
#'   mark_point(x = wt, y = mpg, color = factor(cyl)) |>
#'   guides(color = "none")
#'
#' vplot(mtcars) |>
#'   mark_point(x = wt, y = mpg, color = factor(cyl)) |>
#'   guides(color = guide_legend(reverse = TRUE))
#' @export
guides <- function(plot, ...) {
  .check_plot(plot)
  args <- list(...)
  nm <- names(args)
  if (is.null(nm) || any(!nzchar(nm))) {
    cli::cli_abort("All arguments to {.fn guides} must be named by aesthetic.")
  }
  for (aes in nm) {
    plot <- .set_guide(plot, aes, args[[aes]])
  }
  plot
}

#' @rdname guides
#' @export
guide_none <- function() "none"

#' @rdname guides
#' @export
guide_legend <- function(title = NULL, reverse = FALSE) {
  list(kind = "legend", title = title, reverse = isTRUE(reverse))
}

# Attach a guide spec to the scale for `aesthetic`: update the existing scale if
# one is declared, else add a guide-only scale (its type/palette are derived from
# the data at train time, exactly as if no scale had been declared).
.set_guide <- function(plot, aesthetic, guide) {
  aesthetic <- .canonical_lim_aes(aesthetic)
  idx <- which(vapply(
    plot@scales,
    function(s) identical(s@aesthetic, aesthetic),
    logical(1)
  ))
  if (length(idx)) {
    plot@scales[[idx[length(idx)]]]@guide <- guide
  } else {
    s <- ScaleSpec(aesthetic = aesthetic)
    s@guide <- guide
    plot@scales <- c(plot@scales, list(s))
  }
  plot
}

# Apply a guide spec to a trained (non-position) scale: `"none"` drops the
# legend; `guide_legend()` can rename it or reverse the key order.
.apply_guide <- function(trained, guide) {
  if (is.null(guide) || is.null(trained)) {
    return(trained)
  }
  if (identical(guide, "none")) {
    trained$no_guide <- TRUE
    return(trained)
  }
  if (is.list(guide)) {
    if (!is.null(guide$title)) {
      trained$name <- guide$title
    }
    if (isTRUE(guide$reverse)) {
      trained <- .reverse_guide(trained)
    }
  }
  trained
}

# Reverse a legend's key order (display only; the data -> aesthetic mapping is
# unchanged). Reverses the paired display vectors for the guide's kind.
.reverse_guide <- function(tr) {
  rev_fields <- function(tr, fields) {
    for (f in fields) {
      if (!is.null(tr[[f]])) tr[[f]] <- rev(tr[[f]])
    }
    tr
  }
  switch(tr$kind,
    discrete = rev_fields(tr, c("levels", "labels", "colors")),
    binned = rev_fields(tr, c("levels", "labels", "colors", "breaks")),
    shape = rev_fields(tr, c("levels", "shapes")),
    linetype = rev_fields(tr, c("levels", "linetypes")),
    rev_fields(tr, c(
      "legend_breaks", "legend_labels", "legend_sizes",
      "legend_alphas", "legend_widths"
    ))
  )
}
