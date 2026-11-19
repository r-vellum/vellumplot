#' @include classes.R
NULL

# A faceting specification: a `type` ("wrap" | "grid") and the row/col faceting
# variables (lists of quosures). For wrap, `cols` holds the variable(s) and
# `rows` is empty; for grid, both may be set.
FacetSpec <- S7::new_class(
  "FacetSpec",
  package = "vellumplot",
  properties = list(
    type = S7::class_character,
    rows = S7::new_property(S7::class_list, default = list()),
    cols = S7::new_property(S7::class_list, default = list()),
    ncol = S7::new_property(S7::class_any, default = NULL),
    nrow = S7::new_property(S7::class_any, default = NULL),
    labeller = S7::new_property(S7::class_any, default = NULL) # labeller | NULL
  )
)

# Split an `a + b + c` call into a list of its term expressions.
.split_plus <- function(expr) {
  if (is.call(expr) && identical(expr[[1]], quote(`+`))) {
    c(.split_plus(expr[[2]]), .split_plus(expr[[3]]))
  } else {
    list(expr)
  }
}

# Quosures for one side of a faceting formula (`.` or empty -> none).
.side_quos <- function(side, env) {
  if (is.null(side) || identical(side, quote(.))) {
    return(list())
  }
  lapply(.split_plus(side), function(t) rlang::new_quosure(t, env))
}

# Translate a `scales=` argument into the resolve lattice (free => independent).
.apply_free <- function(plot, scales) {
  if (scales %in% c("free_x", "free")) {
    plot@resolve[["x"]] <- "independent"
  }
  if (scales %in% c("free_y", "free")) {
    plot@resolve[["y"]] <- "independent"
  }
  plot
}

#' Facet a plot into a grid of panels
#'
#' `facet_wrap()` lays panels out in a ribbon wrapped into `ncol`/`nrow`;
#' `facet_grid()` arranges them on a 2-D grid defined by a `rows ~ cols`
#' formula. Position scales are shared across panels by default (so axes align);
#' pass `scales = "free_x"`, `"free_y"`, or `"free"` to train them per panel.
#' Colour, size and the other non-position scales are shared too (one legend);
#' `resolve_scale(color = "independent")` gives each panel its own.
#'
#' @param plot A [PlotSpec].
#' @param facets A faceting formula. For `facet_wrap()`, one-sided such as
#'   `~ cyl` (one or more `+`-separated variables); for `facet_grid()`, a
#'   two-sided `rows ~ cols` (use `.` for no faceting on a side).
#' @param ncol,nrow Number of columns/rows for `facet_wrap()`.
#' @param scales One of `"fixed"` (default), `"free_x"`, `"free_y"`, `"free"`.
#' @param labeller How to label the facet strips: a labeller such as
#'   [label_both()], [label_value()] or [label_wrap_gen()], a named character
#'   vector looking values up (`c("4" = "Four")`), or a function from values to
#'   labels (see [as_labeller()]). The default, `NULL`, shows the values,
#'   several variables on one side joined by `", "`.
#' @return The modified [PlotSpec].
#' @seealso [labellers]
#' @examples
#' vplot(mtcars) |> mark_point(x = wt, y = mpg) |> facet_wrap(~cyl)
#' vplot(mtcars) |> mark_point(x = wt, y = mpg) |> facet_grid(am ~ cyl)
#' vplot(mtcars) |>
#'   mark_point(x = wt, y = mpg) |>
#'   facet_grid(am ~ cyl, labeller = label_both)
#' @export
facet_wrap <- function(
  plot,
  facets,
  ncol = NULL,
  nrow = NULL,
  scales = c("fixed", "free_x", "free_y", "free"),
  labeller = NULL
) {
  .check_plot(plot)
  scales <- match.arg(scales)
  labeller <- .as_facet_labeller(labeller)
  if (!rlang::is_formula(facets)) {
    cli::cli_abort("{.arg facets} must be a formula like {.code ~ cyl}.")
  }
  cols <- .side_quos(rlang::f_rhs(facets), environment(facets))
  if (!length(cols)) {
    cli::cli_abort(
      "{.fn facet_wrap} needs at least one variable, e.g. {.code ~ cyl}."
    )
  }
  plot@facet <- FacetSpec(
    type = "wrap",
    cols = cols,
    ncol = ncol,
    nrow = nrow,
    labeller = labeller
  )
  .apply_free(plot, scales)
}

#' @rdname facet_wrap
#' @export
facet_grid <- function(
  plot,
  facets,
  scales = c("fixed", "free_x", "free_y", "free"),
  labeller = NULL
) {
  .check_plot(plot)
  scales <- match.arg(scales)
  labeller <- .as_facet_labeller(labeller)
  if (!rlang::is_formula(facets)) {
    cli::cli_abort("{.arg facets} must be a formula like {.code rows ~ cols}.")
  }
  env <- environment(facets)
  rows <- .side_quos(rlang::f_lhs(facets), env)
  cols <- .side_quos(rlang::f_rhs(facets), env)
  if (!length(rows) && !length(cols)) {
    cli::cli_abort(
      "{.fn facet_grid} needs a variable on at least one side of the formula."
    )
  }
  plot@facet <- FacetSpec(
    type = "grid",
    rows = rows,
    cols = cols,
    labeller = labeller
  )
  .apply_free(plot, scales)
}

#' Resolve scales as shared or independent across panels
#'
#' The scale-resolution lattice (after Vega-Lite): for each aesthetic, choose
#' whether faceted panels share one trained scale or train independent ones.
#' Every scale defaults to `"shared"`; `facet_*(scales = "free_*")` is sugar
#' for setting `x`/`y` to `"independent"`.
#'
#' A non-position aesthetic (`color`, `fill`, `size`, `alpha`, `shape`, `linetype`,
#' `linewidth`, `pattern`, ...) set to `"independent"` is trained in each panel
#' from that panel's data alone, so each panel spends the whole palette or
#' range on its own values, and each panel gets its own legend. The legends
#' are collected in the legend area, titled with the scale title and the
#' panel's strip text (`"hp (cyl: 4)"`). Scales left shared keep their single
#' legend. `colour` is an alias for `color`; `fill` is its own scale. A plot
#' with a single panel ignores the setting.
#'
#' @param plot A [PlotSpec].
#' @param ... Named arguments mapping an aesthetic (`x`, `y`, `color`, `size`,
#'   ...) to `"shared"` or `"independent"`.
#' @return The modified [PlotSpec].
#' @examples
#' vplot(mtcars) |> mark_point(x = wt, y = mpg) |> facet_wrap(~cyl) |>
#'   resolve_scale(y = "independent")
#'
#' # one colour legend per panel
#' vplot(mtcars) |>
#'   mark_point(x = wt, y = mpg, color = factor(gear)) |>
#'   facet_wrap(~cyl) |>
#'   resolve_scale(color = "independent")
#' @export
resolve_scale <- function(plot, ...) {
  .check_plot(plot)
  args <- list(...)
  nms <- names(args)
  if (length(args) && (is.null(nms) || any(!nzchar(nms)))) {
    cli::cli_abort(
      "All arguments to {.fn resolve_scale} must be named, e.g. {.code resolve_scale(y = \"independent\")}."
    )
  }
  # British spellings name the same scales.
  nms <- .resolve_aes_names(nms)
  valid <- c("x", "y", .aes_legend_keys())
  bad <- setdiff(nms, valid)
  if (length(bad)) {
    cli::cli_abort(c(
      "{.fn resolve_scale} only accepts the {.val {valid}} aesthetic{?s}.",
      i = "Unknown aesthetic{?s}: {.val {bad}}."
    ))
  }
  for (k in seq_along(nms)) {
    plot@resolve[[nms[[k]]]] <- match.arg(
      args[[k]],
      c("shared", "independent")
    )
  }
  plot
}

.resolve_aes_names <- function(nms) {
  alias <- c(
    colour = "color",
    edge_colour = "edge_color"
  )
  hit <- nms %in% names(alias)
  nms[hit] <- unname(alias[nms[hit]])
  nms
}

# The resolution for an aesthetic ("shared" unless set otherwise).
.resolve_for <- function(spec, aesthetic) {
  spec@resolve[[aesthetic]] %||% "shared"
}
