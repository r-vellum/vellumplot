#' @include classes.R
NULL

# Split captured aesthetic quosures into data-mapped channels vs constant
# params. A quosure whose expression is a bare literal (number, string,
# logical) is a constant aesthetic (e.g. `size = 3`, `color = "red"`); anything
# referring to data (a symbol like `wt`, or a call like `factor(cyl)`) is a
# channel evaluated against the data at compile time.
.split_encodings <- function(quos) {
  encoding <- list()
  params <- list()
  for (nm in names(quos)) {
    q <- quos[[nm]]
    if (rlang::quo_is_null(q)) next
    e <- rlang::quo_get_expr(q)
    if (!is.symbol(e) && !is.call(e)) {
      # syntactic literal -> constant aesthetic
      params[[nm]] <- rlang::eval_tidy(q)
    } else {
      encoding[[nm]] <- channel(expr = q)
    }
  }
  list(encoding = encoding, params = params)
}

# Capture `...` plus the explicit geometry args, append a LayerSpec.
.add_layer <- function(plot, mark, dots, extra = list()) {
  quos <- c(dots, extra)
  split <- .split_encodings(quos)
  layer <- LayerSpec(
    mark = mark,
    encoding = split$encoding,
    params = split$params
  )
  plot@layers <- c(plot@layers, list(layer))
  plot
}

#' Add marks to a plot
#'
#' Each `mark_*()` appends a drawing layer to a [PlotSpec]. Encodings are bare
#' column names (or expressions) captured with tidy evaluation, e.g.
#' `x = wt, y = mpg, color = hp`. Scalar values (e.g. `size = 3`,
#' `color = "red"`) are treated as constant aesthetics rather than data
#' mappings.
#'
#' @param plot A [PlotSpec] (from [vplot()]).
#' @param ... Encodings: named channel expressions such as `x`, `y`, `color`,
#'   `fill`, `size`, `shape`, `alpha`.
#' @param size,shape Convenience arguments for the point size (in mm) / shape;
#'   may be a constant or a mapped expression. One of `"circle"`, `"square"`,
#'   `"triangle"`, `"diamond"`, `"plus"`, `"cross"`.
#' @return The modified [PlotSpec].
#' @examples
#' vplot(mtcars) |> mark_point(x = wt, y = mpg, color = hp)
#' @export
mark_point <- function(plot, ..., size = NULL, shape = NULL) {
  .check_plot(plot)
  .add_layer(plot, "point", rlang::enquos(...),
             rlang::enquos(size = size, shape = shape))
}

#' @rdname mark_point
#' @export
mark_line <- function(plot, ...) {
  .check_plot(plot)
  .add_layer(plot, "line", rlang::enquos(...))
}

#' @rdname mark_point
#' @export
mark_rule <- function(plot, ...) {
  .check_plot(plot)
  .add_layer(plot, "rule", rlang::enquos(...))
}

.check_plot <- function(plot, call = rlang::caller_env()) {
  if (!S7::S7_inherits(plot, PlotSpec)) {
    cli::cli_abort(
      "{.arg plot} must be a {.cls PlotSpec} from {.fn vplot}.",
      call = call
    )
  }
}
