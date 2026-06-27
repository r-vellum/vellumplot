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
    if (is.call(e) && identical(rlang::call_name(e), "after_stat")) {
      # after_stat(expr): a stage-2 channel evaluated against the stat output.
      inner <- rlang::new_quosure(e[[2]], rlang::quo_get_env(q))
      encoding[[nm]] <- channel(expr = inner, after = TRUE)
    } else if (!is.symbol(e) && !is.call(e)) {
      # syntactic literal -> constant aesthetic
      params[[nm]] <- rlang::eval_tidy(q)
    } else {
      encoding[[nm]] <- channel(expr = q)
    }
  }
  list(encoding = encoding, params = params)
}

#' Map an aesthetic to a statistic computed by a stat
#'
#' Used inside a `mark_*()` encoding to reference a variable produced by the
#' layer's statistical transform rather than a raw data column, e.g.
#' `y = after_stat(count)` or `y = after_stat(density)`.
#'
#' @param x An expression in terms of the stat's computed variables.
#' @return Its argument (the marker is interpreted at compile time).
#' @export
after_stat <- function(x) x

# Capture `...` plus the explicit geometry args, append a LayerSpec.
.add_layer <- function(plot, mark, dots, extra = list(),
                       stat = "identity", stat_params = list(),
                       position = "identity") {
  quos <- c(dots, extra)
  split <- .split_encodings(quos)
  layer <- LayerSpec(
    mark = mark,
    encoding = split$encoding,
    params = split$params,
    stat = stat, stat_params = stat_params, position = position
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
#' @param position Position adjustment: `"identity"` (default), `"jitter"`
#'   (points), or `"stack"` / `"dodge"` / `"fill"` (bars).
#' @return The modified [PlotSpec].
#' @examples
#' vplot(mtcars) |> mark_point(x = wt, y = mpg, color = hp)
#' @export
mark_point <- function(plot, ..., size = NULL, shape = NULL, position = "identity") {
  .check_plot(plot)
  .add_layer(plot, "point", rlang::enquos(...),
             rlang::enquos(size = size, shape = shape), position = position)
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

#' @rdname mark_point
#' @details
#' `mark_bar()` draws bars from a zero baseline. With an explicit `y` it uses the
#' `y` values as heights; with no `y` it counts rows per category (the `"count"`
#' stat). When `color`/`fill` is mapped, grouped bars are stacked by default; use
#' `position = "dodge"` for side-by-side bars or `"fill"` to normalise to 1.
#' @export
mark_bar <- function(plot, ..., position = "stack") {
  .check_plot(plot)
  .add_layer(plot, "bar", rlang::enquos(...), position = position)
}

#' Statistical marks
#'
#' Marks that apply a statistical transform before drawing. `mark_histogram()`
#' bins a continuous `x` and draws the per-bin counts as bars. `mark_smooth()`
#' fits a model (`"lm"` for now) of `y` on `x` and draws the fitted line, with a
#' confidence ribbon when `se = TRUE`.
#'
#' @param plot A [PlotSpec].
#' @param ... Encodings (tidy-eval), e.g. `x`, `y`, `color`/`fill`.
#' @param bins Number of histogram bins.
#' @param method Smoothing method; `"lm"` (linear) for now.
#' @param se Draw a confidence ribbon around the smooth?
#' @param level Confidence level for the ribbon.
#' @param position Position adjustment for the histogram bars (`"stack"`,
#'   `"dodge"`, `"fill"`).
#' @return The modified [PlotSpec].
#' @examples
#' vplot(mtcars) |> mark_histogram(x = mpg, bins = 10)
#' vplot(mtcars) |> mark_point(x = wt, y = mpg) |> mark_smooth(x = wt, y = mpg)
#' @export
mark_histogram <- function(plot, ..., bins = 30, position = "stack") {
  .check_plot(plot)
  .add_layer(plot, "bar", rlang::enquos(...), stat = "bin",
             stat_params = list(bins = bins), position = position)
}

#' @rdname mark_histogram
#' @export
mark_smooth <- function(plot, ..., method = "lm", se = TRUE, level = 0.95) {
  .check_plot(plot)
  .add_layer(plot, "smooth", rlang::enquos(...), stat = "smooth",
             stat_params = list(method = method, se = se, level = level))
}

.check_plot <- function(plot, call = rlang::caller_env()) {
  if (!S7::S7_inherits(plot, PlotSpec)) {
    cli::cli_abort(
      "{.arg plot} must be a {.cls PlotSpec} from {.fn vplot}.",
      call = call
    )
  }
}
